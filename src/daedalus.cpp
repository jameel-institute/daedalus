
// clang-format off
#include <Rcpp.h>
#include <RcppEigen.h>

#include <cmath>
#include <boost/numeric/odeint.hpp>
// clang-format on

/// @brief State type for ODEs.
typedef Eigen::MatrixXd state_type;

/// @brief Model constants.
const int N_AGE_GROUPS = 4L;
const int N_ECON_SECTORS = 45L;
const int N_GROUPS = N_AGE_GROUPS + N_ECON_SECTORS;
const int iS = 0, iE = 1, iIs = 2, iIa = 3, iH = 4, iR = 5, iD = 6, iV = 7,
          idE = 8, idH = 9;

/// @brief
/// @tparam T
/// @param value The value of the opennness coefficient. For an 80% closure of a
/// sector, the openness coefficient would be 0.2.
/// @param flag
/// @return Either `value` when `flag` = 1.0, or 1.0 when `flag` = 0.0.
template <typename T>
T apply_npi(T value, const double &flag) {
  return (1.0 - (1.0 - value) * flag);
}

/// @brief
/// @param deaths
/// @return
const double get_new_deaths(const Eigen::ArrayXd &deaths) {
  return deaths.sum();
}

/// @brief
/// @param deaths
/// @return
const double get_distancing_coef(const double &new_deaths,
                                 const double &rate = 0.001,
                                 const double &lower_limit = 0.2) {
  return std::pow((1.0 - rate), new_deaths) * (1.0 - lower_limit) + lower_limit;
}

/// @brief A simple observer for the integrator
struct observer {
  std::vector<state_type> &m_states;
  std::vector<double> &m_times;

  /// @brief Constructor for the observer
  /// @param states A vector of `state_type`, the model compartments
  /// @param times A vector of doubles, the model times logged
  observer(std::vector<state_type> &states,  // NOLINT
           std::vector<double> &times)       // NOLINT
      : m_states(states), m_times(times) {}

  /// @brief Overloaded operator for the observer structure
  /// @param x The current system state x.
  /// @param t The current system time t.
  void operator()(const state_type &x, double t) {
    size_t t_index = static_cast<size_t>(t);
    m_states[t_index] = x;
    m_times[t_index] = t;
  }
};

/// @brief Struct containing the daedalus epidemic ODE system
struct epidemic_daedalus {
  Rcpp::List model_params;
  double beta, sigma, p_sigma, epsilon, gamma, eta, omega, rho;
  const double t_start, t_end;
  const Eigen::Matrix<double, N_GROUPS, N_GROUPS> contact_matrix;
  Eigen::Matrix<double, N_GROUPS, N_GROUPS> cm_temp;

  Eigen::Array<double, N_GROUPS, 1> sToE, eToIs, eToIa, isToR, iaToR, isToH,
      hToR, hToD, rToS;
  Eigen::Vector<double, N_GROUPS> comm_inf;

  Eigen::Array<double, N_ECON_SECTORS, 1> contacts_work, openness;

  // intervention flag
  double flag = 0.0, new_deaths = 0.0;
  double distancing_coef = 1.0;

  const bool auto_social_distancing;

  /// @brief 
  /// @param model_params 
  /// @param contact_matrix 
  /// @param contacts_work 
  /// @param openness 
  /// @param t_start 
  /// @param t_end 
  /// @param auto_social_distancing 
  epidemic_daedalus(const Rcpp::List &model_params,
                    const Eigen::MatrixXd &contact_matrix,
                    const Eigen::ArrayXd &contacts_work,
                    const Eigen::ArrayXd &openness, const double &t_start,
                    const double &t_end, const bool &auto_social_distancing)
      : model_params(model_params),
        contact_matrix(contact_matrix),
        cm_temp(contact_matrix),
        contacts_work(contacts_work),
        openness(openness),
        t_start(t_start),
        t_end(t_end),
        auto_social_distancing(auto_social_distancing) {}
  
  /// @brief 
  void init_params() {
    beta = model_params["beta"];
    sigma = model_params["sigma"];
    p_sigma = model_params["p_sigma"];
    epsilon = model_params["epsilon"];
    gamma = model_params["gamma_Ia"];
    eta = model_params["eta"];
    omega = model_params["omega"];
    rho = model_params["rho"];
  }

  /// @brief Operator for the default model
  /// @param x The initial state of the population - rows represent age groups
  /// while columns represent compartments
  /// @param dxdt An object of the same type as `x` to hold the current state of
  /// the system
  /// @param t The simulation time
  void operator()(const state_type &x,
                  state_type &dxdt,  // NOLINT
                  const double t) {
    // resize the dxdt vector to the dimensions of x
    dxdt.resize(x.rows(), x.cols());
    dxdt.setZero();

    // NB: Casting initial conditions matrix columns to arrays is necessary
    // for vectorised operations
    // columns are as follows
    // 0|1| 2| 3|4|5|6|7| 8| 9
    // S|E|Is|Ia|H|R|D|V|dE|dH

    // calculate new deaths first to scale social distancing
    // if 'public concern' mechanism is modelled
    hToD = omega * x.col(iH).array();
    new_deaths = get_new_deaths(hToD);
    if (auto_social_distancing) {
      distancing_coef = get_distancing_coef(new_deaths);
    }

    // compartmental transitions without accounting for contacts
    // Susceptible (unvaccinated) to exposed
    comm_inf = (x.col(iIs) + (x.col(iIa).array() * epsilon).matrix());

    sToE = x.col(iS).array();
    sToE.setZero();

    sToE = beta * distancing_coef * x.col(iS).array() *
           (cm_temp * comm_inf).array();

    // set flag based on intervention time
    flag = t > t_start && t < t_end ? 1.0 : 0.0;

    auto workplace_infected = apply_npi(openness, flag) * contacts_work *
                              comm_inf.array().tail<N_ECON_SECTORS>();

    // workplace infections within sectors
    sToE.tail<N_ECON_SECTORS>() += beta * distancing_coef *
                                   x.col(iS).array().tail<N_ECON_SECTORS>() *
                                   workplace_infected;

    eToIs = sigma * p_sigma * x.col(iE).array();
    eToIa = sigma * (1.0 - p_sigma) * x.col(iE).array();
    isToR = gamma * x.col(iIs).array();
    iaToR = gamma * x.col(iIa).array();
    isToH = eta * x.col(iIs).array();
    hToR = gamma * x.col(iH).array();
    rToS = rho * x.col(iR).array();

    Eigen::ArrayXd sToV = x.col(iS).array();
    sToV.setZero();

    dxdt.col(iS) = -sToE + rToS;
    dxdt.col(iE) = sToE - eToIs - eToIa;
    dxdt.col(iIs) = eToIs - isToR - isToH;
    dxdt.col(iIa) = eToIa - iaToR;
    dxdt.col(iH) = isToH - hToD - hToR;
    dxdt.col(iR) = isToR + iaToR - rToS;

    dxdt.col(iD) = hToD;
    dxdt.col(iV) = sToV;

    dxdt.col(idE) = sToE;
    dxdt.col(idH) = hToR;
  }
};

// [[Rcpp::export(name=".model_daedalus_cpp")]]
Rcpp::List model_daedalus_internal(
    const Eigen::MatrixXd &initial_state, const Rcpp::List &params,
    const Eigen::MatrixXd &contact_matrix, const Eigen::ArrayXd &contacts_work,
    const Eigen::ArrayXd &openness, const double &t_start, const double &t_end,
    const bool auto_social_distancing = false,
    const double &time_end = 100.0,  // double required by boost solver
    const double &increment = 1.0) {
  // initial conditions from input
  state_type x = initial_state;

  const size_t n_reps = params.size();

  Rcpp::List output(n_reps);

  // a controlled stepper for constant step sizes
  boost::numeric::odeint::runge_kutta4<
      state_type, double, state_type, double,
      boost::numeric::odeint::vector_space_algebra>
      stepper;

  const size_t n_times = static_cast<size_t>(time_end) + 1;

  // prepare storage containers for the observer
  std::vector<state_type> x_vec(n_times);  // is a vector of MatrixXd
  std::vector<double> times(n_times);

  // create a default epidemic with parameters
  epidemic_daedalus this_model(params[0], contact_matrix, contacts_work,
                               openness, t_start, t_end,
                               auto_social_distancing);

  for (size_t i = 0; i < n_reps; i++) {
    // reset x
    x = initial_state;
    this_model.model_params = params[i];
    this_model.init_params();

    // run the function without assignment
    boost::numeric::odeint::integrate_const(stepper, this_model, x, 0.0,
                                            time_end, increment,
                                            observer(x_vec, times));

    output[i] = Rcpp::List::create(Rcpp::Named("x") = Rcpp::wrap(x_vec),
                                   Rcpp::Named("time") = Rcpp::wrap(times));
  }

  return output;
}
