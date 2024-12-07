
// clang-format off
#include <Rcpp.h>
#include <RcppEigen.h>
#include <epidemics.h>

#include <cmath>
#include <boost/numeric/odeint.hpp>
// clang-format on

template <typename T>
T apply_npi(T value, const double &flag) {
  return (1.0 - (1.0 - value) * flag);
}

/// @brief A simple observer for the integrator
struct observer {
  std::vector<odetools::state_type> &m_states;
  std::vector<double> &m_times;

  /// @brief Constructor for the observer
  /// @param states A vector of `odetools::state_type`, the model compartments
  /// @param times A vector of doubles, the model times logged
  observer(std::vector<odetools::state_type> &states,  // NOLINT
           std::vector<double> &times)                 // NOLINT
      : m_states(states), m_times(times) {}

  /// @brief Overloaded operator for the observer structure
  /// @param x The current system state x.
  /// @param t The current system time t.
  void operator()(const odetools::state_type &x, double t) {
    size_t t_index = static_cast<size_t>(t);
    m_states[t_index] = x;
    m_times[t_index] = t;
  }
};

/// @brief Struct containing the daedalus epidemic ODE system
struct epidemic_daedalus {
  const double beta, sigma, p_sigma, gamma, eta, omega, t_start, t_end;
  const Eigen::Matrix<double, 49, 49> contact_matrix;
  Eigen::Matrix<double, 49, 49> cm_temp;

  Eigen::Array<double, 49, 1> sToE, eToIs, eToIa, isToR, iaToR, isToH, hToR,
      hToD;
  Eigen::Vector<double, 49> comm_inf;
  Eigen::Vector<double, 4> comm_inf_1;

  Eigen::Array<double, 45, 1> contacts_work, openness;

  // intervention flag
  double flag = 0.0;

  /// @brief Constructor for the daedalus epidemic struct
  /// @param model_params Rcpp List of model parameters
  /// @param contact_matrix The population contact matrix
  epidemic_daedalus(const Rcpp::List &model_params,
                    const Eigen::MatrixXd &contact_matrix,
                    const Eigen::ArrayXd &contacts_work,
                    const Eigen::ArrayXd &openness, const double &t_start,
                    const double &t_end)
      : beta(Rcpp::as<double>(model_params["beta"])),
        sigma(Rcpp::as<double>(model_params["sigma"])),
        p_sigma(Rcpp::as<double>(model_params["p_sigma"])),
        gamma(Rcpp::as<double>(model_params["gamma"])),
        eta(Rcpp::as<double>(model_params["eta"])),
        omega(Rcpp::as<double>(model_params["omega"])),
        contact_matrix(contact_matrix),
        cm_temp(contact_matrix),
        contacts_work(contacts_work),
        openness(openness),
        t_start(t_start),
        t_end(t_end) {}

  /// @brief Operator for the default model
  /// @param x The initial state of the population - rows represent age groups
  /// while columns represent compartments
  /// @param dxdt An object of the same type as `x` to hold the current state of
  /// the system
  /// @param t The simulation time
  void operator()(const odetools::state_type &x,
                  odetools::state_type &dxdt,  // NOLINT
                  const double t) {
    // resize the dxdt vector to the dimensions of x
    dxdt.resize(x.rows(), x.cols());
    dxdt.setZero();

    // NB: Casting initial conditions matrix columns to arrays is necessary
    // for vectorised operations
    // columns are as follows
    // 0|1| 2| 3|4|5|6|7| 8| 9
    // S|E|Is|Ia|H|R|D|V|dI|dH

    // compartmental transitions without accounting for contacts
    // Susceptible (unvaccinated) to exposed
    comm_inf = (x.col(2) + x.col(3));

    sToE = x.col(0).array();
    sToE.setZero();

    sToE = beta * x.col(0).array() * (cm_temp * comm_inf).array();

    // set flag based on time
    flag = t > t_start && t < t_end ? 1.0 : 0.0;

    auto workplace_infected =
        apply_npi(openness, flag) * contacts_work * comm_inf.array().tail<45>();

    // workplace infections within sectors
    sToE.tail<45>() += beta * x.col(0).array().tail<45>() * workplace_infected;

    eToIs = sigma * p_sigma * x.col(1).array();
    eToIa = sigma * (1.0 - p_sigma) * x.col(1).array();
    isToR = gamma * x.col(2).array();
    iaToR = gamma * x.col(3).array();
    isToH = eta * x.col(2).array();
    hToR = gamma * x.col(4).array();
    hToD = omega * x.col(4).array();

    Eigen::ArrayXd sToV = x.col(0).array();
    sToV.setZero();

    dxdt.col(0) = -sToE;                 // -β*S
    dxdt.col(1) = sToE - eToIs - eToIa;  // β*S
    dxdt.col(2) = eToIs - isToR - isToH;
    dxdt.col(3) = eToIa - iaToR;
    dxdt.col(4) = isToH - hToD;
    dxdt.col(5) = isToR + iaToR;

    dxdt.col(6) = hToD;
    dxdt.col(7) = sToV;

    dxdt.col(8) = sToE;
    dxdt.col(9) = hToR;
  }
};

// [[Rcpp::export(name=".model_daedalus_cpp")]]
Rcpp::List model_daedalus_internal(
    const Eigen::MatrixXd &initial_state, const Rcpp::List &params,
    const Eigen::MatrixXd &contact_matrix, const Eigen::ArrayXd &contacts_work,
    const Eigen::ArrayXd &openness, const double &t_start, const double &t_end,
    const double &time_end = 100.0,  // double required by boost solver
    const double &increment = 1.0) {
  // initial conditions from input
  odetools::state_type x = initial_state;

  // create a default epidemic with parameters
  epidemic_daedalus this_model(params, contact_matrix, contacts_work, openness,
                               t_start, t_end);

  size_t n_times = static_cast<size_t>(time_end) + 1;

  // prepare storage containers for the observer
  std::vector<odetools::state_type> x_vec(n_times);  // is a vector of MatrixXd
  std::vector<double> times(n_times);

  // a controlled stepper for constant step sizes
  boost::numeric::odeint::runge_kutta4<
      odetools::state_type, double, odetools::state_type, double,
      boost::numeric::odeint::vector_space_algebra>
      stepper;

  // run the function without assignment
  boost::numeric::odeint::integrate_const(stepper, this_model, x, 0.0, time_end,
                                          increment, observer(x_vec, times));

  return Rcpp::List::create(Rcpp::Named("x") = Rcpp::wrap(x_vec),
                            Rcpp::Named("time") = Rcpp::wrap(times));
}
