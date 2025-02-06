// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

// NOTE: header inclusion causes style clashes b/w clang-format and cpplint
// please include system headers in alphabetical order
// clang-format off
#include <daedalus.h>

#include <iterator>
#include <numeric>
#include <vector>

#include <cpp11.hpp>
#include <cpp11eigen.hpp>
#include <dust2/common.hpp>
// clang-format on

// hardcoded as key to model structure
const size_t iS = daedalus::constants::iS, iE = daedalus::constants::iE,
             iIs = daedalus::constants::iIs, iIa = daedalus::constants::iIa,
             iH = daedalus::constants::iH, iR = daedalus::constants::iR,
             iD = daedalus::constants::iD, idE = daedalus::constants::idE,
             idH = daedalus::constants::idH;

// [[dust2::class(daedalus_ode)]]
// [[dust2::time_type(continuous)]]
// [[dust2::parameter(initial_state, constant = TRUE)]]
// [[dust2::parameter(beta, constant = TRUE)]]
// [[dust2::parameter(sigma, constant = TRUE)]]
// [[dust2::parameter(p_sigma, constant = TRUE)]]
// [[dust2::parameter(epsilon, constant = TRUE)]]
// [[dust2::parameter(rho, constant = TRUE)]]
// [[dust2::parameter(eta, constant = TRUE)]]
// [[dust2::parameter(omega, constant = TRUE)]]
// [[dust2::parameter(gamma_Ia, constant = TRUE)]]
// [[dust2::parameter(gamma_Is, constant = TRUE)]]
// [[dust2::parameter(gamma_H, constant = TRUE)]]
// [[dust2::parameter(n_age_groups, constant = TRUE, type = "int")]]
// [[dust2::parameter(n_econ_groups, constant = TRUE, type = "int")]]
// [[dust2::parameter(n_strata, constant = TRUE, type = "int")]]
// [[dust2::parameter(cm, constant = TRUE)]]
// [[dust2::parameter(cm_work, constant = TRUE)]]
// [[dust2::parameter(cm_cons_work, constant = TRUE)]]
class daedalus_ode {
 public:
  daedalus_ode() = delete;

  using real_type = double;

  /// @brief Shared parameters and values. All const as not expected to update.
  struct shared_state {
    // NOTE: n_strata unknown at compile time
    const Eigen::MatrixXd initial_state;
    const real_type beta, sigma, p_sigma, epsilon, rho, gamma_Ia, gamma_Is;
    const Eigen::ArrayXd eta, omega, gamma_H;

    const int n_strata, n_age_groups, n_econ_groups;
    const std::vector<size_t> i_to_zero;
    const Eigen::MatrixXd cm, cm_cons_work;
    const Eigen::ArrayXd cm_work;  // only needed for element-wise mult
  };

  /// @brief Internal state - unclear purpose.
  struct internal_state {};

  // unclear whether dust2/common.hpp links to monty - probably
  // NOTE: do not remove, causes compilation errors
  using rng_state_type = monty::random::generator<real_type>;

  /// @brief How compartments are packed.
  /// @param shared A `shared_state` object, unclear why needed.
  /// @return A custom packing specification object.
  static dust2::packing packing_state(const shared_state &shared) {
    const std::vector<size_t> dim_vec(1, static_cast<size_t>(shared.n_strata));
    return dust2::packing{
        {"S", dim_vec},  {"E", dim_vec},       {"Is", dim_vec},
        {"Ia", dim_vec}, {"H", dim_vec},       {"R", dim_vec},
        {"D", dim_vec},  {"new_inf", dim_vec}, {"new_hosp", dim_vec}};
  }

  /// @brief Initialise shared parameters.
  /// @param pars A list of parameters passed from R.
  /// @return A shared parameters object.
  static shared_state build_shared(cpp11::list pars) {
    // NOTE: default values are all zero
    const real_type beta = dust2::r::read_real(pars, "beta", 0.0);
    const real_type sigma = dust2::r::read_real(pars, "sigma", 0.0);
    const real_type p_sigma = dust2::r::read_real(pars, "p_sigma", 0.0);
    const real_type epsilon = dust2::r::read_real(pars, "epsilon", 0.0);
    const real_type rho = dust2::r::read_real(pars, "rho", 0.0);
    const real_type gamma_Ia = dust2::r::read_real(pars, "gamma_Ia", 0.0);
    const real_type gamma_Is = dust2::r::read_real(pars, "gamma_Is", 0.0);

    // related to number of groups
    // defaults to daedalus fixed values
    const int n_age_groups = dust2::r::read_int(pars, "n_age_groups", 4);
    const int n_econ_groups = dust2::r::read_int(pars, "n_econ_groups", 45);
    const int n_strata = n_age_groups + n_econ_groups;

    // read vector values (all must have size n_strata)
    Eigen::ArrayXd eta(n_strata);
    Eigen::ArrayXd omega(n_strata);
    Eigen::ArrayXd gamma_H(n_strata);

    dust2::r::read_real_vector(pars, n_strata, eta.data(), "eta", true);
    dust2::r::read_real_vector(pars, n_strata, omega.data(), "omega", true);
    dust2::r::read_real_vector(pars, n_strata, gamma_H.data(), "gamma_H", true);

    // convert initial state - needs n_strata etc.
    const std::vector<size_t> vec_state_dims = {
        n_strata, daedalus::constants::N_COMPARTMENTS};  // for square matrix
    const dust2::array::dimensions<2> state_dims(vec_state_dims.begin());
    Eigen::MatrixXd initial_state(n_strata,
                                  daedalus::constants::N_COMPARTMENTS);
    dust2::r::read_real_array(pars, state_dims, initial_state.data(),
                              "initial_state", true);

    // handling contact matrix
    const std::vector<size_t> vec_cm_dims(2, n_strata);  // for square matrix
    const dust2::array::dimensions<2> cm_dims(vec_cm_dims.begin());
    Eigen::MatrixXd cm(n_strata, n_strata);
    dust2::r::read_real_array(pars, cm_dims, cm.data(), "cm", true);

    // handling contacts from consumers to workers
    const std::vector<size_t> vec_cm_cw_dims = {
        n_econ_groups, n_age_groups};  // for rect matrix
    const dust2::array::dimensions<2> cm_cw_dims(vec_cm_cw_dims.begin());
    Eigen::MatrixXd cm_cw(n_econ_groups, n_age_groups);
    dust2::r::read_real_array(pars, cm_cw_dims, cm_cw.data(), "cm_cons_work",
                              true);

    // handling within-sector contacts
    Eigen::ArrayXd cm_work(n_econ_groups);
    dust2::r::read_real_vector(pars, n_econ_groups, cm_work.data(), "cm_work",
                               true);

    // handling compartments to zero
    const std::vector<size_t> i_to_zero = daedalus::helpers::zero_which(
        daedalus::constants::seq_DATA_COMPARTMENTS, n_strata);

    return shared_state{
        initial_state, beta,          sigma,     p_sigma, epsilon, rho,
        gamma_Ia,      gamma_Is,      eta,       omega,   gamma_H, n_strata,
        n_age_groups,  n_econ_groups, i_to_zero, cm,      cm_cw,   cm_work};
  }

  /// @brief Updated shared parameters.
  /// @param pars A list of parameters passed from R.
  /// @param shared A shared parameter object to update.
  static void update_shared(cpp11::list pars, const shared_state &shared) {
    // NOTE: we are setting these constant
  }

  /// @brief Set initial values of the IVP model.
  /// @param time Time -- not used. Purpose unclear.
  /// @param shared Shared parameter object.
  /// @param state_next Next state as double value.
  static void initial(real_type time, const shared_state &shared,
                      const internal_state &internal,
                      const rng_state_type &rng_state, real_type *state_next) {
    // map an Eigen container
    // NOTE: default mapping is col major (epi/data compartments are cols)
    Eigen::Map<Eigen::MatrixXd>(state_next, shared.n_strata,
                                daedalus::constants::N_COMPARTMENTS) =
        shared.initial_state;
  }

  /// @brief RHS of the ODE model.
  /// @param time Time -- not used.
  /// @param state Pointer to state.
  /// @param shared Shared parameters.
  /// @param state_deriv State change or dX.
  static void rhs(real_type time, const real_type *state,
                  const shared_state &shared, const internal_state &internal,
                  real_type *state_deriv) {
    const size_t vec_size = shared.n_strata;
    // map to Eigen containers
    // dx does not need to be set to zero as this is handled by zero_every()
    // seems like
    Eigen::Map<const Eigen::Matrix<double, Eigen::Dynamic,
                                   daedalus::constants::N_COMPARTMENTS>>
        x(&state[0], vec_size, daedalus::constants::N_COMPARTMENTS);
    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic,
                             daedalus::constants::N_COMPARTMENTS>>
        dx(&state_deriv[0], vec_size, daedalus::constants::N_COMPARTMENTS);

    const auto rate_SE = shared.beta * x.col(daedalus::constants::iS).array() *
                         (shared.cm * x.col(daedalus::constants::iIs)).array();
    const auto rate_EI = shared.sigma * x.col(daedalus::constants::iE).array();
    const auto rate_IR =
        shared.gamma_Is * x.col(daedalus::constants::iIs).array();
    dx.col(daedalus::constants::iS) = -rate_SE;
    dx.col(daedalus::constants::iE) = rate_SE - rate_EI;
    dx.col(daedalus::constants::iIs) = rate_EI - rate_IR;
    dx.col(daedalus::constants::iR) = rate_IR;
    dx.col(daedalus::constants::idE) = rate_SE;
  }

  /// @brief Set every value to zero - unclear.
  /// @param shared Shared state -- unused.
  /// @return Probably an array of zeros.
  static auto zero_every(const shared_state &shared) {
    return dust2::zero_every_type<real_type>{
        {1, shared.i_to_zero}};  // zero incidence data compartments
  }
};
