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
const int N_EPI_COMPARTMENTS = 4;
const int N_DATA_COMPARTMENTS = 1;
const std::vector<size_t> i_DATA_COMPARTMENTS = {4};
const std::vector<size_t> seq_DATA_COMPARTMENTS = {5};
const int N_COMPARTMENTS = N_EPI_COMPARTMENTS + N_DATA_COMPARTMENTS;

// [[dust2::class(daedalus_ode)]]
// [[dust2::time_type(continuous)]]
// [[dust2::parameter(I0, constant = FALSE)]]
// [[dust2::parameter(N, constant = TRUE)]]
// [[dust2::parameter(beta, constant = FALSE)]]
// [[dust2::parameter(sigma, constant = FALSE)]]
// [[dust2::parameter(gamma, constant = FALSE)]]
// [[dust2::parameter(n_strata, constant = FALSE, type = "int")]]
class daedalus_ode {
 public:
  daedalus_ode() = delete;

  using real_type = double;

  /// @brief Shared parameters and values.
  struct shared_state {
    real_type N;
    real_type I0;
    real_type beta;
    real_type sigma;
    real_type gamma;
    int n_strata;
    const std::vector<size_t> i_to_zero;
    Eigen::MatrixXd conmat;
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
    return dust2::packing{{"S", dim_vec},
                          {"E", dim_vec},
                          {"I", dim_vec},
                          {"R", dim_vec},
                          {"cases_inc", dim_vec}};
  }

  /// @brief Initialise shared parameters.
  /// @param pars A list of parameters passed from R.
  /// @return A shared parameters object.
  static shared_state build_shared(cpp11::list pars) {
    const real_type I0 = dust2::r::read_real(pars, "I0", 10);
    const real_type N = dust2::r::read_real(pars, "N", 1000);
    const real_type beta = dust2::r::read_real(pars, "beta", 0.2);
    const real_type sigma = dust2::r::read_real(pars, "sigma", 0.2);
    const real_type gamma = dust2::r::read_real(pars, "gamma", 0.1);
    const int n_strata = dust2::r::read_int(pars, "n_strata", 3);

    // handling compartments to zero
    const std::vector<size_t> i_to_zero =
        daedalus::helpers::zero_which(seq_DATA_COMPARTMENTS, n_strata);

    // handling contact matrix
    const std::vector<size_t> vec_cm_dims(2, n_strata);  // for square matrix
    const dust2::array::dimensions<2> cm_dims(vec_cm_dims.begin());
    Eigen::MatrixXd conmat(n_strata, n_strata);
    dust2::r::read_real_array(pars, cm_dims, conmat.data(), "conmat", true);

    return shared_state{N, I0, beta, sigma, gamma, n_strata, i_to_zero, conmat};
  }

  /// @brief Updated shared parameters.
  /// @param pars A list of parameters passed from R.
  /// @param shared A shared parameter object to update.
  static void update_shared(cpp11::list pars, shared_state &shared) {  // NOLINT
    shared.I0 = dust2::r::read_real(pars, "I0", shared.I0);
    shared.beta = dust2::r::read_real(pars, "beta", shared.beta);
    shared.sigma = dust2::r::read_real(pars, "sigma", shared.sigma);
    shared.gamma = dust2::r::read_real(pars, "gamma", shared.gamma);
    shared.n_strata = dust2::r::read_int(pars, "n_strata", shared.n_strata);
    // TODO(pratik): add conmat update method?
  }

  /// @brief Set initial values of the IVP model.
  /// @param time Time -- not used. Purpose unclear.
  /// @param shared Shared parameter object.
  /// @param state_next Next state as double value.
  static void initial(real_type time, const shared_state &shared,
                      const internal_state &internal,
                      const rng_state_type &rng_state, real_type *state_next) {
    size_t vec_size = shared.n_strata;  // currently a single size_t

    // map an Eigen container
    // NOTE: default mapping is col major (epi/data compartments are cols)
    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, N_COMPARTMENTS>> dx(
        &state_next[0], vec_size, N_COMPARTMENTS);

    // initially all zero, modify S and E
    dx.setZero();
    dx.col(0).setConstant(shared.N - shared.I0);
    dx.col(1).setConstant(shared.I0);
  }

  /// @brief RHS of the ODE model.
  /// @param time Time -- not used.
  /// @param state Pointer to state.
  /// @param shared Shared parameters.
  /// @param state_deriv State change or dX.
  static void rhs(real_type time, const real_type *state,
                  const shared_state &shared, const internal_state &internal,
                  real_type *state_deriv) {
    size_t vec_size = shared.n_strata;  // currently a single size_t

    // map to Eigen containers
    // dx does not need to be set to zero as this is handled by zero_every()
    // seems like
    Eigen::Map<const Eigen::Matrix<double, Eigen::Dynamic, N_COMPARTMENTS>> x(
        &state[0], vec_size, N_COMPARTMENTS);
    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, N_COMPARTMENTS>> dx(
        &state_deriv[0], vec_size, N_COMPARTMENTS);

    const auto rate_SE =
        shared.beta * x.col(0).array() * x.col(2).array() / shared.N;
    const auto rate_EI = shared.sigma * x.col(1).array();
    const auto rate_IR = shared.gamma * x.col(2).array();
    dx.col(0) = -rate_SE;
    dx.col(1) = rate_SE - rate_EI;
    dx.col(2) = rate_EI - rate_IR;
    dx.col(3) = rate_IR;
    dx.col(4) = rate_SE;
  }

  /// @brief Set every value to zero - unclear.
  /// @param shared Shared state -- unused.
  /// @return Probably an array of zeros.
  static auto zero_every(const shared_state &shared) {
    return dust2::zero_every_type<real_type>{
        {1, shared.i_to_zero}};  // zero incidence data compartments
  }
};
