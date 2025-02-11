// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

// NOTE: header inclusion causes style clashes b/w clang-format and cpplint
// please include system headers in alphabetical order
// clang-format off
#include <daedalus.h>

#include <RcppEigen.h>
#include <unsupported/Eigen/CXX11/Tensor>

#include <iterator>
#include <numeric>
#include <vector>

#include <cpp11.hpp>
#include <dust2/common.hpp>
// clang-format on

// [[Rcpp::depends(RcppEigen)]]

// hardcoded as key to model structure
const size_t iS = daedalus::constants::iS, iE = daedalus::constants::iE,
             iIs = daedalus::constants::iIs, iIa = daedalus::constants::iIa,
             iH = daedalus::constants::iH, iR = daedalus::constants::iR,
             iD = daedalus::constants::iD, idE = daedalus::constants::idE,
             idH = daedalus::constants::idH;

// groups = rows, compartments = cols, vax strata = layers
const size_t i_GRPS = daedalus::constants::i_GRPS,
             i_COMPS = daedalus::constants::i_COMPS,
             i_VAX_GRPS = daedalus::constants::i_VAX_GRPS;

// define types for Tensors of known dims
// TODO: remove type as we always use doubles
template <typename T>
using TensorVec = Eigen::Tensor<T, 1>;

template <typename T>
using TensorMat = Eigen::Tensor<T, 2>;

template <typename T>
using TensorAry = Eigen::Tensor<T, 3>;

// [[dust2::class(daedalus_ode)]]
// [[dust2::time_type(continuous)]]
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
    const real_type beta, sigma, p_sigma, epsilon, rho, gamma_Ia, gamma_Is;
    const TensorVec<double> eta, omega, gamma_H;

    const size_t n_strata, n_age_groups, n_econ_groups;
    const std::vector<size_t> i_to_zero;
    const TensorMat<double> cm, cm_cons_work;
    const TensorVec<double> cm_work;  // only needed for element-wise mult
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
    const std::vector<size_t> dim_vec(1, shared.n_strata);
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
    const size_t n_age_groups = dust2::r::read_size(pars, "n_age_groups", 4);
    const size_t n_econ_groups = dust2::r::read_size(pars, "n_econ_groups", 45);
    const size_t n_strata = n_age_groups + n_econ_groups;

    // read vector values (all must have size n_strata)
    TensorVec<double> eta(n_strata);
    TensorVec<double> omega(n_strata);
    TensorVec<double> gamma_H(n_strata);

    dust2::r::read_real_vector(pars, n_strata, eta.data(), "eta", true);
    dust2::r::read_real_vector(pars, n_strata, omega.data(), "omega", true);
    dust2::r::read_real_vector(pars, n_strata, gamma_H.data(), "gamma_H", true);

    // handling contact matrix
    const std::vector<size_t> vec_cm_dims(2, n_strata);  // for square matrix
    const dust2::array::dimensions<2> cm_dims(vec_cm_dims.begin());
    TensorMat<double> cm(n_strata, n_strata);
    dust2::r::read_real_array(pars, cm_dims, cm.data(), "cm", true);

    // handling contacts from consumers to workers
    const std::vector<size_t> vec_cm_cw_dims = {n_econ_groups, n_age_groups};
    const dust2::array::dimensions<2> cm_cw_dims(vec_cm_cw_dims.begin());
    TensorMat<double> cm_cw(n_econ_groups, n_age_groups);
    dust2::r::read_real_array(pars, cm_cw_dims, cm_cw.data(), "cm_cons_work",
                              true);

    // handling within-sector contacts
    TensorVec<double> cm_work(n_econ_groups);
    dust2::r::read_real_vector(pars, n_econ_groups, cm_work.data(), "cm_work",
                               true);

    // handling compartments to zero
    const std::vector<size_t> i_to_zero = daedalus::helpers::zero_which(
        daedalus::constants::seq_DATA_COMPARTMENTS, n_strata);

    return shared_state{
        beta,          sigma,     p_sigma, epsilon, rho,      gamma_Ia,
        gamma_Is,      eta,       omega,   gamma_H, n_strata, n_age_groups,
        n_econ_groups, i_to_zero, cm,      cm_cw,   cm_work};
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
    state_next[0] = 0.0;  // dummy state, see `R/daedalus2.R` for state setting
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
    const size_t n_econ_groups = shared.n_econ_groups;
    const size_t n_age_groups = shared.n_age_groups;

    // map to Eigen Tensor
    Eigen::TensorMap<const TensorMat<double>> t_x(
        state, vec_size, daedalus::constants::N_COMPARTMENTS);
    Eigen::TensorMap<const TensorMat<double>> t_dx(
        state_deriv, vec_size, daedalus::constants::N_COMPARTMENTS);

    // prepare matrix product dimensions
    std::array<Eigen::IndexPair<int>, 1> product_dims = {
        Eigen::IndexPair<int>(1, 0)};

    // all chip ops on dim N have dim N-1
    // compartmental transitions
    // Susceptible (unvaccinated) to exposed
    // // sToE comprises three parts - community, workplace, consumer-worker
    const auto t_comm_inf =
        t_x.chip(iIs, i_COMPS) + (t_x.chip(iIa, i_COMPS) * shared.epsilon);
    const auto t_foi =
        shared.cm.contract(t_comm_inf, product_dims) * shared.beta;

    const auto sToE =
        t_x.chip(iS, i_COMPS) * t_foi;  // dims (n_strata, i_COMPS)

    // TODO: add workplace infections etc.

    // calculate C * I_w and C * I_cons for a n_econ_groups-length array
    // Eigen::ArrayXd workplace_infected =
    //     shared.cm_work * comm_inf.tail(n_econ_groups).array();
    // Eigen::VectorXd consumer_worker_infections =
    //     (shared.cm_cons_work * comm_inf.head(n_age_groups));

    // // add workplace infections within sectors as
    // // (β * S_w * (C_w * I_w and C_cons_wo * I_cons))
    // sToE.tail(n_econ_groups) +=
    //     shared.beta * x.col(iS).array().tail(n_econ_groups) *
    //     (workplace_infected + consumer_worker_infections.array());

    const auto eToIs = shared.sigma * shared.p_sigma * t_x.chip(iE, i_COMPS);
    const auto eToIa =
        shared.sigma * (1.0 - shared.p_sigma) * t_x.chip(iE, i_COMPS);

    const auto isToR = shared.gamma_Is * t_x.chip(iIs, i_COMPS);
    const auto iaToR = shared.gamma_Ia * t_x.chip(iIa, i_COMPS);

    const auto isToH = shared.eta * t_x.chip(iIs, i_COMPS);

    const auto hToR = shared.gamma_H * t_x.chip(iH, i_COMPS);
    const auto hToD = shared.omega * t_x.chip(iH, i_COMPS);

    const auto rToS = shared.rho * t_x.chip(iR, i_COMPS);

    // update next step
    t_dx.chip(iS, i_COMPS) = -sToE + rToS;
    t_dx.chip(iE, i_COMPS) = sToE - eToIs - eToIa;
    t_dx.chip(iIs, i_COMPS) = eToIs - isToR - isToH;
    t_dx.chip(iIa, i_COMPS) = eToIa - iaToR;
    t_dx.chip(iH, i_COMPS) = isToH - hToD - hToR;
    t_dx.chip(iR, i_COMPS) = isToR + iaToR + hToR - rToS;

    t_dx.chip(iD, i_COMPS) = hToD;
    t_dx.chip(idE, i_COMPS) = sToE;
    t_dx.chip(idH, i_COMPS) = isToH;
  }

  /// @brief Set every value to zero - unclear.
  /// @param shared Shared state -- unused.
  /// @return Probably an array of zeros.
  static auto zero_every(const shared_state &shared) {
    return dust2::zero_every_type<real_type>{
        {1, shared.i_to_zero}};  // zero incidence data compartments
  }
};
