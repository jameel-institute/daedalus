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

// product dimensions for Eigen Tensor contraction
const std::array<Eigen::IndexPair<int>, 1> product_dims = {
    Eigen::IndexPair<int>(1, 0)};

// define types for Tensors of known dims
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
// [[dust2::parameter(nu, constant = TRUE)]]
// [[dust2::parameter(psi, constant = TRUE)]]
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
    const TensorMat<double> eta, omega, gamma_H;

    const real_type nu, psi;

    const size_t n_strata, n_age_groups, n_econ_groups;
    const std::vector<size_t> i_to_zero;
    const TensorMat<double> cm, cm_cons_work, cm_work;
  };

  /// @brief Intermediate data.
  struct internal_state {
    TensorMat<double> t_comm_inf, t_foi, workplace_infected, t_comm_inf_age,
        consumer_worker_infections, susc_workers, sToE, eToIs, eToIa, isToR,
        iaToR, isToH, hToR, hToD, rToS;
  };

  static internal_state build_internal(const shared_state &shared) {
    // transition states
    TensorMat<double> mat2d(shared.n_strata, daedalus::constants::N_VAX_STRATA);
    mat2d.setZero();
    TensorMat<double> sToE = mat2d, eToIs = mat2d, eToIa = mat2d, isToR = mat2d,
                      iaToR = mat2d, isToH = mat2d, hToR = mat2d, hToD = mat2d,
                      rToS = mat2d, t_comm_inf = mat2d, t_foi = mat2d;

    // infection related

    TensorMat<double> mat2d_econ(shared.n_econ_groups,
                                 daedalus::constants::N_VAX_STRATA);
    mat2d_econ.setZero();
    TensorMat<double> workplace_infected = mat2d_econ,
                      consumer_worker_infections = mat2d_econ,
                      susc_workers = mat2d_econ;

    TensorMat<double> t_comm_inf_age(shared.n_age_groups,
                                     daedalus::constants::N_VAX_STRATA);
    t_comm_inf_age.setZero();

    // clang-format off
    return internal_state{
      t_comm_inf, t_foi, workplace_infected,
      t_comm_inf_age,
      consumer_worker_infections,
      susc_workers,
      sToE, eToIs, eToIa, isToR, iaToR, isToH, hToR, hToD, rToS};
    // clang-format on
  }

  // unclear whether dust2/common.hpp links to monty - probably
  // NOTE: do not remove, causes compilation errors
  using rng_state_type = monty::random::generator<real_type>;

  /// @brief How compartments are packed.
  /// @param shared A `shared_state` object, unclear why needed.
  /// @return A custom packing specification object.
  static dust2::packing packing_state(const shared_state &shared) {
    const std::vector<size_t> dim_vec(1, shared.n_strata);
    // TODO(pratik): write a function to return this - names may need to be
    // more generic
    return dust2::packing{{"S", dim_vec},           {"E", dim_vec},
                          {"Is", dim_vec},          {"Ia", dim_vec},
                          {"H", dim_vec},           {"R", dim_vec},
                          {"D", dim_vec},           {"new_inf", dim_vec},
                          {"new_hosp", dim_vec},    {"S_vax", dim_vec},
                          {"E_vax", dim_vec},       {"Is_vax", dim_vec},
                          {"Ia_vax", dim_vec},      {"H_vax", dim_vec},
                          {"R_vax", dim_vec},       {"D_vax", dim_vec},
                          {"new_inf_vax", dim_vec}, {"new_hosp_vax", dim_vec}};
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
    const real_type nu = dust2::r::read_real(pars, "nu", 0.0);
    const real_type psi = dust2::r::read_real(pars, "psi", 0.0);

    // related to number of groups
    // defaults to daedalus fixed values
    const size_t n_age_groups = dust2::r::read_size(pars, "n_age_groups", 4);
    const size_t n_econ_groups = dust2::r::read_size(pars, "n_econ_groups", 45);
    const size_t n_strata = n_age_groups + n_econ_groups;

    // broadcasting dims
    const Eigen::array<int, 2> bcast({1, daedalus::constants::N_VAX_STRATA});

    // read vector values (all must have size n_strata)
    TensorMat<double> eta_temp(n_strata, 1);
    TensorMat<double> omega_temp(n_strata, 1);
    TensorMat<double> gamma_H_temp(n_strata, 1);

    dust2::r::read_real_vector(pars, n_strata, eta_temp.data(), "eta", true);
    dust2::r::read_real_vector(pars, n_strata, omega_temp.data(), "omega",
                               true);
    dust2::r::read_real_vector(pars, n_strata, gamma_H_temp.data(), "gamma_H",
                               true);

    TensorMat<double> eta = eta_temp.broadcast(bcast);
    TensorMat<double> omega = omega_temp.broadcast(bcast);
    TensorMat<double> gamma_H = gamma_H_temp.broadcast(bcast);

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
    TensorMat<double> cm_work_temp(n_econ_groups, 1);
    dust2::r::read_real_vector(pars, n_econ_groups, cm_work_temp.data(),
                               "cm_work", true);
    TensorMat<double> cm_work = cm_work_temp.broadcast(bcast);

    // handling compartments to zero
    const std::vector<size_t> i_to_zero = daedalus::helpers::zero_which(
        daedalus::constants::seq_DATA_COMPARTMENTS, n_strata,
        daedalus::constants::N_VAX_STRATA);

    return shared_state{
        beta,      sigma,    p_sigma,  epsilon,      rho,
        gamma_Ia,  gamma_Is, eta,      omega,        gamma_H,
        nu,        psi,      n_strata, n_age_groups, n_econ_groups,
        i_to_zero, cm,       cm_cw,    cm_work};
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
  /// @param internal Intermediate containers.
  /// @param state_deriv State change or dX.
  static void rhs(real_type time, const real_type *state,
                  const shared_state &shared,
                  internal_state &internal,  // NOLINT
                  real_type *state_deriv) {
    // TODO(pratik): prefer to not use these
    const size_t vec_size = shared.n_strata;
    const size_t n_econ_groups = shared.n_econ_groups;
    const size_t n_age_groups = shared.n_age_groups;

    // map to Eigen Tensor
    Eigen::TensorMap<const TensorAry<double>> t_x(
        state, vec_size, daedalus::constants::N_COMPARTMENTS,
        daedalus::constants::N_VAX_STRATA);
    Eigen::TensorMap<TensorAry<double>> t_dx(
        state_deriv, vec_size, daedalus::constants::N_COMPARTMENTS,
        daedalus::constants::N_VAX_STRATA);

    // all chip ops on dim N have dim N-1
    // compartmental transitions
    // Susceptible (unvaccinated) to exposed
    // sToE comprises three parts - community, workplace, consumer-worker
    internal.t_comm_inf =
        t_x.chip(iIs, i_COMPS) + (t_x.chip(iIa, i_COMPS) * shared.epsilon);
    internal.t_foi =
        shared.cm.contract(internal.t_comm_inf, product_dims) * shared.beta;

    // calculate C * I_w and C * I_cons for a n_econ_groups-length array
    internal.workplace_infected =
        shared.beta *
        shared.cm_work *  // this is a 2D tensor with dims (n_econ_grps, n_vax)
        internal.t_comm_inf.slice(
            Eigen::array<int, 2>{vec_size - n_econ_groups, 0},
            Eigen::array<int, 2>{n_econ_groups,
                                 daedalus::constants::N_VAX_STRATA});

    internal.t_comm_inf_age = internal.t_comm_inf.slice(
        Eigen::array<int, 2>{0, 0},
        Eigen::array<int, 2>{n_age_groups, daedalus::constants::N_VAX_STRATA});

    internal.consumer_worker_infections =
        shared.beta *
        shared.cm_cons_work.contract(internal.t_comm_inf_age, product_dims);

    internal.susc_workers =
        t_x.chip(iS, i_COMPS)
            .slice(Eigen::array<int, 2>{n_age_groups, 0},
                   Eigen::array<int, 2>{n_econ_groups,
                                        daedalus::constants::N_VAX_STRATA});

    internal.sToE =
        t_x.chip(iS, i_COMPS) * internal.t_foi;  // dims (n_strata, 2)

    // add workplace infections within sectors as
    // (S_w * (C_w * I_w and C_cons_wo * I_cons))
    internal.sToE.slice(
        Eigen::array<int, 2>{n_age_groups, 0},
        Eigen::array<int, 2>{n_econ_groups,
                             daedalus::constants::N_VAX_STRATA}) +=
        (internal.susc_workers *
         (internal.workplace_infected + internal.consumer_worker_infections));

    internal.eToIs = shared.sigma * shared.p_sigma * t_x.chip(iE, i_COMPS);
    internal.eToIa =
        shared.sigma * (1.0 - shared.p_sigma) * t_x.chip(iE, i_COMPS);

    internal.isToR = shared.gamma_Is * t_x.chip(iIs, i_COMPS);
    internal.iaToR = shared.gamma_Ia * t_x.chip(iIa, i_COMPS);

    internal.isToH = shared.eta * t_x.chip(iIs, i_COMPS);
    internal.hToR = shared.gamma_H * t_x.chip(iH, i_COMPS);
    internal.hToD = shared.omega * t_x.chip(iH, i_COMPS);

    internal.rToS = shared.rho * t_x.chip(iR, i_COMPS);

    // update next step
    t_dx.chip(iS, i_COMPS) = -internal.sToE + internal.rToS;
    t_dx.chip(iE, i_COMPS) = internal.sToE - internal.eToIs - internal.eToIa;
    t_dx.chip(iIs, i_COMPS) = internal.eToIs - internal.isToR - internal.isToH;
    t_dx.chip(iIa, i_COMPS) = internal.eToIa - internal.iaToR;
    t_dx.chip(iH, i_COMPS) = internal.isToH - internal.hToD - internal.hToR;
    t_dx.chip(iR, i_COMPS) =
        internal.isToR + internal.iaToR + internal.hToR - internal.rToS;

    t_dx.chip(iD, i_COMPS) = internal.hToD;
    t_dx.chip(idE, i_COMPS) = internal.sToE;
    t_dx.chip(idH, i_COMPS) = internal.isToH;

    // vaccination related changes
    // TODO(pratik): flexible way of selecting multiple cols from i-th layer
    // .stride() operator limited by start point
    // S => S_v
    t_dx.chip(iS, i_COMPS).chip(0, 1) +=
        -shared.nu * t_x.chip(iS, i_COMPS).chip(0, 1) +
        shared.psi * t_x.chip(iS, i_COMPS).chip(1, 1);
    t_dx.chip(iS, i_COMPS).chip(1, 1) +=
        shared.nu * t_x.chip(iS, i_COMPS).chip(0, 1) -
        shared.psi * t_x.chip(iS, i_COMPS).chip(1, 1);

    // R => R_v
    t_dx.chip(iR, i_COMPS).chip(0, 1) +=
        -shared.nu * t_x.chip(iR, i_COMPS).chip(0, 1) +
        shared.psi * t_x.chip(iR, i_COMPS).chip(1, 1);
    t_dx.chip(iR, i_COMPS).chip(1, 1) +=
        shared.nu * t_x.chip(iR, i_COMPS).chip(0, 1) -
        shared.psi * t_x.chip(iR, i_COMPS).chip(1, 1);
  }

  /// @brief Set every value to zero - unclear.
  /// @param shared Shared state -- unused.
  /// @return Probably an array of zeros.
  static auto zero_every(const shared_state &shared) {
    return dust2::zero_every_type<real_type>{
        {1, shared.i_to_zero}};  // zero incidence data compartments
  }
};
