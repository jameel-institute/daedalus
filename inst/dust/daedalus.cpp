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

const int N_VAX_STRATA = daedalus::constants::N_VAX_STRATA;
const int N_COMPARTMENTS = daedalus::constants::N_COMPARTMENTS;

// broadcasting and contraction dims
const daedalus::types::bcast_dim_type bcast = daedalus::dims::dim_bcast_vax;
const daedalus::types::prod_dim_type product_dims = daedalus::dims::dim_product;

// Tensor types for local use
using TensorVec = daedalus::types::TensorVec<double>;
using TensorMat = daedalus::types::TensorMat<double>;
using TensorAry = daedalus::types::TensorAry<double>;

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
// [[dust2::parameter(uptake_limit, constant = TRUE)]]
// [[dust2::parameter(susc, constant = TRUE)]]
// [[dust2::parameter(psi, constant = TRUE)]]
// [[dust2::parameter(vax_start_time, type = "real_type", constant = TRUE)]]
// [[dust2::parameter(n_age_groups, constant = TRUE, type = "int")]]
// [[dust2::parameter(n_econ_groups, constant = TRUE, type = "int")]]
// [[dust2::parameter(popsize, constant = TRUE, type = "int")]]
// [[dust2::parameter(cm, constant = TRUE)]]
// [[dust2::parameter(cm_work, constant = TRUE)]]
// [[dust2::parameter(cm_cons_work, constant = TRUE)]]
// [[dust2::parameter(hospital_capacity, type = "real_type", constant = TRUE)]]
// [[dust2::parameter(openness, constant = TRUE)]]
class daedalus_ode {
 public:
  daedalus_ode() = delete;

  using real_type = double;

  /// @brief Shared parameters and values. All const as not expected to update.
  struct shared_state {
    // NOTE: n_strata unknown at compile time
    const real_type beta, sigma, p_sigma, epsilon, rho, gamma_Ia, gamma_Is;
    const TensorMat eta, omega, gamma_H;

    const real_type nu, psi, uptake_limit, vax_start_time;

    const size_t n_strata, n_age_groups, n_econ_groups, popsize;
    const std::vector<size_t> i_to_zero;
    const TensorMat cm, cm_cons_work, cm_work;
    const TensorMat susc, openness;
    const double hospital_capacity;

    // flag positions
    const size_t i_growth_flag, i_resp_flag, i_vax_flag, i_resp_start,
        i_resp_end;
  };

  /// @brief Intermediate data.
  struct internal_state {
    TensorMat t_comm_inf, t_foi, workplace_infected, t_comm_inf_age,
        consumer_worker_infections, susc_workers, sToE, eToIs, eToIa, isToR,
        iaToR, isToH, hToR, hToD, rToS;
    double nu_eff;
  };

  static internal_state build_internal(const shared_state &shared) {
    // transition states
    TensorMat mat2d(shared.n_strata, N_VAX_STRATA);
    mat2d.setZero();
    TensorMat sToE = mat2d, eToIs = mat2d, eToIa = mat2d, isToR = mat2d,
              iaToR = mat2d, isToH = mat2d, hToR = mat2d, hToD = mat2d,
              rToS = mat2d, t_comm_inf = mat2d, t_foi = mat2d;

    // infection related

    TensorMat mat2d_econ(shared.n_econ_groups, N_VAX_STRATA);
    mat2d_econ.setZero();
    TensorMat workplace_infected = mat2d_econ,
              consumer_worker_infections = mat2d_econ,
              susc_workers = mat2d_econ;

    TensorMat t_comm_inf_age(shared.n_age_groups, N_VAX_STRATA);
    t_comm_inf_age.setZero();

    // effective vaccination rate is initially the vaccination rate
    double nu_eff = shared.nu;

    // clang-format off
    return internal_state{
      t_comm_inf, t_foi, workplace_infected,
      t_comm_inf_age,
      consumer_worker_infections,
      susc_workers,
      sToE, eToIs, eToIa, isToR, iaToR, isToH, hToR, hToD, rToS,
      nu_eff
    };
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
    const std::vector<size_t> dim_flag(1, 1);
    // TODO(pratik): write a function to return this - names may need to be
    // more generic

    // clang-format off
    return dust2::packing{{"S", dim_vec},            {"E", dim_vec},
                          {"Is", dim_vec},           {"Ia", dim_vec},
                          {"H", dim_vec},            {"R", dim_vec},
                          {"D", dim_vec},            {"new_inf", dim_vec},
                          {"new_hosp", dim_vec},     {"S_vax", dim_vec},
                          {"E_vax", dim_vec},        {"Is_vax", dim_vec},
                          {"Ia_vax", dim_vec},       {"H_vax", dim_vec},
                          {"R_vax", dim_vec},        {"D_vax", dim_vec},
                          {"new_inf_vax", dim_vec},  {"new_hosp_vax", dim_vec},
                          {"growth_flag", dim_flag}, {"resp_flag", dim_flag},
                          {"vax_flag", dim_flag},    {"resp_start", dim_flag},
                          {"resp_end", dim_flag}};
    // clang-format on
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
    const real_type uptake_limit =
        dust2::r::read_real(pars, "uptake_limit", 0.0);
    const real_type vax_start_time =
        dust2::r::read_real(pars, "vax_start_time", 0.0);

    // related to number of groups
    // defaults to daedalus fixed values
    const size_t n_age_groups = dust2::r::read_size(
        pars, "n_age_groups", daedalus::constants::DDL_N_AGE_GROUPS);
    const size_t n_econ_groups = dust2::r::read_size(
        pars, "n_econ_groups", daedalus::constants::DDL_N_ECON_GROUPS);
    const size_t n_strata = n_age_groups + n_econ_groups;
    const size_t popsize = dust2::r::read_size(pars, "popsize", 0.0);

    // read vector values (all must have size n_strata)
    TensorMat eta_temp(n_strata, 1);
    TensorMat omega_temp(n_strata, 1);
    TensorMat gamma_H_temp(n_strata, 1);

    dust2::r::read_real_vector(pars, n_strata, eta_temp.data(), "eta", true);
    dust2::r::read_real_vector(pars, n_strata, omega_temp.data(), "omega",
                               true);
    dust2::r::read_real_vector(pars, n_strata, gamma_H_temp.data(), "gamma_H",
                               true);

    TensorMat eta = eta_temp.broadcast(bcast);
    TensorMat omega = omega_temp.broadcast(bcast);
    TensorMat gamma_H = gamma_H_temp.broadcast(bcast);

    // handling contact matrix
    const std::vector<size_t> vec_cm_dims(2, n_strata);  // for square matrix
    const dust2::array::dimensions<2> cm_dims(vec_cm_dims.begin());
    TensorMat cm(n_strata, n_strata);
    dust2::r::read_real_array(pars, cm_dims, cm.data(), "cm", true);

    // handling contacts from consumers to workers
    const std::vector<size_t> vec_cm_cw_dims = {n_econ_groups, n_age_groups};
    const dust2::array::dimensions<2> cm_cw_dims(vec_cm_cw_dims.begin());
    TensorMat cm_cw(n_econ_groups, n_age_groups);
    dust2::r::read_real_array(pars, cm_cw_dims, cm_cw.data(), "cm_cons_work",
                              true);

    // handling within-sector contacts
    TensorMat cm_work(n_econ_groups, 1);
    dust2::r::read_real_vector(pars, n_econ_groups, cm_work.data(), "cm_work",
                               true);

    // hospital capacity data
    const real_type hospital_capacity =
        dust2::r::read_real(pars, "hospital_capacity", 0.0);
    // handling compartments to zero
    const std::vector<size_t> i_to_zero = daedalus::helpers::get_state_idx(
        daedalus::constants::seq_DATA_COMPARTMENTS, n_strata, N_VAX_STRATA);

    // handling susceptibility matrix: rows are age+econ grps, cols are vax grps
    const std::vector<size_t> vec_susc_dims = {n_strata, N_VAX_STRATA};
    const dust2::array::dimensions<2> susc_dims(vec_susc_dims.begin());
    TensorMat susc(n_strata, N_VAX_STRATA);
    dust2::r::read_real_array(pars, susc_dims, susc.data(), "susc", true);

    // handling openness vector
    TensorMat openness(n_econ_groups, 1);
    dust2::r::read_real_vector(pars, n_econ_groups, openness.data(), "openness",
                               true);

    // locations of response flags
    const size_t i_growth_flag = n_strata * N_VAX_STRATA * N_COMPARTMENTS +
                                 daedalus::constants::i_rel_GROWTH_FLAG;
    const size_t i_resp_flag = n_strata * N_VAX_STRATA * N_COMPARTMENTS +
                               daedalus::constants::i_rel_RESP_FLAG;
    const size_t i_vax_flag = n_strata * N_VAX_STRATA * N_COMPARTMENTS +
                              daedalus::constants::i_rel_VAX_FLAG;
    const size_t i_resp_start = n_strata * N_VAX_STRATA * N_COMPARTMENTS +
                                daedalus::constants::i_rel_RESP_START;
    const size_t i_resp_end = n_strata * N_VAX_STRATA * N_COMPARTMENTS +
                              daedalus::constants::i_rel_RESP_END;

    // clang-format off
    return shared_state{
        beta, sigma, p_sigma, epsilon, rho,
        gamma_Ia, gamma_Is, eta, omega, gamma_H, nu, psi,
        uptake_limit, vax_start_time, n_strata, n_age_groups, n_econ_groups,
        popsize, i_to_zero,
        cm, cm_cw, cm_work, susc, openness,
        hospital_capacity, i_growth_flag, i_resp_flag, i_vax_flag,
        i_resp_start, i_resp_end};
    // clang-format on
  }

  /// @brief Updated shared parameters.
  /// @param pars A list of parameters passed from R.
  /// @param shared A shared parameter object to update.
  static void update_shared(cpp11::list pars, const shared_state &shared) {
    // NOTE: we are setting these constant
  }

  /// @brief Events for daedalus.
  /// @param shared Shared parameters.
  /// @param internal Intermediate containers.
  /// @return A container of events passed to the solver.
  static auto events(const shared_state &shared,
                     const internal_state &internal) {
    // root-finding tests
    // NOTE: iX + 1 gives the 1-indexed compartment
    const std::vector<size_t> idx_hosp = daedalus::helpers::get_state_idx(
        {iH + 1}, shared.n_strata, N_VAX_STRATA);

    auto test_hosp = [&](double t, const double *y) {
      int size_n = shared.n_strata * N_VAX_STRATA;  // get size
      double total_hosp = std::accumulate(y, y + size_n, 0);
      double diff = total_hosp - shared.hospital_capacity;

      return diff;
    };

    auto test_vax_time = [&](double t, const double *y) {
      double diff = t - shared.vax_start_time;

      return diff;
    };

    auto test_resp_dur = [&](double t, const double *y) {
      double diff = t - y[0] - 60.0;  // dummy duration of 60 days
      return diff;
    };

    auto test_epi_growth = [&](double t, const double *y) {
      // NOTE: simple test of epidemic growth incidence-prevalence ratio > gamma
      // See 10.1097/01.aids.0000244213.23574.fa
      return y[0] - (shared.gamma_Is);
    };

    // actions
    // presumably y arg refers to full state
    auto resp_on = [&](const double t, const double sign, double *y) {
      y[shared.i_resp_flag] = 1.0;
      y[shared.i_resp_start] = t;
    };
    auto resp_off = [&](const double t, const double sign, double *y) {
      y[shared.i_resp_flag] = 0.0;
      y[shared.i_resp_end] = t;
    };
    auto vax_on = [&](const double t, const double sign, double *y) {
      y[shared.i_vax_flag] = 1.0;
    };

    // events
    dust2::ode::event<real_type> ev_hosp_trigger(
        idx_hosp, test_hosp, resp_on, dust2::ode::root_type::increase);

    dust2::ode::event<real_type> ev_vax_trigger({}, test_vax_time, vax_on,
                                                dust2::ode::root_type::both);

    dust2::ode::event<real_type> ev_resp_dur({shared.i_resp_start},
                                             test_resp_dur, resp_off,
                                             dust2::ode::root_type::both);

    dust2::ode::event<real_type> ev_growth_trigger(
        {shared.i_growth_flag}, test_epi_growth, resp_off,
        dust2::ode::root_type::decrease);

    return dust2::ode::events_type<real_type>(
        {ev_hosp_trigger, ev_vax_trigger, ev_growth_trigger, ev_resp_dur});
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
    const int n_strata = shared.n_strata;
    const int n_econ_groups = shared.n_econ_groups;
    const int n_age_groups = shared.n_age_groups;

    // map to Eigen Tensor
    Eigen::TensorMap<const TensorAry> t_x(
        state, n_strata, daedalus::constants::N_COMPARTMENTS, N_VAX_STRATA);
    Eigen::TensorMap<TensorAry> t_dx(state_deriv, n_strata,
                                     daedalus::constants::N_COMPARTMENTS,
                                     N_VAX_STRATA);

    // all chip ops on dim N have dim N-1
    // compartmental transitions
    // Susceptible (unvaccinated) to exposed
    // sToE comprises three parts - community, workplace, consumer-worker
    // need rowsums for FOI
    internal.t_comm_inf =
        (t_x.chip(iIs, i_COMPS) + (t_x.chip(iIa, i_COMPS) * shared.epsilon))
            .sum(Eigen::array<Eigen::Index, 1>{1})
            .reshape(Eigen::array<Eigen::Index, 2>{n_strata, 1});

    // FOI must be broadcast for element-wise tensor mult
    internal.t_foi =
        shared.cm.contract(internal.t_comm_inf, product_dims).broadcast(bcast);

    // calculate C * I_w and C * I_cons for a n_econ_groups-length array
    internal.workplace_infected =
        shared.beta *
        shared.cm_work *  // this is a 2D tensor with dims (n_econ_grps, 1)
        daedalus::interventions::switch_by_flag(
            shared.openness,
            state[shared.i_resp_flag]) *  // scale β
        internal.t_comm_inf.slice(
            Eigen::array<Eigen::Index, 2>{n_strata - n_econ_groups, 0},
            Eigen::array<Eigen::Index, 2>{n_econ_groups, 1});

    internal.t_comm_inf_age = internal.t_comm_inf.slice(
        Eigen::array<Eigen::Index, 2>{0, 0},
        Eigen::array<Eigen::Index, 2>{n_age_groups, 1});

    internal.consumer_worker_infections =
        shared.beta *
        daedalus::interventions::switch_by_flag(
            shared.openness,
            state[shared.i_resp_flag]) *  // scale β
        shared.cm_cons_work.contract(internal.t_comm_inf_age, product_dims);

    internal.susc_workers =
        t_x.chip(iS, i_COMPS)
            .slice(Eigen::array<Eigen::Index, 2>{n_age_groups, 0},
                   Eigen::array<Eigen::Index, 2>{n_econ_groups, N_VAX_STRATA});

    internal.sToE = t_x.chip(iS, i_COMPS) * internal.t_foi *
                    shared.beta;  // dims (n_strata, 2)

    // add workplace infections within sectors as
    // (S_w * (C_w * I_w and C_cons_wo * I_cons))
    // NOTE: broadcasting for element-wise tensor mult
    internal.sToE.slice(
        Eigen::array<Eigen::Index, 2>{n_age_groups, 0},
        Eigen::array<Eigen::Index, 2>{n_econ_groups, N_VAX_STRATA}) +=
        (internal.susc_workers *
         (internal.workplace_infected.broadcast(bcast) +
          internal.consumer_worker_infections.broadcast(bcast)));

    // element-wise mult with susceptibility matrix to reduce number of
    // vaccinated infected S => E
    internal.sToE = internal.sToE * shared.susc;

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
    // calculate vaccination rate
    internal.nu_eff =
        daedalus::helpers::scale_nu(t_x, shared.nu, shared.uptake_limit,
                                    shared.popsize, n_strata) *
        state[shared.i_vax_flag];

    // TODO(pratik): flexible way of selecting multiple cols from i-th layer
    // .stride() operator limited by start point
    // S => S_v
    t_dx.chip(iS, i_COMPS).chip(0, 1) +=
        -internal.nu_eff * t_x.chip(iS, i_COMPS).chip(0, 1) +
        shared.psi * t_x.chip(iS, i_COMPS).chip(1, 1);
    t_dx.chip(iS, i_COMPS).chip(1, 1) +=
        internal.nu_eff * t_x.chip(iS, i_COMPS).chip(0, 1) -
        shared.psi * t_x.chip(iS, i_COMPS).chip(1, 1);

    // R => R_v
    t_dx.chip(iR, i_COMPS).chip(0, 1) +=
        -internal.nu_eff * t_x.chip(iR, i_COMPS).chip(0, 1) +
        shared.psi * t_x.chip(iR, i_COMPS).chip(1, 1);
    t_dx.chip(iR, i_COMPS).chip(1, 1) +=
        internal.nu_eff * t_x.chip(iR, i_COMPS).chip(0, 1) -
        shared.psi * t_x.chip(iR, i_COMPS).chip(1, 1);

    // get IPR (incidence prevalence ratio) as growth flag
    const Eigen::Tensor<double, 0> incidence = internal.sToE.sum();
    const Eigen::Tensor<double, 0> prevalence = internal.t_comm_inf.sum();
    state_deriv[shared.i_growth_flag] = incidence(0) / prevalence(0);
  }

  /// @brief Set every value to zero - unclear.
  /// @param shared Shared state -- unused.
  /// @return Probably an array of zeros.
  static auto zero_every(const shared_state &shared) {
    return dust2::zero_every_type<real_type>{
        {1, shared.i_to_zero},
        {1, {shared.i_growth_flag}}};  // zero data and flag compartments
  }
};
