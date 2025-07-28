// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

// NOTE: header inclusion causes style clashes b/w clang-format and cpplint
// please include system headers in alphabetical order
// clang-format off
#include <daedalus.h>

#include <R_ext/Arith.h>
#include <RcppEigen.h>
#include <unsupported/Eigen/CXX11/Tensor>

#include <cmath>
#include <iterator>
#include <numeric>
#include <string>
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
// [[dust2::parameter(hfr, constant = TRUE)]]
// [[dust2::parameter(gamma_Ia, constant = TRUE)]]
// [[dust2::parameter(gamma_Is, constant = TRUE)]]
// [[dust2::parameter(gamma_H_recovery, constant = TRUE)]]
// [[dust2::parameter(gamma_H_death, constant = TRUE)]]
// [[dust2::parameter(nu, constant = TRUE)]]
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
// [[dust2::parameter(response_time, constant = TRUE)]]
// [[dust2::parameter(response_duration, constant = TRUE)]]
// [[dust2::parameter(auto_social_distancing, constant = TRUE)]]
class daedalus_ode {
 public:
  daedalus_ode() = delete;

  using real_type = double;

  /// @brief Shared parameters and values. All const as not expected to update.
  struct shared_state {
    // NOTE: n_strata unknown at compile time
    const real_type beta, sigma, p_sigma, epsilon, rho, gamma_Ia, gamma_Is;
    const TensorMat eta, omega, gamma_H;

    const real_type nu, psi;

    const size_t n_strata, n_age_groups, n_econ_groups, popsize;
    const TensorMat cm, cm_cons_work, cm_work;
    const TensorMat susc, openness;

    // flag positions
    const size_t i_ipr, i_npi_flag, i_vax_flag, i_sd_flag, i_hosp_overflow_flag;

    // event objects
    daedalus::events::response npi, vaccination, public_concern,
        hosp_cap_exceeded;
  };

  /// @brief Intermediate data.
  struct internal_state {
    TensorMat t_comm_inf, t_foi, workplace_infected, t_comm_inf_age,
        consumer_worker_infections, susc_workers, sToE, eToIs, eToIa, isToR,
        iaToR, isToH, hToR, hToD, rToS;
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

    // clang-format off
    return internal_state{
      t_comm_inf, t_foi, workplace_infected,
      t_comm_inf_age,
      consumer_worker_infections,
      susc_workers,
      sToE, eToIs, eToIa, isToR, iaToR, isToH, hToR, hToD, rToS
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
    const std::vector<size_t> dim_flag{};
    // TODO(pratik): write a function to return this - names may need to be
    // more generic

    return dust2::packing{{"susceptible", dim_vec},
                          {"exposed", dim_vec},
                          {"infect_symp", dim_vec},
                          {"infect_asymp", dim_vec},
                          {"hospitalised", dim_vec},
                          {"recovered", dim_vec},
                          {"dead", dim_vec},
                          {"new_infections", dim_vec},
                          {"new_hosp", dim_vec},
                          {"susceptible_vax", dim_vec},
                          {"exposed_vax", dim_vec},
                          {"infect_symp_vax", dim_vec},
                          {"infect_asymp_vax", dim_vec},
                          {"hospitalised_vax", dim_vec},
                          {"recovered_vax", dim_vec},
                          {"dead_vax", dim_vec},
                          {"new_infections_vax", dim_vec},
                          {"new_hosp_vax", dim_vec},
                          {"new_vax", dim_vec},
                          {"ipr", dim_flag},
                          {"npi_flag", dim_flag},
                          {"vax_flag", dim_flag},
                          {"sd_flag", dim_flag},
                          {"hosp_overflow_flag", dim_flag},
                          {"npi_start_time", dim_flag},
                          {"vax_start_time", dim_flag},
                          {"sd_start_time", dim_flag},
                          {"hosp_overflow_start_time", dim_flag}};
  }

  static size_t size_special() {
    return 8;  // flags and start times
  }

  /// @brief Initialise shared parameters.
  /// @param pars A list of parameters passed from R.
  /// @return A shared parameters object.
  static shared_state build_shared(cpp11::list pars) {
    // DEMOGRAPHY PARAMETERS
    // defaults to daedalus fixed values
    const size_t n_age_groups = dust2::r::read_size(
        pars, "n_age_groups", daedalus::constants::DDL_N_AGE_GROUPS);
    const size_t n_econ_groups = dust2::r::read_size(
        pars, "n_econ_groups", daedalus::constants::DDL_N_ECON_GROUPS);
    const size_t n_strata = n_age_groups + n_econ_groups;
    const size_t popsize = dust2::r::read_size(pars, "popsize", 0.0);

    // EPI PARAMETERS
    const real_type beta = dust2::r::read_real(pars, "beta", 0.0);
    const real_type sigma = dust2::r::read_real(pars, "sigma", 0.0);
    const real_type p_sigma = dust2::r::read_real(pars, "p_sigma", 0.0);
    const real_type epsilon = dust2::r::read_real(pars, "epsilon", 0.0);
    const real_type rho = dust2::r::read_real(pars, "rho", 0.0);
    const real_type gamma_Ia = dust2::r::read_real(pars, "gamma_Ia", 0.0);
    const real_type gamma_Is = dust2::r::read_real(pars, "gamma_Is", 0.0);
    const real_type gamma_H_recovery = 
      dust2::r::read_real(pars, "gamma_H_recovery", 0.0);
    const real_type gamma_H_death = 
      dust2::r::read_real(pars, "gamma_H_death", 0.0);

    // EPI PARAMETERS: AGE VARYING
    TensorMat eta_temp(n_strata, 1);
    TensorMat hfr_temp(n_strata, 1);

    dust2::r::read_real_vector(pars, n_strata, eta_temp.data(), "eta", true);
    dust2::r::read_real_vector(pars, n_strata, hfr_temp.data(), "hfr", true);
    TensorMat eta = eta_temp.broadcast(bcast);
    TensorMat hfr = hfr_temp.broadcast(bcast);
    
    // CALCULATE AGE VARYING OMEGA AND Gamma_h
    const TensorMat omega = daedalus::helpers::get_omega(
      hfr, gamma_H_recovery, gamma_H_death
    );
    
    const TensorMat gamma_H = daedalus::helpers::get_gamma_H(
      hfr, gamma_H_recovery, gamma_H_death
    );

    // CONTACT PARAMETERS (MATRICES)
    // contact matrix
    const std::vector<size_t> vec_cm_dims(2, n_strata);  // for square matrix
    const dust2::array::dimensions<2> cm_dims(vec_cm_dims.begin());
    TensorMat cm(n_strata, n_strata);
    dust2::r::read_real_array(pars, cm_dims, cm.data(), "cm", true);

    // contacts from consumers to workers
    const std::vector<size_t> vec_cm_cw_dims = {n_econ_groups, n_age_groups};
    const dust2::array::dimensions<2> cm_cw_dims(vec_cm_cw_dims.begin());
    TensorMat cm_cw(n_econ_groups, n_age_groups);
    dust2::r::read_real_array(pars, cm_cw_dims, cm_cw.data(), "cm_cons_work",
                              true);

    // within-sector contacts
    TensorMat cm_work(n_econ_groups, 1);
    dust2::r::read_real_vector(pars, n_econ_groups, cm_work.data(), "cm_work",
                               true);

    // VACCINATION PARAMETERS
    const real_type nu = dust2::r::read_real(pars, "nu", 0.0);
    const real_type psi = dust2::r::read_real(pars, "psi", 0.0);

    // VACCINATION EFFECT PARAMETERS: rows are age+econ grps, cols are vax grps
    const std::vector<size_t> vec_susc_dims = {n_strata, N_VAX_STRATA};
    const dust2::array::dimensions<2> susc_dims(vec_susc_dims.begin());
    TensorMat susc(n_strata, N_VAX_STRATA);
    dust2::r::read_real_array(pars, susc_dims, susc.data(), "susc", true);

    // EVENT/RESPONSE PARAMETERS
    // response time is only 0.0 when response is NULL or 'none'
    // this is used to set hosp capacity to NA_REAL so the response is not
    // triggered
    const real_type response_time =
        dust2::r::read_real(pars, "response_time", NA_REAL);
    const real_type response_duration =
        dust2::r::read_real(pars, "response_duration", NA_REAL);
    const int auto_social_distancing =
        dust2::r::read_size(pars, "auto_social_distancing", 0);

    // hospital capacity data; make a copy conditional on response time to
    // prevent hospital-capacity triggered responses
    const real_type hospital_capacity =
        dust2::r::read_real(pars, "hospital_capacity", NA_REAL);
    const real_type hosp_cap_response =
        std::isnan(response_time) ? NA_REAL : hospital_capacity;

    // handling openness vector
    TensorMat openness(n_econ_groups, 1);
    dust2::r::read_real_vector(pars, n_econ_groups, openness.data(), "openness",
                               true);

    // RELATIVE LOCATIONS OF RESPONSE-RELATED FLAGS
    // add n_strata to the end for new vaccinations data
    const size_t total_compartments =
        (n_strata * N_VAX_STRATA * N_COMPARTMENTS) + n_strata;
    const size_t i_ipr = total_compartments + daedalus::constants::i_rel_IPR;
    const size_t i_npi_flag =
        total_compartments + daedalus::constants::i_rel_NPI_FLAG;
    const size_t i_vax_flag =
        total_compartments + daedalus::constants::i_rel_VAX_FLAG;
    const size_t i_sd_flag =
        total_compartments + daedalus::constants::i_rel_SD_FLAG;
    const size_t i_hosp_overflow_flag =
        total_compartments + daedalus::constants::i_rel_hosp_overflow_FLAG;

    // start times for events
    const size_t i_real_npi_start =
        total_compartments + daedalus::constants::i_rel_NPI_START_TIME;
    const size_t i_real_sd_start =
        total_compartments + daedalus::constants::i_rel_SD_START_TIME;
    const size_t i_real_hosp_overflow_start =
        total_compartments +
        daedalus::constants::i_rel_hosp_overflow_START_TIME;

    // INTEGERS FOR ROOT TYPES FOR STATE-DEPENDENT EVENTS
    // NOTE: these are handled internally in most classes and not exposed in R
    const int root_type_increasing = 1;
    const int root_type_decreasing = -1;

    // RESPONSE AND VACCINATION CLASSES
    std::vector<size_t> idx_hosp =
        daedalus::helpers::get_state_idx({iH + 1}, n_strata, N_VAX_STRATA);

    // NOTE: NPI response end time passed as parameter; vax end time remains 0.0
    daedalus::events::response npi(
        std::string("npi"), response_time, response_duration, hosp_cap_response,
        gamma_Ia, i_npi_flag, idx_hosp, {i_ipr}, root_type_increasing,
        root_type_decreasing, i_real_npi_start);

    daedalus::events::response vaccination =
        daedalus::inputs::read_response(pars, "vaccination");

    // predicate public concern social distancing on whether it is off,
    // independent and always on, or linked to NPIs
    // params below are for "independent" i.e., always on
    real_type sd_start_time = 1.0;    // cannot start at 0.0
    real_type sd_duration = NA_REAL;  // NA_REAL indicates no end time
    real_type sd_start_state = NA_REAL;
    real_type sd_end_state = NA_REAL;
    // prefer enums or strings but dust2 cannot handle these yet?
    if (auto_social_distancing == 0) {
      sd_start_time = NA_REAL;
    } else if (auto_social_distancing == 2) {
      sd_start_time = response_time;
      sd_duration = response_duration;
      sd_start_state = hosp_cap_response;
      sd_end_state = gamma_Ia;  // not working anyway; see PR #83
    }
    daedalus::events::response public_concern(
        std::string("public_concern"), sd_start_time, sd_duration,
        sd_start_state, sd_end_state, i_sd_flag, {idx_hosp}, {i_ipr},
        root_type_increasing, root_type_decreasing, i_real_sd_start);

    daedalus::events::response hosp_cap_exceeded(
        std::string("hosp_cap_exceeded"), NA_REAL, NA_REAL, hospital_capacity,
        hospital_capacity, i_hosp_overflow_flag, {idx_hosp}, {idx_hosp},
        root_type_increasing, root_type_decreasing, i_real_hosp_overflow_start);

    // clang-format off
    return shared_state{
        beta,         sigma,      p_sigma,      epsilon,
        rho,          gamma_Ia,   gamma_Is,     eta,
        omega,        gamma_H,    nu,           psi,
        uptake_limit, n_strata,   n_age_groups, n_econ_groups,
        popsize,      cm,         cm_cw,
        cm_work,      susc,       openness,
        i_ipr,  // state index holding incidence/prevalence ratio
        i_npi_flag,   i_vax_flag, i_sd_flag,    i_hosp_overflow_flag,
        npi,          vaccination,
        public_concern, hosp_cap_exceeded};
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
    // return events vector
    return daedalus::events::get_combined_events(
        {shared.vaccination.make_events(), shared.npi.make_events(),
         shared.public_concern.make_events(),
         shared.hosp_cap_exceeded.make_events()});
  }

  /// @brief Set initial values of the IVP model.
  /// @param time Time -- not used. Purpose unclear.
  /// @param shared Shared parameter object.
  /// @param state_next Next state as double value.
  static void initial(real_type time, const shared_state &shared,
                      const internal_state &internal,
                      const rng_state_type &rng_state, real_type *state_next) {
    state_next[0] = 0.0;  // dummy state, see `R/daedalus.R` for state setting
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
    Eigen::TensorMap<Eigen::Tensor<double, 1>> t_new_vax(
        state_deriv +
            (n_strata * daedalus::constants::N_COMPARTMENTS * N_VAX_STRATA),
        n_strata);

    // calculate total hospitalisations to check if hosp capacity is exceeded;
    // scale mortality rate by 1.6 if so
    Eigen::Tensor<double, 0> total_hosp = t_x.chip(iH, i_COMPS).sum();
    
    const double omega_modifier =
      daedalus::events::switch_by_flag(daedalus::constants::d_mort_multiplier,
                                       state[shared.i_hosp_overflow_flag]);

    // calculate total deaths and scale beta by concern, but only if an
    // NPI is active
    // TODO(pratik): change in future so public-concern is independent of NPIs
    internal.hToD =
        omega_modifier * shared.omega * t_x.chip(iH, i_COMPS);  // new deaths
    Eigen::Tensor<double, 0> total_deaths = internal.hToD.sum();
    const double beta_tmp =
        shared.beta *
        daedalus::events::switch_by_flag(
            daedalus::helpers::get_concern_coefficient(total_deaths(0)),
            state[shared.i_sd_flag]);

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
        beta_tmp *
        shared.cm_work *  // this is a 2D tensor with dims (n_econ_grps, 1)
        daedalus::events::switch_by_flag(shared.openness,
                                         state[shared.i_npi_flag]) *  // scale β
        internal.t_comm_inf.slice(
            Eigen::array<Eigen::Index, 2>{n_strata - n_econ_groups, 0},
            Eigen::array<Eigen::Index, 2>{n_econ_groups, 1});

    internal.t_comm_inf_age = internal.t_comm_inf.slice(
        Eigen::array<Eigen::Index, 2>{0, 0},
        Eigen::array<Eigen::Index, 2>{n_age_groups, 1});

    internal.consumer_worker_infections =
        beta_tmp *
        daedalus::events::switch_by_flag(shared.openness,
                                         state[shared.i_npi_flag]) *  // scale β
        shared.cm_cons_work.contract(internal.t_comm_inf_age, product_dims);

    internal.susc_workers =
        t_x.chip(iS, i_COMPS)
            .slice(Eigen::array<Eigen::Index, 2>{n_age_groups, 0},
                   Eigen::array<Eigen::Index, 2>{n_econ_groups, N_VAX_STRATA});

    internal.sToE = t_x.chip(iS, i_COMPS) * internal.t_foi *
                    beta_tmp;  // dims (n_strata, 2)

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
    const double nu_eff =
        daedalus::helpers::scale_nu(t_x, shared.nu, shared.popsize, n_strata) *
        state[shared.i_vax_flag];

    // TODO(pratik): flexible way of selecting multiple cols from i-th layer
    // .stride() operator limited by start point
    // S => S_v and S_v => S
    t_dx.chip(iS, i_COMPS).chip(0, 1) +=
        -nu_eff * t_x.chip(iS, i_COMPS).chip(0, 1) +
        shared.psi * t_x.chip(iS, i_COMPS).chip(1, 1);
    t_dx.chip(iS, i_COMPS).chip(1, 1) +=
        nu_eff * t_x.chip(iS, i_COMPS).chip(0, 1) -
        shared.psi * t_x.chip(iS, i_COMPS).chip(1, 1);

    // R => R_v and R_v => R
    t_dx.chip(iR, i_COMPS).chip(0, 1) +=
        -nu_eff * t_x.chip(iR, i_COMPS).chip(0, 1) +
        shared.psi * t_x.chip(iR, i_COMPS).chip(1, 1);
    t_dx.chip(iR, i_COMPS).chip(1, 1) +=
        nu_eff * t_x.chip(iR, i_COMPS).chip(0, 1) -
        shared.psi * t_x.chip(iR, i_COMPS).chip(1, 1);

    t_new_vax = nu_eff * t_x.chip(iS, i_COMPS).chip(0, 1) +
                nu_eff * t_x.chip(iR, i_COMPS).chip(0, 1);

    // get IPR (incidence prevalence ratio) as growth flag
    const Eigen::Tensor<double, 0> incidence = internal.sToE.sum();
    const Eigen::Tensor<double, 0> prevalence = internal.t_comm_inf.sum();
    state_deriv[shared.i_ipr] = incidence(0) / prevalence(0);
  }

  /// @brief Set every value to zero - unclear.
  /// @param shared Shared state -- unused.
  /// @return Probably an array of zeros.
  static auto zero_every(const shared_state &shared) {
    return dust2::zero_every_type<real_type>{
        {1, {shared.i_ipr}}};  // zero IPR value
  }
};
