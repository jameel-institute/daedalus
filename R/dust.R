## Generated by dust2 (version 0.3.24) - do not edit
daedalus_ode <- structure(
  function() get("daedalus_ode"),
  class = "dust_system_generator",
  name = "daedalus_ode",
  package = "daedalus",
  path = NULL,
  parameters = data.frame(
    name = c("beta", "sigma", "p_sigma", "epsilon", "rho", "eta", "omega", "gamma_Ia", "gamma_Is", "gamma_H", "nu", "uptake_limit", "susc", "psi", "vax_start_time", "n_age_groups", "n_econ_groups", "popsize", "cm", "cm_work", "cm_cons_work", "hospital_capacity", "openness", "response_time", "response_duration", "auto_social_distancing"),
    type = c("real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "int", "int", "int", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type", "real_type"),
    constant = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
  properties = list(
    time_type = "continuous",
    has_compare = FALSE,
    has_adjoint = FALSE),
  default_dt = NULL)
