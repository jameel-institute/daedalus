# Package index

## Model functions

Run the DAEDALUS integrated economic-epidemiological model.

- [`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
  : DAEDALUS model for health, social and economic costs of a pandemic
- [`daedalus_multi_infection()`](https://jameel-institute.github.io/daedalus/reference/daedalus_multi_infection.md)
  : Run daedalus for multiple infection parameter sets

## Class `<daedalus_country>`

Represent and access country or territory characteristics.

- [`daedalus_country()`](https://jameel-institute.github.io/daedalus/reference/class_country.md)
  [`is_daedalus_country()`](https://jameel-institute.github.io/daedalus/reference/class_country.md)
  [`print(`*`<daedalus_country>`*`)`](https://jameel-institute.github.io/daedalus/reference/class_country.md)
  : Represent countries and territories for DAEDALUS

## Class `<daedalus_infection>`

Represent and access infection characteristics.

- [`daedalus_infection()`](https://jameel-institute.github.io/daedalus/reference/class_infection.md)
  [`is_daedalus_infection()`](https://jameel-institute.github.io/daedalus/reference/class_infection.md)
  [`print(`*`<daedalus_infection>`*`)`](https://jameel-institute.github.io/daedalus/reference/class_infection.md)
  : Represent infection parameters for DAEDALUS

## Class `<daedalus_vaccination>`

Represent and access vaccine investment scenarios.

- [`daedalus_vaccination()`](https://jameel-institute.github.io/daedalus/reference/class_vaccination.md)
  [`is_daedalus_vaccination()`](https://jameel-institute.github.io/daedalus/reference/class_vaccination.md)
  [`print(`*`<daedalus_vaccination>`*`)`](https://jameel-institute.github.io/daedalus/reference/class_vaccination.md)
  : Represent vaccine investment scenarios for DAEDALUS

## Class `<daedalus_npi>`

Represent and access non-pharmaceutical intervention scenarios.

- [`daedalus_npi()`](https://jameel-institute.github.io/daedalus/reference/class_npi.md)
  [`is_daedalus_npi()`](https://jameel-institute.github.io/daedalus/reference/class_npi.md)
  [`print(`*`<daedalus_npi>`*`)`](https://jameel-institute.github.io/daedalus/reference/class_npi.md)
  [`daedalus_timed_npi()`](https://jameel-institute.github.io/daedalus/reference/class_npi.md)
  : Represent non-pharmaceutical intervention strategies for DAEDALUS

## Class `<daedalus_behaviour>`

Represent and access parameters for behavioural models.

- [`daedalus_old_behaviour()`](https://jameel-institute.github.io/daedalus/reference/class_behaviour.md)
  [`daedalus_new_behaviour()`](https://jameel-institute.github.io/daedalus/reference/class_behaviour.md)
  [`is_daedalus_behaviour()`](https://jameel-institute.github.io/daedalus/reference/class_behaviour.md)
  [`print(`*`<daedalus_behaviour>`*`)`](https://jameel-institute.github.io/daedalus/reference/class_behaviour.md)
  : Represent behaviour-change mechanisms in DAEDALUS

## Class `<daedalus_output>`

Represent and store model output for further processing.

- [`as_daedalus_output()`](https://jameel-institute.github.io/daedalus/reference/class_daedalus_output.md)
  [`validate_daedalus_output()`](https://jameel-institute.github.io/daedalus/reference/class_daedalus_output.md)
  [`is_daedalus_output()`](https://jameel-institute.github.io/daedalus/reference/class_daedalus_output.md)
  [`print(`*`<daedalus_output>`*`)`](https://jameel-institute.github.io/daedalus/reference/class_daedalus_output.md)
  :

  Create and work with `<daedalus_output>` objects

## Model output post-processing

Functions to process model output to calculate pandemic costs and other
outcomes.

- [`get_costs()`](https://jameel-institute.github.io/daedalus/reference/daedalus_costs.md)
  : Get epidemic losses from a DAEDALUS model run
- [`get_fiscal_costs()`](https://jameel-institute.github.io/daedalus/reference/get_fiscal_costs.md)
  : Get pandemic fiscal costs from a model run
- [`get_incidence()`](https://jameel-institute.github.io/daedalus/reference/epi_output_helpers.md)
  [`get_epidemic_summary()`](https://jameel-institute.github.io/daedalus/reference/epi_output_helpers.md)
  [`get_new_vaccinations()`](https://jameel-institute.github.io/daedalus/reference/epi_output_helpers.md)
  [`get_attack_rate()`](https://jameel-institute.github.io/daedalus/reference/epi_output_helpers.md)
  : Calculate daily incidences and summarise epidemic measures
- [`get_life_years_lost()`](https://jameel-institute.github.io/daedalus/reference/get_life_years_lost.md)
  : Get life-years lost by demographic group.

## Package generics

Generics with methods for package S3 classes.

- [`get_data()`](https://jameel-institute.github.io/daedalus/reference/get_data.md)
  : Get parameters from DAEDALUS classes
- [`set_data()`](https://jameel-institute.github.io/daedalus/reference/set_data.md)
  : Set parameters in DAEDALUS classes

## Model constants

Useful constants used internally.

- [`N_AGE_GROUPS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_AGE_GROUPS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`N_VACCINE_STRATA`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`N_VACCINE_DATA_GROUPS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`AGE_GROUPS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_WORKING_AGE`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_SCHOOL_AGE`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`N_ECON_SECTORS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_ECON_SECTORS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_EDUCATION_SECTOR`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`N_ECON_STRATA`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_NOT_WORKING`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`DIM_AGE_GROUPS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`DIM_EPI_COMPARTMENTS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`DIM_ECON_SECTORS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`DIM_VACCINE_STRATA`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_UNVACCINATED_STRATUM`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_VACCINATED_STRATUM`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`i_NEW_VAX_STRATUM`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`VACCINE_GROUPS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`N_OUTPUT_COLS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`N_FLAGS`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`FLAG_NAMES`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  [`MIN_DATA_COL_NAMES`](https://jameel-institute.github.io/daedalus/reference/model_constants.md)
  : DAEDALUS model constants
- [`COMPARTMENTS`](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
  [`idx_COMPARTMENTS`](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
  [`N_EPI_COMPARTMENTS`](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
  [`i_EPI_COMPARTMENTS`](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
  [`N_MODEL_COMPARTMENTS`](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
  [`N_INFECTION_SUBSYSTEM`](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
  : Epidemiological compartments and indices
- [`SUMMARY_MEASURES`](https://jameel-institute.github.io/daedalus/reference/summary_constants.md)
  [`SUMMARY_GROUPS`](https://jameel-institute.github.io/daedalus/reference/summary_constants.md)
  : Model output measures and groups for summaries and derived
  time-series
- [`EDU_EFFECTIVENESS_REMOTE`](https://jameel-institute.github.io/daedalus/reference/econ_constants.md)
  [`EDU_ANNUAL_ROR`](https://jameel-institute.github.io/daedalus/reference/econ_constants.md)
  [`WORK_EXPECTED_YEARS`](https://jameel-institute.github.io/daedalus/reference/econ_constants.md)
  [`EARNINGS_LOSS_DISCOUNT`](https://jameel-institute.github.io/daedalus/reference/econ_constants.md)
  : Economic constants used in DAEDALUS

## Internal functions

Functions used internally.

- [`new_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  [`response_class_names`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  [`is_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  [`validate_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  : Constructor for the \<daedalus_response\> super-class and
  sub-classes
- [`count_settings()`](https://jameel-institute.github.io/daedalus/reference/count_settings.md)
  : Count contact settings
- [`country_name_from_arg()`](https://jameel-institute.github.io/daedalus/reference/country_name_from_arg.md)
  : Get country name from arg
- [`daedalus-package`](https://jameel-institute.github.io/daedalus/reference/daedalus-package.md)
  : daedalus: Model Health, Social, and Economic Costs of a Pandemic
- [`daedalus_internal()`](https://jameel-institute.github.io/daedalus/reference/daedalus_internal.md)
  : Internal function for daedalus
- [`dummy_npi()`](https://jameel-institute.github.io/daedalus/reference/dummy_npi.md)
  : Dummy NPI
- [`dummy_vaccination()`](https://jameel-institute.github.io/daedalus/reference/dummy_vaccination.md)
  : Dummy vaccination
- [`get_beta()`](https://jameel-institute.github.io/daedalus/reference/get_beta.md)
  : Calculate transmission parameter from infection and contact
  parameters
- [`get_daedalus_multi_response_times()`](https://jameel-institute.github.io/daedalus/reference/get_daedalus_multi_response_times.md)
  : Get response times from a dust2 system with multiple groups
- [`get_daedalus_response_times()`](https://jameel-institute.github.io/daedalus/reference/get_daedalus_response_times.md)
  : Get model response times from dust2 output
- [`get_state_indices()`](https://jameel-institute.github.io/daedalus/reference/get_state_indices.md)
  : Get a vector of state indices
- [`get_value_lost_earnings()`](https://jameel-institute.github.io/daedalus/reference/get_value_lost_earnings.md)
  : Calculate the present value of lost earnings due to educational
  disruption
- [`get_value_school_year()`](https://jameel-institute.github.io/daedalus/reference/get_value_school_year.md)
  : Calculate the value of a school year
- [`initial_flags()`](https://jameel-institute.github.io/daedalus/reference/initial_flags.md)
  : Initial values for model flags
- [`assert_class_fields()`](https://jameel-institute.github.io/daedalus/reference/input_checks.md)
  : Check that a class has expected names
- [`make_community_scaling()`](https://jameel-institute.github.io/daedalus/reference/make_community_scaling.md)
  : Prepare community contacts scaling from workplace scaling
- [`make_full_openness()`](https://jameel-institute.github.io/daedalus/reference/make_full_openness.md)
  : Make a dummy openness matrix for dummy NPIs
- [`make_initial_state()`](https://jameel-institute.github.io/daedalus/reference/make_initial_state.md)
  : Generate a default initial state for DAEDALUS
- [`make_susc_matrix()`](https://jameel-institute.github.io/daedalus/reference/make_susc_matrix.md)
  : Prepare susceptibility matrix for a vaccine-country pair
- [`make_workplace_scaling()`](https://jameel-institute.github.io/daedalus/reference/make_workplace_scaling.md)
  : Prepare workplace setting scaling
- [`new_daedalus_hosp_overflow()`](https://jameel-institute.github.io/daedalus/reference/new_daedalus_hosp_overflow.md)
  : Make a hospital overflow response
- [`out_list_to_df()`](https://jameel-institute.github.io/daedalus/reference/out_list_to_df.md)
  : Reshape output data
- [`make_conmat_large()`](https://jameel-institute.github.io/daedalus/reference/prepare_contacts.md)
  [`make_work_contacts()`](https://jameel-institute.github.io/daedalus/reference/prepare_contacts.md)
  [`make_consumer_contacts()`](https://jameel-institute.github.io/daedalus/reference/prepare_contacts.md)
  : Make large contact matrix for Cpp model.
- [`prepare_output()`](https://jameel-institute.github.io/daedalus/reference/prepare_output.md)
  : Prepare DAEDALUS data
- [`prepare_parameters()`](https://jameel-institute.github.io/daedalus/reference/prepare_parameters.md)
  : Prepare infection parameters for model
- [`process_event_times()`](https://jameel-institute.github.io/daedalus/reference/process_event_times.md)
  : Process event data
- [`get_ngm()`](https://jameel-institute.github.io/daedalus/reference/reff_calculation.md)
  : Calculate the effective R
- [`split_multi_soln()`](https://jameel-institute.github.io/daedalus/reference/split_multi_soln.md)
  : Split a dust2 solution with multiple groups
- [`test_openness()`](https://jameel-institute.github.io/daedalus/reference/test_openness.md)
  : Test if openness is passed correctly
- [`drop_null()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
  [`first()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
  [`last()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
  [`weighted_rowsums()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
  [`interest_accumulation()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
  [`annual_rate_daily()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
  : Helper functions
- [`uptake_percent_to_number()`](https://jameel-institute.github.io/daedalus/reference/uptake_percent_to_number.md)
  : Convert an uptake limit from a percentage to a number
- [`validate_behaviour_input()`](https://jameel-institute.github.io/daedalus/reference/validate_behaviour_input.md)
  : Internal helper function to prepare parameters
- [`validate_contact_matrix()`](https://jameel-institute.github.io/daedalus/reference/validate_contact_matrix.md)
  : Validate contact matrix
- [`validate_country_input()`](https://jameel-institute.github.io/daedalus/reference/validate_country_input.md)
  : Validate country input
- [`validate_infection_input()`](https://jameel-institute.github.io/daedalus/reference/validate_infection_input.md)
  [`validate_infection_list_input()`](https://jameel-institute.github.io/daedalus/reference/validate_infection_input.md)
  : Validate and return infection input
- [`validate_npi_input()`](https://jameel-institute.github.io/daedalus/reference/validate_npi_input.md)
  : Validate npi inputs
- [`validate_openness()`](https://jameel-institute.github.io/daedalus/reference/validate_openness.md)
  : Check openness matrix
- [`validate_vaccination_input()`](https://jameel-institute.github.io/daedalus/reference/validate_vaccination_input.md)
  : Validate vaccination inputs
