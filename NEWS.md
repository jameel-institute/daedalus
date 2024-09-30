# daedalus 0.0.12

This patch version adds functions to summarise model outputs:

1. `get_incidence()` provides per-timestep (daily) values of one or more of infections (symptomatic and asymptomatic), hospitalisations (or the demand), and deaths, disaggregated by one or more of the following groups: age group, economic sector, and vaccination status.

2. `get_epidemic_summary()` provides the total number of individuals infected, hospitalised, and dead, in one or more groups, while defaulting to providing an overall summary.

- The model state variable now includes values tracking daily infections (susceptible to exposed) and daily hospitalisations.

- Basic tests added for these functions.

- Package _data.table_ imported to help with model data output processing.

# daedalus 0.0.11

This patch version corrects spare hospital capacity data by using corrected versions of the raw hospital capacity data (#31).

The `country_data` and `country_names` package data has also been updated. Thailand is now available as a country.

# daedalus 0.0.10

This patch version adds a basic vaccination functionality, which is controlled by the newly added `vaccination_rate` parameter to `daedalus()`

- Vaccination starts when a response is triggered, and is active even in the 'no closures' scenario;

- Vaccination rate and start time is uniform across age groups;

- Vaccine-derived protection (reduced susceptibility) is represented by hard-coded parameter `tau`, and the waning of this protection is also hard-coded and represented by parameter `psi`. Default values are 0.5 and 1 / 270, respectively;

- $R_t$ calculation in `r_eff()` includes reduced susceptibility for vaccinated individuals.

Internal changes include:

- The system state is now represented as a 4-dimensional array, with the vaccination groups/strata on the 4th dimension (see package constant `DIM_VACCINE_STRATA`);

- ODE system has been updated to include reduced susceptibility and movements between vaccination strata;

- Vaccination rate `nu` is represented in terms of the proportion of the total population, and the internal function `scale_nu()` increases `nu` as the number of individuals eligible for vaccination decreases.

- Added tests for vaccination functionality.

# daedalus 0.0.9

This patch version adds a spontaneous social distancing mechanism to the model in the internal function `get_distancing_coefficient()`, which modifies the infection transmission rate based on the number of new deaths. The intention is to model public concern that leads to a spontaneous reduction in community social contacts.

- Social distancing is independent of the specific response strategy chosen, but is active while a response strategy is active. In future, social distancing will be conditioned on vaccination regime completion.
- Social distancing is active in the 'no response' scenario.
- Workplace contacts are not affected.
- Social distancing scales the transmission rate by a value between 1.0 (no distancing) and the `lower_limit` (arbitrary value of 0.2) using an exponential decay function.

# daedalus 0.0.8

This patch adds hospital capacity data and two downstream effects.

1. Spare hospital capacity data raw data file has been added to `inst/extdata`. A processing script to generate an intermediate dataset has been added to `data-raw/`. The number of countries with available data has been reduced to 41.

2. `<daedalus_country>` class objects now provide spare capacity data.

3. A call to `daedalus()` now takes the `response_threshold` from the `country`-specific hospital capacity by default. Users can still pass a custom value to override the package data.

4. The model's mortality rate `omega` is increased by `1.6x` when the total number of hospitalisations exceed the spare hospital capacity. This is implemented using a switch `hosp_switch` mutable parameter, and the switch activation and termination is rolled into the event functions that control closures.

5. Added tests to check that hospital capacity has expected effect.

# daedalus 0.0.7

This patch adds functionality to calculate pandemic costs using the newly added function `get_costs()`.
This patch also adds `life_value`, a list of country-wise, age-specific values of statistical life lost as package data.

Other changes:

1. Data preparation scripts and raw data that are used to generate the `life_value` data. Subsetting the countries to those for which this data is available leaves 67 countries; Taiwan and Hong Kong are removed due to lack of life value data.

2. `<daedalus_output>` now includes a measure of the total simulation time which could be useful for future calculations of percentage GDP loss.

3. `<daedalus_output>` also returns the openness coefficient vector associated with the model response strategy.

4. Tests and documentation for all changes in the form of function docs and vignette updates.

5. Package _data.table_ now imported for use in cost calculations.

6. Package DESCRIPTION updated to reflect scenario modelling rather than optimisation.

# daedalus 0.0.6

This patch adds the `<daedalus_infection>` class to represent and hold infection parameter data for `daedalus()`. Access, assignment, and print methods are also added, in addition to internal helpers such as `as_country()`. The class is tested with newly added tests.

Other changes:

1. The `get_data()`, `set_data()` and `prepare_parameters()` generics now have methods for `<daedalus_infection>`s.

2. Removes `make_infection_parameters()` in favour of `prepare_parameters.infection()`.

3. `daedalus()` argument `epidemic` accepting epidemic name string is changed to argument `infection` accepting `<daedalus_infection>` objects or epidemic names.

4. Vignettes have been updated to use `<daedalus_infection>` objects.

5. Website reference page updated to have sections.

# daedalus 0.0.5

This patch adds the `<daedalus_country>` class to represent country and territory data for `daedalus()`. Access, assignment, and print methods are also added, in addition to internal helpers such as `as_daedalus_country()`. The class is tested with newly added tests.

Other changes:

1. Adds the generic functions `get_data()`, `set_data()` and `prepare_parameters()`, with methods for `<daedalus_country>` objects. Methods for infection data are also anticipated.

2. Removes `make_country_parameters()` in favour of `prepare_parameters.daedalus_country()`.

3. `daedalus()` accepts its first argument as either a country name or a `<daedalus_country>`, and no longer accepts a `country_params_manual` argument; all changes to country parameters should be made via the class.

4. `make_initial_state()` works on `<daedalus_country>` objects instead of country names. Since this is an internal function it does not support passing a country string name.

5. Vignettes have been updated to show how to use `<daedalus_country>` objects.

# daedalus 0.0.4

This patch adds some basic pandemic response functionality (#15). This functionality is shown in a new vignette `thresholded_interventions.Rmd`.

1. The function signature for `daedalus()` has been changed to accept `response_strategy`, `implementation_level`, `response_time`, and `response_threshold` arguments which specify these parameters. Sensible defaults for the strategy are "none", and a "light" implementation level for other strategies.

2. The model supports the "none", "economic closures", "school closures", and "elimination" strategies, with economic sector openness coefficients for the "light" and "heavy" implementation of these strategies stored in the package data object `closure_data`. The `data-raw/closure_data.R` file shows how this data is generated from raw data files provided by EPPI and added in `inst/extdata`.

3. Closures are triggered by the response time being reached, or the total hospitalisation reaching the response threshold. The state variable has been expanded by one to accommodate a switch that is initially zero, is changed to one (for on) when the response is activated, and back to zero when the response ends. Responses currently end on the same trigger of the effective R being less than 1.0.

4. The `make_parameters()` function has been split into `make_infect_parameters()` and `make_country_parameters()` to be clearer and more manageable. `daedalus()`-level override for parameters which was originally implemented as `...` has been replaced with three separate overrides: `country_params_manual`, `infect_params_manual`, and `initial_state_manual`, for users to pass custom values.

5. `prepare_output()` has been made internal; it omits data for non-working age groups in economic sectors (as these are always 0), and pads all sector numbers to fit the format `sector_XX`.

6. Internal functions for R effective calculations and getting the total hospitalisations, `r_eff()` and `get_hospitalisations()` have been added.

7. Closures are implemented as `deSolve::events` triggered by a rootfinding function, with separate functions for activation and termination, which are implemented as `make_response_threshold_event()` and `make_rt_end_event()`.

8. `contacts_between_sectors` is now set to 0.0, it was previously 1e-6.

9. Added tests for closure triggers having expected effects; but closure end points are not specifically tested.


# daedalus 0.0.3

This patch version adds workplace infections to the epidemiological model, and adds country demography data.

1. Added worker-to-worker infections within and between sectors, and consumer to worker contacts (#9).

2. Added package data: `country_data` (country demography data) and `economic_contacts` (sector-wise contacts data). Note that worker-to-worker contacts between sectors currently takes a dummy value as it is not expected to be used in the near future (#11).

3. Modified `daedalus::daedalus()` to take a single required argument for country name to run the model with package data for country contact matrices and economic and demographic data (#11). Users can still pass infection parameters via `...`.

4. Made package function `default_parameters()` internal and renamed to `make_parameters()`; added the internal helper function `make_initial_state()` to quickly generate country-appropriate initial states.

5. Added documentation and tests for modified function calls.

# daedalus 0.0.2

This patch version adds a basic epidemiological model (#7).

1. Added the model functions `daedalus()` (user-facing) and `.daedalus_ode()` (internal) to run a simple age-stratified (4 age groups) SEIR-HD epidemiological model taken from @robj411 <https://github.com/robj411/p2_drivers>,

2. Added default model parameters in `default_parameters()` which correspond roughly to pandemic influenza,

3. Added `prepare_output()` to prepare `daedalus()` output,

4. Added tests for all functions,

5. Updated the package infrastructure: `DESCRIPTION`, `NAMESPACE`, `WORDLIST`, `.Rbuildignore`, and `_pkgdown.yml` reference.

6. Disabled the `undesirable_function_linter` in the vignettes as it flags `library()` calls.

7. Updated the Readme and added content to the 'Get started' vignette.

# daedalus 0.0.1

Initial package setup including GitHub Actions workflows and code quality checks.

- Updated GH Actions workflows for R CMD check (fails on 'NOTE') and code test coverage;

- Added linting, citation update, Readme rendering, and license year update workflows;

- Added spellchecking and a known words list.
