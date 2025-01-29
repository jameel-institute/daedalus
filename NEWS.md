# daedalus 0.2.1

This is a patch version of {daedalus} that builds up to combining `daedalus::daedalus()` with `daedalus::daedalus_rtm()`.

**Breaking changes**

- `daedalus_rtm()` has been removed; please see installation instructions in the Readme for how install a {daedalus} version that provides it;

- {Rcpp} and {RcppEigen} have been replaced with {cpp11} and {cpp11eigen};

- Tests using `daedalus_rtm()` have been removed;

- Continuous benchmarking removed for `daedalus_rtm()`.

**Other changes**

- Dependencies {dust2} and {monty} added;

- Added successor function `daedalus2()` which will replace `daedalus()`;

- Toy SEIR model with flexible number of demographic strata added in `inst/dust/daedalus.cpp`; this will build up to the full model;

- Infrastructure files updated for changes above;

- Added small vignette to show functionality.

# daedalus 0.2.0

This is a minor version release of {daedalus} that adds real-time epidemic modelling functionality while removing the implementation of vaccination and vaccination-group infection pathways.
There are no intervening versions between this and v0.1.0.

1. Added new function `daedalus_rtm()` (for real-time modelling) which returns either a `<daedalus_output>` object or a list of these.
    
    - Has an Rcpp backed using RcppEigen and Boost Headers {BH} for Boost _odeint_ solvers; added Rcpp, RcppEigen, and BH as dependencies;

    - Removes reactive intervention implementation and lifting; instead a single intervention on economic sectors can be passed for implementation in a user-specified time-frame ;

    - Allows NPI on social contacts (basic scaling of transmission rate $\beta$) when economic sector NPI is active (default is 1.0 for no distancing); see arg `social_distancing_mandate`;

    - Allows spontaneous social distancing mediated by daily deaths to be turned on (off by default); see arg `auto_social_distancing`;

    - Allows passing a 45-element numeric vector specifying sector-wise openness coefficients;

    - Allows passing a list of `<daedalus_infection>` objects to `infection` to accommodate parameter uncertainty (in this case it returns a list of `<daedalus_output>`);

    - Hosptial capacity only determines when excess mortality is activated;

    - Initial state asumes 1 in 10 million infected.

2. Added C++ code in `src/daedalus.cpp` to help with ODE RHS, observer for ODE system, looping over parameter combinations, and exposing C++ code to R.

3. Added functions to help with `daedalus_rtm()`:

    - `prepare_output_cpp()` to handle output from Rcpp fn (returned as list of matrices);

    - `make_conmat_large()` prepares a 49 $\times$ 49 contact matrix for C++ ODE RHS;

    - `make_work_contacts()` and `make_consumer_contacts()` scale contacts by workers in economic sectors -- $N$ in $\beta SI \text{CM} / N$.

4. Package classes:

    - `<daedalus_country>` now returns life expectancy vector;

5. Package data:

    - Exporting economic sector names;

    - Exporting country-wise life-expectancy data;

    - Added C++-model compartment names as a constant.

6. Downstream functions:

    - `get_costs()` returns life-years lost; values of lives lost vector is renamed to `life_value_lost`

    - `get_life_years_lost()` calculates life-years lost from total deaths and life-expectancy.

7. Package infrastructure:

    - Updated `.Rbuildignore`, `.gitignore`, `.lintr`, `_pkgdown.yml`, `WORDLIST` for C++ code;

    - Updated Readmes to reflect package title and scope;

    - Added basic tests for `daedalus_rtm()`.

# daedalus 0.1.0

This is a minor version release of {daedalus} for use in the IDM conference in 2024.

# daedalus 0.0.24

This patch version adds a vignette on exploring the effect of parameter uncertainty on overall pandemic costs, and updates other vignettes and improves documentation.

# daedalus 0.0.23

This patch corrects epidemic dynamics:

1. Replaces $R_0$ in force of infection calculations with transmission parameter $\beta$;

2. Adds a new function `get_beta()` to calculate model $\beta$ for a country and infection (thanks @patcatgit);

3. Corrects the implementation of `r_eff()` to use model $\beta$;

4. Corrects `prepare_parameters.daedalus_country()` to provide appropriate contact matrix values;

5. Adds tests to check newly added functions.

There is a noticeable speed loss due to repeated calls to `base::eigen()` and `base::solve()` in `r_eff()`.

# daedalus 0.0.22

This patch makes the infection IFR values available for the DAEDALUS-relevant age group in `infection_data` and in `<daedalus_infection>` objects under the name `"ifr"`.

# daedalus 0.0.21

This patch updates and corrects the cost of school closures by adding lost earnings, with simplifying assumptions of a single school-age cohort, that all school age children are in school, a single value of the number of expected work years, and a constant global value of the effectiveness of remote education.

Educational losses are now a function of number of school days lost to closures, the country per-capita GNI, the number of school-age children, the effectiveness of remote education, and a coefficient of lost earnings.

This calculation replaces the previous value of educational losses, which was the education sector GVA loss over the closure period. The education sector GVA loss is now added to the overall economic losses.

Educational losses due to absences are still calculated as GVA losses due to worker absences, and not in terms of lost earnings for students due to student illness and absence.

1. Added internal function `get_value_lost_earnings()` to get a (constant) coefficient for the value of earnings lost due to lost education;

2. Added internal function `get_value_school_year()` to get the value of a school year for any country GNI (gross national income per capita).

3. Added package constants `i_SCHOOL_AGE`, `edu_effectiveness_remote` (0.33), `edu_annual_ror` (0.08), and `earnings_loss_discount` (0.03)

# daedalus 0.0.20

This patch updates the state variable in `daedalus()` to substantially reduce the number of compartments; all empty compartments have been removed. This improves the speed of model runs as there are fewer derivatives to calculate.

1. State variable is a 3D array with the dimensions: `c(49, 9, 3)`. The working-age economic sectors are added on as rows after the age-groups. Epidemiological compartments are unchanged. The third dimension holds data on vaccination status, including new vaccinations.

2. Helper functions are updated to support the new state dimensions.

3. $R_t$ calculation in `r_eff()` has been simplified to the proportional susceptible times $R_0$, pending a more accurate calculation.

4. The internal helper function `prepare_output()` has been refactored to avoid use of `base::array2DF()` to remove the dependency on newer versions of R.

5. Removes unused between-sector contacts in force-of-infection calculations.

6. Moves some contact scaling to the parameter preparation stage to reduce operations during the model run.

7. Correctly sums the total number of infectious individuals in the community; previously, the implementation resulted in segregation by economic sector.

# daedalus 0.0.19

This patch adds functionality to run `daedalus()` with the `country` argument passed as one of `country_codes_iso2c` or `country_codes_iso3c` for 2 and 3 letter ISO country codes.

# daedalus 0.0.18

This patch adds logging of daily new vaccinations and provides an output helper function, `get_new_vaccinations()`, to get daily new vaccinations.

- Adds a third layer to the fourth dimension to the state array to hold new vaccinations;

- Adds a new helper function `values_to_state()` to convert a vector to the state array;

- Updates a number of indexing operations to disregard the new data layer of new vaccinations;

- Updates `daedalus()`, `prepare_output()` and `make_initial_state()` to account for the new vaccinations layer;

- Updates tests for newly added functionality.

# daedalus 0.0.17

This patch removes dual implementation levels for response strategies. The "elimination" strategy has a "high" implementation level, while all other strategies keep their "light" implementation level.

- The `daedalus()` argument `implementation_level` has been removed.

- Tests and documentation have been updated to remove references to implementation levels.

# daedalus 0.0.16

This patch fixes an issue where vaccination start was tied to the `response_time`; it is now correctly controlled by the vaccine investment level passed to `daedalus()`.

- Added a `vax_switch` which is manually turned on in stage 03 of the model (never turned off);

- Added test to check that there is no vaccination before scenario-defined start times;

- Added tests to check that vaccine investment levels control epidemic deaths.

# daedalus 0.0.15

This patch fixes an issue where closures were not ended in model runs where the closure began after the epidemic had stopped growing. This mostly affected edge cases of countries with large spare hospital capacity, and relatively late `response_time`s. In such cases the closure end time was assigned as the simulation end time, inflating costs related to closures. The fix:

- Prevents closures from being activated by the `hospital_capacity` trigger if the epidemic is not growing, even if the hospital capacity threshold is crossed;

- Prevents closures from being activated by the `response_time` trigger if the epidemic is not growing between stage 01 and stage 02. Closures are manually turned off if the epidemic is not growing ($R_t < 1.0$).

Tests for different response times check that the model behaves as expected.

## Miscellaneous changes

- The package now requires a minimum R version >= v4.3 due to the use of the new function `array2DF()`;

- `prepare_parameters.daedalus_country()` now provides a contact matrix scaled by the leading eigenvalue but not the demography for use in `r_eff()`.

# daedalus 0.0.14

This patch adds vaccine investment scenarios to _daedalus_. All model runs must now include an assumption about the level of advance vaccine investment, defaulting to "none".

1. Added vaccine investment scenario related data: `vaccination_scenario_data` for vaccination parameters, `vaccination_scenario_names`, and `vaccination_parameter_names`.

2. Added the `<daedalus_vaccination>` S3 class to hold vaccination parameters, along with class infrastructure (print, get/set, and `prepare_parameters()` methods).

3. Added helper function `daedalus_vaccination()` to return vaccination object corresponding to four vaccine investment scenarios.

4. Added tests for vaccination class, model runs with vaccination, and updated documentation.

# daedalus 0.0.13

This patch version adds ISO 3166 2- and 3- character country codes for the countries in `country_names`, as the package data `country_codes_iso2c` and `country_codes_iso3c`.

# daedalus 0.0.12

This patch version adds functions to summarise model outputs (#30):

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
