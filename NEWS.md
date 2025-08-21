# daedalus (development version)

This patch modifies the costs and fiscal costs calculation by changing how absences due to illness are calculated in `get_costs()` and `get_fiscal_costs()`.

- Only $H$ and $D$ are counted as fully absent;

- $I_a$ were previously counted but are no longer counted;

- $I_s$ are counted as fully absent with full productivity loss using the argument `productivity_loss_infection` in costs functions; this can be changed during costs calculations.

Snapshot tests are updated to reflect that all costs are reduced due to no longer counting $I_a$.

## Notes

- `get_fiscal_costs()` is updated to have its labour availability match the multiplicative implementation in `get_costs()`.

# daedalus 0.2.33

This patch version allows multiple, sequential, time-limited NPIs with varying effects (PR #117).

- Added the function `daedalus_timed_npi()` to create time-limited NPIs which are not responsive to model state, and which accept multiple start and end times (with non-overlapping intervals), with corresponding openness coefficients.

- Changes to `<daedalus_npi>` to accommodate timed NPIs.

- Changes to C++ `daedalus::events::response` class to log the index of any timed events: this allows indexing NPI coefficients, and retains boolean flag for other responses.

- Function `daedalus_npi()` no longer specifies a default end time, and does not allow multiple start and end times (this is moved to `daedalus_timed_npi()`).

- Updated vignette on timed NPIs to show multiple timed-NPI functionality with different openness coefficients.

# daedalus 0.2.32

This patch version demotes `auto_social_distancing` from a full `daedalus::events::response` while retaining the mechanism of behavioural effects (PR #113).
The mechanism options are mostly handled on the R side (options `"off"` and `"independent"`, by setting initial flag values), with only `"npi_linked"` handled on the C++ side (by setting the reference flag index to the same as `i_npi_flag`).
This allows the mechanism to remain NPI-linked when necessary without needing to specify time- and state-dependence rules separately.

The internal function `daedalus::events::switch_by_flag` converts flag values > 1.0 to 1.0 internally to account for changes to the NPI flag to an implicit index in PR #117.

# daedalus 0.2.31

This patch version adds functionality to allow multiple, sequential, time-limited NPIs (PR #111 reviewed by @pabloperguz).

- `<daedalus_response>` and sub-classes have separate `time_off` and `max_duration` members; `time_off` may be a vector while `max_duration` is a single number defaulting to `365` for `<daedalus_npi>` only. `<daedalus_vaccination>` has no default value allowing events to continue indefinitely.

- Events returned from `daedalus()` now differentiate between time-limitation and maximum-duration events; the internal function `get_daedalus_response_times()` handles these different names.

- C++ class `daedalus::events::response` now expects members `time_on` and `time_off` to be vectors; this is handled for `<daedalus_npi>` and `<daedalus_vaccination>` via `daedalus::inputs::read_response` but is manually set for other events.

- C++ class `daedalus::events::response` member function `make_time_test` now checks for start and end time with an `expected_value` argument for the expected value of the flag. Function `make_duration_test` only handles the condition that the maximum duration is reached.

- A new vignette `timed_response.Rmd` shows how to use the time-limitation functionality in `<daedalus_npi>`.

- General tests for events in `test-daedalus_events.R` have been moved to either NPI or vaccination test files.

### Breaking changes

- Older versions of _daedalus_ that pass the `duration` argument to a response object to implement time-limits will fail.

### Notes

- The 'auto social-distancing' option `npi_linked` is not fully compatible with the new changes to `<daedalus_npi>`, and auto social-distancing will only launch during the first interval that an NPI is active; this will be fixed in PR #113.

# daedalus 0.2.30

This patch version:

- Introduces the `<daedalus_hosp_overflow>` class for internal use;

- Fixes issues related to vaccination 'pulling along' other state-dependent events by correcting the R and C++ functions `get_state_indices()` and `daedalus::helpers::get_state_idx`;

- Fixes flag checking in the `daedalus::response` class event factories (previous indexing used the absolute indices of flags in `state` which worked for the wrong reason).

# daedalus 0.2.29

This patch version implements a request from UKHSA to allow modelling pre-existing immunity by placing some proportion of the model population in the vaccinated stratum during model initialisation, by specifying `p_immune` in `initial_state_manual` in `daedalus()` and `daedalus_multi_infection()`. See function documentation details for more (PR #118 reviewed by @OliverPolhillUKHSA).

# daedalus 0.2.28

This patch makes a small fix to the internal function `prepare_data()` to make it robust to user-created country demography vectors when the vector names are stripped. Timesteps are now taken from the parent function `daedalus()` and `daedalus_multi_infection()`.

# daedalus 0.2.27

IPR is calculated using the transmission rate $\beta$ in the absence of modifiers from an NPI or spontaneous behavioural changes.

# daedalus 0.2.26

This patch adds the `<daedalus_npi>` class which is used to store NPI response parameters. `daedalus()` now accepts these objects as inputs to `response_strategy`.

- Changes to the `<daedalus_response>` super-class to add an optional string identifier to identify pre-defined scenarios.

- Helper functions added for the new NPI class;

- Minor edits to the `<daedalus_response>` super-class to accommodate the NPI class;

- Minor edits to `<daedalus_vaccination>`;

- C++ `daedalus::inputs::read_response` now reads `<daedalus_response>` class member `name`.

## Notes

From this version onwards, _daedalus_ requires _daedalus.data_ >= v0.0.3 to provide the data `closure_strategy_names` and `closure_strategy_data`.

# daedalus 0.2.25

This patch version adds the use of HFR and hospital transition rates to calculate competing rates within the C++ code, which was previously handled in `daedalus.data`.

## Notes

- A next step will be to implement transitions out of the `H` compartment as conditional probabilities, rather than competing rates.

- Previous versions of `daedalus` which rely on `daedalus.data` and the old parameter sets are now superseded. Please update `daedalus.data` to version 0.0.2 if you intend to continue working with this new version of `daedalus`.

# daedalus 0.2.24

This patch version implements changes to the `<daedalus_response>` super-class that allows events to end on increasing _or_ decreasing roots on state. This is mostly (and currently only) to help end vaccination after a specific number of doses have been given out. This patch currently only applies to vaccination, with other events being updated later.

- **NOTE** that state-dependent events now have a minimum duration of 7 days; this is to prevent unexpected termination when the solver finds both an increasing and decreasing root within a single step, leading to immediate event termination after launch.

# daedalus 0.2.23

This patch version adds the function `get_fiscal_costs()` to calculate the costs to governments of pandemic response. These are calculated as post-processing on a model run, given some user-specified assumptions about public spending, interest rates, and tax rates.

# daedalus 0.2.22

This patch version overhauls how events are represented and handled in R.

## Breaking changes

- `<daedalus_vaccination>` now requires a `country` to be specified so that the uptake-limit can be respected by the event (but it does not yet, see Notes and PR #91).

## Other changes

- Adds the `<daedalus_response>` S3 super-class in R from which event sub-classes should inherit;

- Adds the `daedalus::inputs` namespace to C++ with a function to read the response class from R and generate a `daedalus::events::response`;

- Implements `<daedalus_vaccination>` inheriting from `<daedalus_response>` and passing to C++ using `read_response`. Also changes how `<daedalus_vaccination>` is initialised to account for state-based event-ending.

## Notes

`<daedalus_vaccination>`s require changes to `daedalus::events::response` to allow for the event to end on an increasing root (the uptake-limit); for now a pre-existing mechanism is used to ensure that the uptake-limit is respected.

# daedalus 0.2.21

This patch version logs the realised times of all events. When events are launched multiple times, the start time is updated. All start times are held as special variables in `state`. The NPI start time is used to determine the realised NPI end time by testing for a duration, and enables time-limitation for state-triggered events. Other events do not have a fixed end time (other than NPI-linked social distancing).

- Added class member function `make_duration_test` to class `daedalus::events::response`. Events are now treated as having a start time and a duration, rather than an end time.

**Note that** this change makes it more likely that short-duration state-triggered events will be launched multiple times in a single simulation run of 600 days. This **will** result in unpredictable closure durations in most cases, as closures can be launched by breaching hospital capacity.

- Added class member function `make_duration_test` to class `daedalus::events::response`.

- Updated `get_daedalus_response_times()` and `get_costs()` to correctly accommodate multiple closures.

- Updates to input checking, output handling, tests, and documentation for time-limitations on state-triggered events.

- Event data are now included in `<daedalus_output>` from `daedalus()` and `daedalus_multi_infection()`.

- `daedalus::events::response` member functions for event condition checks now include a check on expected flag status to prevent events which would not change flag status.

- Most tests account for new closure duration calculations due to multiple epidemic peaks and event launches.

# daedalus 0.2.20

Restored model functionality where mortality is increased when hospital capacity is exceeded.

- Added a new flag to the state which indicates whether hospital capacity is exceeded or not;

- Promoted the hospital-capacity-exceeded mechanism to a full event that is launched and ended only on a state trigger;

- Updated some `daedalus::events::response` class to accept a vector of states to sum for state-off triggers;

- Updated `daedalus::events::response` to generate only events with a known time or state trigger;

- Fixed an issue first seen in PR #91 where using 0.0 as a dummy or null state values caused events to trigger unexpectedly, now using R provided `NA_REAL` in `inst/dust/daedalus.cpp`.

# daedalus 0.2.19

Implements a new vignette with a technical description of the model.

# daedalus 0.2.18

Patch version to correct erroneous book-keeping of costs due to absences and closures; tests added to check that absence costs are not very low.

# daedalus 0.2.17

Splits off spontaneous social distancing from being linked with NPIs, and fixes a bug where specifying `response = "none"` left social distancing on by default.

New argument `auto_social_distancing` in both `daedalus()` and `daedalus_multi_infection()` allows this feature to be off, independent and on all the time, or linked to NPIs and on only when NPIs are active.

# daedalus 0.2.16

Patch version to allow passing a list of infection objects to the Daedalus model using the new function `daedalus_multi_infection()`. This helps model multiple different pandemics on the same population, or to model uncertainty in outcomes due to uncertainty in infection parameters for a single pandemic pathogen.`daedalus_multi_infection()` returns a list of output objects.

- Added a vignette showing how to use `daedalus_multi_infection()`;

- Added functions to more cleanly handle inputs to `daedalus()` and `daedalus_multi_infection()`;

- Some functions used in v0.1.0 have been removed.

# daedalus 0.2.15

Fixes to construction of initial model state.

# daedalus 0.2.14

This patch renames `daedalus2()` to `daedalus()`. All references and helper functions for `daedalus2()` have replaced their `daedalus()` equivalents, if any.

- All data has been moved to the package [_daedalus.data_](https://github.com/jameel-institute/daedalus.data), and is imported from there.

# daedalus 0.2.13

This patch version replaces all calls to `daedalus()` with `daedalus2()`; this excludes equivalence tests. Some tests have been updated to conform to the function signature of `daedalus2()`; this is mostly related to how the NPI triggering  hospital threshold is passed to the function.

# daedalus 0.2.12

This patch version adds a `response_duration` argument to `daedalus2()`, and tests for time-limited responses.

# daedalus 0.2.11

This patch version adds logging of new vaccinations in each age and economic group.

- `daedalus2()` now returns data on new vaccinations and is compatible with `get_new_vaccinations()`; new vaccinations are stored as a vector of the size of age + economic groups after the main state but before the model flags;

- `daedalus_ode` does not zero new vaccinations - this is handled by `get_new_vaccinations()`;

- `daedalus2()` accepts optional ODE control arguments for _dust2_ via `...`;

- Internal function `prepare_output_cpp()` accommodates new vaccination data in `output`.

# daedalus 0.2.10

- `daedalus_ode` class no longer zeroes data compartments for new infections and new hospitalisations --- this is handled by the pre-existing `get_incidence()` function. This change will be reversed in a future rewrite of `get_incidence()`;

- `<daedalus_country>` class takes argument `group_working_age` for the index of the working age group;

- `<daedalus_output>` class prints information on country, epidemic, and response;

- Added `FLAG_NAMES` for variables in `dust2` output to remove;

- Added internal function `get_daedalus2_response_times()` to extract event data from `daedalus2_internal()` for `<daedalus_output>` class;

- `daedalus2()` returns `<daedalus_output>` class object similar to `daedalus()`;

- Removed some constants associated with now-removed `daedalus_rtm()` function;

- Function `prepare_output_cpp()` rewritten to handle output from `daedalus2_internal()`;

- Updated tests for classed output from `daedalus2()`;

    - Skipping tests for vaccination start time as this data is not compatible with class `<daedalus_output>`; these will be switched on in future.

- Added continuous benchmarking for `daedalus2()`;

- Updated vignette on `daedalus2()` for classed output.

# daedalus 0.2.9

- Added public-concern social distancing to `daedalus2()`, and re-added it to `daedalus()`; this reverses the removal in v0.2.3. The functions are equivalent: social-distancing is active only when an NPI is active.

- Added a filter on `NULL` to `daedalus2()` parameter collection step - this helps remove parameters and allows treating missing values as a meaningful indicator in the C++ code (as 0.0 can be a valid value in many cases).

- **Removed** default response duration of 60 days in `daedalus2()` as this is not present in `daedalus()` leading to non-equivalence in tests.

# daedalus 0.2.8

## Breaking changes

daedalus2() output type is now a two-element list with event data included. Time-series data are in `data`. This output type will be converted to `<daedalus_output>` in future versions.

This patch version implements a `response` class to better organise event handling. All events can be launched and ended at specific times, or by specific state variables crossing threshold values. Only some of these triggers can be controlled by the user from R; others are hard-coded in C++:

- NPI response start time is added as an argument to `daedalus2()` similar to `daedalus()`;

- NPI response starts on `hospital_threshold` (a country parameters) being crossed;

- NPI response is intended to end when the [incidence-prevalence ratio reaches the recovery rate](https://doi.org/10.1097/01.aids.0000244213.23574.fa) (for asymptomatic infections); this does not appear to work as the solver jumps over this root.

- Vaccination start time can be specified by the user in the vaccination object or strategy; no specific end triggers are specified as vaccination rate $\nu$ is scaled within the ODE RHS to decline as the uptake limit is reached.

## C++ code

- Added class `response` under namespace `daedalus::events` to be a general data structure and lambda generator for NPI and vaccination events.

- Added `response` objects to shared state to hold event parameters that were previously floating around; reorganised parameter reading.

- Replaced contents of `events` struct in `daedalus_ode` class with `response` member functions for NPI and vaccination objects.

- Removed state variables logging response start and end times.

# daedalus 0.2.7

Added a mechanism to have vaccination switched on from model start time if specified by users in a `<daedalus_vaccination>` object passed to the `vaccine_investment` argument; this is because {dust2} events cannot [root-find on time at `t = 0` --- see linked PR](https://github.com/mrc-ide/dust2/pull/152).

- Internal function `dummy_vaccination()` allows creation of a `<daedalus_vaccination>` with start time = 0 using internal class constructor. Vaccination class validator now accepts `"dummy"` as a valid vaccination strategy name. This is not affected by the changes to the initial vaccination flag (all other parameters are also set to 0.0).

# daedalus 0.2.6

This version adds the response strategy functionality, with response start and end implemented as {dust2} events.

- Added dependency on {dust2} on branch `mrc-6531`;

## Parameter preparation

- Country class returns hospital capacity and population size;

- Vaccination class returns uptake limit and start time.

## Constants and flags

- Added `N_FLAGS` constant and `initial_flags()`.

- Flags are implemented as state variables.

- Flag relative positions in state added to constants header.

## Helper functions

- Renamed helper function `daedalus::helpers::zero_which` to `get_state_idx`;

- Helper function `daedalus::interventions::switch_by_flag` added to interventions helpers header; used to switch vaccination rate and implement NPIs.

## Model state

- Tracking the incidence-prevalence ratio as a measure of whether the epidemic is growing as a simpler alternative to $R_t$; tracked in state variable `growth_flag`.

## Other

- Added tests for events;

- NOTE: Skipping test for response ended by epidemic shrinking as solver jumps over root; awaiting fix for this.

# daedalus 0.2.5

- Adds a correction for reducing eligibles to vaccination in `daedalus2()` by re-implementing `scale_nu()` in C++. Adds a simple test for vaccination uptake limit.

- Adds C++ headers for types and dimensions.

# daedalus 0.2.4

## Breaking changes

The vaccination class `<daedalus_vaccination>` has renamed invariants. Workflows using custom vaccination regimes will break. No issues are expected for the dashboard.

_daedalus_ now requires R > v3.5 (due to use of package data) --- this is a belated update.

## Model changes

- `daedalus2()` supports infections for the vaccinated stratum similar to `daedalus()`; vaccination is now leaky. The default is absolutely no vaccination, which differs from `daedalus()` where 'none' refers to advance investment;

- `daedalus()` takes vaccine efficacy and waning period from the `vaccine_investment` argument, see below;

## Class changes

- `<daedalus_vaccination>` has been updated with shorter invariants that drop prefix `vax` from names;

- `<daedalus_vaccination>`s now hold information on the vaccine efficacy (single value) and waning period (single value); these can be modified using the construction helper `daedalus_vaccination()` or the `set_data()` method;

- A new `prepare_parameters2()` method for the vaccination class to satisfy `daedalus2()`;

## Other changes

- Tests for vaccination working in `daedalus2()`;

- Updated to function documentation for changes to vaccination class.

- Code formatting using [Air](https://posit-dev.github.io/air/), added `air.toml` and disabled `commas_linter` as it conflicts with Air.

# daedalus 0.2.3

## Breaking changes

- `daedalus()` no longer includes auto-social distancing; this is to check for equivalence with `daedalus2()`. This will be re-added once the event-trigger mapping feature is developed.

- `daedalus2()` implementation of vaccination is very basic: vaccination begins at model start time and does not end, vaccination does not correct for reducing eligible population, vaccination is non-leaky

## Model changes

1. Added full DAEDALUS ODE structure to `daedalus_ode` class for `daedalus2()` -- except events, auto-social distancing, and excess mortality; `daedalus2()` accepts `country` and `infection` similar to `daedalus()`.

    - `daedalus2()` takes vaccination arguments for vaccination rate and waning rate.

2. Fixes consumer-worker contact scaling in `daedalus2()`.

3. `daedalus_ode` class uses `Eigen::Tensor` over `Eigen::Matrix` for numerical operations.

## C++ code

1. Added `inst/include/daedalus_constants.h` for model related constants;

2. Added `inst/include/daedalus_helpers.h` with function `zero_which()` to return compartments to zero.

## R code

1. Added intermediate internal helper generic and methods `prepare_parameters2()` and `make_initial_state2()` for use with `daedalus2()`.

## Other

1. Added more tests for output expectations and fixed population size for `daedalus2()`.

2. Added tests for equivalence between `daedalus()` and `daedalus2()`.

3. Added function documentation and updated replacement DAEDALUS vignette.

4. Update `.lintr` for _lintr_ v3.2.0.

## Dependencies

- Now linking to _Rcpp_ and _RcppEigen_ over _cpp11eigen_.

# daedalus 0.2.2

This version adds support for including contact matrices in the `daedalus` class object and using them in the FOI calculation.

# daedalus 0.2.1

This is a patch version of {daedalus} that builds up to combining `daedalus::daedalus()` with `daedalus::daedalus_rtm()`.

**Breaking changes**

- `daedalus_rtm()` has been removed; please see installation instructions in the Readme for how install a {daedalus} version that provides it;

- {Rcpp} and {RcppEigen} have been replaced with {cpp11} and {cpp11eigen};

- Tests using `daedalus_rtm()` have been removed;

- Continuous benchmarking removed for `daedalus_rtm()`.

**Changes for dust2 implementation**

- Dependencies {dust2} and {monty} added;

- Added successor function `daedalus2()` which will replace `daedalus()`;

- Toy SEIR model with flexible number of demographic strata added in `inst/dust/daedalus.cpp`; this will build up to the full model;

- Infrastructure files updated for changes above;

- Added small vignette to check functionality;

- Added small header and package header with a helper function `zero_which()` in `daedalus::helpers` namespace to identify which compartments in the state to zero with `zero_every()`.

# daedalus 0.2.0

This is a minor version release of {daedalus} that adds real-time epidemic modelling functionality while removing the implementation of vaccination and vaccination-group infection pathways.
There are no intervening versions between this and v0.1.0.

**NOTE:** This version has the following identified errors:

1. Consumer-to-worker infections are not correctly handled leading to later and lower epidemic peaks than expected.

2. The number of recovered does not include recoveries from hospitalisation.

**Changes**

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

**NOTE:** This version has been found to have the following identified errors:

1. Consumer-to-worker infections are not correctly handled leading to higher epidemic peaks which also occur sooner than expected.

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
