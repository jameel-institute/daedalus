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
