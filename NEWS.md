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
