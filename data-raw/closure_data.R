## code to prepare `closure_data` dataset goes here

library(readxl)
library(usethis)
library(data.table)
library(stringr)

closures <- readxl::read_xlsx(
  "inst/extdata/economic_closures.xlsx",
  sheet = "configurations"
)

# heal names and set DT
new_colnames <- colnames(closures) |>
  str_to_lower() |>
  str_replace_all("\\s\\(|\\s", "_") |>
  str_remove_all("\\)")
setDT(closures)
setnames(closures, new_colnames)

# round all coefficient values to two significant figures
coefficients <- setdiff(colnames(closures), "sector")
closures[, (coefficients) := lapply(.SD, function(x) round(x, 2)),
  .SDcols = coefficients
]

# replace previous magic numbers with column names (configurations/strategies)
configuration <- c(
  "lockdown", "elimination", "lockdown",
  "economic_closures", "lockdown_school_closures",
  "school_closures"
)

# get sector openness coefficients
closure_coefs <- as.list(closures)[configuration]

# prepare strategy and implementation identifiers
strategies <- c("elimination", "economic_closures", "school_closures")
levels <- c("heavy", "light")

# prepare data.table of strategies and implementation level
closure_data <- CJ(strategy = strategies, level = levels)
closure_data[, openness := closure_coefs]

closure_data <- split(closure_data, by = "strategy") |>
  lapply(function(dt) {
    coefs <- dt[["openness"]]
    names(coefs) <- dt[["level"]]
    coefs
  })

# add special strategy "none" to represent no response
# might have non-zero values in future to represent spontaneous public
# effects on openness
closure_data[["none"]] <- list(
  light = rep(1.0, N_ECON_SECTORS),
  heavy = rep(1.0, N_ECON_SECTORS)
)

# JIDEA-132: pick a single implementation level for each response strategy
# elimination: heavy; all others: light
levels_to_keep <- c("heavy", rep("light", 3L))
strategies <- c(strategies, "none")

closure_data <- Map(
  closure_data[strategies], levels_to_keep,
  f = function(s, l) {
    s[[l]]
  }
)

usethis::use_data(closure_data, overwrite = TRUE)
