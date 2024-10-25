## code to prepare `infection_data` dataset goes here

library(data.table)
library(checkmate)
library(stringr)

# read data from inst/extdata/
data <- fread("inst/extdata/sevenpathogens.csv")

# standardise names
colnames(data) <- str_to_lower(colnames(data)) |> str_replace_all("\\s|-", "_")

#### IFR and IHR data ####
# split off IHR and IFR data for separate handling
data_ifr_ihr <- data[code_label %like% "(ifr\\d+)|(ihr\\d+)", ]
data_ifr_ihr <- melt(
  data_ifr_ihr,
  id.vars = c("code_label", "definition"),
  variable.name = "epidemic"
)

# assign age groups and get the mean for IHR and IFR
bin_size <- 5
n_age_groups <- 4
n_pathogens <- 7
n_measures <- 2

data_ifr_ihr[, c("age_lower", "age_upper") := list(
  as.numeric(str_extract(code_label, "\\d+")) * bin_size - bin_size,
  as.numeric(str_extract(code_label, "\\d+")) * bin_size
)]

data_ifr_ihr[, age_group := fcase(
  age_upper <= 5L, "0-4",
  age_lower >= 5L & age_upper <= 20L, "5-19",
  age_lower >= 20L & age_upper <= 65, "20-64",
  age_lower >= 65, "65+"
)]

data_ifr_ihr[, measure := str_extract(code_label, "\\w{3}")]

# summarise IFR and IHR for group wise rates
data_ifr_ihr <- data_ifr_ihr[, list(
  value = mean(value)
), by = c("measure", "age_group", "epidemic")]

# check data
assert_data_frame(
  data_ifr_ihr,
  types = c("character", "factor", "character", "numeric"),
  any.missing = FALSE, all.missing = FALSE,
  nrows = n_age_groups * n_pathogens * n_measures, ncols = 4L
)

#### Calculate model rates ####
data_params <- data[!code_label %like% "(ifr\\d+)|(ihr\\d+)", ] |>
  melt(
    id.vars = c("code_label", "definition"),
    variable.name = "epidemic"
  )
data_params$definition <- NULL

#### R0 ####
data_r0 <- data_params[code_label == "R0", ]
data_r0[, measure := "r0"]
data_r0 <- data_r0[, c("measure", "epidemic", "value")]

#### sigma: E -> I ####

## sigma: E -> Ia or Is
data_sigma <- data_params[code_label == "Tlat", ]
data_sigma[, value := 1 / value][, measure := "sigma"]
data_sigma <- data_sigma[, c("measure", "epidemic", "value")]

#### p(sigma): proportion symptomatic ####
data_psigma <- data_params[code_label == "ps", ]
data_psigma[, measure := "p_sigma"]
data_psigma <- data_psigma[, c("measure", "epidemic", "value")]

#### eta: Is -> H ####
# hospitalisation rate = IHR{i} / p_sigma
data_ifr_ihr <- dcast(
  data_ifr_ihr,
  age_group + epidemic ~ measure,
  value.var = "value"
)
data_hosp <- data_params[code_label %in% c("Tsh", "Thd", "Threc", "ps"), ] |>
  dcast(
    epidemic ~ code_label,
    value.var = "value"
  )

data_omega_eta <- merge(data_ifr_ihr, data_hosp)

# calculate daily rates
# eta = (ihr / p_sigma) / Tsh
# p(death) = ifr / ihr
# T{hosp} = p(death) * Thd + (1 - p(death)) * Threc
# omega = p(death) / T{hosp}
# gamma_H = (1 - p(death)) / T{hosp}
data_omega_eta[, c("eta", "p_death") := list(
  (ihr / ps) / Tsh,
  ifr / ihr
)]
data_omega_eta[, t_hosp := p_death * Thd + (1 - p_death) * Threc]
data_omega_eta[, c("omega", "gamma_H") := list(
  p_death / t_hosp,
  (1 - p_death) / t_hosp
)]

data_omega_eta <- data_omega_eta[, c(
  "epidemic", "age_group", "ifr", "eta", "omega", "gamma_H"
)]

## epsilon: relative contribution of asymptomatics
# NOTE: all values are identical - consider removing
data_epsilon <- data_params[code_label == "red", ]
data_epsilon[, measure := "epsilon"]
data_epsilon <- data_epsilon[, c("measure", "epidemic", "value")]

## gamma_Ia: recovery rate for infectious asymptomatic
data_gamma_is <- data_params[code_label == "Tsr", ]
data_gamma_is[, value := 1 / value][, measure := "gamma_Is"]
data_gamma_is <- data_gamma_is[, c("measure", "epidemic", "value")]

## gamma_Ia: recovery rate for infectious asymptomatic
data_gamma_ia <- data_params[code_label == "Tay", ]
data_gamma_ia[, value := 1 / value][, measure := "gamma_Ia"]
data_gamma_ia <- data_gamma_ia[, c("measure", "epidemic", "value")]

## rho: rate of waning of infection-derived immunity
# NOTE: all values are identical - consider removing
data_rho <- data_params[code_label == "Ti", ]
data_rho[, value := 1 / value][, measure := "rho"]
data_rho <- data_rho[, c("measure", "epidemic", "value")]

## combine pathogen data and convert to named list of lists
infection_data <- rbindlist(
  list(
    data_r0, data_sigma, data_psigma, data_epsilon, data_rho,
    data_gamma_ia, data_gamma_is
  )
)
infection_data <- split(infection_data, by = "epidemic") |>
  lapply(function(dt) {
    l <- as.list(dt[["value"]])
    names(l) <- dt[["measure"]]

    l
  })

## combine age-specific data
data_omega_eta <- split(data_omega_eta, by = c("epidemic")) |>
  lapply(function(dt) {
    list(
      ifr = dt[["ifr"]],
      eta = dt[["eta"]],
      omega = dt[["omega"]],
      gamma_H = dt[["gamma_H"]]
    )
  })

infection_data <- Map(
  infection_data, data_omega_eta[names(infection_data)],
  f = function(x, y) {
    c(x, y)
  }
)

usethis::use_data(infection_data, overwrite = TRUE)

# save infection parameter names, access first element
infection_parameter_names <- names(infection_data[[1L]])
usethis::use_data(infection_parameter_names, overwrite = TRUE)

# save epidemic names for easy reference
epidemic_names <- names(infection_data)
usethis::use_data(epidemic_names, overwrite = TRUE)
