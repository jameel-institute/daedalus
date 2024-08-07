#!/usr/bin/env Rscript
## code to prepare `country_data` ##

library(data.table)
library(stringr)
library(usethis)
library(checkmate)

# column names
# Npop: individuals in each age group
# NNs: working age (?) individuals in each economic sector
# CM: contacts between groups `i` and `j`, for 16 age groups per
# Walker et al. 2020

# load country data
country_data <- fread("data-raw/country_data.csv")
sector_contacts <- fread("data-raw/sectorcontacts.csv")

# NOTE: there is an issue with Monaco where most data are missing

#### process demography data ####
cols_demography <- colnames(country_data)[colnames(country_data) %like% "Npop"]
country_demography <- country_data[, c("country", ..cols_demography)] |>
  melt(id.vars = "country")

bin_size <- 5L
n_age_groups <- 4L

country_demography[, c("age_lower", "age_upper") := list(
  as.numeric(str_extract(variable, "\\d+")),
  as.numeric(str_extract(variable, "\\d+"))
)]
country_demography[, `:=`(
  age_lower = (age_lower * bin_size) - bin_size,
  age_upper = age_upper * bin_size
)]

# assume that intervals are open on the RHS, but note the subsetting
# appears different
# TODO: check with EPPI re: intervals
daedalus_demography <- copy(country_demography)
daedalus_demography[, age_group := fcase(
  age_upper <= 5L, "0-4",
  age_lower >= 5L & age_upper <= 20L, "5-19",
  age_lower >= 20L & age_upper <= 65, "20-64",
  age_lower >= 65, "65+"
)]
daedalus_demography <- daedalus_demography[
  ,
  list(value = sum(value)),
  by = c("country", "age_group")
]

# split by country and convert to named vector
daedalus_demography <- split(daedalus_demography, by = "country")
daedalus_demography <- lapply(daedalus_demography, function(dt) {
  v <- round(dt[["value"]]) # round to avoid fractions if any
  names(v) <- dt[["age_group"]]
  v
})

# run assertion on demography data
assert_list(
  daedalus_demography, "numeric",
  any.missing = FALSE, all.missing = FALSE,
  len = nrow(country_data), names = "unique"
)
assert_true(
  all(
    vapply(
      daedalus_demography, test_integerish, logical(1),
      lower = 0, any.missing = FALSE, all.missing = FALSE,
      len = n_age_groups
    )
  )
)

#### process contacts data ####
cols_contacts <- colnames(country_data)[colnames(country_data) %like% "CM"]
country_contacts <- country_data[, c("country", ..cols_contacts)]

# remove "CM" from the name, melt and prepare for long-format
setnames(
  country_contacts,
  old = cols_contacts,
  new = str_remove(cols_contacts, "(CM)")
)
country_contacts <- melt(country_contacts, id.vars = "country")

country_contacts[, c("to", "from") := transpose(
  str_split(country_contacts$variable, "")
)]
country_contacts$variable <- NULL

# assign age groups
lookup <- setNames(seq_along(letters), letters)
daedalus_contacts <- copy(country_contacts)
daedalus_contacts[, c("to", "from") := list(
  lookup[to] * bin_size,
  lookup[from] * bin_size
)]

# TODO: check whether values represent a symmetric matrix, i.e., does
# to and from matter
# assume open intervals on RHS
daedalus_contacts[, c("to", "from") := list(
  fcase(
    to <= 5L, "0-4",
    to >= 5L & to <= 20L, "5-19",
    to >= 20L & to <= 65L, "20-64",
    to >= 65L, "65+"
  ),
  fcase(
    from <= 5L, "0-4",
    from >= 5L & from <= 20L, "5-19",
    from >= 20L & from <= 65L, "20-64",
    from >= 65L, "65+"
  )
)]

# TODO: correct contact averaging procedure
daedalus_contacts <- daedalus_contacts[,
  list(value = mean(value, na.rm = TRUE)),
  by = c("country", "to", "from")
]

levels <- c("0-4", "5-19", "20-64", "65+")
daedalus_contacts[, c("to", "from") := list(
  to = factor(to, levels = levels),
  from = factor(from, levels = levels)
)]

# split by country and cast to wide
daedalus_contacts <- split(daedalus_contacts, by = "country")
daedalus_contacts <- lapply(daedalus_contacts, function(dt) {
  dt <- dcast(dt, to ~ from, value.var = "value")
  dt <- as.matrix(dt, rownames = "to")
  dt
})

# check processed matrices
# NOTE: Andorra and Monaco are both NaN - check this data
assert_true(
  all(
    vapply(
      daedalus_contacts, test_matrix, logical(1),
      mode = "numeric", nrows = n_age_groups,
      ncols = n_age_groups
    )
  )
)
check_true(
  all(
    vapply(
      daedalus_contacts, test_numeric, logical(1),
      lower = 0.0, finite = TRUE, any.missing = FALSE, all.missing = FALSE
    )
  )
)

#### process economic sector data ####
# TODO: figure out which sector is non-working
cols_workers <- colnames(country_data)[
  colnames(country_data) %like% "NNs"
]
country_workers <- country_data[, c("country", ..cols_workers)]
country_workers <- melt(country_workers, id.vars = "country")

# fill NAs with zeros; TODO: check if defensible
country_workers$value <- nafill(country_workers$value, "const", fill = 0L)

# match sector names from sector contacts data
setnames(country_workers, "variable", "sector")
country_workers[, sector := as.numeric(str_extract(sector, "\\d+"))]
country_workers[, sector_name := sector_contacts$sector[sector]]

# split by country and return vector of sector-wise values only
daedalus_workers <- split(country_workers, by = "country") |>
  lapply(`[[`, "value")

# check worker data
assert_list(
  daedalus_workers, "numeric",
  any.missing = FALSE, all.missing = FALSE,
  len = nrow(country_data), names = "unique"
)
assert_true(
  all(
    vapply(
      daedalus_workers, test_integerish, logical(1),
      lower = 0, any.missing = FALSE, all.missing = FALSE,
      len = daedalus:::N_ECON_SECTORS
    )
  )
)

### combine and save country-wise data ####
# NOTE: names are provisional
country_data <- Map(daedalus_demography, daedalus_contacts, daedalus_workers,
  f = function(x, y, z) {
    l <- list(
      demography = x,
      contact_matrix = y,
      workers = z
    )
    l
  }
)
names(country_data) <- names(daedalus_contacts)

# order alphabetically
country_data <- country_data[sort(names(country_data))]

# allow overwriting as this will probably change often
usethis::use_data(country_data, overwrite = TRUE)

#### save country names ####
country_names <- names(country_data)
usethis::use_data(country_names, overwrite = TRUE)
