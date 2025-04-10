#!/usr/bin/env Rscript
## code to prepare `country_data` ##

library(data.table)
library(stringr)
library(usethis)
library(checkmate)

# to convert names to ISO codes
library(countrycode)

# column names
# Npop: individuals in each age group
# NNs: working age (?) individuals in each economic sector
# CM: contacts between groups `i` and `j`, for 16 age groups per
# Walker et al. 2020

# load country data
country_data <- fread("inst/extdata/country_data.csv")
sector_contacts <- fread("inst/extdata/sectorcontacts.csv")

# NOTE: there is an issue with Monaco where most data are missing

#### process demography data ####
cols_demography <- colnames(country_data)[colnames(country_data) %like% "Npop"]
country_demography <- country_data[, c("country", ..cols_demography)] |>
  melt(id.vars = "country")

bin_size <- 5L
n_age_groups <- 4L

country_demography[,
  c("age_lower", "age_upper") := list(
    as.numeric(str_extract(variable, "\\d+")),
    as.numeric(str_extract(variable, "\\d+"))
  )
]
country_demography[, `:=`(
  age_lower = (age_lower * bin_size) - bin_size,
  age_upper = age_upper * bin_size
)]

# assume that intervals are open on the RHS, but note the subsetting
# appears different
# TODO: check with EPPI re: intervals
daedalus_demography <- copy(country_demography)
daedalus_demography[,
  age_group := fcase(
    age_upper <= 5L,
    "0-4",
    age_lower >= 5L & age_upper <= 20L,
    "5-19",
    age_lower >= 20L & age_upper <= 65,
    "20-64",
    age_lower >= 65,
    "65+"
  )
]

daedalus_demography_tmp <- copy(daedalus_demography) # needed later

daedalus_demography <- daedalus_demography[,
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
  daedalus_demography,
  "numeric",
  any.missing = FALSE,
  all.missing = FALSE,
  len = nrow(country_data),
  names = "unique"
)
assert_true(all(vapply(
  daedalus_demography,
  test_integerish,
  logical(1),
  lower = 0,
  any.missing = FALSE,
  all.missing = FALSE,
  len = n_age_groups
)))

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

country_contacts[,
  c("to", "from") := transpose(str_split(country_contacts$variable, ""))
]
country_contacts$variable <- NULL

# assign age groups - assume upper limit as identifier
# e.g. 0-5 identified by 5
lookup <- setNames(seq_along(letters), letters)
daedalus_contacts <- copy(country_contacts)
daedalus_contacts[,
  c("to", "from") := list(lookup[to] * bin_size, lookup[from] * bin_size)
]

# TODO: check whether values represent a symmetric matrix, i.e., does
# to and from matter
# assume open intervals on RHS
daedalus_contacts[,
  c("to_new", "from_new") := list(
    fcase(
      to <= 5L,
      "0-4",
      to >= 5L & to <= 20L,
      "5-19",
      to >= 20L & to <= 65L,
      "20-64",
      to >= 65L,
      "65+"
    ),
    fcase(
      from <= 5L,
      "0-4",
      from >= 5L & from <= 20L,
      "5-19",
      from >= 20L & from <= 65L,
      "20-64",
      from >= 65L,
      "65+"
    )
  )
]

# get weighted sum of contacts between new age group bins
# and scale by bin population
setnames(daedalus_demography_tmp, "value", "popsize")
daedalus_demography_tmp <- daedalus_demography_tmp[, c(
  "country",
  "age_upper",
  "popsize"
)]
setnames(daedalus_contacts, "value", "contacts")
daedalus_contacts <- merge(
  daedalus_contacts,
  daedalus_demography_tmp,
  by.x = c("country", "to"),
  by.y = c("country", "age_upper")
)

# weighted sum calculation here: must correspond to P2 drivers repo
daedalus_contacts <- daedalus_contacts[,
  list(
    contacts = sum(contacts * popsize),
    popsize = sum(unique(popsize)) # unique as popsize is repeated during join
  ),
  by = c("to_new", "from_new", "country")
]
daedalus_contacts[, contacts := contacts / popsize]

levels <- c("0-4", "5-19", "20-64", "65+")
daedalus_contacts[,
  c("to_new", "from_new") := list(
    to = factor(to_new, levels = levels),
    from = factor(from_new, levels = levels)
  )
]

# subset columns
daedalus_contacts <- daedalus_contacts[, c(
  "country",
  "to_new",
  "from_new",
  "contacts"
)]

# substitute missing values with 1s
daedalus_contacts[, contacts := nafill(contacts, fill = 1.0)]

# split by country and cast to wide
daedalus_contacts <- split(daedalus_contacts, by = "country")
daedalus_contacts <- lapply(daedalus_contacts, function(dt) {
  dt <- dcast(dt, to_new ~ from_new, value.var = "contacts")
  dt <- as.matrix(dt, rownames = "to_new")
  dt
})

# check processed matrices
# NOTE: Andorra and Monaco are both NaN - check this data
assert_true(all(vapply(
  daedalus_contacts,
  test_matrix,
  logical(1),
  mode = "numeric",
  nrows = n_age_groups,
  ncols = n_age_groups
)))
check_true(all(vapply(
  daedalus_contacts,
  test_numeric,
  logical(1),
  lower = 0.0,
  finite = TRUE,
  any.missing = FALSE,
  all.missing = FALSE
)))

#### process economic sector data ####
# TODO: figure out which sector is non-working
cols_workers <- colnames(country_data)[colnames(country_data) %like% "NNs"]
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
  daedalus_workers,
  "numeric",
  any.missing = FALSE,
  all.missing = FALSE,
  len = nrow(country_data),
  names = "unique"
)
assert_true(all(vapply(
  daedalus_workers,
  test_integerish,
  logical(1),
  lower = 0,
  any.missing = FALSE,
  all.missing = FALSE,
  len = daedalus:::N_ECON_SECTORS
)))

#### process economic sector gross value added data ####

# NOTE: all GVA values are in millions of dollars per day
# reading row 1 as colnames not working
gva_data <- readxl::read_xlsx(
  "inst/extdata/sector_gva_data.xlsx",
  sheet = "Daily"
)
# set colnames manually
colnames(gva_data) <- gva_data[1, ]

# remove first row
gva_data <- gva_data[-1L, ]

# convert GVA gva_data to numerics
setDT(gva_data)
gva_data[,
  setdiff(colnames(gva_data), "Country") := lapply(.SD, as.numeric),
  .SDcols = setdiff(colnames(gva_data), "Country")
]
setnames(gva_data, "Country", "country")

# some sanity checks
expected_n_cols <- 46L
stopifnot(
  "GVA data should have 46 columns (45 econ sectors + country name)" = ncol(
    gva_data
  ) ==
    expected_n_cols,
  "GVA data columns should be numeric" = all(vapply(
    gva_data[, -1L],
    is.numeric,
    logical(1)
  )),
  "GVA values should all be positive" = all(vapply(
    gva_data[, -1L],
    function(x) {
      all(x > 0)
    },
    logical(1L)
  ))
)

# NOTE: the check for positive values fails as Iceland Sector D:21 Pharma
# has a small negative value. Assuming this is a typo.

# melt data and split into a list of vector
gva_data <- melt(gva_data, id.vars = "country")
# NOTE: COERCE ALL VALUES POSITIVE
gva_data$value <- abs(gva_data$value)

daedalus_gva_data <- split(gva_data, by = "country")
daedalus_gva_data <- lapply(daedalus_gva_data, `[[`, "value")

### combine and save country-wise data ####
# NOTE: names are provisional
country_names <- names(daedalus_demography)
names(country_names) <- country_names
check_set_equal(
  names(daedalus_demography),
  names(daedalus_contacts),
  ordered = TRUE
)
assert_set_equal(
  names(daedalus_demography),
  names(daedalus_workers),
  ordered = TRUE
)

# get names of countries with GVA data
countries_with_gva <- names(daedalus_gva_data)
names(countries_with_gva) <- countries_with_gva
country_data_tmp <- lapply(countries_with_gva, function(n) {
  l <- list(
    demography = daedalus_demography[[n]],
    contact_matrix = daedalus_contacts[[n]],
    workers = daedalus_workers[[n]],
    gva = daedalus_gva_data[[n]]
  )
  l
})
assert_set_equal(names(country_data_tmp), countries_with_gva, ordered = TRUE)

# order alphabetically and overwrite original data read in
country_data <- copy(country_data_tmp[sort(names(country_data_tmp))])

# NOTE: removing Hong Kong and Taiwan as there is no corresponding
# data on life expectancy and/or value
to_remove <- c("Hong Kong", "Taiwan")

country_data <- country_data[!names(country_data) %in% to_remove]

#### adding hospital capacity data ####
hospital_capacity <- fread("inst/extdata/hospital_capacity.csv")

country_data <- country_data[names(country_data) %in% hospital_capacity$country]

hospital_capacity <- split(hospital_capacity, by = "country")
hospital_capacity <- lapply(hospital_capacity, `[[`, "spare_capacity")
hospital_capacity <- hospital_capacity[
  names(hospital_capacity) %in% names(country_data)
]

# check names match
stopifnot(
  "Country data and hospital capacity names must match" = identical(
    names(country_data),
    names(hospital_capacity)
  )
)

country_data <- Map(country_data, hospital_capacity, f = function(x, y) {
  x[["hospital_capacity"]] <- round(y * sum(x[["demography"]]) / 1000)

  x
})

# allow overwriting as this will probably change often
usethis::use_data(country_data, overwrite = TRUE)

#### save country names ####
country_names <- names(country_data)
usethis::use_data(country_names, overwrite = TRUE)

#### save country codes ####
country_codes_iso3c <- countrycode(country_names, "country.name", "iso3c")
country_codes_iso2c <- countrycode(country_names, "country.name", "iso2c")

usethis::use_data(country_codes_iso3c, overwrite = TRUE)
usethis::use_data(country_codes_iso2c, overwrite = TRUE)
