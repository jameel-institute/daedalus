## code to prepare `life_expectancy_value` dataset goes here
# prepares the life expectancy and value-of-a-life-year data

library(data.table)
library(stringr)
library(usethis)

#### life expectancy per age group per country ####

life_expectancy <- fread("inst/extdata/life_expectancy.csv")
# subset relevant columns
life_expectancy <- life_expectancy[, c("Location", "Dim1", "Dim2", "Value")]
setnames(life_expectancy, c("country", "sex", "age_group", "value"))

# average over sex
life_expectancy <- life_expectancy[,
  list(value = mean(value)),
  by = c("country", "age_group")
]

# rescale values to DAEDALUS age groups
bin_size <- 5L
n_age_groups <- 4L

life_expectancy[, c("age_lower", "age_upper") :=
  transpose(
    str_split(
      str_extract(life_expectancy$age_group, "\\d+-\\d+"),
      "-"
    )
  )]
life_expectancy[
  age_group %in% c("85+ years", "<1 year"),
  c("age_lower", "age_upper") := str_extract(age_group, "\\d+")
]

life_expectancy[, c("age_lower", "age_upper") := lapply(
  .SD, as.numeric
), .SDcols = c("age_lower", "age_upper")]

# assign new age groups
life_expectancy[, age_group := fcase(
  age_upper <= 5L, "0-4",
  age_lower >= 5L & age_upper <= 20L, "5-19",
  age_lower >= 20L & age_upper <= 65, "20-65",
  age_lower >= 65, "65+"
)]

# average life expectancy over new age groups
life_expectancy <- life_expectancy[,
  list(value = mean(value)),
  by = c("country", "age_group")
]

# split data
life_expectancy <- split(life_expectancy, by = "country")
life_expectancy <- lapply(life_expectancy, `[[`, "value")

#### value of a life year per country ####
# the value of a life year in each country is taken as the gross national income
# (GNI) per capita in the latest year for which data is available
# in international dollars adjusted for purchasing power parity (PPP)

life_value <- fread("inst/extdata/value_life_year.csv")
life_value <- life_value[, c("Country Name", "MOST RECENT")]
setnames(life_value, c("country", "vly"))

# subset for countries in life expectancy
# 180 countries remain
life_value <- life_value[country %in% names(life_expectancy), ]

# split by country and combine with life expectancy data
life_value <- split(life_value, by = "country")
life_value <- lapply(life_value, `[[`, "vly")

#### value of life lost per age group ####
# subset life expectancy by life value as some are not available
# these are Cuba, NK, Syria

life_expectancy <- life_expectancy[names(life_value)]

life_value <- Map(
  life_expectancy, life_value,
  f = function(x, y) x * y
)

usethis::use_data(life_value, overwrite = TRUE)
