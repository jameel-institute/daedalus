## code to prepare `hospital_capacity_data` dataset goes here
# Hospital capacity is the spare hospital capacity available for pandemic
# response, and is calculated as
# total capacity * total population * (1 - bed occupancy rate)
# NOTE: capacity is in beds per thousand people

library(data.table)
library(usethis)

# read data
data_beds <- fread("inst/extdata/hospital_capacity_raw.csv")
data_occupancy <- fread("inst/extdata/hospital_BOR.csv")

# subset columns
data_beds <- data_beds[, c("Country Name", "Capacity")]
data_occupancy <- data_occupancy[, c("COUNTRY", "BOR")]

colnames(data_beds) <- c("country", "capacity")
colnames(data_occupancy) <- c("country", "bor")

# remove columns with NA for the country
data_beds <- data_beds[!is.na(country), ]
data_occupancy <- data_occupancy[!is.na(country), ]

# remove countries where the name is ""
data_beds <- data_beds[country != "", ]
data_occupancy <- data_occupancy[country != "", ]

# merge data and assign missing BOR as 85%
data_hosp_capacity <- merge(data_beds, data_occupancy, all.x = TRUE)
data_hosp_capacity$bor <- nafill(data_hosp_capacity$bor, fill = 85.0)

data_hosp_capacity$spare_capacity <- data_hosp_capacity$capacity *
  (1 - data_hosp_capacity$bor / 100)

write.csv(
  data_hosp_capacity,
  "inst/extdata/hospital_capacity.csv",
  row.names = FALSE
)

# save data in extdata and add to countries in `data-raw/country_data.R`
# as we need a single source for which countries are available
