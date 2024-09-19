## code to prepare `hospital_capacity_data` dataset goes here
# Hospital capacity is the spare hospital capacity available for pandemic
# response, and is calculated as
# total capacity * total population * (1 - bed occupancy rate)
# NOTE: capacity is in beds per thousand people

library(readxl)
library(usethis)

data_beds <- read_excel("inst/extdata/hospital_capacity.xlsx")
data_occupancy <- read_excel(
  "inst/extdata/hospital_capacity.xlsx",
  sheet = "BOR"
)
data_beds <- data_beds[, c("Country Name", "Capacity")]
data_occupancy <- data_occupancy[, c("COUNTRY", "BOR")]

colnames(data_beds) <- c("country", "capacity")
colnames(data_occupancy) <- c("country", "bor")

data_hosp_capacity <- merge(data_beds, data_occupancy)
data_hosp_capacity$spare_capacity <- data_hosp_capacity$capacity *
  (1 - data_hosp_capacity$bor / 100)

write.csv(
  data_hosp_capacity, "inst/extdata/hospital_capacity.csv",
  row.names = FALSE
)

# save data in extdata and add to countries in `data-raw/country_data.R`
# as we need a single source for which countries are available
