## code to prepare `sector_gva_data` dataset goes here

# NOTE: all GVA values are in millions of dollars per day

library(readxl)
library(data.table)

# reading row 1 as colnames not working
data <- readxl::read_xlsx(
  "inst/extdata/sector_gva_data.xlsx",
  sheet = "Daily"
)
# set colnames manually
colnames(data) <- data[1, ]

# remove first row
data <- data[-1L, ]

# convert GVA data to numerics
setDT(data)
data[, setdiff(colnames(data), "Country") := lapply(.SD, as.numeric),
  .SDcols = setdiff(colnames(data), "Country")
]
setnames(data, "Country", "country")

# some sanity checks
expected_n_cols <- 46L
stopifnot(
  "Data should have 46 columns (45 econ sectors + country name)" =
    ncol(data) == expected_n_cols,
  "GVA data should be numeric" =
    all(vapply(data[, -1L], is.numeric, logical(1)))
)

# melt data and split into a list of vector
data <- melt(data, id.vars = "country")

sector_gva_data <- split(data, by = "country")
sector_gva_data <- lapply(sector_gva_data, `[[`, "value")

# place in alphabetical order
sector_gva_data <- sector_gva_data[sort(names(sector_gva_data))]

usethis::use_data(sector_gva_data, overwrite = TRUE)
