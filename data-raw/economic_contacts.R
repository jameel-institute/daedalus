#!/usr/bin/env Rscript
## code to prepare `economic_contacts` dataset goes here

library(usethis)
library(data.table)
library(checkmate)

# notation
# `cw`: contacts between workers within sectors
# `cm_cw`: contacts between consumers and workers
# `cm_ww`: contacts worker-to-worker between economic sectors

#### process social contacts within economic sectors ####
contacts_workplace <- fread("inst/extdata/sectorcontacts.csv")
contacts_workplace <- contacts_workplace[["n_cnt"]]

assert_numeric(
  contacts_workplace,
  lower = 0, any.missing = FALSE, all.missing = FALSE,
  len = 45L
)

#### worker-to-worker contacts between sectors ####
# prepare dummy values with low to no effect
contacts_between_sectors <- matrix(
  0.0, N_ECON_SECTORS, N_ECON_SECTORS
)
diag(contacts_between_sectors) <- 0.0

economic_contacts <- list(
  contacts_workplace = contacts_workplace,
  contacts_between_sectors = contacts_between_sectors
)

usethis::use_data(economic_contacts, overwrite = TRUE)
