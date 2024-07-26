# Basic tests for the DAEDALUS model
# Prepare some data
polymod <- socialmixr::polymod
# suppress messages that clog up test output
suppressMessages(
  contact_data <- socialmixr::contact_matrix(
    polymod,
    countries = "United Kingdom",
    age.limits = c(0, 5, 20, 65),
    symmetric = TRUE
  )
)

# get demography vector
demography <- contact_data[["demography"]][["population"]]

# prepare contact matrix
contact_matrix <- t(contact_data[["matrix"]]) / demography

# initial state: one in every 1 million is infected
initial_i <- 1e-6
initial_state <- c(
  S = 1.0 - initial_i, E = 0.0,
  Is = initial_i, Ia = 0.0,
  H = 0.0, R = 0, D = 0
)

# build for all age groups
initial_state <- rbind(
  initial_state,
  initial_state,
  initial_state,
  initial_state
)

# multiply by demography vector for absolute values
initial_state <- initial_state * demography

# construct state array, all working age individuals are uniformly
# distributed into economic sectors including non-working
state <- array(
  0.0,
  dim = c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
)
state[, , i_NOT_WORKING] <- initial_state
state[i_WORKING_AGE, , ] <- matrix(
  state[i_WORKING_AGE, , i_NOT_WORKING] * 1.0 / (N_ECON_STRATA),
  nrow = N_EPI_COMPARTMENTS, ncol = N_ECON_STRATA
)

#### Prepare workplace contacts ####
cw <- rep(5.0, N_ECON_SECTORS)

#### Prepare consumer worker contacts ####
consumer_contacts_per_sector <- withr::with_seed(
  0L,
  stats::runif(N_ECON_SECTORS, min = 5.0, max = 10.0)
)
cmcw <- matrix(
  consumer_contacts_per_sector,
  nrow = N_ECON_SECTORS, ncol = N_AGE_GROUPS
) * demography / sum(demography)

# helper function returning inputs for tests
default_inputs <- function() {
  list(
    initial_state = state,
    parameters = default_parameters(
      contact_matrix = contact_matrix,
      contacts_workplace = cw,
      contacts_consumer_worker = cmcw
    )
  )
}
