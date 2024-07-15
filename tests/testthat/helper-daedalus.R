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

# helper function returning inputs for tests
default_inputs <- function() {
  list(
    initial_state = initial_state,
    parameters = default_parameters(contact_matrix = contact_matrix)
  )
}
