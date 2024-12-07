test_that("multiplication works", {
  expect_no_condition(
    daedalus_rtm("GBR", "influenza_1918")
  )
})

bench::mark(
  daedalus_rtm("GBR", "influenza_2009", time_end = 50)
)


x = daedalus_country("GBR")
params = list(
  beta = 0.1, sigma = 1/3, p_sigma = 2/3, gamma = 1/5,
  eta = 1/1000, omega = 1/200
)

state = make_initial_state(x, list())
state = as.matrix(state[,,1])

state = cbind(state, matrix(0, nrow(state), 1))

## 49x49 contacts
cm = matrix(NA, 49, 49)
cm[1:4, 1:4] = x$contact_matrix
cm[1:4, 5:49] = matrix(cm[1:4, 3], 4, 45)
cm[5:49, 1:4] = matrix(cm[3, 1:4], 45, 4, byrow = T)
# diag(cm)[5:49] = x$contacts_workplace + cm[3, 3] # community + work

cm[is.na(cm)] = cm[3, 3]

demog = rep(x$demography[3], 49)
demog[1:4] = x$demography
cm = cm / demog

## 4x4 contacts
# demog = x$demography
# cm = x$contact_matrix
# cm = cm / demog

openness = rep(1.0, 45)

contacts_work = x$contacts_workplace / x$workers

test_that("DAEDALUS CPP works", {
  expect_no_condition(
    .model_daedalus_cpp(
      state, params, cm, contacts_work, openness,
      t_start = 10, t_end = 100,
      time_end = 100
    )
  )
})
