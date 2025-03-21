fevent <- function(
  npi = list(
    name = "none",
    start = list(
      varb = c("time", "hosp", "growth"),
      sign = c("equal", "increase", "decrease"),
      value = c(10, 1e3, NA_real_)
    ),
    end = list(
      varb = c("time", "hosp", "growth"),
      sign = c("equal", "decrease", "decrease"),
      value = c(50, 2e3, NA_real_)
    )
  )
) {
  # matrices of start and end times
  start <- matrix(NA, 2, 3)
  rownames(start) <- c("npi", "vax")
  colnames(start) <- c("time", "hosp", "growth")
  end <- start

  # signs for start and end
  start_sign <- start
  end_sign <- start

  start["npi", ] <- npi$start$value
  start_sign["npi", ] <- npi$start$sign

  end["npi", ] <- npi$end$value
  end_sign["npi", ] <- npi$end$sign

  list(
    start = start,
    end = end,
    start_sign = start_sign,
    end_sign = end_sign
  )
}
