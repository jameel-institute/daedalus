# class <daedalus_npi>: basic expectations

    Code
      npi
    Message
      <daedalus_npi/daedalus_response>
      NPI strategy: school_closures
      * Start time (days): 30
      * End time (days): NA
      * Openness (mean prop.): 0.93
      * Maximum duration (days): 365

# class <daedalus_npi>: sequential time-limited NPIs

    Code
      npi
    Message
      <daedalus_npi/daedalus_response>
      NPI strategy: custom_timed
      * Start time (days): 30 and 90
      * End time (days): 50 and 120
      * Openness (mean prop.): 0.93 and 0.93
      * Maximum duration (days): NA

---

    Code
      o$event_data
    Output
                              name      time index sign
      1 hosp_cap_exceeded_state_on  20.01819    11    1
      2              npi_time_on_1  30.00000     4    1
      3             npi_time_off_1  50.00000     5    1
      4              npi_time_on_2  90.00000     6    1
      5             npi_time_off_2 120.00000     7    1

# daedalus: time-launched response duration is correct

    Code
      o$event_data
    Output
                              name     time index sign
      1              npi_time_on_1 10.00000     4    1
      2             npi_time_off_1 40.00000     5    1
      3 hosp_cap_exceeded_state_on 76.94376     9    1

