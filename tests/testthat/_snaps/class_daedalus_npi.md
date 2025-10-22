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
      o$response_data$npi_info
    Output
      $npi_times_start
      [1] 30 90
      
      $npi_times_end
      [1]  50 120
      
      $npi_durations
      [1] 20 30
      
      $npi_periods
       [1]  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48
      [20]  49  50  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
      [39] 107 108 109 110 111 112 113 114 115 116 117 118 119 120
      

# daedalus: time-launched response duration is correct

    Code
      o$response_data$npi_info
    Output
      $npi_times_start
      [1] 10
      
      $npi_times_end
      [1] 40
      
      $npi_durations
      [1] 30
      
      $npi_periods
       [1] 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
      [26] 35 36 37 38 39 40
      

# Test for epidemic size

    Code
      get_epidemic_summary(daedalus("GB", "sars_cov_1", "elimination", time_end = 100))
    Output
             value                measure
      1   51945.86           total_deaths
      2 6202062.30          epidemic_size
      3  471037.51 total_hospitalisations

