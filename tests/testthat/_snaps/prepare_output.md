# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment econ_sector        value
      1     1       0-4  susceptible    sector_0 3.453667e+06
      2     2       0-4  susceptible    sector_0 3.453664e+06
      3     1      5-19  susceptible    sector_0 1.134561e+07
      4     2      5-19  susceptible    sector_0 1.134559e+07
      5     1     20-65  susceptible    sector_0 7.785732e+05
      6     2     20-65  susceptible    sector_0 7.785727e+05
      7     1       65+  susceptible    sector_0 9.673048e+06
      8     2       65+  susceptible    sector_0 9.673044e+06
      9     1       0-4      exposed    sector_0 0.000000e+00
      10    2       0-4      exposed    sector_0 2.096073e+00
      11    1      5-19      exposed    sector_0 0.000000e+00
      12    2      5-19      exposed    sector_0 1.778277e+01
      13    1     20-65      exposed    sector_0 0.000000e+00
      14    2     20-65      exposed    sector_0 4.141361e-01
      15    1       65+      exposed    sector_0 0.000000e+00
      16    2       65+      exposed    sector_0 4.004034e+00
      17    1       0-4  infect_symp    sector_0 3.453670e+00
      18    2       0-4  infect_symp    sector_0 3.196092e+00
      19    1      5-19  infect_symp    sector_0 1.134562e+01
      20    2      5-19  infect_symp    sector_0 1.168497e+01
      21    1     20-65  infect_symp    sector_0 7.785740e-01
      22    2     20-65  infect_symp    sector_0 7.139657e-01
      23    1       65+  infect_symp    sector_0 9.673058e+00
      24    2       65+  infect_symp    sector_0 8.747167e+00
      25    1       0-4 infect_asymp    sector_0 0.000000e+00
      26    2       0-4 infect_asymp    sector_0 1.199020e-01
      27    1      5-19 infect_asymp    sector_0 0.000000e+00
      28    2      5-19 infect_asymp    sector_0 1.006646e+00
      29    1     20-65 infect_asymp    sector_0 0.000000e+00
      30    2     20-65 infect_asymp    sector_0 2.364872e-02
      31    1       65+ infect_asymp    sector_0 0.000000e+00
      32    2       65+ infect_asymp    sector_0 2.301395e-01
      33    1       0-4 hospitalised    sector_0 0.000000e+00
      34    2       0-4 hospitalised    sector_0 3.041864e-02
      35    1      5-19 hospitalised    sector_0 0.000000e+00
      36    2      5-19 hospitalised    sector_0 1.038736e-01
      37    1     20-65 hospitalised    sector_0 0.000000e+00
      38    2     20-65 hospitalised    sector_0 6.835359e-03
      39    1       65+ hospitalised    sector_0 0.000000e+00
      40    2       65+ hospitalised    sector_0 8.451430e-02
      41    1       0-4    recovered    sector_0 0.000000e+00
      42    2       0-4    recovered    sector_0 4.759597e-01
      43    1      5-19    recovered    sector_0 0.000000e+00
      44    2      5-19    recovered    sector_0 1.652437e+00
      45    1     20-65    recovered    sector_0 0.000000e+00
      46    2     20-65    recovered    sector_0 1.068011e-01
      47    1       65+    recovered    sector_0 0.000000e+00
      48    2       65+    recovered    sector_0 1.317700e+00
      49    1       0-4         dead    sector_0 0.000000e+00
      50    2       0-4         dead    sector_0 1.580683e-04

