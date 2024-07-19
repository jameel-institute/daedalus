# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment econ_sector        value
      1     1       0-4  susceptible    sector_1 3.453667e+06
      2     2       0-4  susceptible    sector_1 3.453664e+06
      3     1      5-19  susceptible    sector_1 1.134561e+07
      4     2      5-19  susceptible    sector_1 1.134559e+07
      5     1     20-65  susceptible    sector_1 7.958748e+05
      6     2     20-65  susceptible    sector_1 7.958743e+05
      7     1       65+  susceptible    sector_1 9.673048e+06
      8     2       65+  susceptible    sector_1 9.673044e+06
      9     1       0-4      exposed    sector_1 0.000000e+00
      10    2       0-4      exposed    sector_1 2.097303e+00
      11    1      5-19      exposed    sector_1 0.000000e+00
      12    2      5-19      exposed    sector_1 1.778780e+01
      13    1     20-65      exposed    sector_1 0.000000e+00
      14    2     20-65      exposed    sector_1 4.237952e-01
      15    1       65+      exposed    sector_1 0.000000e+00
      16    2       65+      exposed    sector_1 4.006770e+00
      17    1       0-4  infect_symp    sector_1 3.453670e+00
      18    2       0-4  infect_symp    sector_1 3.196227e+00
      19    1      5-19  infect_symp    sector_1 1.134562e+01
      20    2      5-19  infect_symp    sector_1 1.168551e+01
      21    1     20-65  infect_symp    sector_1 7.958756e-01
      22    2     20-65  infect_symp    sector_1 7.298821e-01
      23    1       65+  infect_symp    sector_1 9.673058e+00
      24    2       65+  infect_symp    sector_1 8.747469e+00
      25    1       0-4 infect_asymp    sector_1 0.000000e+00
      26    2       0-4 infect_asymp    sector_1 1.199720e-01
      27    1      5-19 infect_asymp    sector_1 0.000000e+00
      28    2      5-19 infect_asymp    sector_1 1.006927e+00
      29    1     20-65 infect_asymp    sector_1 0.000000e+00
      30    2     20-65 infect_asymp    sector_1 2.420039e-02
      31    1       65+ infect_asymp    sector_1 0.000000e+00
      32    2       65+ infect_asymp    sector_1 2.302958e-01
      33    1       0-4 hospitalised    sector_1 0.000000e+00
      34    2       0-4 hospitalised    sector_1 3.041909e-02
      35    1      5-19 hospitalised    sector_1 0.000000e+00
      36    2      5-19 hospitalised    sector_1 1.038754e-01
      37    1     20-65 hospitalised    sector_1 0.000000e+00
      38    2     20-65 hospitalised    sector_1 6.987426e-03
      39    1       65+ hospitalised    sector_1 0.000000e+00
      40    2       65+ hospitalised    sector_1 8.451531e-02
      41    1       0-4    recovered    sector_1 0.000000e+00
      42    2       0-4    recovered    sector_1 4.759699e-01
      43    1      5-19    recovered    sector_1 0.000000e+00
      44    2      5-19    recovered    sector_1 1.652477e+00
      45    1     20-65    recovered    sector_1 0.000000e+00
      46    2     20-65    recovered    sector_1 1.091783e-01
      47    1       65+    recovered    sector_1 0.000000e+00
      48    2       65+    recovered    sector_1 1.317723e+00
      49    1       0-4         dead    sector_1 0.000000e+00
      50    2       0-4         dead    sector_1 1.580695e-04

