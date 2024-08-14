# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment econ_sector        value
      1     1       0-4  susceptible    sector_0 1.993130e+06
      2     2       0-4  susceptible    sector_0 1.993128e+06
      3     1      5-19  susceptible    sector_0 5.949103e+06
      4     2      5-19  susceptible    sector_0 5.949089e+06
      5     1     20-65  susceptible    sector_0 3.758015e+06
      6     2     20-65  susceptible    sector_0 3.758012e+06
      7     1       65+  susceptible    sector_0 6.832967e+06
      8     2       65+  susceptible    sector_0 6.832963e+06
      9     1       0-4      exposed    sector_0 0.000000e+00
      10    2       0-4      exposed    sector_0 1.777108e+00
      11    1      5-19      exposed    sector_0 0.000000e+00
      12    2      5-19      exposed    sector_0 1.304829e+01
      13    1     20-65      exposed    sector_0 0.000000e+00
      14    2     20-65      exposed    sector_0 3.193388e+00
      15    1       65+      exposed    sector_0 0.000000e+00
      16    2       65+      exposed    sector_0 3.878263e+00
      17    1       0-4  infect_symp    sector_0 1.993132e+00
      18    2       0-4  infect_symp    sector_0 1.664198e+00
      19    1      5-19  infect_symp    sector_0 5.949109e+00
      20    2      5-19  infect_symp    sector_0 5.345187e+00
      21    1     20-65  infect_symp    sector_0 3.758019e+00
      22    2     20-65  infect_symp    sector_0 3.127962e+00
      23    1       65+  infect_symp    sector_0 6.832974e+00
      24    2       65+  infect_symp    sector_0 5.219720e+00
      25    1       0-4 infect_asymp    sector_0 0.000000e+00
      26    2       0-4 infect_asymp    sector_0 7.082830e-02
      27    1      5-19 infect_asymp    sector_0 0.000000e+00
      28    2      5-19 infect_asymp    sector_0 5.158499e-01
      29    1     20-65 infect_asymp    sector_0 0.000000e+00
      30    2     20-65 infect_asymp    sector_0 1.275081e-01
      31    1       65+ infect_asymp    sector_0 0.000000e+00
      32    2       65+ infect_asymp    sector_0 1.558312e-01
      33    1       0-4 hospitalised    sector_0 0.000000e+00
      34    2       0-4 hospitalised    sector_0 1.161405e-05
      35    1      5-19 hospitalised    sector_0 0.000000e+00
      36    2      5-19 hospitalised    sector_0 1.120443e-01
      37    1     20-65 hospitalised    sector_0 0.000000e+00
      38    2     20-65 hospitalised    sector_0 3.791736e-04
      39    1       65+ hospitalised    sector_0 0.000000e+00
      40    2       65+ hospitalised    sector_0 3.789965e-01
      41    1       0-4    recovered    sector_0 0.000000e+00
      42    2       0-4    recovered    sector_0 4.621337e-01
      43    1      5-19    recovered    sector_0 0.000000e+00
      44    2      5-19    recovered    sector_0 1.463626e+00
      45    1     20-65    recovered    sector_0 0.000000e+00
      46    2     20-65    recovered    sector_0 8.694918e-01
      47    1       65+    recovered    sector_0 0.000000e+00
      48    2       65+    recovered    sector_0 1.522047e+00
      49    1       0-4         dead    sector_0 0.000000e+00
      50    2       0-4         dead    sector_0 5.058501e-07

