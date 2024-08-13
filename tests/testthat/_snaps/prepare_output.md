# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment econ_sector        value
      1     1       0-4  susceptible    sector_0 1.993130e+06
      2     2       0-4  susceptible    sector_0 1.993123e+06
      3     1      5-19  susceptible    sector_0 5.949103e+06
      4     2      5-19  susceptible    sector_0 5.949085e+06
      5     1     20-65  susceptible    sector_0 3.758015e+06
      6     2     20-65  susceptible    sector_0 3.758014e+06
      7     1       65+  susceptible    sector_0 6.832967e+06
      8     2       65+  susceptible    sector_0 6.832961e+06
      9     1       0-4      exposed    sector_0 0.000000e+00
      10    2       0-4      exposed    sector_0 6.627097e+00
      11    1      5-19      exposed    sector_0 0.000000e+00
      12    2      5-19      exposed    sector_0 1.632677e+01
      13    1     20-65      exposed    sector_0 0.000000e+00
      14    2     20-65      exposed    sector_0 1.483166e+00
      15    1       65+      exposed    sector_0 0.000000e+00
      16    2       65+      exposed    sector_0 5.483026e+00
      17    1       0-4  infect_symp    sector_0 1.993132e+00
      18    2       0-4  infect_symp    sector_0 1.969045e+00
      19    1      5-19  infect_symp    sector_0 5.949109e+00
      20    2      5-19  infect_symp    sector_0 5.547202e+00
      21    1     20-65  infect_symp    sector_0 3.758019e+00
      22    2     20-65  infect_symp    sector_0 3.020357e+00
      23    1       65+  infect_symp    sector_0 6.832974e+00
      24    2       65+  infect_symp    sector_0 5.319521e+00
      25    1       0-4 infect_asymp    sector_0 0.000000e+00
      26    2       0-4 infect_asymp    sector_0 2.637017e-01
      27    1      5-19 infect_asymp    sector_0 0.000000e+00
      28    2      5-19 infect_asymp    sector_0 6.446058e-01
      29    1     20-65 infect_asymp    sector_0 0.000000e+00
      30    2     20-65 infect_asymp    sector_0 5.942915e-02
      31    1       65+ infect_asymp    sector_0 0.000000e+00
      32    2       65+ infect_asymp    sector_0 2.203522e-01
      33    1       0-4 hospitalised    sector_0 0.000000e+00
      34    2       0-4 hospitalised    sector_0 1.231796e-05
      35    1      5-19 hospitalised    sector_0 0.000000e+00
      36    2      5-19 hospitalised    sector_0 1.135145e-01
      37    1     20-65 hospitalised    sector_0 0.000000e+00
      38    2     20-65 hospitalised    sector_0 3.748632e-04
      39    1       65+ hospitalised    sector_0 0.000000e+00
      40    2       65+ hospitalised    sector_0 3.813025e-01
      41    1       0-4    recovered    sector_0 0.000000e+00
      42    2       0-4    recovered    sector_0 5.216636e-01
      43    1      5-19    recovered    sector_0 0.000000e+00
      44    2      5-19    recovered    sector_0 1.503067e+00
      45    1     20-65    recovered    sector_0 0.000000e+00
      46    2     20-65    recovered    sector_0 8.484611e-01
      47    1       65+    recovered    sector_0 0.000000e+00
      48    2       65+    recovered    sector_0 1.541949e+00
      49    1       0-4         dead    sector_0 0.000000e+00
      50    2       0-4         dead    sector_0 5.210498e-07

