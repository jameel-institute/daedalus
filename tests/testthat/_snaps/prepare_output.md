# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment econ_sector        value
      1     1       0-4  susceptible    sector_0 1.993130e+06
      2     2       0-4  susceptible    sector_0 1.993128e+06
      3     1      5-19  susceptible    sector_0 5.949103e+06
      4     2      5-19  susceptible    sector_0 5.949099e+06
      5     1     20-65  susceptible    sector_0 3.758015e+06
      6     2     20-65  susceptible    sector_0 3.758015e+06
      7     1       65+  susceptible    sector_0 6.832967e+06
      8     2       65+  susceptible    sector_0 6.832966e+06
      9     1       0-4      exposed    sector_0 0.000000e+00
      10    2       0-4      exposed    sector_0 1.443020e+00
      11    1      5-19      exposed    sector_0 0.000000e+00
      12    2      5-19      exposed    sector_0 3.408532e+00
      13    1     20-65      exposed    sector_0 0.000000e+00
      14    2     20-65      exposed    sector_0 2.365583e-01
      15    1       65+      exposed    sector_0 0.000000e+00
      16    2       65+      exposed    sector_0 9.620536e-01
      17    1       0-4  infect_symp    sector_0 1.993132e+00
      18    2       0-4  infect_symp    sector_0 1.872108e+00
      19    1      5-19  infect_symp    sector_0 5.949109e+00
      20    2      5-19  infect_symp    sector_0 5.487339e+00
      21    1     20-65  infect_symp    sector_0 3.758019e+00
      22    2     20-65  infect_symp    sector_0 3.251922e+00
      23    1       65+  infect_symp    sector_0 6.832974e+00
      24    2       65+  infect_symp    sector_0 5.972769e+00
      25    1       0-4 infect_asymp    sector_0 0.000000e+00
      26    2       0-4 infect_asymp    sector_0 8.347651e-02
      27    1      5-19 infect_asymp    sector_0 0.000000e+00
      28    2      5-19 infect_asymp    sector_0 1.971950e-01
      29    1     20-65 infect_asymp    sector_0 0.000000e+00
      30    2     20-65 infect_asymp    sector_0 1.374581e-02
      31    1       65+ infect_asymp    sector_0 0.000000e+00
      32    2       65+ infect_asymp    sector_0 5.600735e-02
      33    1       0-4 hospitalised    sector_0 0.000000e+00
      34    2       0-4 hospitalised    sector_0 1.764977e-02
      35    1      5-19 hospitalised    sector_0 0.000000e+00
      36    2      5-19 hospitalised    sector_0 5.234271e-02
      37    1     20-65 hospitalised    sector_0 0.000000e+00
      38    2     20-65 hospitalised    sector_0 3.234297e-02
      39    1       65+ hospitalised    sector_0 0.000000e+00
      40    2       65+ hospitalised    sector_0 5.900984e-02
      41    1       0-4    recovered    sector_0 0.000000e+00
      42    2       0-4    recovered    sector_0 2.768202e-01
      43    1      5-19    recovered    sector_0 0.000000e+00
      44    2      5-19    recovered    sector_0 8.186306e-01
      45    1     20-65    recovered    sector_0 0.000000e+00
      46    2     20-65    recovered    sector_0 5.008677e-01
      47    1       65+    recovered    sector_0 0.000000e+00
      48    2       65+    recovered    sector_0 9.152617e-01
      49    1       0-4         dead    sector_0 0.000000e+00
      50    2       0-4         dead    sector_0 9.147122e-05

