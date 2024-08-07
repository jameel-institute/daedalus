# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment econ_sector        value
      1     1       0-4  susceptible    sector_0 1.993130e+06
      2     2       0-4  susceptible    sector_0 1.993122e+06
      3     1      5-19  susceptible    sector_0 5.949103e+06
      4     2      5-19  susceptible    sector_0 5.949085e+06
      5     1     20-65  susceptible    sector_0 3.758015e+06
      6     2     20-65  susceptible    sector_0 3.758014e+06
      7     1       65+  susceptible    sector_0 6.832967e+06
      8     2       65+  susceptible    sector_0 6.832962e+06
      9     1       0-4      exposed    sector_0 0.000000e+00
      10    2       0-4      exposed    sector_0 6.928411e+00
      11    1      5-19      exposed    sector_0 0.000000e+00
      12    2      5-19      exposed    sector_0 1.628182e+01
      13    1     20-65      exposed    sector_0 0.000000e+00
      14    2     20-65      exposed    sector_0 1.107330e+00
      15    1       65+      exposed    sector_0 0.000000e+00
      16    2       65+      exposed    sector_0 4.430223e+00
      17    1       0-4  infect_symp    sector_0 1.993132e+00
      18    2       0-4  infect_symp    sector_0 1.981438e+00
      19    1      5-19  infect_symp    sector_0 5.949109e+00
      20    2      5-19  infect_symp    sector_0 5.539073e+00
      21    1     20-65  infect_symp    sector_0 3.758019e+00
      22    2     20-65  infect_symp    sector_0 2.996088e+00
      23    1       65+  infect_symp    sector_0 6.832974e+00
      24    2       65+  infect_symp    sector_0 5.253761e+00
      25    1       0-4 infect_asymp    sector_0 0.000000e+00
      26    2       0-4 infect_asymp    sector_0 2.716678e-01
      27    1      5-19 infect_asymp    sector_0 0.000000e+00
      28    2      5-19 infect_asymp    sector_0 6.395174e-01
      29    1     20-65 infect_asymp    sector_0 0.000000e+00
      30    2     20-65 infect_asymp    sector_0 4.408541e-02
      31    1       65+ infect_asymp    sector_0 0.000000e+00
      32    2       65+ infect_asymp    sector_0 1.778419e-01
      33    1       0-4 hospitalised    sector_0 0.000000e+00
      34    2       0-4 hospitalised    sector_0 1.234026e-05
      35    1      5-19 hospitalised    sector_0 0.000000e+00
      36    2      5-19 hospitalised    sector_0 1.134390e-01
      37    1     20-65 hospitalised    sector_0 0.000000e+00
      38    2     20-65 hospitalised    sector_0 3.738813e-04
      39    1       65+ hospitalised    sector_0 0.000000e+00
      40    2       65+ hospitalised    sector_0 3.797809e-01
      41    1       0-4    recovered    sector_0 0.000000e+00
      42    2       0-4    recovered    sector_0 5.235519e-01
      43    1      5-19    recovered    sector_0 0.000000e+00
      44    2      5-19    recovered    sector_0 1.501044e+00
      45    1     20-65    recovered    sector_0 0.000000e+00
      46    2     20-65    recovered    sector_0 8.436704e-01
      47    1       65+    recovered    sector_0 0.000000e+00
      48    2       65+    recovered    sector_0 1.528817e+00
      49    1       0-4         dead    sector_0 0.000000e+00
      50    2       0-4         dead    sector_0 5.214640e-07

