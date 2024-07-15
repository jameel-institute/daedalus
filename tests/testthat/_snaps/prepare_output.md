# prepare_output: basic expectations with re-infections

    Code
      head(prepare_output(output), 50L)
    Output
         time age_group  compartment        value
      1     1       0-4  susceptible 3.453667e+06
      2     2       0-4  susceptible 3.453661e+06
      3     1      5-19  susceptible 1.134561e+07
      4     2      5-19  susceptible 1.134558e+07
      5     1     20-65  susceptible 3.581437e+07
      6     2     20-65  susceptible 3.581429e+07
      7     1       65+  susceptible 9.673048e+06
      8     2       65+  susceptible 9.673037e+06
      9     1       0-4      exposed 0.000000e+00
      10    2       0-4      exposed 4.726044e+00
      11    1      5-19      exposed 0.000000e+00
      12    2      5-19      exposed 2.851689e+01
      13    1     20-65      exposed 0.000000e+00
      14    2     20-65      exposed 6.296975e+01
      15    1       65+      exposed 0.000000e+00
      16    2       65+      exposed 9.858002e+00
      17    1       0-4  infect_symp 3.453670e+00
      18    2       0-4  infect_symp 3.477924e+00
      19    1      5-19  infect_symp 1.134562e+01
      20    2      5-19  infect_symp 1.281435e+01
      21    1     20-65  infect_symp 3.581440e+01
      22    2     20-65  infect_symp 3.758104e+01
      23    1       65+  infect_symp 9.673058e+00
      24    2       65+  infect_symp 9.376692e+00
      25    1       0-4 infect_asymp 0.000000e+00
      26    2       0-4 infect_asymp 2.655695e-01
      27    1      5-19 infect_asymp 0.000000e+00
      28    2      5-19 infect_asymp 1.590363e+00
      29    1     20-65 infect_asymp 0.000000e+00
      30    2     20-65 infect_asymp 3.537065e+00
      31    1       65+ infect_asymp 0.000000e+00
      32    2       65+ infect_asymp 5.555170e-01
      33    1       0-4 hospitalised 0.000000e+00
      34    2       0-4 hospitalised 3.135082e-02
      35    1      5-19 hospitalised 0.000000e+00
      36    2      5-19 hospitalised 1.075803e-01
      37    1     20-65 hospitalised 0.000000e+00
      38    2     20-65 hospitalised 3.301443e-01
      39    1       65+ hospitalised 0.000000e+00
      40    2       65+ hospitalised 8.659953e-02
      41    1       0-4    recovered 0.000000e+00
      42    2       0-4    recovered 4.969522e-01
      43    1      5-19    recovered 0.000000e+00
      44    2      5-19    recovered 1.735899e+00
      45    1     20-65    recovered 0.000000e+00
      46    2     20-65    recovered 5.266832e+00
      47    1       65+    recovered 0.000000e+00
      48    2       65+    recovered 1.364661e+00
      49    1       0-4         dead 0.000000e+00
      50    2       0-4         dead 1.604660e-04

