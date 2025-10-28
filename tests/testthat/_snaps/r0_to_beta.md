# Calculating beta from R0

    Code
      get_beta(infection, country)
    Output
      [1] 0.02679135

# Getting the next-generation matrix

    Code
      get_ngm(country, infection)
    Output
                  [,1]      [,2]      [,3]       [,4]
      0-4   0.18631519 0.1495673 0.4575726 0.02784939
      5-19  0.04990423 0.8493952 0.5713183 0.07214655
      20-64 0.15881787 0.3382386 1.4122404 0.21092348
      65+   0.01148082 0.0734091 0.3434458 0.14794466

