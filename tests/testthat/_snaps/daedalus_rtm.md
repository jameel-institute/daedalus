# daedalus real-time modelling: downstream func compat

    Code
      get_epidemic_summary(output)
    Output
             value                measure
      1    74016.6           total_deaths
      2 53505977.3          epidemic_size
      3   144895.0 total_hospitalisations

---

    Code
      get_costs(output)
    Output
      $total_cost
      [1] 75269.71
      
      $economic_costs
      $economic_costs$economic_cost_total
      [1] 12388.26
      
      $economic_costs$economic_cost_closures
      [1] 0
      
      $economic_costs$economic_cost_absences
      [1] 12388.26
      
      $economic_costs$sector_cost_closures
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      [39] 0 0 0 0 0 0 0
      
      $economic_costs$sector_cost_absences
       [1]   71.543627    3.847111  114.324056   14.131760    8.460358  196.303945
       [7]   44.867162   18.386690   63.744469   28.614199   72.649163   88.338392
      [13]   52.284154   36.825836   25.939804  102.685085   91.864411   32.218177
      [19]  104.951442  113.871640   79.483849  119.922195  177.006130  161.873886
      [25]  764.243150 1444.871325  194.580599   53.237381   38.632098  163.306403
      [31]   85.058924  415.977169  223.950787  226.071304  375.964629  881.194033
      [37] 1866.606609  991.546779  691.318342  645.219145  855.624781 1027.772578
      [43]  220.657082  216.715163   37.199279
      
      
      $education_costs
      $education_costs$education_cost_total
      [1] 855.6248
      
      $education_costs$education_cost_closures
      [1] 0
      
      $education_costs$education_cost_absences
      [1] 855.6248
      
      
      $life_value_lost
      $life_value_lost$life_value_lost_total
      [1] 62025.83
      
      $life_value_lost$life_value_lost_age
            0-4      5-19     20-65       65+ 
       3760.974 14754.418  5609.041 37901.395 
      
      
      $life_years_lost
      $life_years_lost$life_years_lost_total
      [1] 1352209
      
      $life_years_lost$life_years_lost_age
            0-4      5-19     20-65       65+ 
       81992.02 321657.24 122281.24 826278.51 
      
      

