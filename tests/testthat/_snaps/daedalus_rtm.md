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
      
      
      $education_costs
      $education_costs$education_cost_total
      [1] 855.6248
      
      $education_costs$education_cost_closures
      [1] 0
      
      $education_costs$education_cost_absences
      [1] 855.6248
      
      
      $life_years_lost
      $life_years_lost$life_years_lost_total
      [1] 62025.83
      
      $life_years_lost$life_years_lost_age
            0-4      5-19     20-65       65+ 
       3760.974 14754.418  5609.041 37901.395 
      
      

