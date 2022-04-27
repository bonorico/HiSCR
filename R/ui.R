# UI

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("AN_incr_range",
                  "Select a range for the fraction decrease in AN count:",
                  min = 0.1,
                  max = 0.9,
                  value = c(0.25, 0.75),
                  step = 0.05
      ),
      
      radioButtons("transition_var",
                   "Vary another condition:",
                   choices = c("Number of abscesses", 
                               "Number of draining fistulae"),
                   selected = "Number of abscesses"),
      
      conditionalPanel(
        condition = "input.transition_var == 'Number of abscesses'",
        sliderInput("abscesses_incr_range",
                    "Select a range for the change in number of abscesses from baseline:",
                    min = -10,
                    max = 10,
                    value = c(0, 5),
                    step = 1
        )
        
      ),
      
      conditionalPanel(
        condition = "input.transition_var == 'Number of draining fistulae'",
        sliderInput("fist_incr_range",
                    "Select a range for the change in number of draining fistulae from baseline:",
                    min = -10,
                    max = 10,
                    value = c(0, 5),
                    step = 1
        )
        
      ),
      
      radioButtons("sm",
                   "Select measure of treatment effect:",
                   choices = c("Inverse risk ratio (1/RR)", 
                               "Odds ratio (OR)"),
                   selected = "Inverse risk ratio (1/RR)"),
      
      conditionalPanel(
        condition = "input.sm == 'Inverse risk ratio (1/RR)'",
        radioButtons("method_RR",
                     "Select a method for the 95% confidence intervals (CIs) of 1/RR:",
                     choices = c("Wald", "Small sample size adjustment", "Bootstrap"),
                     selected = "Wald"
        )
        
      ),
      
      conditionalPanel(
        condition = "input.sm == 'Odds ratio (OR)'",
        radioButtons("method_OR",
                     "Select a method for the 95% confidence intervals (CIs) of OR:",
                     choices = c("Median-unbiased estimation", "Fisher", "Wald", "Small sample size adjustment"),
                     selected = "Median-unbiased estimation"
        )
        
      )
      
      
      
    ),
    
    mainPanel(
      imageOutput("plot")
    )
  )
  
  
)
