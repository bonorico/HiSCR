# UI

ui <- fluidPage(
  
  h5("Phase III clinical trial (ACT vs Placebo): HiSCR response at week 16 by varying HiSCR definitions*"),
  h6("By Federico Bonofiglio"),
  
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
                     "Select a method for the 95% confidence intervals (CIs):",
                     choices = c("Wald", "Small sample size adjustment", "Bootstrap"),
                     selected = "Wald"
        )
        
      ),
      
      conditionalPanel(
        condition = "input.sm == 'Odds ratio (OR)'",
        radioButtons("method_OR",
                     "Select a method for the 95% confidence intervals (CIs):",
                     choices = c("Median-unbiased estimation", "Fisher", "Wald", "Small sample size adjustment"),
                     selected = "Median-unbiased estimation"
        )
        
      ),
      
      br(), br(),
      p("*The standard HiSCR definition is at least a -50% change in AN count and no increase in the other conditions relative to baseline at week 16.")
      
      
      
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(
          "Graphical result",
          
          column(12, align = "center",
                 
                 imageOutput("plot"),
          ),
          
          hr(),
          
          fluidRow(
            
            column(12, 
                   br(), br(),br(), br(),br(), br(),
                   p("-->The red dot is the reference estimate under the standard HiSCR definition*."),
                   p("-->The horizontal red line (y = 1) indicates no treatment effect.")
            )
          )
          
        ),
        
        tabPanel(
          "Contingency table",
          h5("Based on the graphical result, you can narrow down your computation and inspect it in more detail."),
          h6("Please do not use the slider inputs. Select your options below (changes from baseline). You can still vary effect-measure and method via the leftside bar."),
          
          fluidRow(
            column(4,
                   numericInput("AN_decr",
                                "Select a fraction decrease in AN count:",
                                min = 0.1,
                                max = 0.9,
                                value = 0.5,
                                step = 0.05
                   )
            ),
            
            column(4,
                   numericInput("absc_decr",
                                "Select a change in number of abscesses:",
                                min = -10,
                                max = 10,
                                value = 0,
                                step = 1
                   )
                   
            ),
            
            column(4,
                   numericInput("fist_decr",
                                "Select a change in number of draining fistulae:",
                                min = -10,
                                max = 10,
                                value = 0,
                                step = 1
                   )
                   
            )
          ),
          
          p("Contingency table (patients count by varying HiSCR definitions above):"),
          verbatimTextOutput("table"),
          p("Resulting point estimate and 95% CIs:"),
          verbatimTextOutput("estimate"),
          p("Method used:"),
          verbatimTextOutput("method")
        )
        
      )
      
    )
  )
  
  
)
