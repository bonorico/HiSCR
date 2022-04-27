# Server logic

server <- function(input, output)
{
  
  withProgress(message = 'Computing animation',
               detail = 'Please wait...', value = 0, 
               {
                 for (i in 1:7) {
                   incProgress(1/7)
                   Sys.sleep(0.25)
                 }
               }
               )
  
  output$plot <- renderImage(
    {
      #convert inputs
      transition_var <- switch(input$transition_var,
                               "Number of abscesses" = "abscesses_incr",
                               "Number of draining fistulae" = "fist_incr"
      )
      
      sm <- switch(input$sm,
                   "Inverse risk ratio (1/RR)" = "RR",
                   "Odds ratio (OR)" = "OR"
                   
      )
      
      method_RR <- switch(input$method_RR,
                          "Wald" = "wald", 
                          "Small sample size adjustment" = "small", 
                          "Bootstrap" = "boot"
      )
      
      method_OR <- switch(input$method_OR,
                          "Median-unbiased estimation" = "midp", 
                          "Fisher" = "fisher", 
                          "Wald" = "wald", 
                          "Small sample size adjustment" = "small"
      )
      
      # make animation
      outfile <- tempfile(fileext='.gif')
      
      p = plot_effect(data, 
                      transition_var = transition_var,
                      AN_incr_range = input$AN_incr_range*(-1),
                      fist_incr_range = {
                        if (transition_var == "fist_incr")
                          input$fist_incr_range
                        else
                          0
                      },
                      abscesses_incr_range = {
                        if (transition_var == "abscesses_incr")
                          input$abscesses_incr_range
                        else
                          0
                      },
                      sm = sm, 
                      method_RR = method_RR,
                      method_OR = method_OR
      )
      
      anim_save("outfile.gif", 
                animation = animate(p))
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif',
            width = 700,
            height = 500
           )
    },
    deleteFile = TRUE
  )
  
}
