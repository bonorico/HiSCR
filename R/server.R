# Server logic

server <- function(input, output)
{
  
 
  # convert string inputs
  sm <- reactive(
    switch(input$sm,
           "Inverse risk ratio (1/RR)" = "RR",
           "Odds ratio (OR)" = "OR"
           
    ) 
  )
  
  method_RR <- reactive(
    switch(input$method_RR,
           "Wald" = "wald", 
           "Small sample size adjustment" = "small", 
           "Bootstrap" = "boot"
    )
    
  )
  
  
  method_OR <- reactive(
    switch(input$method_OR,
           "Median-unbiased estimation" = "midp", 
           "Fisher" = "fisher", 
           "Wald" = "wald", 
           "Small sample size adjustment" = "small"
    )
  )

  output$plot <- renderImage(
    {
      progress <- shiny::Progress$new()   # Adding progress bars
      on.exit(progress$close())
      progress$set(message="Updating results ...", value=0)
      
      #convert slider inputs
      AN <- input$AN_incr_range
      fist <- input$fist_incr_range
      absc <- input$abscesses_incr_range
      transition_var <- switch(input$transition_var,
                               "Number of abscesses" = "abscesses_incr",
                               "Number of draining fistulae" = "fist_incr"
      )
      
      progress$inc(0.7, detail="Please wait")
      
      # make animation
      outfile <- tempfile(fileext='.gif')
      
      p = plot_effect(data, 
                      transition_var = transition_var,
                      AN_incr_range = -seq(AN[1], AN[2], 0.05),
                      fist_incr_range = {
                        if (transition_var == "fist_incr")
                          seq(fist[1], fist[2], length.out = abs(fist[1] - fist[2]))
                        else
                          0
                      },
                      abscesses_incr_range = {
                        if (transition_var == "abscesses_incr")
                          seq(absc[1], absc[2], length.out = abs(absc[1] - absc[2]))
                        else
                          0
                      },
                      sm = sm(), 
                      method_RR = method_RR(),
                      method_OR = method_OR()
      )
      
      anim_save("outfile.gif", 
                animation = animate(p))
      
      progress$inc(0.3, detail="Rendering results") 
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 350
      )
    },
    deleteFile = TRUE
  )
  
  res <- reactive(
    RR(
      HiSCR(data, 
            (-1)*input$AN_decr, 
            input$fist_decr, 
            input$absc_decr),
      TRT, newHiSCR,          # fixed 
      sm(), method_RR(), method_OR()
    )
  )
  
  output$table <- renderPrint(
    {
      print(res()$data)
      }
  )
  
  output$estimate <- renderPrint(
    {
      print(res()$measure)
      }
  )
  
  output$method <- renderPrint(
    {
      attributes(res())$method
    }
  )
  
}
