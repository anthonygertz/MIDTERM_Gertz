shinyServer(function(input, output) {
  
  
  

  


  output$county_dynamic <- renderUI({
    county_choices <- censusdata %>%
      filter(state == input$state) %>%
      distinct(county) %>%
      deframe()
    selectInput('County',
                'Select County:',
                choices = c('', county_choices),
                selected = NULL) 
  }) #county_dynamic closing Paren   

   
  
}) #shinyServer closing Paren