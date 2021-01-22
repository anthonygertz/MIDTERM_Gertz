shinyServer(function(input, output) {
  
  output$county_dynamic <- renderUI({
    county_choices <- floods %>%
      filter(State == input$state) %>%
      distinct(County) %>% #I don't think I need this...
      deframe()
    selectInput('county',
                'Select County:',
                choices = c('', county_choices),
                selected = NULL)
  }) #county_dynamic closing Paren  

  output$county_full <- renderTable({
    req(input$county)
        floods %>%
      filter(County == input$county) %>%
      arrange(date_order, Event) %>%
      select('Year', 'Month', 'Event','Damage_Nominal', 'Damage_CPIAdj', 'CPI') %>%
      mutate(Year = as.character(Year)) %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE)
           
  
  }) #county_full closing Paren    
      
  output$state_full <- renderTable({  
    req(input$state)
    floods %>%
      filter(State == input$state) %>%
      arrange(date_order, County) %>%
      select('Year', 'Month', 'Event', 'County')
  }) #state_full closing Paren
   
#Text outputs for dynamic use in table titles 
#For some reason if I call one of these outputs more than once, it destroys my dashboard layout
  
  output$state_selection <- renderText({
    req(input$state)
    paste('State of ', input$state)
  }) #state_selection closing Paren
  
  output$state_selection1 <- renderText({
    req(input$state)
    paste('State of ', input$state)
  }) #state_selection closing Paren
  
  output$county_selection <- renderText({
    req(input$county)
    paste(input$county, ' County, ', input$state)
  }) #county_selection closing Paren   

  output$county_selection1 <- renderText({
    req(input$county)
    paste(input$county, ' County, ', input$state)
  }) #county_selection closing Paren     
  

  
  
}) #shinyServer closing Paren