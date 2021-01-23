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

#Per year tables of the population change and damage numbers  
  
  output$county_ratio <- renderTable({
    req(input$county)
    disasters %>%
      filter(State == input$state & County == input$county) %>%
      select('Year', 'Affected Population Change', 'Total Damage', 
             'Expected Damage', 'Magic Number') %>% 
      mutate(Year = as.character(Year),
           `Total Damage` = as.integer(`Total Damage`),
           `Expected Damage` = as.integer(`Expected Damage`)) %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE) %>% 
      select(-'State', -'County') #WHY? WHY DO I HAVE TO DO THIS???
  }, striped = TRUE, align = 'r') #county_ratio closing Parent
  
  output$state_ratio <- renderTable({
    req(input$state)
    disasters %>%
      filter(State == input$state) %>%
      select('Year', 'Affected Population Change', 'Total Damage', 
             'Expected Damage', 'Magic Number') %>% 
      group_by(Year) %>% 
      summarise(`Affected Population Change` = sum(`Affected Population Change`, na.rm = TRUE),
                `Total Damage` = sum(`Total Damage`),
                `Expected Damage` = sum(`Expected Damage`, na.rm = TRUE),
                `Magic Number` = mean(`Magic Number`, na.rm = TRUE)) %>% 
      mutate(Year = as.character(Year),
             `Total Damage` = as.integer(`Total Damage`),
             `Expected Damage` = as.integer(`Expected Damage`),
             `Affected Population Change` = as.integer(`Affected Population Change`)) %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE)
  }, striped = TRUE, align = 'r') #state_ratio closing Parent
  
#Summary Tables of Disaster Data  
  
  output$county_sum_table <- renderTable({  
    req(input$county)
    floods %>%
      filter(State == input$state & County == input$county) %>%
      select('Year', 'Month', 'Event','Damage_Nominal', 'Damage_CPIAdj', 'CPI') %>%
      group_by(Year) %>%
      summarise('Total Nominal Damage' = sum(Damage_Nominal), 
                'Total CPI Adjusted Damage' = sum(Damage_CPIAdj),
                'Max Nominal Damage' = max(Damage_Nominal),
                'Max CPI Adjusted Damage' = max(Damage_CPIAdj),
                'Mean Nominal Damage' = mean(Damage_Nominal),
                'Mean CPI Adjusted Damage' = mean(Damage_CPIAdj)) %>%
      mutate(Year = as.character(Year),
             `Mean Nominal Damage` = as.integer(`Mean Nominal Damage`),
             `Mean CPI Adjusted Damage` = as.integer(`Mean CPI Adjusted Damage`)) %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE)
  }, striped = TRUE, align = 'r') #county_sum_table closing Paren
      
  output$state_sum_table <- renderTable({
    req(input$state)
    floods %>%
      filter(State == input$state) %>%
      select('Year', 'Month', 'Event','Damage_Nominal', 'Damage_CPIAdj', 'CPI') %>%
      group_by(Year) %>%
      summarise('Total Nominal Damage' = sum(Damage_Nominal), 
                'Total CPI Adjusted Damage' = sum(Damage_CPIAdj),
                'Max Nominal Damage' = max(Damage_Nominal),
                'Max CPI Adjusted Damage' = max(Damage_CPIAdj),
                'Mean Nominal Damage' = mean(Damage_Nominal),
                'Mean CPI Adjusted Damage' = mean(Damage_CPIAdj)) %>%
      mutate(Year = as.character(Year),
             `Mean Nominal Damage` = as.integer(`Mean Nominal Damage`),
             `Mean CPI Adjusted Damage` = as.integer(`Mean CPI Adjusted Damage`)) %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE)
  }, striped = TRUE, align = 'r') #state_sum_table closing Paren
  
#format.data.frame cannot select individuals columns (I tried for a long time), you know, because R...so to 
#avoid commas being added to the year column, I convert that column to a character beforehand temporarily
  
  output$county_full <- renderTable({
    req(input$county)
    floods %>%
      filter(State == input$state & County == input$county) %>%
      arrange(date_order, Event) %>%
      select('Year', 'Month', 'Event','Damage_Nominal', 'Damage_CPIAdj') %>%
      mutate(Year = as.character(Year),
            `Nominal Damage` = Damage_Nominal,
            `CPI Adjusted Damage` = Damage_CPIAdj) %>%
      select(-'Damage_Nominal', -'Damage_CPIAdj') %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE)
  }, striped = TRUE, align = 'r') #county_full closing Paren    

  output$state_full <- renderTable({  
    req(input$state)
    floods %>%
      filter(State == input$state) %>%
      arrange(date_order, County) %>%
      select('Year', 'Month', 'Event', 'County', 'Damage_Nominal', 'Damage_CPIAdj') %>%
      mutate(Year = as.character(Year), 
             `Nominal Damage` = Damage_Nominal, 
             `CPI Adjusted Damage` = Damage_CPIAdj) %>%
      select(-'Damage_Nominal', -'Damage_CPIAdj') %>%
      format.data.frame(justify = 'right', big.mark = ',', trim = TRUE)
  }, striped = TRUE, align = 'r') #state_full closing Paren
   
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
  
  output$state_selection2 <- renderText({
    req(input$state)
    paste('State of ', input$state)
  }) #state_selection closing Paren
  
  output$state_selection3 <- renderText({
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
  
  output$county_selection2 <- renderText({
    req(input$county)
    paste(input$county, ' County, ', input$state)
  }) #county_selection closing Paren  

  output$county_selection3 <- renderText({
    req(input$county)
    paste(input$county, ' County, ', input$state)
  }) #county_selection closing Paren      
  
}) #shinyServer closing Paren