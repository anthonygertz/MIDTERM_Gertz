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
      
  output$state_sum_year <- renderTable({
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
  }, striped = TRUE, align = 'r') #state_sum_year closing Paren
  
  output$state_sum_table <- renderTable({
    req(input$state)
    disasters %>%
      filter(State == input$state) %>%
      select('State', 'Magic Number') %>%
      group_by(State) %>%
      summarise('Magic Number Minimum' = min(`Magic Number`, na.rm = TRUE), 
                'Magic Number Maximum' = max(`Magic Number`, na.rm = TRUE),
                'Magic Number Mean' = mean(`Magic Number`, na.rm = TRUE),
                'Magic Number Median' = median(`Magic Number`, na.rm = TRUE))
   }, striped = TRUE, align = 'r') #state_sum_table closing Paren
  
  output$state_all <- renderTable({
      disasters %>%
      select('State', 'Magic Number') %>%
      group_by(State) %>%
      summarise('Magic Number Minimum' = min(`Magic Number`, na.rm = TRUE), 
                'Magic Number Maximum' = max(`Magic Number`, na.rm = TRUE),
                'Magic Number Mean' = mean(`Magic Number`, na.rm = TRUE),
                'Magic Number Median' = median(`Magic Number`, na.rm = TRUE))
  }, striped = TRUE, align = 'r') #state_all closing Paren

  output$state_county_table <- renderTable({
    req(input$state)
    disasters %>%
      filter(State == input$state) %>%
      select('County', 'Magic Number') %>%
      group_by(County) %>%
      summarise('Magic Number Minimum' = min(`Magic Number`, na.rm = TRUE), 
                'Magic Number Maximum' = max(`Magic Number`, na.rm = TRUE),
                'Magic Number Mean' = mean(`Magic Number`, na.rm = TRUE),
                'Magic Number Median' = median(`Magic Number`, na.rm = TRUE))
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

  output$county_plot <- renderPlotly({
    req(input$county)
    ggplotly(disasters %>%
      filter(State %in% input$state & County %in% input$county) %>%
        na.omit() %>%
      as.data.table() %>%
      melt(measure.vars = c('Expected Damage', 'Total Damage'), 
           variable.name = 'damcat', 
           value.name = 'damage') %>%
      ggplot(aes(y = damage, x = Year, color = damcat, group = damcat))
      + geom_line()
      + theme_bw()
      + theme(legend.title = element_blank())
      + scale_x_continuous(breaks = 1995:2019)
      + scale_y_continuous(labels =  unit_format(unit = 't', scale = 1e-3, prefix = '$'))
      + labs(title = 'Expected Damage vs. Actual Damage Per Year')
      + xlab('')
      + ylab('')
      + theme(axis.text.x = element_text(angle = 45))
    ) #ggplotly closing Paren

  }) #county_plot closing Paren
  
  output$county_plot_ratio <- renderPlotly({
    req(input$county)
    ggplotly(disasters %>%
               filter(State %in% input$state & County %in% input$county) %>%
               na.omit() %>%
               ggplot(aes(y = `Magic Number`, x = Year))
             + geom_line()
             + theme_bw()
             + theme(legend.title = element_blank())
             + scale_x_continuous(breaks = 1995:2019)
             + labs(title = 'Magic Number Per Disaster Year')
             + xlab('')
             + ylab('')
             + theme(axis.text.x = element_text(angle = 45))
    ) #ggplotly closing Paren
    
  }) #county_plot_ratio closing Paren 

  output$state_plot_ratio <- renderPlotly({
    req(input$state)
    ggplotly(disasters %>%
               filter(State %in% input$state) %>%
               na.omit() %>%
               group_by(Year) %>% 
               summarise(`Affected Population Change` = sum(`Affected Population Change`, na.rm = TRUE),
                         `Total Damage` = sum(`Total Damage`),
                         `Expected Damage` = sum(`Expected Damage`, na.rm = TRUE),
                         `Magic Number` = mean(`Magic Number`, na.rm = TRUE)) %>%
               ggplot(aes(y = `Magic Number`, x = Year))
             + geom_line()
             + theme_bw()
             + theme(legend.title = element_blank())
             + scale_x_continuous(breaks = 1995:2019)
             + labs(title = 'Mean Magic Number Per Disaster Year')
             + xlab('')
             + ylab('')
             + theme(axis.text.x = element_text(angle = 45))
    ) #ggplotly closing Paren
    
  }) #state_plot_ratio closing Paren
  
  # output$county_plot_damage <- renderPlotly({
  #   req(input$county)
  #   ggplotly(floods %>%
  #              filter(State %in% input$state & County %in% input$county) %>%
  #              na.omit() %>%
  #              group_by(Year) %>%
  #              summarise('Total Nominal Damage' = sum(Damage_Nominal), 
  #                        'Total CPI Adjusted Damage' = sum(Damage_CPIAdj),
  #                        'Max Nominal Damage' = max(Damage_Nominal),
  #                        'Max CPI Adjusted Damage' = max(Damage_CPIAdj),
  #                        'Mean Nominal Damage' = mean(Damage_Nominal),
  #                        'Mean CPI Adjusted Damage' = mean(Damage_CPIAdj)) %>%
  #              as.data.table() %>%
  #              melt(measure.vars = list(input$damage_class1, input$damage_class2), 
  #                   variable.name = 'damcat', 
  #                   value.name = 'damage') %>%
  #              ggplot(aes(y = damage, x = Year, color = damcat, group = damcat))
  #            + geom_line()
  #            + theme_bw()
  #            + theme(legend.title = element_blank())
  #            + scale_x_continuous(breaks = 1995:2019)
  #            + scale_y_continuous(labels =  unit_format(unit = 't', scale = 1e-3, prefix = '$'))
  #            + labs(title = 'Adjusted Damage Per Disaster Year')
  #            + xlab('')
  #            + ylab('')
  #            + theme(axis.text.x = element_text(angle = 45))
  #   ) #ggplotly closing Paren
  #  
  #}) #county_plot_damage closing Paren 
       
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