shinyUI(
  dashboardPage(
    dashboardHeader(title = span(
      "Comparative Resiliency Across Time",
      style = 'color: white; font-size: 30px, font-weight: bold'),
      titleWidth = 400),
    
    dashboardSidebar(
      selectInput('state',
                  'Select State:',
                  choices = c("", state_choices),
                  selected = NULL),
      uiOutput('county_dynamic'),
      sidebarMenu(
        menuItem('County Summary', tabName = 'county_sum'),
        menuItem('State Summary', tabName = 'state_sum')
      ) #sidebarMenu Paren
      
    ), #Dashboard Slider Paren
    
    dashboardBody(
      tabItems(
        tabItem(tabName = 'county_sum', 
                tabBox(
                    title = '',
                    tabPanel('Calculated Ratio Per Year',
                             h3(textOutput('county_selection1')),
                             p(''),
                             tableOutput('county_ratio')),
                    tabPanel('Per Year Summary', 
                             h3(textOutput('county_selection2')),
                             tableOutput('county_sum_table')),
                             p(''),
                    tabPanel('Complete List',
                             h3(textOutput('county_selection3')),
                             p(''),
                             tableOutput('county_full'))
                      ) #tabBox Paren
                ), #tabItem Paren
        
        tabItem(tabName = 'state_sum',
                tabBox(
                    title = '',
                    tabPanel('Calculated Ratio Per Year',
                             h3(textOutput('state_selection1')),
                             p(''),
                             tableOutput('state_ratio')),
                    tabPanel('Per Year Summary', 
                             h3(textOutput('state_selection2')),
                             p(''),
                             tableOutput('state_sum_table')),
                    tabPanel('Complete List', 
                             h3(textOutput('state_selection3')),
                             p(''),
                             tableOutput('state_full'))
                      ) #tabBox Paren 
                ) #tabItem Paren 
                
        
              ) #tabItems Paren
    
      
    ) #DashboardBody Paren
    
        
) #dashboardPage Paren 
) #shinyUI Paren

