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
                    tabPanel('Summary', textOutput('county_selection')),
                    tabPanel('Complete List',
                             textOutput('county_selection1'),
                             tableOutput('county_full'))
                      ) #tabBox Paren
                ), #tabItem Paren
        
        tabItem(tabName = 'state_sum',
                tabBox(
                    title = '',
                    tabPanel('Summary', textOutput('state_selection')),
                    tabPanel('Complete List', 
                             textOutput('state_selection1'),
                             tableOutput('state_full'))
                      ) #tabBox Paren 
                ) #tabItem Paren 
                
        
              ) #tabItems Paren
    
      
    ) #DashboardBody Paren
    
        
) #dashboardPage Paren 
) #shinyUI Paren

