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
      uiOutput('county_dynamic')
    ), #Dashboard Slider Paren
    
    dashboardBody(
      
    ) #DashboardBody Paren

    
    
    
        
) #dashboardPage Paren 
) #shinyUI Paren