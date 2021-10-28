#This is what I've put together so far. Feel free to change any of it, and we 
#can discuss more in person too. I've mostly mirrored the example in the link 
#below.
#https://github.com/amrrs/sample_revenue_dashboard_shiny/blob/master/app.R
#https://stackoverflow.com/questions/49473915/r-how-do-i-use-selectinput-in-shiny-to-change-the-x-and-fill-variables-in-a-gg


#source("wrangling.R")

library(shiny)
library(shinydashboard)

#create header
header <- dashboardHeader(title = "Strawberry Shiny App")
#create sidebar
sidebar <- dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Source", icon = icon("share",lib='glyphicon'), 
             href = "https://quickstats.nass.usda.gov/#56E84525-1350-34A1-9ED7-27363BD5A7D3"),
    menuItem("Select X value:", 
             selectInput('x', 'X', choices = c("State", "Year"),
                         selected = "State")),
    menuItem("Select Y value:",
             selectInput('y', 'Y', choices = unique(strawberry["Measurement(s)"])))
))
#create body
frow1 <- fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
    
    box(
        title = "Barchart"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("outplot")
    )
    
    ,box(
        title = "BOX2"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyRegion", height = "300px")
    ) 
    
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)


#COMBINE INTO UI
ui <- dashboardPage(
    header,
    sidebar,
    body,
    skin = "green"
    
)


server <- function(input, output) { 
    #creating the valueBoxOutput content
    #average pesticides per acre per year
    lb_yr_ac <- strawberry[strawberry["Measurement(s)"] == " MEASURED IN LB / ACRE / YEAR",]
    #isolate neurotoxins
    nt <- !(lb_yr_ac$neurotoxins == '')
    cg <- !is.na(lb_yr_ac$carcinogen)
    
    
    output$value1 <- renderValueBox({
        valueBox(
            format(mean(lb_yr_ac$Value),digits = 2)
            ,paste('Overall pesticide application mean lb/acre/year')
            ,color = "green")})
    
     output$value2 <- renderValueBox({
        valueBox(
            format(mean(lb_yr_ac$Value[nt]),digits = 2)
            ,paste('Neurotoxin application mean lb/acre/year')
            ,color = "blue")
        })
     
     output$value3 <- renderValueBox({
         valueBox(
             format(round(mean(lb_yr_ac$Value[cg]), digits = 2), nsmall = 1)
             ,paste('Carcinogen application mean lb/acre/year')
             ,color = "yellow")
     })
     
      reactive({
      unit <- .data[[input$y]]
      x_val <- .data[[input$x]]
      print(typeof(x_val))
      select <- strawberry[strawberry["Measurement(s)"] == unit,]
      select %<>% group_by(x_val) %>% summarise_at(vars(Value),list(mean = mean))
      
      output$outplot <- renderPlot({
          ggplot(select) +
              geom_bar(aes(x = x_val, y = select$Value), position = position_stack(reverse = TRUE)) +
              coord_flip() + 
              theme(legend.position = "top")
      
     })
      
      })
      
}
shinyApp(ui, server)

