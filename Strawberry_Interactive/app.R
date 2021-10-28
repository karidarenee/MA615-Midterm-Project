#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#YEAR and State 


#source: https://github.com/amrrs/sample_revenue_dashboard_shiny/blob/master/app.R

source("wrangling.R")

library(shiny)
library(shinydashboard)

#create header
header <- dashboardHeader(title = "Strawberry Shiny App")
#create sidebar
sidebar <- dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Source", icon = icon("send",lib='glyphicon'), 
             href = "https://quickstats.nass.usda.gov/#56E84525-1350-34A1-9ED7-27363BD5A7D3")
))
#create body
frow1 <- fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
    
    box(
        title = "Revenue per Account"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyPrd", height = "300px")
    )
    
    ,box(
        title = "Revenue per Product"
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
    skin = "red"
    
)


server <- function(input, output) { 
    #creating the valueBoxOutput content
    #average pesticides per acre per year
    lb_yr_ac <- strawberry[strawberry["Measurement(s)"] == " MEASURED IN LB / ACRE / YEAR",]
    output$value1 <- renderValueBox({
        valueBox(
            formatC(mean(lb_yr_ac$Value), format="d", big.mark=',')
            ,paste('Mean pound per acre per year of pesticides:',mean(lb_yr_ac$Value))
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")
        
        
    })
    
    
    }
shinyApp(ui, server)
