#This is what I've put together so far. Feel free to change any of it, and we 
#can discuss more in person too. I've mostly mirrored the example in the link 
#below.
#https://github.com/amrrs/sample_revenue_dashboard_shiny/blob/master/app.R
#https://stackoverflow.com/questions/49473915/r-how-do-i-use-selectinput-in-shiny-to-change-the-x-and-fill-variables-in-a-gg


source("wrangling.R")

library(shiny)
library(shinydashboard)
library(DT)


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
             selectInput('y', 'Y', choices = unique(strawberry["Measurement(s)"]),
                         selected = " MEASURED IN LB / ACRE / YEAR"))
))
#create body
frow1 <- fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
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
         title = "Examine Raw Data"
         ,status = "primary"
         ,solidHeader = TRUE 
         ,collapsible = TRUE 
         ,tabPanel("Data", DT::dataTableOutput("mytable"))
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
    lb_yr_ac <- strawberry[strawberry["Measurement(s)"] == "MEASURED IN LB / ACRE / YEAR",]
    #isolate neurotoxins
    nt <- !(lb_yr_ac$neurotoxins == '')
    cg <- !is.na(lb_yr_ac$carcinogen)
    
        output$value1 <- renderValueBox({
        valueBox(
            format(mean(lb_yr_ac$Value,na.rm=TRUE),digits = 2)
            ,paste('Overall pesticide application mean lb/acre/year')
            ,color = "green")})
    
     output$value2 <- renderValueBox({
        valueBox(
            format(mean(lb_yr_ac$Value[nt], na.rm=TRUE),digits = 2)
            ,paste('Neurotoxin application mean lb/acre/year')
            ,color = "blue")
        })
     
     output$value3 <- renderValueBox({
         valueBox(
             format(round(mean(lb_yr_ac$Value[cg],na.rm=TRUE), digits = 2), nsmall = 1)
             ,paste('Carcinogen application mean lb/acre/year')
             ,color = "yellow")
     })
     
  
     
     output$outplot <-  renderPlot({
       x_val <- input$x
       unit <-  input$y
       
       straw_select <- strawberry[strawberry["Measurement(s)"] == as.character(unit),]
       straw_select %<>% 
         #Step 2
         group_by_at(.vars = c(x_val)) %>% 
         #Step 3
         summarise(mean = mean(Value,na.rm=TRUE), sd = sd(Value))%>% 
         as_tibble()
       
       straw_select$min_err <- ifelse((straw_select$mean-straw_select$sd<0),0,straw_select$mean-straw_select$sd)
       straw_select$max_err <- straw_select$mean+straw_select$sd
       
       
       ggplot(straw_select) +
         geom_col(aes(x = unlist(straw_select[x_val]), y = unlist(mean)), fill = "#D55E00")+
         geom_errorbar(aes(x =unlist(straw_select[x_val]), ymin=min_err, ymax=max_err), width=0,
                       position=position_dodge(.9))+ 
         xlab(input$x) +
         ylab(paste(input$y))+
         labs(title = "Mean of Selected Variables with St.Dev Errors")+
         scale_y_continuous(limits = c(0, NA))
       
     })
     
     
     output$mytable <- renderDataTable({
       DT::datatable(strawberry[strawberry['Measurement(s)'] == input$y, ])
     })
     
     
     
}
      

shinyApp(ui, server)

