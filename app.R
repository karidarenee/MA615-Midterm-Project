#https://github.com/amrrs/sample_revenue_dashboard_shiny/blob/master/app.R
#https://stackoverflow.com/questions/49473915/r-how-do-i-use-selectinput-in-shiny-to-change-the-x-and-fill-variables-in-a-gg
#https://github.com/RiveraDaniel/Regression/blob/master/ui.R


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
                         selected = "State"))
))
#create body
frow0 <- fluidRow( 
  column(12,
         p("Welcome to our shiny app. Here you can explore strawberry yield by 
           state and year. On the left side panel, you can select whether you 
           want to examine values grouped by year or state 
           and the unit in which the values are measured. Both the barchart and
           table will update to match your selections. ", 
           style="text-align:justify;color:white
           ;background-color:#00a65a;padding:15px;font-size: 17px;"),
  )
)


frow1 <- fluidRow( 
  column(12,
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3")
)
)

frow2 <- fluidRow(
    column(12, 
    box(width='100%'
        ,title = "Barchart"
        ,status = "primary"
        ,solidHeader = FALSE 
        ,collapsible = TRUE 
        ,plotOutput("outplot")
    )
    )
    
    
)


# box(
#   title = "Box title", width = NULL, status = "primary",
#   div(style = 'overflow-x: scroll', tableOutput('table'))
# )
frow3 <- fluidRow(
  column(12, 
   box(width='100%'
       ,title = "Examine Raw Data"
       ,status = "primary"
       ,solidHeader = FALSE
       ,collapsible = TRUE
       ,div(style = 'overflow-x: scroll',tabPanel("Data", DT::dataTableOutput("mytable")))
  )
  )
  
)

# combine the two fluid rows to make the body
body <- dashboardBody(frow0, frow2, frow3,frow1,
                      tags$head( 
                        tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size to 20
                      ))


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
       unit <- "STRAWBERRIES - YIELD, MEASURED IN CWT / ACRE"
       
       straw_select <- strawb_yield[strawb_yield["Data.Item"] == unit,]
       straw_select %<>% 
         #Step 2
         group_by_at(.vars = c(x_val)) %>% 
         #Step 3
         summarise(mean = mean(Value,na.rm=TRUE), sd = sd(Value))%>% 
         as_tibble()
       
       straw_select$min_err <- ifelse((straw_select$mean-straw_select$sd<0),0,straw_select$mean-straw_select$sd)
       straw_select$max_err <- straw_select$mean+straw_select$sd
       
       
       ggplot(straw_select) +
         geom_col(aes(x = unlist(straw_select[x_val]), y = unlist(mean)), fill = "#00a65a")+
          geom_errorbar(aes(x =unlist(straw_select[x_val]), ymin=min_err, ymax=max_err), width=0,
                        position=position_dodge(.9)) +
         xlab(x_val) +
         ylab("YIELD, MEASURED IN CWT / ACRE")+
         labs(title = "Mean of Strawberry Yield with St.Dev Errors")+
         scale_y_continuous(limits = c(0, NA)) +
         theme(text=element_text(size=20), #change font size of all text
                axis.text=element_text(size=15), #change font size of axis text
                axis.title=element_text(size=15), #change font size of axis titles
                plot.title=element_text(size=20) #change font size of plot title
         )
       
     })
     
     
     output$mytable <- renderDataTable({
       DT::datatable(strawb_yield[strawb_yield['Data.Item'] == "STRAWBERRIES - YIELD, MEASURED IN CWT / ACRE",], 
                     options = list(paging = TRUE,    ## paginate the output
                                    pageLength = 15,  ## number of rows to output for each page
                                    scrollX = TRUE,   ## enable scrolling on X axis
                                    scrollY = TRUE)   ## enable scrolling on Y axis
                     )
     })
     

     
}
      

shinyApp(ui, server)

