library(shiny)
##Here I would like to divide the page into sections,one is for displaying our data frame into tables, one is for out plots, etc.
##But I'm still working on it,and I just put our data into two tables-- in "Data Frame" section, static and dynamic as well.
##Feel free to change any of them.
ui<-fluidPage(
    sidebarLayout(
        sidebarPanel(
            textOutput("panel")
        ),
        mainPanel(
            tabsetPanel(
                id = "tabset",
                tabPanel("Data Frame",
                         tableOutput("static"),
                         dataTableOutput("dynamic")
                ),
                tabPanel("Panel 2", "two"),
                tabPanel("Panel 3", "three")
            )
        )
    )
)

server<-function(input,output,session){
    output$static<-renderTable(head(strawberry))
    output$dynamic<-renderDataTable(strawberry, options = list(pageLength = 5))
    output$panel<-renderText({
        paste("Current panel: ", input$tabset)
    })
}

shinyApp(ui,server)