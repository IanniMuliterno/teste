#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Teste do botao"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
                            uiOutput("numvar_ctrl"),
                   
                            actionButton("do", "Plotar")                     
            
            
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("boxp_numvar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
dt <- iris
    
    lista_numvar <- dt %>% select_if(is.numeric) %>% 
        names()
    
    output$numvar_ctrl <- renderUI({
        selectInput("variaveis_select", "Mostrar",
                    choices = lista_numvar,
                    multiple = T
        )
    })
    
    
    observeEvent(input$do, {
    
    vars_plotar <- input$variaveis_select        
        
    output$boxp_numvar <- renderPlotly({
        req(input$variaveis_select)
        
        p <-   dt %>% select(vars_plotar) %>% 
            pivot_longer(everything()) %>% 
            ggplot( aes(x=name, y=value)) + geom_boxplot()
        ggplotly(p)           
        
    })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
