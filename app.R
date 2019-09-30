# Get data-----------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(gridExtra)
library(shinydashboard)
library(RColorBrewer)

dados <- read.csv("Data/DATASUS DOEXT")

dados$DTINVESTIG <- as.Date(dados$DTINVESTIG)
dados$DTOBITO <- as.Date(dados$DTOBITO)
dados$LOCOCOR <- as.character(dados$LOCOCOR)
dados$LOCOCOR <- ifelse(is.na(dados$LOCOCOR), "Desconhecido", dados$LOCOCOR)
dados$LOCOCOR <- ifelse(dados$LOCOCOR == "6", "Desconhecido", dados$LOCOCOR)
dados$SEXO <- as.character(dados$SEXO)
dados$SEXO <- ifelse(is.na(dados$SEXO), "Desconhecido", dados$SEXO)
dados$RACACOR2 <- as.character(dados$RACACOR2)
dados$RACACOR2 <- ifelse(is.na(dados$RACACOR2), "Desconhecida", dados$RACACOR2)
dados$CIRCOBITO <- as.character(dados$CIRCOBITO)
dados$CIRCOBITO <- ifelse(is.na(dados$CIRCOBITO), "Desconhecido", dados$CIRCOBITO)
dados$FONTE <- as.character(dados$FONTE)
dados$FONTE <- ifelse(is.na(dados$FONTE), "Desconhecido", dados$FONTE)
dados$TPPOS <- as.character(dados$TPPOS)
dados$TPPOS <- ifelse(is.na(dados$TPPOS), "Desconhecido", dados$TPPOS)


#POR RACA---------------
ui <- dashboardPage(
    dashboardHeader( title = "Letalidade policial"
                     
                     
    ),#dashHEADER
    
    dashboardSidebar(
        selectInput("lococor", "Local da morte", unique(dados$LOCOCOR), multiple = TRUE, 
                    selected = unique(dados$LOCOCOR)),
        selectInput("fonte", "Fonte", unique(dados$FONTE), multiple = TRUE, 
                    selected = unique(dados$FONTE)),
        checkboxGroupInput("invest", "Investigação", unique(dados$TPPOS), selected = unique((dados$TPPOS))),
        checkboxGroupInput("racacor", "Raça/cor", unique(dados$RACACOR2), selected = unique((dados$RACACOR2))),
        sliderInput("bin", "Ajuste das colunas", 30, 200, 60)
        
    ),#dashSIDEBAR
    
    dashboardBody(
        fluidRow(
            title = "pizza",
            dateRangeInput("Data", "Período", start =  "2006-01-01", 
                           end = "2017-12-31",
                           min = "2006-01-01", 
                           max = "2017-12-31")
            
            
        ),#fluidrow
        fluidRow(
            box(
                plotOutput('plotpizza1')
            ),#box
            box(
                plotOutput('plotpizza2')
            )#box
        ),#fluidRow
        fluidRow(
            
            plotOutput("plotraca")
            
        )#fluidrow
    )#dashBODY
    
)#dashPAGE


#POR TPPOS-----------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #POR RACA PLOT-----------
    output$plotpizza1 <- renderPlot({
        numeros <-dados %>% filter(DTOBITO %in% seq.Date(min(input$Data), 
                                                         max(input$Data), by = "day"),
                                   RACACOR2 %in% input$racacor,
                                   LOCOCOR %in% input$lococor,
                                   FONTE %in% input$fonte,
                                   TPPOS %in% input$invest) %>% 
            group_by(RACACOR2) %>% summarise(n())
        names(numeros) <- c("Raça", "Numero_de_casos")
        numeros$Porcentagem_de_casos <- (numeros$Numero_de_casos*100)/sum(numeros$Numero_de_casos)
        numeros[6,2] <- sum(numeros$Numero_de_casos)
        numeros[6,1] <- "Total"
        pizza1 <- numeros %>% ggplot(aes(x = "", y = Porcentagem_de_casos , fill = Raça)) +
            geom_bar(width = 1, stat = "identity") + 
            scale_fill_brewer(type = 'div', palette = "Set1") + 
            labs(title = "Por Raça/cor") +
            theme_light() + coord_polar("y", start=0)
        
        pizza1
    })
    
    output$plotpizza2 <- renderPlot({
        
        
        numeros2 <-dados %>% filter(DTOBITO %in% seq.Date(min(input$Data), 
                                                          max(input$Data), by = "day"),
                                    RACACOR2 %in% input$racacor,
                                    LOCOCOR %in% input$lococor,
                                    FONTE %in% input$fonte,
                                    TPPOS %in% input$invest) %>% 
            group_by(TPPOS) %>% summarise(n())
        names(numeros2) <- c("Investigação", "Numero_de_casos")
        numeros2$Porcentagem_de_casos <- (numeros2$Numero_de_casos*100)/sum(numeros2$Numero_de_casos)
        numeros2[6,2] <- sum(numeros2$Numero_de_casos)
        numeros2[6,1] <- "Total"
        pizza2 <- numeros2 %>% ggplot(aes(x = "", y = Porcentagem_de_casos , fill = Investigação)) +
            geom_bar(width = 1, stat = "identity") + 
            scale_fill_brewer(type = 'div', palette = "Set1") + 
            labs(title = "Investigação") +
            theme_light() + coord_polar("y", start=0)
        pizza2
    })
    
    output$plotraca <- renderPlot({
        dados %>% filter(LOCOCOR %in% input$lococor,
                         RACACOR2 %in% input$racacor,
                         FONTE %in% input$fonte,
                         TPPOS %in% input$invest) %>% 
            ggplot(aes(x = DTOBITO)) +
            geom_histogram(color = "white", fill = "blue", binwidth = input$bin) + 
            scale_x_date(limits = input$Data) +
            scale_y_continuous(labels = scales::comma) + xlab("Data") + 
            ylab("Número de casos") +
            theme_light()
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
