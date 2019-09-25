library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)

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

ui <- navbarPage(title = "Mortes por intercenção policial",
#POR RACA---------------
    tabPanel("Por Raça",
    fluidPage(
    titlePanel("Por Raça"),
     sidebarLayout(
        sidebarPanel(
            selectInput("circobito", "Tipo de ocorrência", unique(dados$CIRCOBITO)),
            selectInput("lococor", "Local da morte", unique(dados$LOCOCOR), multiple = TRUE, 
                        selected = unique(dados$LOCOCOR)),
            selectInput("fonte", "Fonte", unique(dados$FONTE), multiple = TRUE, 
                        selected = unique(dados$FONTE)),
            checkboxGroupInput("invest", "", unique(dados$TPPOS), selected = unique((dados$TPPOS))),
            sliderInput("bin", "Ajuste das colunas", 30, 200, 60)
            ),
        mainPanel(dateRangeInput("Data", "Período", start =  "2006-01-01", 
                                  end = "2017-12-31",
                                  min = "2006-01-01", 
                                  max = "2017-12-31"), 
                  tableOutput("tableraca"), 
                  plotOutput("plotraca")
        )#mainPanel
        )#slidebar
        )#FluidPage
        ),
#POR TPPOS-----------
    tabPanel("Investigação",
             fluidPage(
                 
                 titlePanel("Investigação"),
                 
                 # Sidebar with a slider input for number of bins 
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("circobitotppos", "Tipo de ocorrência", unique(dados$CIRCOBITO)),
                         selectInput("lococortppos", "Local da morte", unique(dados$LOCOCOR), multiple = TRUE, 
                                     selected = unique(dados$LOCOCOR)),
                         selectInput("fontetppos", "Fonte", unique(dados$FONTE), multiple = TRUE, 
                                     selected = unique(dados$FONTE)),
                         selectInput("racatppos", "Raça/Cor", unique(dados$RACACOR2), selected = unique((dados$RACACOR2)), 
                                     multiple = TRUE),
                         sliderInput("bintppos", "Ajuste das colunas", 30, 200, 60)
                         
                     ),
                     
                     
                     mainPanel(dateRangeInput("Datatppos", "Período", start =  "2006-01-01", 
                                              end = "2017-12-31",
                                              min = "2006-01-01", 
                                              max = "2017-12-31"), 
                               plotOutput("plottppos"), 
                               plotOutput("plottppos2")
        )#mainPanel
        )#slidebar
        )#FluidPage
        )
        )#navBarPage
        
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
#POR RACA PLOT-----------
    output$tableraca <- renderTable({
        numeros <-dados %>% filter(DTOBITO %in% seq.Date(min(input$Data), 
                                                          max(input$Data), by = "day"),
                                    LOCOCOR %in% input$lococor,
                                    FONTE %in% input$fonte,
                                    TPPOS %in% input$invest) %>% 
            group_by(RACACOR2) %>% summarise(n())
        names(numeros) <- c("Raça/cor", "Numero_de_casos")
        numeros$Porcentagem_de_casos <- (numeros$Numero_de_casos*100)/sum(numeros$Numero_de_casos)
        numeros[6,2] <- sum(numeros$Numero_de_casos)
        numeros[6,1] <- "Total"
        numeros
        
        })

    output$plotraca <- renderPlot({
        dados %>% filter(LOCOCOR %in% input$lococor,
                          FONTE %in% input$fonte,
                          TPPOS %in% input$invest) %>% 
            ggplot(aes(x = DTOBITO, fill = RACACOR2)) +
            geom_histogram(color = "white", binwidth = input$bin) + 
            scale_x_date(limits = input$Data) +
            scale_y_continuous(labels = scales::comma) +
            theme_light()
        
    })
    
#POR TPPOS PLOT----------------
    output$plottppos2 <- renderPlot({
        dados %>% filter(LOCOCOR %in% input$lococortppos,
                         FONTE %in% input$fontetppos,
                         RACACOR2 %in% input$racatppos) %>% 
            ggplot(aes(x = DTOBITO, y = DTINVESTIG, color = RACACOR2)) + geom_point() + 
            scale_x_date(limits = input$Datatppos) + 
            scale_y_date(limits = input$Datatppos) + theme_light()
        
    })
    
    output$plottppos <- renderPlot({
        dados %>% filter(LOCOCOR %in% input$lococortppos,
                         FONTE %in% input$fontetppos,
                         RACACOR2 %in% input$racatppos) %>% 
            ggplot(aes(x = DTOBITO, fill = TPPOS)) +
            geom_histogram(color = "white", binwidth = input$bintppos) + 
            scale_x_date(limits = input$Datatppos) +
            scale_y_continuous(labels = scales::comma) +
            theme_light()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
