library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)

dados <- fread("dados_limpos.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hello Shiny"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "bins",
                        label = "Seletor Numérico:",
                        min = 50,
                        max = 200,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput(outputId = 'ListaNumeros'),
           verbatimTextOutput(outputId = 'listaUF'),
           plotlyOutput(outputId = 'data'),
           plotlyOutput(outputId = 'uf'),
           #plotlyOutput(outputId = 'atendida'),
           #plotlyOutput(outputId = 'atendidaAno')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   dados_secionados <-  reactive({
       dados %>% 
            filter(UF %in% c('DF', 'SC', 'GO'))
    })
   
   output$listaUF <- renderPrint({
       unique(dados_secionados()$UF)
   })

    
    output$ListaNumeros <- renderPrint({
        seq(1:input$bins)
    })
    
    output$data <- renderPlotly({
       ggplotly(
            data.frame(table(as.Date(dados_secionados()$DataArquivamento))) %>% 
            rename(Data = Var1, Qtd = Freq) %>% 
            ggplot(aes(as.Date(Data), Qtd, 
                       text = paste("Data:", as.Date(Data), "<br>", "N:", Qtd))) +
            geom_line(group = 1) +
            theme_bw() +
            ggtitle('Qtd Reclamações por mes') +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_x_date(date_labels = '%b-%Y', breaks = '6 month') +
            xlab('Data')
        )
    })
    
    output$uf <- renderPlotly({
        ggplotly(
            data.frame(table(dados_secionados()$UF)) %>% 
                rename(UF = Var1 , Qtd = Freq) %>% 
                ggplot(aes(x = reorder(UF, Qtd), y = Qtd,
                           text = paste("UF:", UF, "<br>", "QTD:", Qtd)
                ))+
                geom_bar(fill = 'blue', stat = 'identity') +
                coord_flip() +
                xlab('UF') +
                theme_bw() +
                ggtitle('Quantidade de raclamação')
        )
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)