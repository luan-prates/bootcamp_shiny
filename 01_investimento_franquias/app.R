library(shiny)

dados = read.csv("slr12.csv", sep = ";")
modelo = lm(CusInic ~ FrqAnual, data = dados)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Prevendo Custos para abrir uma Franquia"),
    fluidRow(
        column(4,
            h2("Dados"),
            tableOutput("Dados")
        ),
        column(8,
            plotOutput("Graf")
        )
    ),
    
    fluidRow(
        column(6,
            h3("valor anual da franquia"),
            numericInput("NovoValor", "Insira Novo Valor", 1500, min = 1, max = 9999999),
            actionButton("Processar", "Processar")
        ),
        column(6,
               h1(textOutput("Resultado"))
            
        )
        
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Graf = renderPlot({
        plot(CusInic ~ FrqAnual, data = dados)
        abline(modelo)
    })
    
    output$Dados = renderTable({head(dados, 10)})
    
    observeEvent(input$Processar, {
        valr = input$NovoValor
        prev = predict(modelo, data.frame(FrqAnual = eval(parse(text = valr))))
        prev = paste0("Custo Inicial para a franquia R$: ", round(prev,2))
        output$Resultado = renderText({prev})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
