library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Geração de Regras de Recomendação"),
    fluidRow(
        column(3,
               
               fileInput("arquivo", "Escolha o arquivo:",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
               actionButton("Processar","Processar")
        ),
        column(3,
               numericInput("nisuporte", "Suporte Mínimo:", 0.04, min = 0.001, max = 1.0)
        ),
        column(3,
               numericInput("niconfianca", "Confiança Mínima:", 0.08, min = 0.001, max = 1.0)
        ),
        column(3,
               numericInput("inminimo", "Tamanho Mínimo:", 2, min = 1, max = 40)
               
        )
    ),
    fluidRow(
        column(3,
               
               plotOutput("Graf1")
        ),
        column(3,
               plotOutput("Graf2")
        ),
        column(3,
               
               plotOutput("Graf3")
        ),
        column(3,
               plotOutput("Graf4")
        )
    ),
    fluidRow(
        column(12,
               
               h1(textOutput("txtregras")),
               tableOutput("tregras"))
    )
)


# Define server logic required
server <- function(input, output) {
    #evento do botao para executar o calculo
    observeEvent(input$Processar, {
        
        file1 <- input$arquivo
        transacoes <-read.transactions(file1$datapath,format="basket",sep=",")
        regras <- apriori(transacoes, parameter= list(supp=input$nisuporte, conf=input$niconfianca, minlen=input$inminimo))
        
        output$Graf1 = renderPlot({  plot(regras, method="graph", control=list(type="items"))   })
        output$Graf2 = renderPlot({  plot(regras, method="matrix", control=list(type="items"))  })
        output$Graf3 = renderPlot({  plot(regras, method="matrix3D", measure="lift")  })
        output$Graf4 = renderPlot({  plot(regras, method="grouped")  })
        
        output$txtregras = renderText({"Regras" })   
        
        output$tregras <- renderTable({
            as(regras, "data.frame")
            })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
