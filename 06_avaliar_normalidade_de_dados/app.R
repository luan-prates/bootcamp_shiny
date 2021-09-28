rsconnect::setAccountInfo(name='luanprates02', token='5CB3097C6DF6CE512BEEF452587D9A24', secret='pXMJu8V6ziXd5nBK42SpKyNZJvapaj9C/CCVp9bQ')

library(shiny)

ui <- fluidPage(
    
    titlePanel("Teste de Normalidade"),
    
    fluidRow(
        column(6,
               helpText("Será analisada a primeira coluna do arquivo"),
               fileInput("arquivo", "Escolha o arquivo:",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
        ),
        column(6,
               
               actionButton("Processar","Processar")
        )
    ),
    
    fluidRow(
        column(4,
               plotOutput("Grafhist")
        ),
        column(4,
               plotOutput("Grafqqplot")
        ),
        column(4,
               
               h1(textOutput("test"))
               
        )
    )
    
)

server <- function(input, output) {
    
    observeEvent(input$Processar,
                 {
                     file1 <- input$arquivo
                     data =  read.csv(file1$datapath, header = T)
                     
                     output$Grafhist = renderPlot({hist(data[,1], main  = "Histograma")})
                     output$Grafqqplot = renderPlot({qqnorm(data[,1]) 
                         qqline(data)
                     })
                     
                     tst= shapiro.test(data[,1])[2]
                     tst = paste0("Valor de P: ", tst)
                     output$test = renderText({tst })
                     
                     
                 })
    
}

shinyApp(ui = ui, server = server)

