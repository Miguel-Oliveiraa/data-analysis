#install.packages("dplyr")
# Bibliotecas
library(ggplot2)
library(shiny)
library(dplyr)

# carregando o dataset
iris <- read.csv("Iris.csv")


# criando um sumário de cada espécie e salvando em daframes separados
iris_summary <- iris %>%
  group_by(Species) %>%
  summarise("Comprimento médio da Sépala" = mean(SepalLengthCm),
            "Largura média da Sépala" = mean(SepalWidthCm),
            "Comprimento médio da Pétala" = mean(PetalLengthCm),
            "Largura média da Pétala" = mean(PetalWidthCm))

ui <- fluidPage(
  #Título da Página
  titlePanel("Vizualização do dataset Iris"),
  sidebarLayout(
    sidebarPanel(
      # Seleciona a variável para o eixo y
      selectInput(
        inputId = "y",
        label = "eixo-y",
        choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
        selected = "SepalLengthCm"
      ),
      
      # Seleciona a vaiável para o eixo x
      selectInput(
        inputId = "x",
        label = "eixo-x",
        choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
        selected = "SepalWidthCm"
      ),
      
      # Seleciona a Espécie que vai ser exibida no gráfico com um checkbox
      checkboxGroupInput(
        inputId = "species",
        label = "Select Species", 
        choices = c("Iris-setosa", "Iris-versicolor", "Iris-virginica"),
        selected = c("Iris-setosa", "Iris-versicolor", "Iris-virginica")
      )
    ),
    
    
    mainPanel(
      tags$h3("Scatter Plot"), 
      plotOutput(outputId = "scatterplot"),
      tags$h2("Tabela de Dados"), 
      tableOutput('table')
      
    )
    
    
  )
)


server <- function(input, output) {
  # criando um dataframe reativo para ajudar na vizualização
  df <- reactive({
    iris %>%
      filter(Species %in% input$species)
  })
  
  # tabela dos dados sumarizados
  output$table <- renderTable(iris_summary)
  
  # gráfico scatterplot
  output$scatterplot <- renderPlot({
    ggplot(
      df(), 
      aes_string(x = input$x, y = input$y)) + 
      geom_point(aes(col = df()$Species), size=3) + scale_color_discrete(name ="Species") +
      geom_smooth(aes(group=df()$Species, color = df()$Species), method='lm')
    
  })
  
  
}

# combinando a ui e o servidor em um shinyApp
shinyApp(ui = ui, server = server)
