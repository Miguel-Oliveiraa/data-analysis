#install.packages("shinydashboard")
## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Carregue seus dados
apple_data <- read.csv("AAPL.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analise individual", tabName = "analise-individual", icon = icon("chart-simple")),
      menuItem("Comparar", tabName = "comparar", icon = icon("code-compare"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analise-individual",
              sidebarLayout(
                sidebarPanel(
                  # Seleciona a variável para o eixo y
                  selectInput(
                    inputId = "classe",
                    label = "Escolha uma classe",
                    choices = c("Preço abertura", "Preço Maximo", "Preço Minimo", "Preço fechamento", "Preço ajustado", "Volume"),
                    selected = NULL,
                  ),
                  dateInput(
                    inputId="start_date_1",
                    label="Selecione a data inicial",
                    value = "1980-12-12",
                    min = "1980-12-12",
                    max = "2022-03-24",
                    format = "yyyy-mm-dd",
                    startview = "year",
                    weekstart = 0,
                    language = "pt-br",
                    autoclose = TRUE,
                  ),
                  dateInput(
                    inputId="end_date_1",
                    label="Selecione a data final",
                    value = "2022-03-24",
                    min = "1980-12-12",
                    max = "2022-03-24",
                    format = "yyyy-mm-dd",
                    startview = "year",
                    weekstart = 0,
                    language = "pt-br",
                    autoclose = TRUE,
                  ),
                ),
                mainPanel(
                  navbarPage(NULL,
                             tabPanel("Tabela",
                                      tableOutput('table_01')),
                             tabPanel("Gráfico em linha",
                                      box(plotOutput('lineplot'))),
                             tabPanel("Histograma",
                                      box()),
                             tabPanel("Boxplot",
                                      box())
                  )
                )
                
              )
      ),
      tabItem(tabName = "comparar",
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    inputId = "classe_x",
                    label = "Eixo x",
                    choices = c("Preço abertura", "Preço Maximo", "Preço Minimo", "Preço fechamento", "Preço ajustado", "Volume"),
                    selected = NULL,
                  ),
                  selectInput(
                    inputId = "classe_y",
                    label = "Eixo y",
                    choices = c("Preço abertura", "Preço Maximo", "Preço Minimo", "Preço fechamento", "Preço ajustado", "Volume"),
                    selected = NULL,
                  ),
                  dateInput(
                    inputId="start_date_2",
                    label="Selecione a data inicial",
                    value = "1980-12-12",
                    min = "1980-12-12",
                    max = "2022-03-24",
                    format = "yyyy-mm-dd",
                    startview = "year",
                    weekstart = 0,
                    language = "pt-br",
                    width = NULL,
                    autoclose = TRUE,
                    datesdisabled = NULL,
                    daysofweekdisabled = NULL
                  ),
                  dateInput(
                    inputId="end_date_2",
                    label="Selecione a data final",
                    value = "2022-03-24",
                    min = "1980-12-12",
                    max = "2022-03-24",
                    format = "yyyy-mm-dd",
                    startview = "year",
                    weekstart = 0,
                    language = "pt-br",
                    width = NULL,
                    autoclose = TRUE,
                    datesdisabled = NULL,
                    daysofweekdisabled = NULL
                  ),
                ),
                mainPanel(
                  navbarPage(NULL,
                             tabPanel("Tabela",
                                      tableOutput('table_02')),
                             tabPanel("Gráfico em linha",
                                      box(plotOutput('lineplot_tab2'))),
                             tabPanel("Gráfico em barra das médias",
                                      box()),
                             tabPanel("Scatterplot",
                                      box(plotOutput('scatterplot')))
                  )
                )
              ))
    )
  )
)

server <- function(input, output) {
  ######################## ABA 01 ##################################
  # Crie uma função reativa para mapear a opção selecionada no selectInput para o nome da coluna correspondente
  selected_column <- reactive({
    switch(input$classe,
           "Preço abertura" = "Open",
           "Preço Maximo" = "High",
           "Preço Minimo" = "Low",
           "Preço fechamento"= "Close",
           "Preço ajustado" = "Adj_Close",
           "Volume" = "Volume"
    )
  })
  
  # Coletar as datas de início e fim selecionadas pelos usuários
  start_date_1 <- reactive({as.Date(input$start_date_1)}) 
  end_date_1 <- reactive({as.Date(input$end_date_1)})
  
  # Filtre seus dados com base nas datas selecionadas
  filtered_data_1 <- reactive({
    apple_data %>%
      filter(Date >= start_date_1() & Date <= end_date_1())
  })
  ############################ Tabela aba 01 #######################
  #calcula moda
  calculate_mode <- function(x) {
    unique_x <- unique(x)
    counts <- table(x)
    return(unique_x[which.max(counts)])
  }
  
  # Crie um resumo com base na coluna selecionada
  apple_summary <- reactive({
    col_name <- selected_column()
    summary_data <- filtered_data_1() %>%
      summarise(
        moda = calculate_mode(get(col_name)),
        média = mean(get(col_name)),
        mediana = median(get(col_name)),
        desvio_padrão = sd(get(col_name)),
        valor_mínimo = min(get(col_name)),
        valor_máximo = max(get(col_name))
      )
    
    return(summary_data)
  })
  # Renderize a tabela com base nos dados resumidos
  output$table_01 <- renderTable(apple_summary())
  
  # Renderiza o gráfico de linhas com base nos dados resumidos
  output$lineplot <- renderPlot({
    col_name <- selected_column()
    ggplot(filtered_data_1(), aes_string(x = col_name, y = col_name)) +
      geom_line() +
      geom_smooth(method = "lm")
  })
  
  ######################## ABA 02 ##################################
  selected_column_x <- reactive({
    switch(input$classe_x,
           "Preço abertura" = "Open",
           "Preço Maximo" = "High",
           "Preço Minimo" = "Low",
           "Preço fechamento"= "Close",
           "Preço ajustado" = "Adj_Close",
           "Volume" = "Volume"
    )
  })
  selected_column_y <- reactive({
    switch(input$classe_y,
           "Preço abertura" = "Open",
           "Preço Maximo" = "High",
           "Preço Minimo" = "Low",
           "Preço fechamento"= "Close",
           "Preço ajustado" = "Adj_Close",
           "Volume" = "Volume"
    )
  })
  
  # Coletar as datas de início e fim selecionadas pelos usuários
  start_date_2 <- reactive({as.Date(input$start_date_2)}) 
  end_date_2 <- reactive({as.Date(input$end_date_2)})
  
  filtered_data_2 <- reactive({
    apple_data %>%
      filter(Date >= start_date_2() & Date <= end_date_2())
  })
  
  ############################ Tabela aba 02 #######################
  # Crie um resumo com base na coluna selecionada
  correlacao_dados <- reactive({
    filtered_data_2() %>%
      summarise(
        Correlacao = cor(.data[[selected_column_x()]], .data[[selected_column_y()]], method = "spearman")
      )
  })
  
  # Renderize a tabela de correlação por jogo
  output$table_02 <- renderTable({
    correlacao_dados()
  })
  
  output$lineplot_tab2 <- renderPlot({
    col_name_x <- selected_column_x()
    col_name_y <- selected_column_y()
    
    ggplot(
      filtered_data_2(), 
      aes_string(x = col_name_x, y = col_name_y)) + 
      geom_line() +
      labs(x = col_name_x, y = col_name_y) +
      ggtitle("Gráfico de Linha")
  })
  
  output$scatterplot <- renderPlot({
    ggplot(
      filtered_data_2(), 
      aes_string(x = selected_column_x(), y = selected_column_y())) + 
      geom_point(aes(col = filtered_data_2()$Species), size=3) + scale_color_discrete(name ="Species") +
      geom_smooth(aes(group=filtered_data_2()$Species, color = filtered_data_2()$Species), method='lm')
    
  })
  
}

shinyApp(ui, server)