#install.packages("shinydashboard")
## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

valve <- read.csv("Valve_Player_Data.csv")

valve_summary <- valve

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
                    choices = c("Average players", "Gain in users", "Percent Gain", "Peak Players"),
                    selected = NULL,
                  ),
                  dateInput(
                    inputId="start_date_1",
                    label="Selecione a data inicial",
                    value = NULL,
                    min = "mm-yyyy",
                    max = "mm-yyyy",
                    format = "mm-yyyy",
                    startview = "year",
                    weekstart = 0,
                    language = "pt-br",
                    width = NULL,
                    autoclose = TRUE,
                    datesdisabled = NULL,
                    daysofweekdisabled = NULL
                  ),
                  dateInput(
                    inputId="end_date_1",
                    label="Selecione a data final",
                    value = NULL,
                    min = NULL,
                    max = NULL,
                    format = "mm-yyyy",
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
                             box(tableOutput('table'))),
                    tabPanel("Gráfico em linha",
                             box()),
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
                    choices = c("Average players", "Gain in users", "Percent Gain", "Peak Players"),
                    selected = NULL,
                  ),
                  selectInput(
                    inputId = "classe_y",
                    label = "Eixo y",
                    choices = c("Average players", "Gain in users", "Percent Gain", "Peak Players"),
                    selected = NULL,
                  ),
                  dateInput(
                    inputId="start_date",
                    label="Selecione a data inicial",
                    value = NULL,
                    min = "mm-yyyy",
                    max = "mm-yyyy",
                    format = "mm-yyyy",
                    startview = "year",
                    weekstart = 0,
                    language = "pt-br",
                    width = NULL,
                    autoclose = TRUE,
                    datesdisabled = NULL,
                    daysofweekdisabled = NULL
                  ),
                  dateInput(
                    inputId="end_date",
                    label="Selecione a data final",
                    value = NULL,
                    min = NULL,
                    max = NULL,
                    format = "mm-yyyy",
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
                                      box()),
                             tabPanel("Gráfico em linha",
                                      box()),
                             tabPanel("Gráfico em barra das médias",
                                      box()),
                             tabPanel("Scatterplot",
                                      box())
                  )
                )
              ))
    )
  )
)

server <- function(input, output) {

  valve <- read.csv("Valve_Player_Data.csv")

  valve_summary <- valve

  df <- reactive({
    valve_summary %>%
      mutate(Month_Year = as.Date(paste0("01 ", Month_Year), "%d %B %Y")) %>%
      filter(Month_Year >= input$start_date_1 & Month_Year <= input$end_date_1)
  })

  output$table <- renderTable(head(valve_summary, 10))
}

shinyApp(ui, server)