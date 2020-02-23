library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(plotly)
library(ShinyRatingInput) # Stefan Wilhelm
                          # (https://github.com/stefanwilhelm/ShinyRatingInput)
library(shinyjs) # Shinyjs is provided by Dean Attali
                 # (https://github.com/daattali/shinyjs)

source("plots.R")
source("helpers.R") #helper function is provided by Dean Attali
                    #(https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Wordcloud", tabName = "wordcloud"),
    menuItem(text = "Scatter Plot", tabName = "plot"),
    menuItem(text = "Recommender", tabName = "recommender")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "wordcloud",
      selectInput("selection", "Choose a period:",
                  choices = periods),
      sliderInput(inputId = "words", label = "Number of Words",
                  min = 1, max = 100, value = 50),
      actionButton("update", "Change"),
      hr(),
      plotOutput("plot1")
    ),
    tabItem(
      tabName = "plot",
      sliderInput(inputId = "year", label = "Year",
                  min = 1874, max = 2020,
                  value = c(1976, 2020)),
      # Add a plot output
      plotlyOutput("plot2")
    ),
    tabItem(
      tabName = "recommender",
      includeCSS("books.css"),
      fluidRow(
        box(width = 12, title = "Step 1: Please rate as many movies as possible", status = "danger", solidHeader = TRUE, collapsible = TRUE,
            div(class = "rateitems", uiOutput('ratings')
           )
        )
      ),
      fluidRow(
        useShinyjs(),
        box(
          width = 12, status = "danger", solidHeader = TRUE,
          title = "Step 2: Discover movies",
          br(),
          withBusyIndicatorUI(
            actionButton("btn", "Click here to view the results ♥", class = "btn-warning")
          ),
          br(),
          tableOutput("results")
    )
  )
)
)
)

dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Movie Recommender"),
  sidebar,
  body
  )

