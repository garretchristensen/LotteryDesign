library(shiny)
library(bslib)
library(shinyWidgets)

# Theme
my_theme <- bs_theme(bootswatch = "minty")

ui <- fluidPage(
  theme = my_theme,
  titlePanel("Design Future Lotteries"),
  sidebarLayout(
    sidebarPanel(
      h4("Lottery Parameters"),
      sliderInput("exp", "Exponent Base", min = 2, max = 5, value = 2),
      sliderInput("relative", "Relative Worth", min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput("multi", "Multiplier Base", min = 1, max = 10, value = 2),
      numericInput("Nm", "Number of Men", min = 50, max = 301, value = 62),
      numericInput("Nw", "Number of Women", min = 50, max = 108, value = 66)
    ),
    mainPanel(
      # Collapsible intro
      tags$div(
        style = "margin-bottom: 20px;",
        shiny::actionButton("showIntro", "Show/Hide Introduction", icon = icon("info-circle")),
        shiny::collapsePanel(
          id = "introPanel", open = TRUE,
          tags$div(
            style = "margin-top: 10px;",
            HTML(
              "<b>Lottery Design Choices:</b><br>
              There are lots of different ways to weight applicants in the lottery. Here we let you pick the weighting formula and show how that would change results using the applicant data for the 2022 race.<br><br>
              The general goals of the lottery are as follows:<ul>
                <li>We want equal numbers of men and women</li>
                <li>We'd like to get a mix of new and veteran runners, without guaranteeing entry for either</li>
                <li>Previous unsuccessful applications should be the major determinant of selection</li>
                <li>We value volunteering and trail work</li>
                <li>We'd like new entrants to have a decent chance to run within a couple-few years</li>
              </ul>
              So here are the activities for which we will award points:<ul>
                <li>Volunteer shifts at High Lonesome or other Freestone Endurance events</li>
                <li>Extra volunteer trailwork <i>beyond</i> the eight hours required</li>
                <li>Previous applications for the race</li>
                <li>Previous finishes of the race</li>
              </ul>"
            )
          )
        )
      ),
      tabsetPanel(
        tabPanel("Results Table", tableOutput("resultsTable")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", plotOutput("resultsPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Toggle intro panel
  observeEvent(input$showIntro, {
    shinyjs::toggle(id = "introPanel")
  })
  
  # Place your actual calculation logic here
  output$resultsTable <- renderTable({
    head(mtcars) # Replace with your actual results table
  })
  output$summary <- renderPrint({
    summary(mtcars) # Replace with your own summary
  })
  output$resultsPlot <- renderPlot({
    plot(mtcars$mpg, mtcars$hp) # Replace with your own plot
  })
}

# shinyjs is needed for toggling collapse
if (!require(shinyjs)) install.packages("shinyjs")
library(shinyjs)

shinyApp(
  ui = tagList(
    useShinyjs(), # Enable shinyjs
    ui
  ),
  server = server
)