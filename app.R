library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(shinyjs)

# Load data
temp <- read.csv("./2025HLdata.csv", stringsAsFactors = FALSE)
df <- as_tibble(temp)
df$fullname <- paste(df$First_Name, df$Last_Name, sep = " ", collapse = NULL)

# Theme
my_theme <- bs_theme(bootswatch = "minty")

ui <- fluidPage(
  useShinyjs(),
  theme = my_theme,
  tags$head(
    tags$style(HTML("
      .intro-logo {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        margin-bottom: 12px;
      }
      .intro-logo img {
        height: 80px;
        margin-right: 24px;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        background: #222A2A;
        padding: 8px;
      }
      .intro-title {
        font-size: 2.1em;
        color: #B7822E;
        font-weight: bold;
        margin-bottom: 8px;
      }
    "))
  ),
  titlePanel("Design Future Lotteries"),
  sidebarLayout(
    sidebarPanel(
      h4("Lottery Parameters"),
      sliderInput("exp", label = "Exponent Base", min = 2, max = 5, value = 2),
      sliderInput("mult", label = "Multiplier Base", min = 1, max = 10, value = 2),
      numericInput("Nm", label = "Number of Men", min = 50, max = 150, value = 95),
      numericInput("Nw", label = "Number of Women", min = 50, max = 150, value = 103),
      hr(),
      h4("Applicant Profile (for ticket calculation)"),
      sliderInput("apps", label = "Previous Applications", min = 0, max = 6, value = 0),
      sliderInput("finishes", label = "Previous Finishes", min = 0, max = 4, value = 0),
      sliderInput("volunteer", label = "Volunteer Points", min = 0, max = 30, value = 0),
      sliderInput("trailwork", label = "Extra Trailwork Points", min = 0, max = 10, value = 0)
    ),
    mainPanel(
      tags$div(
        style = "margin-bottom: 20px;",
        actionButton("toggleIntro", "Show/Hide Introduction", icon = icon("info-circle")),
        div(
          id = "introPanel",
          style = "margin-top: 10px;",
          wellPanel(
            tags$div(
              class = "intro-logo",
              tags$img(src = "image1", alt = "Freestone Logo"),
              tags$div(class = "intro-title", "Lottery Design Choices")
            ),
            HTML(
              "There are lots of different ways to weight applicants in the lottery. Here we let you pick the weighting formula and show how that would change results using the applicant data for the 2025 race.<br><br>
              <b>The general goals of the lottery are as follows:</b>
              <ul>
                <li>We want equal numbers of men and women</li>
                <li>We'd like to get a mix of new and veteran runners, without guaranteeing entry for either</li>
                <li>Previous unsuccessful applications should be the major determinant of selection</li>
                <li>We value volunteering and trail work</li>
                <li>We'd like new entrants to have a decent chance to run within a couple-few years</li>
              </ul>
              <b>So here are the activities for which we will award points:</b>
              <ul>
                <li>Volunteer shifts at High Lonesome or other Freestone Endurance events</li>
                <li>Extra volunteer trailwork <i>beyond</i> the eight hours required</li>
                <li>Previous applications for the race</li>
                <li>Previous finishes of the race</li>
              </ul>
              <b>The current model is:</b> <code>Tickets = exp^(n + k + 1) + mult * ln(v + t + 1)</code> where:
              <ul>
                <li>n = Previous Applications</li>
                <li>k = Finish Multiplier</li>
                <li>v = Volunteer Points</li>
                <li>t = Extra Trailwork Points</li>
              </ul>"
            )
          )
        )
      ),
      fluidRow(
        column(12, h4("Your tickets in the lottery:"), verbatimTextOutput("tickets"))
      ),
      fluidRow(
        column(12, h4("These are the odds. Women left, men right:"))
      ),
      fluidRow(
        column(6, tableOutput("wtable")),
        column(6, tableOutput("mtable"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggleIntro, {
    toggle(id = "introPanel")
  })
  
  # Calculate tickets for a specific profile
  output$tickets <- renderText({
    k_sim <- input$finishes
    v_sim <- pmin(input$volunteer, 30)
    t_sim <- pmin(input$trailwork, 10)
    # Tickets = exp^(n + k + 1) + mult * ln(v + t + 1)
    round(input$exp^(k_sim + input$apps + 1) + input$mult * log(v_sim + t_sim + 1), 2)
  })
  
  # Helper function to compute and format odds table
  lottery_odds_table <- function(group, n_pick) {
    applicants <- as.numeric(pull((group %>% count(Tickets))[,2], n))
    tickets_per_applicant <- sort(unique(group$Tickets))
    original_tickets <- applicants * tickets_per_applicant
    ticket_counts <- original_tickets
    for (i in 1:n_pick) {
      prob_of_selecting_category <- ticket_counts / sum(ticket_counts)
      exp_ticket_reduction <- prob_of_selecting_category * tickets_per_applicant
      ticket_counts <- ticket_counts - exp_ticket_reduction
    }
    tickets_taken <- original_tickets - ticket_counts
    odds_of_selection <- tickets_taken / original_tickets
    num_people_taken <- odds_of_selection * applicants
    
    # Build a nice output data frame
    data.frame(
      "Tickets per Person" = tickets_per_applicant,
      "Selection Probability (%)" = round(odds_of_selection * 100, 2),
      "# Applicants" = applicants,
      "Expected Selected" = round(num_people_taken, 2)
    )
  }
  
  # Women's odds table
  output$wtable <- renderTable({
    dff <- df
    dff$Applications <- dff$Previous_Applications
    dff$k <- dff$`Finish Multiplier`
    dff$v <- pmin(dff$Volunteer_Points, 30)
    dff$t <- pmin(dff$Extra_Trailwork_Points, 10)
    dff$Tickets <- input$exp^(dff$k + dff$Applications + 1) + input$mult * log(dff$v + dff$t + 1)
    women <- dff[dff$Gender == "F", ]
    if (nrow(women) == 0) return(data.frame())
    lottery_odds_table(women, input$Nw)
  })
  
  # Men's odds table
  output$mtable <- renderTable({
    dff <- df
    dff$Applications <- dff$Previous_Applications
    dff$k <- dff$`Finish Multiplier`
    dff$v <- pmin(dff$Volunteer_Points, 30)
    dff$t <- pmin(dff$Extra_Trailwork_Points, 10)
    dff$Tickets <- input$exp^(dff$k + dff$Applications + 1) + input$mult * log(dff$v + dff$t + 1)
    men <- dff[dff$Gender == "M", ]
    if (nrow(men) == 0) return(data.frame())
    lottery_odds_table(men, input$Nm)
  })
}

shinyApp(ui = ui, server = server)