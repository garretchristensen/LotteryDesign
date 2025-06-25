library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(readxl)
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
  titlePanel("Design Future Lotteries"),
  sidebarLayout(
    sidebarPanel(
      h4("Lottery Parameters"),
      sliderInput("exp", label = "Exponent Base", min = 2, max = 5, value = 2),
      sliderInput("mult", label = "Multiplier Base", min = 1, max = 10, value = 2),
      numericInput("Nm", label = "Number of Men", min = 50, max = 301, value = 62),
      numericInput("Nw", label = "Number of Women", min = 50, max = 108, value = 66),
      hr(),
      h4("Applicant Profile (for ticket calculation)"),
      sliderInput("apps", label = "Previous Applications", min = 0, max = 6, value = 0),
      sliderInput("finishes", label = "Previous Finishes", min = 0, max = 4, value = 0),
      sliderInput("volunteer", label = "Volunteer Shifts", min = 0, max = 30, value = 0),
      sliderInput("trailwork", label = "Extra Trailwork", min = 0, max = 10, value = 0)
    ),
    mainPanel(
      tags$div(
        style = "margin-bottom: 20px;",
        actionButton("toggleIntro", "Show/Hide Introduction", icon = icon("info-circle")),
        div(
          id = "introPanel",
          style = "margin-top: 10px;",
          wellPanel(
            HTML(
              "<b>Lottery Design Choices:</b><br>
              There are lots of different ways to weight applicants in the lottery. Here we let you pick the weighting formula and show how that would change results using the applicant data for the 2022 race.<br><br>
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
                <li>k = Weighted Finishes</li>
                <li>v = Volunteer Points</li>
                <li>t = Extra Trailwork</li>
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
    k_sim <- ifelse(input$finishes == 0, 0,
                   ifelse(input$finishes == 1, 0.5,
                          ifelse(input$finishes == 2, 1,
                                 ifelse(input$finishes == 3, 1.5,
                                        ifelse(input$finishes >= 4, 0.5, 0)))))
    v_sim <- pmin(input$volunteer, 30)
    t_sim <- pmin(input$trailwork, 10)
    # Tickets = exp^(n + k + 1) + mult * ln(v + t + 1)
    round(input$exp^(k_sim + input$apps + 1) + input$mult * log(v_sim + t_sim + 1), 2)
  })

  # Women's odds table
  output$wtable <- renderTable({
    dff <- df
    dff$Applications <- dff$Previous_Applications
    dff$k <- ifelse(dff$Previous_Finishes == 0, 0,
                    ifelse(dff$Previous_Finishes == 1, 0.5,
                           ifelse(dff$Previous_Finishes == 2, 1,
                                  ifelse(dff$Previous_Finishes == 3, 1.5,
                                         ifelse(dff$Previous_Finishes >= 4, 0.5, 0)))))
    dff$v <- pmin(dff$Volunteer_Shifts, 30)
    dff$t <- pmin(dff$Extra_Trailwork, 10)
    dff$tickets <- input$exp^(dff$k + dff$Applications + 1) + input$mult * log(dff$v + dff$t + 1)
    women <- dff[dff$Gender == "F", ]
    applicants <- pull((women %>% count(tickets))[, 2], n)
    tickets_per_applicant <- sort(women$tickets[!duplicated(women$tickets)])
    original_tickets <- applicants * tickets_per_applicant
    ticket_counts <- original_tickets
    n_women_pick <- input$Nw
    for (i in 1:n_women_pick) {
      prob_of_selecting_category <- ticket_counts / sum(ticket_counts)
      exp_ticket_reduction <- prob_of_selecting_category * tickets_per_applicant
      ticket_counts <- ticket_counts - exp_ticket_reduction
    }
    tickets_taken <- original_tickets - ticket_counts
    odds_of_selection <- tickets_taken / original_tickets
    num_people_taken <- odds_of_selection * applicants
    data.frame(
      "Tickets" = tickets_per_applicant,
      "Odds" = round(odds_of_selection, 3),
      "Applicants" = applicants,
      "Expected_Selected" = round(num_people_taken, 2)
    )
  })

  # Men's odds table
  output$mtable <- renderTable({
    dff <- df
    dff$Applications <- dff$Previous_Applications
    dff$k <- ifelse(dff$Previous_Finishes == 0, 0,
                    ifelse(dff$Previous_Finishes == 1, 0.5,
                           ifelse(dff$Previous_Finishes == 2, 1,
                                  ifelse(dff$Previous_Finishes == 3, 1.5,
                                         ifelse(dff$Previous_Finishes >= 4, 0.5, 0)))))
    dff$v <- pmin(dff$Volunteer_Shifts, 30)
    dff$t <- pmin(dff$Extra_Trailwork, 10)
    dff$tickets <- input$exp^(dff$k + dff$Applications + 1) + input$mult * log(dff$v + dff$t + 1)
    men <- dff[dff$Gender == "M", ]
    applicants <- pull((men %>% count(tickets))[, 2], n)
    tickets_per_applicant <- sort(men$tickets[!duplicated(men$tickets)])
    original_tickets <- applicants * tickets_per_applicant
    ticket_counts <- original_tickets
    n_men_pick <- input$Nm
    for (i in 1:n_men_pick) {
      prob_of_selecting_category <- ticket_counts / sum(ticket_counts)
      exp_ticket_reduction <- prob_of_selecting_category * tickets_per_applicant
      ticket_counts <- ticket_counts - exp_ticket_reduction
    }
    tickets_taken <- original_tickets - ticket_counts
    odds_of_selection <- tickets_taken / original_tickets
    num_people_taken <- odds_of_selection * applicants
    data.frame(
      "Tickets" = tickets_per_applicant,
      "Odds" = round(odds_of_selection, 3),
      "Applicants" = applicants,
      "Expected_Selected" = round(num_people_taken, 2)
    )
  })
}

shinyApp(ui = ui, server = server)