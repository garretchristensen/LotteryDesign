library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(DT)
library(shinyjs)
library(markdown)
library(ggplot2)

# Load your data
temp <- read.csv("./2025HLdata.csv", stringsAsFactors = FALSE)
df <- as_tibble(temp)
df$fullname <- paste(df$First_Name, df$Last_Name, sep = " ", collapse = NULL)

if (!"Gender" %in% names(df)) {
  if ("Lottery.Pool" %in% names(df)) {
    df$Gender <- ifelse(df$Lottery.Pool == "F", "Female", "Male")
  }
}

# k calculation
df$k <- ifelse(df$Previous_Finishes == 0, 0,
               ifelse(df$Previous_Finishes == 1, 0.5,
                      ifelse(df$Previous_Finishes == 2, 1,
                             ifelse(df$Previous_Finishes == 3, 1.5,
                                    ifelse(df$Previous_Finishes >= 4, 0.5, 0)))))
df$v <- pmin(df$Volunteer_Points, 30)
df$t <- pmin(df$Extra_Trailwork_Points, 10)
df$tickets <- 2^(df$k + df$Previous_Applications + 1) + 2 * log(df$v + df$t + 1)

default_nw <- 103
default_nm <- 95

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(HTML("
      .intro-logo {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        margin-bottom: 18px;
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
      .odds-cols {
        display: flex;
        gap: 32px;
      }
      .odds-col {
        flex: 1 1 0;
      }
      .dt-compact-custom {
        font-size: 12px !important;
      }
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_paginate {
        font-size: 12px !important;
      }
    "))
  ),
  tags$div(
    class = "intro-logo",
    tags$img(src = "image1.png", alt = "Freestone Logo"),
    tags$div(class = "intro-title", "Lottery Design Choices")
  ),
  tags$br(),
  actionButton("toggleIntro", "Show/Hide Introduction", icon = icon("info-circle")),
  hidden(
    div(id = "introPanel", wellPanel(includeMarkdown("intro.md")))
  ),
  tags$br(),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        h4("Lottery Parameters"),
        sliderInput("exp", label = "Exponent Base", min = 2, max = 5, value = 2),
        sliderInput("mult", label = "Multiplier", min = 1, max = 10, value = 2),
        numericInput("Nm", label = "Number of Men to Pick", min = 50, max = 150, value = default_nm),
        numericInput("Nw", label = "Number of Women to Pick", min = 50, max = 150, value = default_nw),
        hr(),
        h4("Applicant Profile (for ticket calculation)"),
        sliderInput("apps", label = "Previous Applications", min = 0, max = 6, value = 0),
        sliderInput("finishes", label = "Previous Finishes", min = 0, max = 5, value = 0),
        sliderInput("volunteer", label = "Volunteer Points", min = 0, max = 30, value = 0),
        sliderInput("trailwork", label = "Extra Trailwork Points", min = 0, max = 10, value = 0),
        h5("Your tickets in the lottery:"),
        verbatimTextOutput("userTickets")
      )
    ),
    column(
      width = 8,
      wellPanel(
        h4("Lottery Odds"),
        tags$div(
          class = "odds-cols",
          div(class = "odds-col",
              tags$p("Odds for women:"),
              DT::dataTableOutput("oddsW")
          ),
          div(style = "width: 32px;"), # Spacer
          div(class = "odds-col",
              tags$p("Odds for men:"),
              DT::dataTableOutput("oddsM")
          )
        ),
        br(),
        h4("Expected Distribution of Picks by Previous Applications"),
        fluidRow(
          column(6, plotOutput("histW")),
          column(6, plotOutput("histM"))
        ),
        br(),
        h4("Expected Distribution of Picks by Previous Finishes"),
        fluidRow(
          column(6, plotOutput("finHistW")),
          column(6, plotOutput("finHistM"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggleIntro, {
    toggle(id = "introPanel")
  })
  
  output$userTickets <- renderText({
    k_sim <- ifelse(input$finishes == 0, 0,
                    ifelse(input$finishes == 1, 0.5,
                           ifelse(input$finishes == 2, 1,
                                  ifelse(input$finishes == 3, 1.5,
                                         ifelse(input$finishes >= 4, 0.5, 0)))))
    v_sim <- pmin(input$volunteer, 30)
    t_sim <- pmin(input$trailwork, 10)
    val <- input$exp ^ (k_sim + input$apps + 1) + input$mult * log(v_sim + t_sim + 1)
    round(val, 2)
  })
  
  population_tickets <- reactive({
    dff <- df
    dff$k <- ifelse(dff$Previous_Finishes == 0, 0,
                    ifelse(dff$Previous_Finishes == 1, 0.5,
                           ifelse(dff$Previous_Finishes == 2, 1,
                                  ifelse(dff$Previous_Finishes == 3, 1.5,
                                         ifelse(dff$Previous_Finishes >= 4, 0.5, 0)))))
    dff$v <- pmin(dff$Volunteer_Points, 30)
    dff$t <- pmin(dff$Extra_Trailwork_Points, 10)
    dff$tickets <- input$exp ^ (dff$k + dff$Previous_Applications + 1) + input$mult * log(dff$v + dff$t + 1)
    dff
  })
  
  calc_odds <- function(group, n_pick) {
    applicants <- pull((group %>% count(tickets))[, 2], n)
    tickets_per_applicant <- sort(unique(group$tickets))
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
    data.frame(
      Tickets = tickets_per_applicant,
      Odds = odds_of_selection,
      Applicants = applicants,
      Expected.Picked = num_people_taken
    )
  }
  
  # By Previous Applications
  picks_by_prevapps <- function(group, n_pick) {
    odds_tbl <- calc_odds(group, n_pick)
    group <- left_join(group, odds_tbl, by = c("tickets" = "Tickets"))
    group$expected_picked <- group$Expected.Picked
    group %>%
      group_by(Previous_Applications) %>%
      summarise(expected_num_picked = sum(expected_picked, na.rm = TRUE)) %>%
      ungroup()
  }
  # By Previous Finishes
  picks_by_prevfinishes <- function(group, n_pick) {
    odds_tbl <- calc_odds(group, n_pick)
    group <- left_join(group, odds_tbl, by = c("tickets" = "Tickets"))
    group$expected_picked <- group$Expected.Picked
    group %>%
      group_by(Previous_Finishes) %>%
      summarise(expected_num_picked = sum(expected_picked, na.rm = TRUE)) %>%
      ungroup()
  }
  
  output$histW <- renderPlot({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    if (nrow(women) == 0) return(NULL)
    dist <- picks_by_prevapps(women, input$Nw)
    ggplot(dist, aes(x = factor(Previous_Applications), y = expected_num_picked)) +
      geom_col(fill = "#B7822E", color = "black") +
      labs(x = "Previous Applications", y = "Expected # Women Picked", 
           title = "Women: by # Previous Applications") +
      theme_minimal(base_size = 14)
  })
  
  output$histM <- renderPlot({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    if (nrow(men) == 0) return(NULL)
    dist <- picks_by_prevapps(men, input$Nm)
    ggplot(dist, aes(x = factor(Previous_Applications), y = expected_num_picked)) +
      geom_col(fill = "#2270AE", color = "black") +
      labs(x = "Previous Applications", y = "Expected # Men Picked", 
           title = "Men: by # Previous Applications") +
      theme_minimal(base_size = 14)
  })
  
  output$finHistW <- renderPlot({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    if (nrow(women) == 0) return(NULL)
    dist <- picks_by_prevfinishes(women, input$Nw)
    ggplot(dist, aes(x = factor(Previous_Finishes), y = expected_num_picked)) +
      geom_col(fill = "#E1B882", color = "black") +
      labs(x = "Previous Finishes", y = "Expected # Women Picked", 
           title = "Women: by # Previous Finishes") +
      theme_minimal(base_size = 14)
  })
  
  output$finHistM <- renderPlot({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    if (nrow(men) == 0) return(NULL)
    dist <- picks_by_prevfinishes(men, input$Nm)
    ggplot(dist, aes(x = factor(Previous_Finishes), y = expected_num_picked)) +
      geom_col(fill = "#9EC6F5", color = "black") +
      labs(x = "Previous Finishes", y = "Expected # Men Picked", 
           title = "Men: by # Previous Finishes") +
      theme_minimal(base_size = 14)
  })
  
  output$oddsW <- DT::renderDataTable({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    if (nrow(women) == 0) return(data.frame())
    odds <- calc_odds(women, input$Nw)
    # Create display dataframe with pretty names
    odds_display <- odds %>%
      mutate(`Odds (%)` = round(Odds * 100, 2)) %>%
      select(
        `Tickets` = Tickets,
        `Odds (%)`,
        `Applicants` = Applicants,
        `Expected Picked` = Expected.Picked
      )
    datatable(odds_display, options = list(
      dom = 'tip',        # show table, info, and pagination (no search or length change)
      pageLength = 10,    # show 10 rows/page by default
      lengthMenu = c(5, 10, 25, 50, 100), # allow the user to pick page size
      className = 'dt-compact-custom',
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ), rownames = FALSE, class = "compact dt-compact-custom") %>%
      formatRound(c('Tickets', 'Applicants', 'Expected Picked'), digits = 2)
  })
  
  output$oddsM <- DT::renderDataTable({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    if (nrow(men) == 0) return(data.frame())
    odds <- calc_odds(men, input$Nm)
    odds_display <- odds %>%
      mutate(`Odds (%)` = round(Odds * 100, 2)) %>%
      select(
        `Tickets` = Tickets,
        `Odds (%)`,
        `Applicants` = Applicants,
        `Expected Picked` = Expected.Picked
      )
    datatable(odds_display, options = list(
      dom = 'tip',
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50, 100),
      className = 'dt-compact-custom',
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ), rownames = FALSE, class = "compact dt-compact-custom") %>%
      formatRound(c('Tickets', 'Applicants', 'Expected Picked'), digits = 2)
  })
}

shinyApp(ui = ui, server = server)