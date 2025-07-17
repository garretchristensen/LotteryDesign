library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(DT)
library(shinyjs)
library(markdown)
library(ggplot2)

data_choices <- c("2025" = "2025HLdata.csv",
                  "2024" = "2024HLdata.csv",
                  "2023" = "2023HLdata.csv")

load_lottery_data <- function(fname) {
  temp <- read.csv(fname, stringsAsFactors = FALSE)
  df <- as_tibble(temp)
  df$fullname <- paste(df$First_Name, df$Last_Name, sep = " ", collapse = NULL)
  if (!"Gender" %in% names(df)) {
    if ("Lottery.Pool" %in% names(df)) {
      df$Gender <- ifelse(df$Lottery.Pool == "F", "Female", "Male")
    }
  }
  df$k <- ifelse(df$Previous_Finishes == 0, 0,
                 ifelse(df$Previous_Finishes == 1, 0.5,
                        ifelse(df$Previous_Finishes == 2, 1,
                               ifelse(df$Previous_Finishes == 3, 1.5,
                                      ifelse(df$Previous_Finishes >= 4, 0.5, 0)))))
  df$v <- pmin(df$Volunteer_Points, 30)
  df$t <- pmin(df$Extra_Trailwork_Points, 10)
  df$tickets <- 2^(df$k + df$Previous_Applications + 1) + 2 * log(df$v + df$t + 1)
  df
}

default_nw <- 103
default_nm <- 95

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(HTML("
      .intro-logo { display: flex; align-items: center; justify-content: flex-start; margin-bottom: 18px; }
      .intro-logo img { height: 80px; margin-right: 24px; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); background: #222A2A; padding: 8px; }
      .intro-title { font-size: 2.1em; color: #B7822E; font-weight: bold; margin-bottom: 8px; }
      .odds-cols { display: flex; gap: 32px; }
      .odds-col { flex: 1 1 0; }
      .dt-compact-custom { font-size: 12px !important; }
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_paginate { font-size: 12px !important; }
      .collapsible-btn { margin-bottom: 10px; background: #e3e3e3; border: 1px solid #bbb; color: #333; font-weight: bold; }
      .collapsible-title { font-size: 1.15em; font-weight: bold; }
      .formula-box { background: #f4f4f4; padding: 10px 18px; border-radius: 8px; font-size: 1.05em; margin-bottom: 10px; }
      .formula-highlight { color: #A97B1C; font-weight: bold; }
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
        uiOutput("ticketFormula"),
        selectInput("year_select", "Select Data Year:", choices = names(data_choices), selected = "2025"),
        sliderInput("exp", label = "Exponent Base", min = 2, max = 5, value = 2),
        sliderInput("mult", label = "Multiplier", min = 1, max = 10, value = 2),
        numericInput("Nm", label = "Number of Men to Pick", min = 50, max = 150, value = default_nm),
        numericInput("Nw", label = "Number of Women to Pick", min = 50, max = 150, value = default_nw),
        hr(),
        h4("Lottery Rule Options"),
        checkboxInput("auto_threeplus_zero", "Auto-select all 3+ year applicants with zero previous finishes", value = FALSE),
        checkboxInput("no_firsttime_zero", "Disallow first-time applicants with zero finishes", value = FALSE),
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
        actionButton("toggleAvgOdds", "Show/Hide Average Odds by Previous Applications", class="collapsible-btn"),
        tags$div(
          id = "avgOddsPanel",
          tags$div(class = "collapsible-title", "Average Odds by Previous Applications"),
          tags$div(
            class = "odds-cols",
            div(class = "odds-col",
                tags$p("Women:"),
                DT::dataTableOutput("avgOddsW")
            ),
            div(style = "width: 32px;"),
            div(class = "odds-col",
                tags$p("Men:"),
                DT::dataTableOutput("avgOddsM")
            )
          )
        ),
        br(),
        actionButton("toggleOdds", "Show/Hide Full Detail Lottery Odds", class="collapsible-btn"),
        tags$div(
          id = "oddsPanel",
          tags$div(class = "collapsible-title", "Full Detail Lottery Odds"),
          tags$div(
            class = "odds-cols",
            div(class = "odds-col",
                tags$p("Odds for women:"),
                DT::dataTableOutput("oddsW")
            ),
            div(style = "width: 32px;"),
            div(class = "odds-col",
                tags$p("Odds for men:"),
                DT::dataTableOutput("oddsM")
            )
          )
        ),
        br(),
        actionButton("toggleOddsDist", "Show/Hide Distribution of Odds", class="collapsible-btn"),
        tags$div(
          id = "oddsDistPanel",
          tags$div(class = "collapsible-title", "Distribution of Odds (Tickets vs Odds)"),
          fluidRow(
            column(6, plotOutput("oddsDistW")),
            column(6, plotOutput("oddsDistM"))
          )
        ),
        br(),
        actionButton("toggleHistApps", "Show/Hide Picks by Previous Applications", class="collapsible-btn"),
        tags$div(
          id = "histAppsPanel",
          tags$div(class = "collapsible-title", "Expected Distribution of Picks by Previous Applications"),
          fluidRow(
            column(6, plotOutput("histW")),
            column(6, plotOutput("histM"))
          )
        ),
        br(),
        actionButton("toggleHistFin", "Show/Hide Picks by Previous Finishes", class="collapsible-btn"),
        tags$div(
          id = "histFinPanel",
          tags$div(class = "collapsible-title", "Expected Distribution of Picks by Previous Finishes"),
          fluidRow(
            column(6, plotOutput("finHistW")),
            column(6, plotOutput("finHistM"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggleIntro, { toggle(id = "introPanel") })
  observeEvent(input$toggleOdds, { toggle(id = "oddsPanel") })
  observeEvent(input$toggleAvgOdds, { toggle(id = "avgOddsPanel") })
  observeEvent(input$toggleOddsDist, { toggle(id = "oddsDistPanel") })
  observeEvent(input$toggleHistApps, { toggle(id = "histAppsPanel") })
  observeEvent(input$toggleHistFin, { toggle(id = "histFinPanel") })
  
  output$ticketFormula <- renderUI({
    exp_val <- input$exp
    mult_val <- input$mult
    apps <- input$apps
    finishes <- input$finishes
    volunteer <- input$volunteer
    trailwork <- input$trailwork
    
    k_val <- ifelse(finishes == 0, 0,
                    ifelse(finishes == 1, 0.5,
                           ifelse(finishes == 2, 1,
                                  ifelse(finishes == 3, 1.5,
                                         ifelse(finishes >= 4, 0.5, 0)))))
    formula_text <- paste0(
      "<div class='formula-box'><span class='formula-highlight'>Tickets = ",
      exp_val, "^(", k_val, " + ", apps, " + 1)",
      " + ", mult_val, " * log(", volunteer, " + ", trailwork, " + 1)</span></div>"
    )
    HTML(formula_text)
  })
  
  lottery_data <- reactive({
    load_lottery_data(data_choices[[input$year_select]])
  })
  
  processed_pop <- reactive({
    df <- lottery_data()
    df$SelectionStatus <- "Lottery"
    if (input$auto_threeplus_zero) {
      idx <- df$Previous_Applications >= 3 & df$Previous_Finishes == 0
      df$SelectionStatus[idx] <- "Auto-Selected"
    }
    if (input$no_firsttime_zero) {
      idx <- df$Previous_Applications == 0 & df$Previous_Finishes == 0
      df$SelectionStatus[idx] <- "Removed"
    }
    df
  })
  
  auto_selected_counts <- reactive({
    df <- processed_pop()
    list(
      women = sum(df$Gender == "Female" & df$SelectionStatus == "Auto-Selected"),
      men = sum(df$Gender == "Male" & df$SelectionStatus == "Auto-Selected")
    )
  })
  
  lottery_population <- reactive({
    df <- processed_pop()
    df[df$SelectionStatus == "Lottery", ]
  })
  
  adjusted_Nw <- reactive({
    max(input$Nw - auto_selected_counts()$women, 0)
  })
  adjusted_Nm <- reactive({
    max(input$Nm - auto_selected_counts()$men, 0)
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
    dff <- processed_pop()
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
    if (nrow(group) == 0 || n_pick <= 0) return(data.frame())
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
  
  assign_odds <- function(df, n_pick) {
    group_lottery <- df[df$SelectionStatus == "Lottery", ]
    odds_tbl <- calc_odds(group_lottery, n_pick)
    df$Odds <- NA
    if (nrow(odds_tbl) > 0 && nrow(group_lottery) > 0) {
      odds_map <- setNames(odds_tbl$Odds, odds_tbl$Tickets)
      idx <- which(df$SelectionStatus == "Lottery")
      df$Odds[idx] <- odds_map[as.character(df$tickets[idx])]
    }
    df$Odds[df$SelectionStatus == "Auto-Selected"] <- 1
    df$Odds[df$SelectionStatus == "Removed"] <- 0
    df
  }
  
  avg_odds_by_prevapps <- function(df) {
    df %>%
      group_by(Previous_Applications) %>%
      summarise(
        n_people = n(),
        avg_odds = mean(Odds, na.rm = TRUE),
        min_odds = min(Odds, na.rm = TRUE),
        max_odds = max(Odds, na.rm = TRUE)
      ) %>%
      ungroup()
  }
  
  picks_by_prevapps <- function(df) {
    df %>%
      group_by(Previous_Applications) %>%
      summarise(expected_num_picked = sum(Odds, na.rm = TRUE)) %>%
      ungroup()
  }
  
  picks_by_prevfinishes <- function(df) {
    df %>%
      group_by(Previous_Finishes) %>%
      summarise(expected_num_picked = sum(Odds, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # ---- DETAILED ODDS TABLE LOGIC ----
  # Show one row per unique "profile", where a profile is a unique combination of
  # - tickets
  # - previous applications
  # - previous finishes
  # - status (Lottery/Auto-Selected/Removed)
  # This is needed so people with same tickets but different histories/selection status are distinct rows.
  # However, if both rules are OFF, group only by tickets (for back-compat).
  
  detailed_odds_df <- function(applicants_df, n_pick, rules_on) {
    df <- assign_odds(applicants_df, n_pick)
    if (rules_on) {
      df %>%
        group_by(
          tickets, Odds, SelectionStatus, Previous_Applications, Previous_Finishes
        ) %>%
        summarise(
          N = n(),
          .groups = "drop"
        ) %>%
        mutate(
          `Odds (%)` = round(Odds * 100, 2),
          Status = SelectionStatus
        ) %>%
        arrange(desc(Status), desc(Odds), tickets)
    } else {
      df %>%
        group_by(tickets) %>%
        summarise(
          Odds = mean(Odds, na.rm = TRUE),
          N = n(),
          `Previous Applications` = min(Previous_Applications),
          `Previous Finishes` = min(Previous_Finishes)
        ) %>%
        mutate(
          `Odds (%)` = round(Odds * 100, 2),
          Status = "Lottery"
        ) %>%
        select(
          tickets, `Odds (%)`, Status, `Previous Applications`, `Previous Finishes`, N
        ) %>%
        arrange(desc(Odds), tickets)
    }
  }
  
  output$avgOddsW <- DT::renderDataTable({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    women <- assign_odds(women, adjusted_Nw())
    avg_tbl <- avg_odds_by_prevapps(women)
    avg_tbl <- avg_tbl %>%
      mutate(
        `Average Odds (%)` = round(avg_odds * 100, 2),
        `Min Odds (%)` = round(min_odds * 100, 2),
        `Max Odds (%)` = round(max_odds * 100, 2)
      ) %>%
      select(
        `Previous Applications` = Previous_Applications,
        `N People` = n_people,
        `Average Odds (%)`,
        `Min Odds (%)`,
        `Max Odds (%)`
      )
    datatable(avg_tbl, options = list(
      dom = 't',
      pageLength = 10,
      className = 'dt-compact-custom'
    ), rownames = FALSE, class = "compact dt-compact-custom")
  })
  
  output$avgOddsM <- DT::renderDataTable({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    men <- assign_odds(men, adjusted_Nm())
    avg_tbl <- avg_odds_by_prevapps(men)
    avg_tbl <- avg_tbl %>%
      mutate(
        `Average Odds (%)` = round(avg_odds * 100, 2),
        `Min Odds (%)` = round(min_odds * 100, 2),
        `Max Odds (%)` = round(max_odds * 100, 2)
      ) %>%
      select(
        `Previous Applications` = Previous_Applications,
        `N People` = n_people,
        `Average Odds (%)`,
        `Min Odds (%)`,
        `Max Odds (%)`
      )
    datatable(avg_tbl, options = list(
      dom = 't',
      pageLength = 10,
      className = 'dt-compact-custom'
    ), rownames = FALSE, class = "compact dt-compact-custom")
  })
  
  output$oddsDistW <- renderPlot({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    women <- assign_odds(women, adjusted_Nw())
    ggplot(women, aes(x = tickets, y = Odds, color = SelectionStatus, shape = SelectionStatus)) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Lottery" = "#B7822E", "Auto-Selected" = "forestgreen", "Removed" = "red")) +
      scale_shape_manual(values = c("Lottery" = 16, "Auto-Selected" = 17, "Removed" = 4)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", title = "Women: Odds by Ticket Value") +
      theme_minimal(base_size = 14)
  })
  
  output$oddsDistM <- renderPlot({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    men <- assign_odds(men, adjusted_Nm())
    ggplot(men, aes(x = tickets, y = Odds, color = SelectionStatus, shape = SelectionStatus)) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Lottery" = "#2270AE", "Auto-Selected" = "forestgreen", "Removed" = "red")) +
      scale_shape_manual(values = c("Lottery" = 16, "Auto-Selected" = 17, "Removed" = 4)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", title = "Men: Odds by Ticket Value") +
      theme_minimal(base_size = 14)
  })
  
  output$histW <- renderPlot({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    women <- assign_odds(women, adjusted_Nw())
    dist <- picks_by_prevapps(women)
    ggplot(dist, aes(x = factor(Previous_Applications), y = expected_num_picked)) +
      geom_col(fill = "#B7822E", color = "black") +
      labs(x = "Previous Applications", y = "Expected # Women Picked", 
           title = "Women: by # Previous Applications") +
      theme_minimal(base_size = 14)
  })
  
  output$histM <- renderPlot({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    men <- assign_odds(men, adjusted_Nm())
    dist <- picks_by_prevapps(men)
    ggplot(dist, aes(x = factor(Previous_Applications), y = expected_num_picked)) +
      geom_col(fill = "#2270AE", color = "black") +
      labs(x = "Previous Applications", y = "Expected # Men Picked", 
           title = "Men: by # Previous Applications") +
      theme_minimal(base_size = 14)
  })
  
  output$finHistW <- renderPlot({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    women <- assign_odds(women, adjusted_Nw())
    dist <- picks_by_prevfinishes(women)
    ggplot(dist, aes(x = factor(Previous_Finishes), y = expected_num_picked)) +
      geom_col(fill = "#E1B882", color = "black") +
      labs(x = "Previous Finishes", y = "Expected # Women Picked", 
           title = "Women: by # Previous Finishes") +
      theme_minimal(base_size = 14)
  })
  
  output$finHistM <- renderPlot({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    men <- assign_odds(men, adjusted_Nm())
    dist <- picks_by_prevfinishes(men)
    ggplot(dist, aes(x = factor(Previous_Finishes), y = expected_num_picked)) +
      geom_col(fill = "#9EC6F5", color = "black") +
      labs(x = "Previous Finishes", y = "Expected # Men Picked", 
           title = "Men: by # Previous Finishes") +
      theme_minimal(base_size = 14)
  })
  
  # ---- DETAILED ODDS TABLES ----
  
  output$oddsW <- DT::renderDataTable({
    dff <- population_tickets()
    women <- dff[dff$Gender == "Female", ]
    rules_on <- input$auto_threeplus_zero || input$no_firsttime_zero
    odds_display <- detailed_odds_df(women, adjusted_Nw(), rules_on)
    odds_display <- odds_display %>%
      select(
        `Tickets` = tickets,
        `Odds (%)`,
        `Status`,
        `Previous Applications`,
        `Previous Finishes`,
        `N Applicants` = N
      )
    datatable(odds_display, options = list(
      dom = 'tip',
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50, 100),
      className = 'dt-compact-custom',
      columnDefs = list(
        list(targets = "_all", className = "dt-center")
      )
    ), rownames = FALSE, class = "compact dt-compact-custom") %>%
      formatRound(c('Tickets'), digits = 2)
  })
  
  output$oddsM <- DT::renderDataTable({
    dff <- population_tickets()
    men <- dff[dff$Gender == "Male", ]
    rules_on <- input$auto_threeplus_zero || input$no_firsttime_zero
    odds_display <- detailed_odds_df(men, adjusted_Nm(), rules_on)
    odds_display <- odds_display %>%
      select(
        `Tickets` = tickets,
        `Odds (%)`,
        `Status`,
        `Previous Applications`,
        `Previous Finishes`,
        `N Applicants` = N
      )
    datatable(odds_display, options = list(
      dom = 'tip',
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50, 100),
      className = 'dt-compact-custom',
      columnDefs = list(
        list(targets = "_all", className = "dt-center")
      )
    ), rownames = FALSE, class = "compact dt-compact-custom") %>%
      formatRound(c('Tickets'), digits = 2)
  })
}

shinyApp(ui = ui, server = server)