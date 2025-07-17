# Freestone Lottery Odds Shiny App - Full Version, Odds ALWAYS Displayed to Two Decimals (No More, No Less)
# All comments, features, and formatting preserved.
# In the detailed odds tables, both "tickets" and "Odds (%)" columns are now always two decimals (as character), never more.

library(shiny)
library(dplyr)
library(tibble)
library(DT)
library(bslib)
library(ggplot2)
library(shinyjs)
library(markdown)

# Function: Calculate odds for each entry in a multi-pick lottery without replacement
calc_lottery_odds <- function(tickets, n_picks) {
  if (length(tickets) == 0 || n_picks <= 0) return(numeric(0))
  survivors <- rep(1, length(tickets))
  odds <- rep(0, length(tickets))
  for (draw in seq_len(n_picks)) {
    total_tix <- sum(tickets * survivors)
    if (total_tix == 0) break
    prob_win <- (tickets * survivors) / total_tix
    odds <- odds + prob_win * (1 - odds)
    survivors <- survivors * (1 - prob_win)
  }
  odds
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
      .odds-table-compact { font-size: 11px !important; }
      .dataTables_wrapper .odds-table-compact .dataTables_scrollHeadInner,
      .dataTables_wrapper .odds-table-compact .dataTables_scrollBody { max-width: 730px !important; }
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
        selectInput("year_select", "Select Data Year:", choices = c("2025", "2024", "2023"), selected = "2025"),
        sliderInput("exp", label = "Exponent Base", min = 2, max = 5, value = 2),
        sliderInput("mult", label = "Multiplier", min = 1, max = 10, value = 2),
        numericInput("Nm", label = "Number of Men to Pick", min = 0, max = 200, value = default_nm),
        numericInput("Nw", label = "Number of Women to Pick", min = 0, max = 200, value = default_nw),
        hr(),
        h4("Discrete Rule Options"),
        radioButtons("threeplus_rule", "3+ year applicant rule:",
                     choices = list("None" = "none",
                                    "Auto-select 3+ year applicants with no finishes" = "zero_finishes",
                                    "Auto-select 3+ year applicants regardless of finishes" = "all_finishes"),
                     selected = "none"),
        checkboxInput("no_firsttime_zero", "Disallow first-time applicants with no finishes", value = FALSE),
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
          tags$div(class = "collapsible-title", "Full Detail Lottery Odds (Grouped by Profile)"),
          tags$div(
            class = "odds-cols",
            div(class = "odds-col",
                tags$p("Women:"),
                DT::dataTableOutput("oddsW")
            ),
            div(style = "width: 32px;"),
            div(class = "odds-col",
                tags$p("Men:"),
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
  
  # Show dynamic ticket formula
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
  
  # Load and preprocess data from selected year, recalculate tickets
  base_data <- reactive({
    fname <- switch(input$year_select,
                    "2025" = "2025HLdata.csv",
                    "2024" = "2024HLdata.csv",
                    "2023" = "2023HLdata.csv")
    df <- read.csv(fname, stringsAsFactors = FALSE) %>%
      mutate(
        Gender = ifelse(Lottery.Pool == "F", "Female", "Male"),
        Previous_Applications = as.numeric(Previous_Applications),
        Previous_Finishes = as.numeric(Previous_Finishes),
        Volunteer_Points = as.numeric(Volunteer_Points),
        Extra_Trailwork_Points = as.numeric(Extra_Trailwork_Points)
      )
    df$k <- ifelse(df$Previous_Finishes == 0, 0,
                   ifelse(df$Previous_Finishes == 1, 0.5,
                          ifelse(df$Previous_Finishes == 2, 1,
                                 ifelse(df$Previous_Finishes == 3, 1.5,
                                        ifelse(df$Previous_Finishes >= 4, 0.5, 0)))))
    df$v <- pmin(df$Volunteer_Points, 30)
    df$t <- pmin(df$Extra_Trailwork_Points, 10)
    df$tickets <- input$exp ^ (df$k + df$Previous_Applications + 1) +
      input$mult * log(df$v + df$t + 1)
    df
  })
  
  # Show user's ticket count for chosen slider profile
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
  
  # Apply rules to assign SelectionStatus
  processed_data <- reactive({
    df <- base_data()
    df$SelectionStatus <- "Lottery"
    
    # Apply 3+ year applicant rules (mutually exclusive)
    if (input$threeplus_rule == "zero_finishes") {
      df$SelectionStatus[df$Previous_Applications >= 3 & df$Previous_Finishes == 0] <- "Auto-Selected"
    } else if (input$threeplus_rule == "all_finishes") {
      df$SelectionStatus[df$Previous_Applications >= 3] <- "Auto-Selected"
    }
    
    # Apply first-time applicant rule
    if (input$no_firsttime_zero) {
      df$SelectionStatus[df$Previous_Applications == 0 & df$Previous_Finishes == 0] <- "Removed"
    }
    df
  })
  
  # Assign odds for each gender, only for those in the pool
  assign_odds <- function(df, gender, n_picks) {
    in_draw <- which(df$SelectionStatus == "Lottery" & df$Gender == gender)
    odds <- rep(NA_real_, nrow(df))
    if (length(in_draw) > 0 && n_picks > 0) {
      odds[in_draw] <- calc_lottery_odds(df$tickets[in_draw], n_picks)
    }
    odds[df$SelectionStatus == "Auto-Selected" & df$Gender == gender] <- 1
    odds[df$SelectionStatus == "Removed" & df$Gender == gender] <- 0
    odds
  }
  
  # Compute odds for all applicants, only pool applicants affect calculation
  assigned_data <- reactive({
    df <- processed_data()
    n_picks_w <- max(0, input$Nw - sum(df$Gender == "Female" & df$SelectionStatus == "Auto-Selected"))
    n_picks_m <- max(0, input$Nm - sum(df$Gender == "Male" & df$SelectionStatus == "Auto-Selected"))
    df$Odds <- assign_odds(df, "Female", n_picks_w)
    df$Odds[df$Gender == "Male"] <- assign_odds(df, "Male", n_picks_m)[df$Gender == "Male"]
    df
  })
  
  # --- DETAILED ODDS TABLES: odds and tickets as "x.xx" character, always two decimals, never more. ---
  output$oddsW <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Female")
    grouped <- df %>%
      group_by(tickets, Previous_Applications, Previous_Finishes, SelectionStatus, Odds) %>%
      summarise(N = n(), .groups = "drop")
    grouped <- grouped %>%
      mutate(
        # Always two decimals, as character, for display and CSV/export
        tickets_char = formatC(round(tickets,2), format="f", digits=2),
        odds_percent = formatC(round(100 * Odds,2), format="f", digits=2)
      ) %>%
      arrange(as.numeric(tickets_char)) %>%
      select(
        "Tickets" = tickets_char,
        "N" = N,
        "Previous Applications" = Previous_Applications,
        "Previous Finishes" = Previous_Finishes,
        "Selection Status" = SelectionStatus,
        "Odds (%)" = odds_percent
      )
    datatable(grouped,
              options = list(
                pageLength = 20, 
                dom = "tip", 
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(width = '65px', targets = 0:4)
                )
              ),
              rownames = FALSE,
              class = "compact dt-compact-custom odds-table-compact"
    )
  })
  
  output$oddsM <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Male")
    grouped <- df %>%
      group_by(tickets, Previous_Applications, Previous_Finishes, SelectionStatus, Odds) %>%
      summarise(N = n(), .groups = "drop")
    grouped <- grouped %>%
      mutate(
        tickets_char = formatC(round(tickets,2), format="f", digits=2),
        odds_percent = formatC(round(100 * Odds,2), format="f", digits=2)
      ) %>%
      arrange(as.numeric(tickets_char)) %>%
      select(
        "Tickets" = tickets_char,
        "N" = N,
        "Previous Applications" = Previous_Applications,
        "Previous Finishes" = Previous_Finishes,
        "Selection Status" = SelectionStatus,
        "Odds (%)" = odds_percent
      )
    datatable(grouped,
              options = list(
                pageLength = 20, 
                dom = "tip", 
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(width = '65px', targets = 0:4)
                )
              ),
              rownames = FALSE,
              class = "compact dt-compact-custom odds-table-compact"
    )
  })
  
  # --- Average odds by applications, with min/max ---
  output$avgOddsW <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Female")
    avg <- df %>%
      group_by(Previous_Applications) %>%
      summarise(
        N = n(),
        `Avg Odds (%)` = round(mean(Odds, na.rm = TRUE) * 100, 2),
        `Min Odds (%)` = round(min(Odds, na.rm = TRUE) * 100, 2),
        `Max Odds (%)` = round(max(Odds, na.rm = TRUE) * 100, 2),
        .groups = "drop") %>%
      rename(`Previous Applications` = Previous_Applications)
    datatable(avg, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$avgOddsM <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Male")
    avg <- df %>%
      group_by(Previous_Applications) %>%
      summarise(
        N = n(),
        `Avg Odds (%)` = round(mean(Odds, na.rm = TRUE) * 100, 2),
        `Min Odds (%)` = round(min(Odds, na.rm = TRUE) * 100, 2),
        `Max Odds (%)` = round(max(Odds, na.rm = TRUE) * 100, 2),
        .groups = "drop") %>%
      rename(`Previous Applications` = Previous_Applications)
    datatable(avg, options = list(dom = "t"), rownames = FALSE)
  })
  
  # --- Ticket value vs odds scatter plot ---
  output$oddsDistW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    ggplot(df, aes(x = tickets, y = Odds, color = SelectionStatus)) +
      geom_jitter(width = 0.1, height = 0) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", title = "Women: Ticket vs Odds") +
      theme_minimal()
  })
  output$oddsDistM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    ggplot(df, aes(x = tickets, y = Odds, color = SelectionStatus)) +
      geom_jitter(width = 0.1, height = 0) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", title = "Men: Ticket vs Odds") +
      theme_minimal()
  })
  
  # --- Expected picks by previous applications (bar) ---
  output$histW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    p <- df %>%
      group_by(Previous_Applications) %>%
      summarise(ExpectedPicks = sum(Odds, na.rm = TRUE))
    ggplot(p, aes(x = factor(Previous_Applications), y = ExpectedPicks)) +
      geom_col(fill = "#B7822E") +
      labs(x = "Previous Applications", y = "Expected # Picked") +
      theme_minimal()
  })
  output$histM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    p <- df %>%
      group_by(Previous_Applications) %>%
      summarise(ExpectedPicks = sum(Odds, na.rm = TRUE))
    ggplot(p, aes(x = factor(Previous_Applications), y = ExpectedPicks)) +
      geom_col(fill = "#2270AE") +
      labs(x = "Previous Applications", y = "Expected # Picked") +
      theme_minimal()
  })
  
  # --- Expected picks by previous finishes (bar) ---
  output$finHistW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    p <- df %>%
      group_by(Previous_Finishes) %>%
      summarise(ExpectedPicks = sum(Odds, na.rm = TRUE))
    ggplot(p, aes(x = factor(Previous_Finishes), y = ExpectedPicks)) +
      geom_col(fill = "#E1B882") +
      labs(x = "Previous Finishes", y = "Expected # Picked") +
      theme_minimal()
  })
  output$finHistM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    p <- df %>%
      group_by(Previous_Finishes) %>%
      summarise(ExpectedPicks = sum(Odds, na.rm = TRUE))
    ggplot(p, aes(x = factor(Previous_Finishes), y = ExpectedPicks)) +
      geom_col(fill = "#9EC6F5") +
      labs(x = "Previous Finishes", y = "Expected # Picked") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)