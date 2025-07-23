library(shiny)
library(dplyr)
library(tibble)
library(DT)
library(bslib)
library(ggplot2)
library(shinyjs)
library(markdown)
library(memoise)  # Only for Monte Carlo memoization
library(digest)   # Only for Monte Carlo memoization

# Function: Exact odds calculation (without replacement)
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

# Function: Simulate one draw (Monte Carlo)
simulate_draw <- function(tickets, n_picks) {
  selected <- integer(0)
  remaining <- seq_along(tickets)
  remaining_tickets <- tickets
  for (i in seq_len(n_picks)) {
    if (length(remaining) == 0) break
    probs <- remaining_tickets / sum(remaining_tickets)
    pick <- sample(remaining, 1, prob = probs)
    selected <- c(selected, pick)
    remaining_tickets <- remaining_tickets[remaining != pick]
    remaining <- remaining[remaining != pick]
  }
  selected
}

# Function: Monte Carlo odds over many simulations
simulate_odds <- function(tickets, n_picks, n_sim = 10000) {
  counts <- numeric(length(tickets))
  for (i in seq_len(n_sim)) {
    picks <- simulate_draw(tickets, n_picks)
    counts[picks] <- counts[picks] + 1
  }
  counts / n_sim
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
  br(),
  actionButton("toggleIntro", "Show/Hide Introduction", icon = icon("info-circle")),
  hidden(
    div(id = "introPanel", wellPanel(includeMarkdown("intro.md")))
  ),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      h4("Lottery Parameters"),
      uiOutput("ticketFormula"),
      selectInput("year_select", "Data Year:", choices = c("2026", "2025", "2024", "2023"), selected = "2025"),
      sliderInput("exp", "Exponent Base:", min = 2, max = 5, value = 2),
      sliderInput("mult", "Multiplier:", min = 1, max = 10, value = 2),
      numericInput("Nm", "Number of Men to Pick:", min = 0, max = 200, value = default_nm),
      numericInput("Nw", "Number of Women to Pick:", min = 0, max = 200, value = default_nw),
      h4("Discrete Rules"),
      radioButtons("threeplus_rule", "3+ year applicant rule:",
                   choices = list("None" = "none",
                                  "Auto-select 3+ year applicants with no finishes" = "zero_finishes",
                                  "Auto-select 3+ year applicants regardless of finishes" = "all_finishes"),
                   selected = "none"),
      checkboxInput("no_firsttime_zero", "Disallow first-time applicants with no finishes", FALSE),
      hr(),
      radioButtons("odds_method", "Odds Calculation Method:",
                   choices = c("Deterministic (Fast)" = "deterministic",
                               "Monte Carlo (N = 1,000, Slow)" = "mc1000",
                               "Monte Carlo (N = 10,000, Slower)" = "mc10000"),
                   selected = "deterministic"),
      hr(),
      h4("Example Applicant Profile"),
      sliderInput("apps", label = "Previous Applications", min = 0, max = 6, value = 0),
      sliderInput("finishes", label = "Previous Finishes", min = 0, max = 5, value = 0),
      sliderInput("volunteer", label = "Volunteer Points", min = 0, max = 30, value = 0),
      sliderInput("trailwork", label = "Extra Trailwork Points", min = 0, max = 10, value = 0),
      h5("Your tickets in the lottery:"),
      verbatimTextOutput("userTickets"),
      hr(),
      h5(textOutput("statsTitle", inline = TRUE)),
      textOutput("avgAppsStats"),
      textOutput("avgFinishesStats"),
      textOutput("trailworkStats"),
      textOutput("volunteerStats")

    ),
    mainPanel(
      h3("Lottery Odds Summary"),
      actionButton("toggleAvgOdds", "Show/Hide Average Odds by Previous Applications", class = "collapsible-btn"),
      tags$div(
        id = "avgOddsPanel",
        fluidRow(
          column(6, DTOutput("avgOddsW")),
          column(6, DTOutput("avgOddsM"))
        )
      ),
      br(),
      h3("Detailed Odds"),
      actionButton("toggleOdds", "Show/Hide Full Detail Lottery Odds", class = "collapsible-btn"),
      tags$div(
        id = "oddsPanel",
        fluidRow(
          column(6, DTOutput("oddsW")),
          column(6, DTOutput("oddsM"))
        )
      ),
      br(),
      h3("Distribution of Odds (Tickets vs Odds)"),
      actionButton("toggleOddsDist", "Show/Hide Distribution of Odds", class = "collapsible-btn"),
      tags$div(
        id = "oddsDistPanel",
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

# Wrap simulate_odds call with progress bar
withProgressSimulate <- function(tix, picks, n_sim, label = "Running Monte Carlo...") {
  withProgress(message = label, value = 0.3, {
    Sys.sleep(0.1)  # Allow UI to update
    simulate_odds(tix, picks, n_sim)
  })
}


server <- function(input, output, session) {
  
  observeEvent(input$toggleIntro, { shinyjs::toggle(id = "introPanel") })
  observeEvent(input$toggleAvgOdds, { shinyjs::toggle(id = "avgOddsPanel") })
  observeEvent(input$toggleOdds, { shinyjs::toggle(id = "oddsPanel") })
  observeEvent(input$toggleOddsDist, { shinyjs::toggle(id = "oddsDistPanel") })
  observeEvent(input$toggleHistApps, { toggle("histAppsPanel") })
  observeEvent(input$toggleHistFin, { toggle("histFinPanel") })

  # Memoized simulate_odds including exp, mult, year, rules, and number picked in key
  memo_simulate_odds <- memoise::memoise(function(tix, picks, n_sim, exp_val, mult_val, year, rule, no_firsttime_zero) {
    key <- digest::digest(list(round(tix, 8), picks, n_sim, exp_val, mult_val, year, rule, no_firsttime_zero))
    attr(tix, "digest_key") <- key
    withProgressSimulate(tix, picks, n_sim)
  })
  
  
  # Reactive selector for display odds column
  display_odds_col <- reactive({
    if (input$odds_method %in% c("mc1000", "mc10000")) "Odds_MC" else "Odds_Exact"
  })
  
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
  
  base_data <- reactive({
    fname <- switch(input$year_select,
                    "2026" = "2026HLdata.csv",
                    "2025" = "2025HLdata.csv",
                    "2024" = "2024HLdata.csv",
                    "2023" = "2023HLdata.csv")
    df <- read.csv(fname, stringsAsFactors = FALSE) %>%
      mutate(
        Gender = ifelse(Lottery.Pool == "F", "Female", "Male"),
        Previous_Applications = as.numeric(Previous_Applications),
        Previous_Finishes = as.numeric(Previous_Finishes),
        Volunteer_Points = pmin(as.numeric(Volunteer_Points), 30),
        Extra_Trailwork_Points = pmin(as.numeric(Extra_Trailwork_Points), 10)
      )
    df$k <- ifelse(df$Previous_Finishes == 0, 0,
                   ifelse(df$Previous_Finishes == 1, 0.5,
                          ifelse(df$Previous_Finishes == 2, 1,
                                 ifelse(df$Previous_Finishes == 3, 1.5, 0.5))))
    df$tickets <- input$exp ^ (df$k + df$Previous_Applications + 1) +
      input$mult * log(df$Volunteer_Points + df$Extra_Trailwork_Points + 1)
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
  
  processed_data <- reactive({
    df <- base_data()
    df$SelectionStatus <- "Lottery"
    if (input$threeplus_rule == "zero_finishes") {
      df$SelectionStatus[df$Previous_Applications >= 3 & df$Previous_Finishes == 0] <- "Auto-Selected"
    } else if (input$threeplus_rule == "all_finishes") {
      df$SelectionStatus[df$Previous_Applications >= 3] <- "Auto-Selected"
    }
    if (input$no_firsttime_zero) {
      df$SelectionStatus[df$Previous_Applications == 0 & df$Previous_Finishes == 0] <- "Removed"
    }
    df
  })
  
  calc_deterministic_odds <- function(tix, picks, exp_val, mult_val, year, rule, no_firsttime_zero) {
    key <- digest::digest(list(round(tix, 8), picks, exp_val, mult_val, year, rule, no_firsttime_zero))
    attr(tix, "deterministic_key") <- key
    calc_lottery_odds(tix, picks)
  }
  
  # Simplified assigned_data() reactive function
  assigned_data <- reactive({
    df <- processed_data()
    n_picks_w <- max(0, input$Nw - sum(df$Gender == "Female" & df$SelectionStatus == "Auto-Selected"))
    n_picks_m <- max(0, input$Nm - sum(df$Gender == "Male" & df$SelectionStatus == "Auto-Selected"))
    
    df$Odds_Exact <- NA_real_
    df$Odds_MC <- NA_real_
    
    run_mc <- input$odds_method %in% c("mc1000", "mc10000")
    n_sim <- switch(input$odds_method,
                    "mc1000" = 1000,
                    "mc10000" = 10000,
                    0)
    
    for (gender in c("Female", "Male")) {
      idx <- which(df$Gender == gender & df$SelectionStatus == "Lottery")
      picks <- ifelse(gender == "Female", n_picks_w, n_picks_m)
      
      if (length(idx) > 0 && picks > 0) {
        tix <- df$tickets[idx]
        
        # Direct calculation - no memoization needed, super fast
        df$Odds_Exact[idx] <- calc_lottery_odds(tix, picks)
        
        # Only memoize the slow Monte Carlo simulations
        if (run_mc) {
          df$Odds_MC[idx] <- memo_simulate_odds(
            tix = tix,
            picks = picks,
            n_sim = n_sim,
            exp_val = input$exp,
            mult_val = input$mult,
            year = input$year_select,
            rule = input$threeplus_rule,
            no_firsttime_zero = input$no_firsttime_zero
          )
        }
      }
      
      # Handle auto-selected and removed cases
      df$Odds_Exact[df$Gender == gender & df$SelectionStatus == "Auto-Selected"] <- 1
      df$Odds_MC[df$Gender == gender & df$SelectionStatus == "Auto-Selected"] <- 1
      df$Odds_Exact[df$Gender == gender & df$SelectionStatus == "Removed"] <- 0
      df$Odds_MC[df$Gender == gender & df$SelectionStatus == "Removed"] <- 0
    }
    
    df
  }) 
  
  get_display_odds <- function(df) {
    method <- input$odds_method
    if (method %in% c("mc1000", "mc10000")) df$Odds_MC else df$Odds_Exact
  }
  
  output$avgOddsW <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    avg <- df %>%
      group_by(Previous_Applications) %>%
      summarise(
        N = n(),
        `Avg Odds (%)` = round(mean(Display_Odds, na.rm = TRUE) * 100, 2),
        `Min Odds (%)` = round(min(Display_Odds, na.rm = TRUE) * 100, 2),
        `Max Odds (%)` = round(max(Display_Odds, na.rm = TRUE) * 100, 2),
        .groups = "drop") %>%
      rename(`Previous Applications` = Previous_Applications)
    datatable(avg, options = list(dom = "t"), rownames = FALSE)
  }) 
  output$avgOddsM <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    avg <- df %>%
      group_by(Previous_Applications) %>%
      summarise(
        N = n(),
        `Avg Odds (%)` = round(mean(Display_Odds, na.rm = TRUE) * 100, 2),
        `Min Odds (%)` = round(min(Display_Odds, na.rm = TRUE) * 100, 2),
        `Max Odds (%)` = round(max(Display_Odds, na.rm = TRUE) * 100, 2),
        .groups = "drop") %>%
      rename(`Previous Applications` = Previous_Applications)
    datatable(avg, options = list(dom = "t"), rownames = FALSE)
  }) 
  
  output$oddsW <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    grouped <- df %>%
      group_by(tickets, Previous_Applications, Previous_Finishes, SelectionStatus, Display_Odds) %>%
      summarise(N = n(), .groups = "drop") %>%
      mutate(
        tickets_char = formatC(round(tickets,2), format="f", digits=2),
        odds_percent = formatC(round(100 * Display_Odds,2), format="f", digits=2)
      ) %>%
      arrange(as.numeric(tickets_char)) %>%
      select("Tickets" = tickets_char, "N" = N,
             "Previous Apps" = Previous_Applications,
             "Previous Fins" = Previous_Finishes,
             "Selection Status" = SelectionStatus,
             "Odds (%)" = odds_percent)
    
    datatable(grouped, 
              options = list(
                dom = "tip", 
                pageLength = 20,
                # Add styling to make font smaller
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'font-size': '12px'});",
                  "$(this.api().table().body()).css({'font-size': '11px'});",
                  "}"
                )
              ), 
              rownames = FALSE)
  })
  
  output$oddsM <- renderDT({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    grouped <- df %>%
      group_by(tickets, Previous_Applications, Previous_Finishes, SelectionStatus, Display_Odds) %>%
      summarise(N = n(), .groups = "drop") %>%
      mutate(
        tickets_char = formatC(round(tickets,2), format="f", digits=2),
        odds_percent = formatC(round(100 * Display_Odds,2), format="f", digits=2)
      ) %>%
      arrange(as.numeric(tickets_char)) %>%
      select("Tickets" = tickets_char, "N" = N,
             "Previous Apps" = Previous_Applications,
             "Previous Fins" = Previous_Finishes,
             "Selection Status" = SelectionStatus,
             "Odds (%)" = odds_percent)
    
    datatable(grouped, 
              options = list(
                dom = "tip", 
                pageLength = 20,
                # Add styling to make font smaller
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'font-size': '12px'});",
                  "$(this.api().table().body()).css({'font-size': '11px'});",
                  "}"
                )
              ), 
              rownames = FALSE)
  })
  
  
  # --- Expected picks by previous applications (bar) ---
  output$histW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(Previous_Applications) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(Previous_Applications), y = ExpectedPicks)) +
      geom_col(fill = "#B7822E") +
      labs(x = "Previous Applications", y = "Expected # Picked") +
      theme_minimal()
  })
  
  output$histM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(Previous_Applications) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(Previous_Applications), y = ExpectedPicks)) +
      geom_col(fill = "#2270AE") +
      labs(x = "Previous Applications", y = "Expected # Picked") +
      theme_minimal()
  })
  
  
  # --- Expected picks by previous finishes (bar) ---
  output$finHistW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(Previous_Finishes) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(Previous_Finishes), y = ExpectedPicks)) +
      geom_col(fill = "#E1B882") +
      labs(x = "Previous Finishes", y = "Expected # Picked") +
      theme_minimal()
  })
  
  output$finHistM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    df$Display_Odds <- df[[display_odds_col()]]
    df <- df %>% group_by(Previous_Finishes) %>% summarise(ExpectedPicks = sum(Display_Odds, na.rm = TRUE))
    ggplot(df, aes(x = factor(Previous_Finishes), y = ExpectedPicks)) +
      geom_col(fill = "#9EC6F5") +
      labs(x = "Previous Finishes", y = "Expected # Picked") +
      theme_minimal()
  })
  
  output$oddsDistW <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Female")
    ggplot() +
      geom_jitter(data = df, aes(x = tickets, y = Odds_Exact, color = "Direct"), width = 0.1, height = 0) +
      geom_jitter(data = df, aes(x = tickets, y = Odds_MC, color = "Monte Carlo"), width = 0.1, height = 0) +
      scale_color_manual(values = c("Direct" = "#2270AE", "Monte Carlo" = "#B7822E")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", color = "Method", title = "Women: Ticket vs Odds (Both Methods)") +
      theme_minimal()
  })
  
  output$oddsDistM <- renderPlot({
    df <- assigned_data() %>% filter(Gender == "Male")
    ggplot() +
      geom_jitter(data = df, aes(x = tickets, y = Odds_Exact, color = "Direct"), width = 0.1, height = 0) +
      geom_jitter(data = df, aes(x = tickets, y = Odds_MC, color = "Monte Carlo"), width = 0.1, height = 0) +
      scale_color_manual(values = c("Direct" = "#2270AE", "Monte Carlo" = "#B7822E")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Tickets", y = "Odds", color = "Method", title = "Men: Ticket vs Odds (Both Methods)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
