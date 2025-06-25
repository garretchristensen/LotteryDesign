library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(shinyjs)
library(DT)

# Load data
temp <- read.csv("./2025HLdata.csv", stringsAsFactors = FALSE)
df <- as_tibble(temp)
df$fullname <- paste(df$First_Name, df$Last_Name, sep = " ", collapse = NULL)

# Prepare gender column as in your working code
df$Gender <- ifelse(df$Lottery.Pool == "F", "Female", "Male")

# Calculate k, v, t, and tickets as in the working code
df$k <- ifelse(df$Previous_Finishes==0 , 0,
               ifelse(df$Previous_Finishes==1,  0.5,
                      ifelse(df$Previous_Finishes==2, 1, 
                             ifelse(df$Previous_Finishes==3, 1.5,
                                    ifelse(df$Previous_Finishes>=4, 0.5, 0)))))
df$v <- pmin(df$Volunteer_Shifts, 30)
df$t <- pmin(df$Extra_Trailwork, 10)
df$tickets <- 2^(df$k + df$Previous_Applications + 1) + 2*log(df$v + df$t + 1)

# Gender splits
women <- df[df$Gender == "Female", ]
men <- df[df$Gender == "Male", ]

n_women_pick <- 103
n_men_pick <- 95

# ---- WOMEN ODDS ----
w_applicants <- pull((women %>% count(tickets))[,2], n)
w_tickets_per_applicant <- sort(women$tickets[!duplicated(women$tickets)])
w_original_tickets <- w_applicants * w_tickets_per_applicant
w_ticket_counts <- w_original_tickets

for (i in 1:n_women_pick) {
  w_prob_of_selecting_category <- w_ticket_counts / sum(w_ticket_counts)
  w_exp_ticket_reduction <- w_prob_of_selecting_category * w_tickets_per_applicant
  w_ticket_counts <- w_ticket_counts - w_exp_ticket_reduction
}
w_tickets_taken <- w_original_tickets - w_ticket_counts
w_odds_of_selection <- w_tickets_taken / w_original_tickets
w_num_people_taken <- w_odds_of_selection * w_applicants
w_odds <- data.frame(
  tickets_per_applicant = w_tickets_per_applicant,
  odds_of_selection = w_odds_of_selection,
  applicants = w_applicants,
  num_people_taken = w_num_people_taken
)

# ---- MEN ODDS ----
m_applicants <- pull((men %>% count(tickets))[,2], n)
m_tickets_per_applicant <- sort(men$tickets[!duplicated(men$tickets)])
m_original_tickets <- m_applicants * m_tickets_per_applicant
m_ticket_counts <- m_original_tickets

for (i in 1:n_men_pick) {
  m_prob_of_selecting_category <- m_ticket_counts / sum(m_ticket_counts)
  m_exp_ticket_reduction <- m_prob_of_selecting_category * m_tickets_per_applicant
  m_ticket_counts <- m_ticket_counts - m_exp_ticket_reduction
}
m_tickets_taken <- m_original_tickets - m_ticket_counts
m_odds_of_selection <- m_tickets_taken / m_original_tickets
m_num_people_taken <- m_odds_of_selection * m_applicants
m_odds <- data.frame(
  tickets_per_applicant = m_tickets_per_applicant,
  odds_of_selection = m_odds_of_selection,
  applicants = m_applicants,
  num_people_taken = m_num_people_taken
)

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
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
  # This is how you display the image:
  tags$div(
    class = "intro-logo",
    tags$img(src = "logo.png", alt = "Freestone Logo"), # Place logo.png in www/
    tags$div(class = "intro-title", "Lottery Design Choices")
  ),
  bsCollapse(id = "collapse", multiple=TRUE,
             bsCollapsePanel("What are my odds?",
                             "These are the odds for women with the given number of tickets:",
                             DT::dataTableOutput("oddsW"),
                             "These are the odds for men with the given number of tickets:",
                             DT::dataTableOutput("oddsM"),
                             style = "info"
             )
             # Add other panels as needed...
  )
)

server <- function(input, output, session) {
  output$oddsW <- DT::renderDataTable({
    datatable(w_odds) %>%
      formatPercentage('odds_of_selection', digits = 2) %>%
      formatRound(c('tickets_per_applicant', 'num_people_taken'), digits = 2)
  })
  output$oddsM <- DT::renderDataTable({
    datatable(m_odds) %>%
      formatPercentage('odds_of_selection', digits = 2) %>%
      formatRound(c('tickets_per_applicant', 'num_people_taken'), digits = 2)
  })
}

shinyApp(ui = ui, server = server)