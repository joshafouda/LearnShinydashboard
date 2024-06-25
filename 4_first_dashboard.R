set.seed(123) # For reproducibility

# Load necessary library
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(shinydashboard)

# Simulating the dataframe
soccer <- tibble(
  match_no = 1:64,
  day_of_week = sample(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 64, replace = TRUE),
  date = format(seq.Date(from = as.Date("2022-11-20"), by = "days", length.out = 64), "%d-%b-%y"),
  hour = hms::as_hms(runif(64, 0, 86400)),
  venue = sample(c("Al Bayt Stadium", "Khalifa International Stadium", "Al Thumama Stadium", "Ahmed bin Ali Stadium"), 64, replace = TRUE),
  referee = sample(c("Daniele Orsato", "Raphael Claus", "Wilton Sampaio", "Abdulrahman Ibrahim Al Jassim"), 64, replace = TRUE),
  group = sample(c("Group A", "Group B", "Group C", "Group D", "Group E", "Group F", "Group G", "Group H"), 64, replace = TRUE),
  `1` = sample(c("QATAR", "ENGLAND", "SENEGAL", "UNITED STATES", "NETHERLANDS", "WALES"), 64, replace = TRUE),
  `2` = sample(c("ECUADOR", "IRAN", "NETHERLANDS", "WALES", "FRANCE", "SPAIN"), 64, replace = TRUE),
  attendance = sample(20000:90000, 64, replace = TRUE),
  `1_xg` = runif(64, 0, 2.5),
  `2_xg` = runif(64, 0, 2.5),
  `1_poss` = sample(30:70, 64, replace = TRUE),
  `2_poss` = 100 - `1_poss`,
  `1_goals` = sample(0:4, 64, replace = TRUE),
  `2_goals` = sample(0:4, 64, replace = TRUE),
  score = paste0(`1_goals`, ",", `2_goals`),
  `1_attempts` = sample(5:25, 64, replace = TRUE),
  `2_attempts` = sample(5:25, 64, replace = TRUE),
  `1_conceded` = `2_goals`,
  `2_conceded` = `1_goals`,
  `1_goal_inside_penalty_area` = sample(0:4, 64, replace = TRUE),
  `2_goal_inside_penalty_area` = sample(0:4, 64, replace = TRUE),
  `1_goal_outside_penalty_area` = rep(0, 64),
  `2_goal_outside_penalty_area` = rep(0, 64),
  `1_ontarget` = sample(0:9, 64, replace = TRUE),
  `2_ontarget` = sample(0:9, 64, replace = TRUE),
  `1_offtarget` = sample(0:9, 64, replace = TRUE),
  `2_offtarget` = sample(0:9, 64, replace = TRUE),
  `1_attempts_inside_penalty_area` = sample(0:14, 64, replace = TRUE),
  `2_attempts_inside_penalty_area` = sample(0:14, 64, replace = TRUE),
  `1_attempts_outside_penalty_area` = sample(0:9, 64, replace = TRUE),
  `2_attempts_outside_penalty_area` = sample(0:9, 64, replace = TRUE),
  `1_yellow_cards` = sample(0:4, 64, replace = TRUE),
  `2_yellow_cards` = sample(0:4, 64, replace = TRUE),
  `1_red_cards` = rep(0, 64),
  `2_red_cards` = rep(0, 64),
  faul_against_1 = sample(5:20, 64, replace = TRUE),
  faul_against_2 = sample(5:20, 64, replace = TRUE),
  `1_offsides` = sample(0:9, 64, replace = TRUE),
  `2_offsides` = sample(0:9, 64, replace = TRUE),
  `1_passes` = sample(200:800, 64, replace = TRUE),
  `2_passes` = sample(200:800, 64, replace = TRUE),
  `1_passes_compeletd` = sample(150:750, 64, replace = TRUE),
  `2_passes_compeletd` = sample(150:750, 64, replace = TRUE),
  `1_corners` = sample(0:9, 64, replace = TRUE),
  `2_corners` = sample(0:9, 64, replace = TRUE),
  `1_free_kicks` = sample(5:25, 64, replace = TRUE),
  `2_free_kicks` = sample(5:25, 64, replace = TRUE),
  `1_panelties_scored` = sample(0:1, 64, replace = TRUE),
  `2_panelties_scored` = sample(0:1, 64, replace = TRUE),
  `1_goal_prevented` = sample(0:19, 64, replace = TRUE),
  `2_goal_prevented` = sample(0:19, 64, replace = TRUE),
  `1_own_goal` = rep(0, 64),
  `2_own_goal` = rep(0, 64),
  `1_forced_turnovers` = sample(20:100, 64, replace = TRUE),
  `2_forced_turnovers` = sample(20:100, 64, replace = TRUE),
  `1_defensive_pressure_applied` = sample(100:500, 64, replace = TRUE),
  `2_defensive_pressure_applied` = sample(100:500, 64, replace = TRUE)
)

#View(soccer)

# Function to get the day & time for a particular match
daytime_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    mutate(datetime = paste(date, hour)) %>%
    pull(datetime)
}

# Function to get the venue for a particular match
venue_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    pull(venue)
}

# Function to get the group of the matchup
grp_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    pull(group)
}

# Function to get the name of team 1 for a particular match
team1_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    pull(`1`)
}

# Function to get the name of team 2 for a particular match
team2_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    pull(`2`)
}

# Function to get the number of goals scored by team 1 for a particular match
score1_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    pull(`1_goals`)
}

# Function to get the number of goals scored by team 2 for a particular match
score2_fn <- function(match){
  soccer %>%
    filter(match_no == match) %>%
    pull(`2_goals`)
}

# Function to plot histogram of attendance with a specific match highlighted
plot_histogram <- function(match){
  match_df <- soccer %>% filter(match_no == match)
  ggplot(soccer, aes(attendance)) + 
    geom_histogram(bins = 10, fill = 'lime green', col = 'white') + 
    geom_vline(xintercept = match_df$attendance, col = 'dark orange', linetype = 'dashed') +
    geom_text(aes(x = match_df$attendance, y = 20, label = paste("Attendance for match", match)), col = 'dark orange', vjust = -1) +
    labs(title = 'Distribution of attendance numbers.', x = 'Attendance', y = 'Frequency') + 
    theme_classic()
}

################################## Plots #########################################

# Summarize the total goals for each team
total_goals <- soccer %>%
  select(`1`, `2`, `1_goals`, `2_goals`) %>%
  rename(team1 = `1`, team2 = `2`, goals1 = `1_goals`, goals2 = `2_goals`) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_type", values_to = "team") %>%
  pivot_longer(cols = c(goals1, goals2), names_to = "goal_type", values_to = "goals") %>%
  filter((team_type == "team1" & goal_type == "goals1") | (team_type == "team2" & goal_type == "goals2")) %>%
  group_by(team) %>%
  summarize(total_goals = sum(goals)) %>%
  arrange(desc(total_goals))

# Create the plot
goals_plot <- ggplot(total_goals, aes(x = reorder(team, total_goals), y = total_goals)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(title = "Total Number of Goals by Country", x = "Country", y = "Total Number of Goals") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

# Summarize the total red cards for each team
red_cards <- soccer %>%
  select(`1`, `2`, `1_red_cards`, `2_red_cards`) %>%
  rename(team1 = `1`, team2 = `2`, red1 = `1_red_cards`, red2 = `2_red_cards`) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_type", values_to = "team") %>%
  pivot_longer(cols = c(red1, red2), names_to = "card_type", values_to = "red_cards") %>%
  filter((team_type == "team1" & card_type == "red1") | (team_type == "team2" & card_type == "red2")) %>%
  group_by(team) %>%
  summarize(total_red_cards = sum(red_cards)) %>%
  arrange(desc(total_red_cards))

# Create the red cards plot
red_plot <- ggplot(red_cards, aes(x = reorder(team, total_red_cards), y = total_red_cards)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(title = "Total Number of Red Cards by Country", x = "Country", y = "Total Number of Red Cards") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))


# Summarize the total yellow cards for each team
yellow_cards <- soccer %>%
  select(`1`, `2`, `1_yellow_cards`, `2_yellow_cards`) %>%
  rename(team1 = `1`, team2 = `2`, yellow1 = `1_yellow_cards`, yellow2 = `2_yellow_cards`) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_type", values_to = "team") %>%
  pivot_longer(cols = c(yellow1, yellow2), names_to = "card_type", values_to = "yellow_cards") %>%
  filter((team_type == "team1" & card_type == "yellow1") | (team_type == "team2" & card_type == "yellow2")) %>%
  group_by(team) %>%
  summarize(total_yellow_cards = sum(yellow_cards)) %>%
  arrange(desc(total_yellow_cards))

# Create the yellow cards plot
yellow_plot <- ggplot(yellow_cards, aes(x = reorder(team, total_yellow_cards), y = total_yellow_cards)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(title = "Total Number of Yellow Cards by Country", x = "Country", y = "Total Number of Yellow Cards") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))


##############################################################################33

header <- dashboardHeader(title = "Soccer tournament")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Matches", tabName = "matchtab", icon = icon("futbol")), 
    menuItem("General statistics", tabName = "statstab", icon = icon("chart-line")) 
  )
)


body <- dashboardBody(tabItems(
  tabItem(tabName = "matchtab", 
          fluidRow(selectInput("match", "Match number", choices = 1:nrow(soccer)),
                   infoBoxOutput("daytime"), infoBoxOutput("venue"), infoBoxOutput("grp")),
          fluidRow(valueBoxOutput("team1", width = 3), valueBoxOutput("score1", width = 3), 
                   valueBoxOutput("team2", width = 3), valueBoxOutput("score2", width = 3)),
          fluidRow(plotOutput("histogram"))),
  tabItem(tabName = "statstab", 
          tabBox(tabPanel("Goals", plotOutput("goals", height = "700px")),
                 tabPanel("Yellow cards", plotOutput("yellow", height = "700px")),
                 tabPanel("Red cards", plotOutput("red"))))
))

ui <- dashboardPage(skin = "green", header, sidebar, body)

server <- function(input, output) {
  # Fill in outputs in first to third rows of the first page
  output$daytime <- renderInfoBox(infoBox("Day and time", 
                                          daytime_fn(input$match), 
                                          icon = icon("calendar"), 
                                          color = "green"))
  output$venue <- renderInfoBox(infoBox("Venue", 
                                        venue_fn(input$match), 
                                        icon = icon("map"), 
                                        color = "green"))
  output$grp <- renderInfoBox(infoBox("Group", 
                                      grp_fn(input$match), 
                                      color = "green"))
  output$team1 <- renderValueBox(valueBox("Team 1", team1_fn(input$match), color = "blue"))
  output$score1 <- renderValueBox(valueBox("# of goals", score1_fn(input$match), color = "blue"))
  output$team2 <- renderValueBox(valueBox("Team 2", team2_fn(input$match), color = "red"))
  output$score2 <- renderValueBox(valueBox("# of goals", score2_fn(input$match), color = "red"))
  output$histogram <- renderPlot(plot_histogram(input$match))
  # Fill in outputs in the second page
  output$goals <- renderPlot(goals_plot)
  output$yellow <- renderPlot(yellow_plot)
  output$red <- renderPlot(red_plot)
}

# Put the UI and server together
shinyApp(ui, server)
