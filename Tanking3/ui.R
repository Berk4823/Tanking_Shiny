library(markdown)
library(dplyr)
library(ggplot2)
library(data.table)
# loading packages

# I used the standard template for Shiny Dashboard

dashboardPage(skin = "green",
  dashboardHeader(title = "Tanking?"),
  dashboardSidebar(
    sidebarUserPanel("Michael Berkowitz", image = "https://images-na.ssl-images-amazon.com/images/I/51m%2BFFxEtAL._SY355_.jpg"),
    sidebarMenu(id = "menu1",
      menuItem("Lottery",
        menuSubItem("Lottery Rules", tabName = "dashboard", icon = icon("dashboard")),
        menuSubItem("Lottery Simulator", tabName = "lotterysim", icon = icon("play")),
        menuSubItem("Lottery Probabilities", tabName = "lotteryprob", icon = icon("th-large"))
      ),
      
      menuItem("Player Data", tabName = "player", icon = )
    ),
    
    conditionalPanel(
      condition = "input.menu1 == 'lotteryprob'",
      sliderInput("VarTeam", 
                  label = "Select A Team", 
                  min = 1,
                  max = 14,
                  value = 1, 
                  step = 1)
    ),
    
    conditionalPanel(
      condition = "input.menu1 == 'lotterysim'",
      h5("Simulate the Lottery"),
      actionButton("action", label = "Press Me!")
    ),
    
    conditionalPanel(
      condition = "input.menu1 == 'player'",
      selectInput("player", 
                  label = "Select A Stat", 
                  choices = c("Number of Championships", "Win Shares", "All Star"),
                  selected = "Number of Championships", multiple = F)
    )
    # there are three conditional panels above. These will only appear when certain conditions are met. I did this 
    # in order to reduce the clutter and only keep widgets visible when they are needed. 
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      
    ),
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          tabBox(width = 12, height = "250px", id = "tabset5",
            tabPanel("Current Lottery Rules",
              tags$ul(id = "Rules1",
                tags$li("Weighted probability assigned to every team."), 
                tags$li("First 3 picks are determined by random selection without replacement."), 
                tags$li("Picks 4-14 are given by reverse order of standings to the teams remaining."),
                tags$li("Link to full rules:", tags$a(href= "https://www.draftsite.com/nba/rules/", "Click here!"))
              )
              # text on the main page
            ),
            
            tabPanel("Proposed Lottery Rules",
              tags$ul(id = "Rules2",
                tags$li("Split the non-playoff teams into two halves of 7, Bottom 7 and Top 7"), 
                tags$li("Each team in the Bottom 7 has an equal chance at the 1st Pick. (1/7 chance)"),
                tags$li("Repeat this process for the 2nd and 3rd Pick with the remaining 6 teams (1/6, 1/5 chance respectively)"),
                tags$li("Remaining picks 4-7 are given by reverse order of standings."),
                tags$li("Repeat this process for Picks 8-14 with the Top 7 teams.")
              )
              # text on the main page
            )
          ),
          
          tabBox(width = 12, height = "250px", id = "tabset3",
            tabPanel("Current Lottery Issues",
              tags$ul(id = "Issues", 
                tags$li(" Bottom teams are incentivized to tank to improve the roster."), 
                tags$li("No strong alternative to the lottery for these teams improve."), 
                tags$li("Once mathematically eliminated, borderline playoff teams", br(), "also have incentive to tank.")
              )
              # text on the main page
            ),
            
            tabPanel("Proposed Fixes",
              tags$ul(id = "Fixes1", 
                tags$li("Decrease the bottom few teams' chance of winning the top picks."), 
                  tags$ul(id = "Fixes2",
                    tags$li("This should help dis-incentivize bottom teams from tanking.")
                  )
                # text on the main page
              ),
              
              tags$ul(id = "Fixes3", 
                tags$li("Increase the borderline-playoff teams' chance of moving up in the draft."),
                  tags$ul(id = "Fixes4",
                    tags$li("This should incentivize borderline teams to finish out the season strong.")
                  )
              )
              # text on the main page
            )
          )
        )
      ),
      
      tabItem(tabName = "lotterysim",
        fluidRow(
          infoBoxOutput("winner_current"),
          infoBoxOutput("winner_proposed")
          # info box widget displaying the team that won each lottery. 
        ),
        
        fluidRow(
          valueBoxOutput("current_percent"), 
          valueBoxOutput("proposed_percent")
          # value box widget displaying the % chance for the winning team for each lottery. 
        ),
        
        fluidRow(
          box(width = 6, background = "olive", collapsible = T,
            height = "auto",
            h3(id = "tabseta", "Current Lottery System"), 
            tableOutput("value_current_results")
            # box displaying the full lottery results for the current system. 
          ),
          
          box(width = 6,
            height = "auto", background = "blue", collapsible = T,
            h3(id = "tabsetb", "Proposed Lottery System"),
            tableOutput("value_proposed_results")
            # box displaying the full lottery results for the proposed system. 
          )
        )
      ),
      
      tabItem(tabName = "player",
        fluidRow(
          box(width = 5,
            title = "Player Data",
            height = "auto",
            conditionalPanel(
              condition = "input.player == 'Win Shares'",
              selectInput("type_stat", 
                          label = "Select A Stat", 
                          choices = c("mean", "median", "min", "max"),
                          selected = "mean", multiple = F)
            ),
            # selectInput widget to choose a stat for player data, inside of a conditional panel
            
            conditionalPanel(
              condition = "input.player == 'Number of Championships'",
              selectInput("type_stat2", 
                          label = "Select A Stat", 
                          choices = c("mean", "max"),
                          selected = "mean", multiple = F)
            )
          ),
          # selectInput widget to choose a stat for championship data, inside of a conditional panel
          box(width = 12, 
              height = "auto",
              plotOutput("playerplot")
          )
          # output that returns a different plot depending on the choice made in the above widgets
        )
      ),
      
      tabItem(tabName = "lotteryprob",
        fluidRow(
          tabBox(width = 12, id = "tabset7", title = uiOutput("team_title"), 
                 tabPanel("Probability Distribution",
                 plotlyOutput("probability")),
                 tabPanel("Probability Differential",
                 plotOutput("probability2")
                 )
          ),
          # box showing the probability distribution in one tab, and probability differential in another tab. 
          
          box(width = 12,
              height = "auto",
              tabBox(width = 12, id = "tabset6",
                     tabPanel("Current Lottery Probabilities", dataTableOutput("CLTable")),
                     tabPanel("Proposed Lottery Probabilities", dataTableOutput("PLTable"))),
              collapsible = T, collapsed = T
          )
          # box showing the probability tables for each lottery. 
        )
      )
    )
  )
)