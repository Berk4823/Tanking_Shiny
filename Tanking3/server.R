library(markdown)
library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
# loading packages

function(input, output){
  
  current_lottery_res = reactive({
    if(input$action >= 0){
      current_lottery = data.frame(Results = Simulate_NBA_current())
    }
    return(current_lottery)
  })
  # this reactive function is simulating the NBA lottery under the current rules. Simulate_NBA_current is a function
  # defined in the global file, and it simulates the lottery.
  
  proposed_lottery_res = reactive({
    if(input$action >= 0){
      proposed_lottery = data.frame(Results = Simulate_NBA_proposed())
    }
    return(proposed_lottery)
  })
  # this reactive function is simulating the NBA lottery under the proposed rules. Simulate_NBA_proposed is also defined 
  # in the global file. 
  
  output$value_current_results = renderTable({ 
    current_lottery_res()
  })
  # output corresponding to "value_current_results" on ui file-- the full lottery results under the current system. 
  
  output$value_proposed_results = renderTable({ 
    proposed_lottery_res() 
  })
  # output corresponding to "value_proposed_results" on ui file-- the full lottery results under the proposed system. 
  
  output$winner_current = renderInfoBox({
    infoBox(
      "Winner!!", current_lottery_res()[1, ],
      icon = icon("shield"), color = "olive", fill = T
    )
  })
  # output corresponding to "winner_current" on the ui file-- The winner of the current lottery. The function current_lottery_res()
  # returns a data frame with the lottery results, [1, ] is taking only the first row. 
  
  output$winner_proposed = renderInfoBox({
    infoBox(
      "Winner!!", proposed_lottery_res()[1, ],
      icon = icon("shield"), color = "blue", fill = T
    )
  })
  # output corresponding to "winner_proposed" on the ui file-- The winner of the proposed lottery. 
  
  output$current_percent = renderValueBox({
    infoBox(
      "Chance to Win", paste0(NBAProb[1, 
                                      nba_switch(
                                        as.character(
                                          current_lottery_res()[1, ]
                                          )
                                        )
                                      ]*100, "%"),
      icon = icon("calculator"), color = "olive", fill = T
    )
  })
  # output corresponding to "current_percent" on the ui file-- The % chance to win under the current rules. nba_switch is a function 
  # defined in the global file. It is a switch function that replaces a team name with a number. In the above code, the result 
  # from the simulation is turned into a number that is then plugged into NBAProb, a data frame with the probabilities 
  # for the lottery. 
  
  output$proposed_percent = renderValueBox({
    infoBox(
      "Chance to Win", paste0(14.3, "%"),
      icon = icon("calculator"), color = "blue", fill = T
    )
  })
  # output corresponding to "proposed_percent" on the ui file-- The % chance to win under the proposed rules. Based on the rules, 
  # this chance is always 14.3%
  
  output$probability = renderPlotly({
    q = paste0(input$VarTeam, ".x", collapse = "")
    v = paste0(input$VarTeam, ".y", collapse = "")
    wawa2 = plot_ly(NBAProb3, 
                    x = ~Pk, 
                    y = ~get(q), 
                    type = "bar", 
                    name = "Current") %>% 
      add_trace(y = ~get(v),
                name = "Proposed") %>% 
      layout(xaxis = list(title = "Pick"), 
             yaxis = list(title = "Probability")) 
    wawa2
  })
  # ouput corresponding to "probability" on the ui file. It shows the probability distribution by team
  
  output$probability2 = renderPlot({
    q = paste0("Team", input$VarTeam, collapse = "")
    z = nba_prob %>% select(get(q))
    z = ifelse(z > 0, "Increase", "Decrease")
    wawa = ggplot(data = nba_prob, aes(x = Pk, y = get(q))) + geom_col(aes(fill = z)) + 
      xlab("Pick") +
      ylab("Probability Differential") + 
      theme(axis.title = element_text(size=16)) + 
      theme(axis.text = element_text(size = 13)) +
      theme(legend.title=element_blank()) + 
      theme(legend.text = element_text(size = 11))
    wawa
  })
  # output corresponding to "probability2" on the ui file. It shows the probability differential between the two systems. 
  
  output$playerplot = renderPlot({
    if(input$player == "Number of Championships"){
      Champ_by_pick = NBA_final %>% 
        group_by(Pk) %>% 
        summarise(Total_Champ = get(input$type_stat2)(Number_Champ))
        # input$type_stat2 from ui are all statistics, like mean, median, etc. So for example, get(input$type_stat2)
        # could be mean, yielding mean(Number_Champ), 
      
      return(ggplot(data = Champ_by_pick, aes(x = Pk, y = Total_Champ)) + geom_col(fill = "seagreen4") +
               xlab("Pick") + 
               ylab(input$type_stat2) + 
               ggtitle(input$player) + 
               theme(axis.title = element_text(size=18)) + 
               theme(plot.title = element_text(size = 22, hjust = 0.5)) + 
               theme(axis.text = element_text(size = 13)))
    }
    # if the user chooses "Number of Championships, this is the plot that is returned. 
    
    if(input$player == "Win Shares"){
      NBA_final_wsgraph = NBA_final %>% 
        group_by(Pk) %>% 
        summarise(Avg_WS = get(input$type_stat)(WS_48)) %>% 
        # see above explanation under the "Number of Championships"
        arrange (Pk)
      
      NBAGRAPH_WS = ggplot(data = NBA_final_wsgraph, aes(x = Pk, y = Avg_WS)) + geom_point() +
        geom_smooth(method = "lm") + 
        xlab("Pick") + 
        ylab(input$type_stat) + 
        ggtitle(input$player) +
        theme(axis.title = element_text(size=18)) + 
        theme(plot.title = element_text(size = 22, hjust = 0.5)) +
        theme(axis.text = element_text(size = 13))
      return(NBAGRAPH_WS)
    }
    # if the user chooses "Win Shares", this is the plot that is returned
    
    if(input$player == "All Star"){
      return(ggplot(data = n1, aes(x = Pk, y = Proportion_AllStar)) + geom_line(aes(color = Is_AllStar)) + 
               xlab("Pick") + 
               ylab("Proportion") + 
               ggtitle("All Star Proportion") + 
               theme(axis.title = element_text(size=18)) + 
               theme(plot.title = element_text(size = 22, hjust = 0.5)) + 
               theme(axis.text = element_text(size = 13)) + 
               theme(legend.title=element_blank()))
    # if the user chooses "All Star", this is the plot that is returned. 
    }
  })
  # output corresponding to "playerplot" on the ui file. The multiple if conditions are determining which plot is 
  # rendered based on the user input. 
  
  output$CLTable = renderDataTable({
    return(NBAProb)
  })
  # output corresponding to "CLTable" on the ui file. It shows the probability table for the current lottery. 
  output$PLTable = renderDataTable({
    return(NBAProb2)
  })
  # output corresponding to "PLTable" on the ui file. It shows the probability table for the proposed lottery. 
  output$team_title = renderUI({
    z = as.character(input$VarTeam)
    return(nba_switch2(z))
  })
  # output corresponding to "team_title" on the ui file. It shows the team title based on user input. nba_switch2 
  # is a function defined in the global file-- it is a switch function that takes a number and returns the name 
  # of the team.
} 