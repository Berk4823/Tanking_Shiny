library(markdown)
library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)

function(input, output) {
  current_lottery_res <- reactive({
    if(input$action >= 0){
      current_lottery = data.frame(Results = Simulate_NBA())
    }
    return(current_lottery)
  })
  proposed_lottery_res <- reactive({
    if(input$action >= 0){
      proposed_lottery = data.frame(Results = Simulate_NBA2())
    }
    return(proposed_lottery)
  })
  output$value = renderTable({ 
    current_lottery_res()
  })
  output$value2 = renderTable({ 
    proposed_lottery_res() 
  })
  output$test1 <- renderInfoBox({
    infoBox(
      "Winner!!", current_lottery_res()[1, ],
      icon = icon("shield"), color = "olive", fill = T
    )
  })
  output$test2 <- renderInfoBox({
    infoBox(
      "Winner!!", proposed_lottery_res()[1, ],
      icon = icon("shield"), color = "blue", fill = T
    )
  })
  output$test3 <- renderValueBox({
    infoBox(
      "Chance to Win", paste0(NBAProb[1, nba_switch(as.character(current_lottery_res()[1, ]))]*100, "%"),
      icon = icon("calculator"), color = "olive", fill = T
    )
  })
  output$test4 <- renderValueBox({
    infoBox(
      "Chance to Win", paste0(14.3, "%"),
      icon = icon("calculator"), color = "blue", fill = T
    )
  })
  output$probability = renderPlotly({
    q = paste0(input$VarTeam, ".x", collapse = "")
    v = paste0(input$VarTeam, ".y", collapse = "")
    wawa2 = plot_ly(NBAProb3, x = ~Pk, y = ~get(q), type = "bar", name = "Current") %>% add_trace(y = ~get(v), name = "Proposed") %>% layout(xaxis = list(title = "Pick"), yaxis = list(title = "Probability")) 
    wawa2
  })
  output$probability2 = renderPlot({
    q = paste0("Team", input$VarTeam, collapse = "")
    z = nba_prob %>% select(get(q))
    z = ifelse(z > 0, "Increase", "Decrease")
    wawa = ggplot(data = nba_prob, aes(x = Pk, y = get(q))) + geom_col(aes(fill = z)) + xlab("Pick") +
      ylab("Probability Differential") + theme(axis.title = element_text(size=16)) + theme(axis.text = element_text(size = 13)) +
      theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 11))
    wawa
  })
  output$playerplot = renderPlot({
    if(input$player == "Number of Championships"){
      Champ_by_pick = NBA_final %>% group_by(Pk) %>% summarise(Total_Champ = get(input$type_stat2)(Number_Champ))
      return(ggplot(data = Champ_by_pick, aes(x = Pk, y = Total_Champ)) + geom_col(fill = "seagreen4") + xlab("Pick") +
        ylab(input$type_stat2) + ggtitle(input$player) + theme(axis.title = element_text(size=18)) + theme(plot.title = element_text(size = 22, hjust = 0.5)) +
        theme(axis.text = element_text(size = 13)))
    }
    if(input$player == "Win Shares"){
      NBA_final_wsgraph = NBA_final %>% group_by(Pk) %>% summarise(Avg_WS = get(input$type_stat)(WS_48)) %>% 
        arrange (Pk)
      NBAGRAPH_WS = ggplot(data = NBA_final_wsgraph, aes(x = Pk, y = Avg_WS)) + geom_point() +
        geom_smooth(method = "lm") + xlab("Pick") + ylab(input$type_stat) + ggtitle(input$player) +
        theme(axis.title = element_text(size=18)) + theme(plot.title = element_text(size = 22, hjust = 0.5)) +
        theme(axis.text = element_text(size = 13))
      return(NBAGRAPH_WS)
    }
    if(input$player == "All Star"){
      return(ggplot(data = n1, aes(x = Pk, y = Proportion_AllStar)) + geom_line(aes(color = Is_AllStar)) +
        xlab("Pick") + ylab("Proportion") + ggtitle("All Star Proportion") +
        theme(axis.title = element_text(size=18)) + theme(plot.title = element_text(size = 22, hjust = 0.5)) +
        theme(axis.text = element_text(size = 13)) + theme(legend.title=element_blank()))
    }
  })
  output$CLTable = renderDataTable({
    return(NBAProb)
  })
  output$PLTable = renderDataTable({
    return(NBAProb2)
  })
  output$team_title = renderUI({
    z = as.character(input$VarTeam)
    return(nba_switch2(z))
  })
} 