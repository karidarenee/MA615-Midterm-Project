pacman::p_load(ggplot2, magrittr, dplyr)

bar_plt <- function(x, y) {
  x_val <- x
  unit <-  y
  
  straw_select <- strawberry[strawberry["Measurement(s)"] == as.character(unit),]
  straw_select %<>% 
    #Step 2
    group_by_at(.vars = c(x_val)) %>% 
    #Step 3
    summarise(mean = mean(Value,na.rm=TRUE), sd = sd(Value))%>% 
    as_tibble()
  
  straw_select$min_err <- ifelse((straw_select$mean-straw_select$sd<0),0,straw_select$mean-straw_select$sd)
  straw_select$max_err <- straw_select$mean+straw_select$sd
  
  
  ggplot(straw_select) +
    geom_col(aes(x = unlist(straw_select[x_val]), y = unlist(mean)), fill = "#D55E00")+
    geom_errorbar(aes(x =unlist(straw_select[x_val]), ymin=min_err, ymax=max_err), width=0,
                  position=position_dodge(.9))+ 
    xlab(input$x) +
    ylab(paste(input$y))+
    labs(title = "Mean of Selected Variables with St.Dev Errors")+
    scale_y_continuous(limits = c(0, NA))
}

map.straw <- function(df){
  library(plotly)
  states = read_csv("csvData.csv") 
  states$State = tolower(states$State) 
  df$State = tolower(df$State) 
  library(plyr)
  df.lb2 = ddply(df,c("State", "Year"),numcolwise(sum)) #I sum the amount of pesticide by year for each state

  df_map = left_join(df.lb2, states, by = "State") %>%
    select(Year, State, Code, Value)%>%
    mutate(hover = paste0(State, "\n", Value))
  
  
  minwage_graph = plot_geo(df_map, 
                           locationmode = "USA-states", 
                           frame = ~Year) %>%
    add_trace(locations = ~Code,
              z = ~Value,#colored by z
              color = ~Value,
              colorscale = "Electric",
              text = ~hover,
              hoverinfo = "text")%>%
    layout(geo = list(scope = 'usa'))
  minwage_graph   
}


