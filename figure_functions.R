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

map.straw <- function(df, title.straw){
  library(plotly)
  states = read_csv("csvData.csv") 
  states$State = tolower(states$State) 
  df$State = tolower(df$State) 
  library(plyr)
  df.lb2 = ddply(df,c("State", "Year"),numcolwise(sum)) #I sum the amount of pesticide by year for each state

  df_map = left_join(df.lb2, states, by = "State") %>%
    select(Year, State, Code, Value)%>%
    mutate(hover = paste0(State, "\n", Value))
  
  
  map_straw = plot_geo(df_map, 
                           locationmode = "USA-states", 
                           frame = ~Year) %>%
    add_trace(locations = ~Code,
              z = ~Value,#colored by z
              color = ~Value,
              colorscale = "Electric",
              text = ~hover,
              hoverinfo = "text")%>%
    layout(geo = list(scope = 'usa'),
           title = title.straw,
           font = list(family = "DM Sans"))
  map_straw    
}


# #Distribution Plots
# #Here I choose "Measurements = number" as the things we care about.
# 
# #Numbers & toxicity_bee.
library(sm)
bee_tox <- function(){
  bee.level<-factor(df.num$toxicity_bee,levels = c(1,2,3), labels = c("1 Slight","2 Moderate","3 High"))
  # plot densities
  sm.density.compare(df.num$Value, df.num$toxicity_bee, xlab="Number of Applications")
  title(main="Number of applications grouped by bee toxicity")
 # add legend via mouse click
  colfill_1<-c(2:(2+length(levels(bee.level))))
  legend("topright", levels(bee.level),fill=colfill_1)
  
}
# 
# #Numbers & carcinogen
carc_apps <- function(){
 carcinogen.level<-factor(df.num$carcinogen,levels = c(1,2,3), labels = c("1 Possible","2 Probable","3 Known"))
 # plot densities
 x <- sm.density.compare(df.num$Value, df.num$carcinogen, xlab="Measured in Number")
 title(main="Numbers of carcinogen applications")
 # add legend via mouse click
 colfill_2<-c(2:(2+length(levels(carcinogen.level))))
 legend("topright", levels(carcinogen.level),fill = colfill_2)
 return(x)
}
 
# #Grouped Barchart
library(ggplot2)
library(plotly)
group_bars <- function(){
 p <- ggplot(df.num,aes(color=as.factor(toxicity_bee), y=Value, x=carcinogen)) + 
   xlab("Number of Applications")+
   scale_color_discrete(name = "Level of Bee Toxicity")+
   geom_jitter(width = 0.1)
 ggplotly(p)
  
# # All strawberries measured in number with toxicity_bee exist with slight level of carcinogen. 
# # There does not exist strawberries measured in number with toxicity_bee in the 2 and 3 level of carcinogen.
}

#Plot of the proportions of strawberry measured in number for each state.
library(plotly)
fig <- plot_ly() 
state_fig<-function(){
  fig <- fig %>%
    add_trace(
      type = "pie",
      name = "",
      values = c(.78, .20, .02),
      labels = c("California", "Florida", "Washington"),
      text = c("Proportion in number", "Proportion in number", "Proportion in number"),
      hovertemplate = "%{label}: <br>Popularity: %{percent} </br> %{text}")
  fig
}
