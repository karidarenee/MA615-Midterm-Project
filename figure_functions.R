pacman::p_load(ggplot2, magrittr, dplyr, plotly)
source("wrangling.R")
#Subsets by different measurement
strawberry$`Measurement(s)` = str_trim(strawberry$`Measurement(s)`)
df.lb = subset(strawberry, `Measurement(s)` == "MEASURED IN LB")
df.app = subset(strawberry, `Measurement(s)` == "MEASURED IN LB / ACRE / APPLICATION")
df.year = subset(strawberry, `Measurement(s)` == "MEASURED IN LB / ACRE / YEAR")
df.num = subset(strawberry, `Measurement(s)` == "MEASURED IN NUMBER")
df.PCT = subset(strawberry, `Measurement(s)` == "MEASURED IN PCT OF AREA BEARING")

#Calculate proportions of strawberries by 'Measurements=Number'
df.num<-tibble::as_tibble(df.num)

#Count frequency/proportion for each state
df.num %>%
  group_by(State) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#Calculate proportions of values(numbers) grouped by 'toxicity_bee'.
df.value<-tibble::as_tibble(df.num$Value)
df.toxicity_bee<-tibble::as_tibble(df.num$toxicity_bee)
df.toxicity<-cbind(df.value,df.toxicity_bee)

#Rename column 2 into 'toxicity_bee'
names(df.toxicity)[2]<- 'toxicity_bee'
df.toxicity %>%
  group_by(toxicity_bee) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

options(warn = oldw)

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
  library(plyr)
  
  states = read_csv("csvData.csv") 
  states$State = tolower(states$State) 
  df$State = tolower(df$State) 
  
  
  df.lb2 = ddply(df,c("State", "Year"),numcolwise(sum)) #I sum the amount of pesticide by year for each state
  df_map = left_join(df.lb2, states, by = "State") %>%
    select(Year, State, Code, Value)%>%
    mutate(hover = paste0(State, "\n", Value))
  
  
  map_straw = plot_geo(df_map, 
                           locationmode = "USA-states", 
                           frame = ~Year) %>%
    add_trace(locations = ~Code,
              z = ~Value, #colored by z
              color = ~Value,
              colorscale = "Viridis",
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
  sm.density.compare(df.num$Value, df.num$toxicity_bee, xlab="Measured in Number")
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

group_bars <- function(){
  
  m = list(
    l = 100,
    r = 250,
    b = 100,
    t = 100,
    pad = 0
  )
  
 p <- ggplot(df.num,aes(color=as.factor(toxicity_bee), y=Value, x=carcinogen)) + 
   xlab("Carcinogen Level")+
   ylab("Average Number of Applications") + 
   scale_color_discrete(name = "Bee Toxicity")+
   geom_jitter(width = 0.1)
   
 ggplotly(p,width = 1100, height = 600)%>% layout(margin = m)
  
# # All strawberries measured in number with toxicity_bee exist with slight level of carcinogen. 
# # There does not exist strawberries measured in number with toxicity_bee in the 2 and 3 level of carcinogen.
}

#Plot of the proportions of strawberry measured in number for each state.

fig <- plot_ly() 
state_fig<-function(){
  m = list(
    l = 100,
    r = 100,
    b = 100,
    t = 100,
    pad = 0
  )
  fig <- fig %>%
    add_trace(
      type = "pie",
      name = "",
      values = c(.78, .20, .02),
      labels = c("California", "Florida", "Washington"),
      text = c("Proportion of Dataset", "Proportion of Dataset", "Proportion of Dataset"),
      hovertemplate = "%{label}: <br>Popularity: %{percent} </br> %{text}") %>% 
    layout(margin = m)
  fig
}



shiny_bar <- function(x_val){
  unit <- "STRAWBERRIES - YIELD, MEASURED IN CWT / ACRE"
  straw_select <- strawb_yield[strawb_yield["Data.Item"] == unit,]
  straw_select %<>% 
    #Step 2
    dplyr::group_by_at(.vars = c(x_val)) %>% 
    #Step 3
    dplyr::summarise(mean = mean(Value,na.rm=TRUE), sd = sd(Value), count = dplyr::n())%>% 
    as_tibble()
  
  straw_select$min_err <- ifelse((straw_select$mean-straw_select$sd<0),0,straw_select$mean-straw_select$sd)
  straw_select$max_err <- straw_select$mean+straw_select$sd
  
  
  ggplot(straw_select) +
    geom_col(aes(x = unlist(straw_select[x_val]), y = unlist(mean)), fill = "#00a65a")+
    geom_errorbar(aes(x =unlist(straw_select[x_val]), ymin=min_err, ymax=max_err), width=0,
                  position=position_dodge(.9)) +
    xlab(x_val) +
    ylab("YIELD, MEASURED IN CWT / ACRE")+
    labs(title = "Mean of Strawberry Yield with St.Dev Errors and Count")+
    geom_text(aes(x=unlist(straw_select[x_val]),
                  y= rep(-15,dim(straw_select)[1]), 
                  label=unlist(count)), size=6, color = "#74776B") + 
    scale_y_continuous(limits = c(-20, NA)) +
    theme(text=element_text(size=7), #change font size of all text
          axis.text=element_text(size=10), #change font size of axis text
          axis.title=element_text(size=10), #change font size of axis titles
          plot.title=element_text(size=15) #change font size of plot title
    )
}