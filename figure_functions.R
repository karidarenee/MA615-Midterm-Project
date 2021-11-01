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

