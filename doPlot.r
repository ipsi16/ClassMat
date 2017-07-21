library(dplyr)
library(lazyeval)
library(plotly)
library(scales)


# big plot for selection
bigplot = function(data.df){
  coloursvec <- brewer.pal(8, "Dark2")
  g <- ggplot(data.df, aes(x= x, y= y)) + theme_bw() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())
  p <- ggplotly(g, width = 80, height =80) %>% config(displayModeBar = FALSE, scrollZoom = TRUE, doubleClick= 'reset') %>% layout(dragmode = NULL)
  
  return(p)
}
  

# doplot function to generate scatterplots
doPlotly = function(counter, data.df, label1, label2, brushedlines, clickedline, coloursvec){

  
  selected_classes <- c(label1 , label2)
  
  formatCol <- 'label'
  formula <- interp(~ifelse(label %in% selected_classes , ifelse( label == selected_classes[1] , "b", "c"), "a"), label = as.name(formatCol))
  plot.df <- data.df %>% mutate_(formatCol = formula)
  #print(plot.df)
  #print(plot.df %>% arrange(formatCol))
  
  
  
  if(!(label1 == label2))
  {
    g <- ggplot(plot.df  %>%
                arrange(formatCol) , aes(x= x, y= y)) + theme_bw() +
    geom_point(aes(color = formatCol),size=0.5) + 
    scale_colour_manual(values= c("b" = coloursvec[as.integer(label1)], "c" = coloursvec[as.integer(label2)], "a" = "#E4E4E4")) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())

  
  }
  else
  {
  g <- ggplot(plot.df  %>%
                arrange(formatCol) , aes(x= x, y= y)) + theme_light() +
    geom_point(aes(color = formatCol),size=0.5) + 
    scale_colour_manual(values= c("b" = coloursvec[as.integer(label1)], "c" = coloursvec[as.integer(label2)], "a" = "#E4E4E4")) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank(),
          panel.border = element_rect(colour = "black", size=3)) 
  
  }
  # # # if brush is used highlight brushed points
    if (!is.null(brushedlines)){
      brushedlines <- brushedlines %>% mutate_(formatCol = formula)
      print(brushedlines)
      g <- g  + 
        geom_point(aes(x= x, y= y),
                   data= brushedlines,
                   color = "black")
      
    }
      
     # # if hover when brushedlines = 0 highlight hovered points
     # if (!is.null(clickedline)){
     #   g <- g  + 
     #     geom_point(aes(x= x, y= y, alpha = 1),
     #                data= clickedline,
     #                colour="blue",
     #                size=3)  
     # }
  
  p <- ggplotly(g,height=100,width = 100) %>% config(displayModeBar = FALSE, scrollZoom = TRUE, doubleClick= 'reset') %>% layout(dragmode ="lasso") 
  
                                  #   to ADD Toolbar                     
                                  #   ,autosizable = T, fillFrame = T, showLink = FALSE, displaylogo = FALSE, scrollZoom = TRUE, doubleClick= 'reset',
                                  #   modeBarButtonsToRemove = list(
                                  #   'sendDataToCloud',
                                  #   'toImage',
                                  #   'autoScale2d',
                                  #   'resetScale2d',
                                  #   'hoverClosestCartesian',
                                  #   'hoverCompareCartesian'
                                  # )) 
  attr(p,'data') <- g$data
  return(p)
}

doPlot = function(counter, data.df, label1, label2, brushedlines, clickedline, coloursvec){
  
  
  selected_classes <- c(label1 , label2)
  
  formatCol <- 'label'
  formula <- interp(~ifelse(label %in% selected_classes , ifelse( label == selected_classes[1] , "b", "c"), "a"), label = as.name(formatCol))
  plot.df <- data.df %>% mutate_(formatCol = formula)
 
  if(!(label1 == label2)){
    g <- ggplot(plot.df  %>%
                  arrange(formatCol) , aes(x= x, y= y)) + theme_bw() +
      geom_point(aes(color = formatCol),size=0.5) + 
      scale_colour_manual(values= c("b" = coloursvec[as.integer(label1)], "c" = coloursvec[as.integer(label2)], "a" = "#E4E4E4")) +
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())
    
    
  }
  else
  {
    g <- ggplot(plot.df  %>%
                  arrange(formatCol) , aes(x= x, y= y)) + theme_light() +
      geom_point(aes(color = formatCol),size=0.5) + 
      scale_colour_manual(values= c("b" = coloursvec[as.integer(label1)], "c" = coloursvec[as.integer(label2)], "a" = "#E4E4E4")) +
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank(),
            panel.border = element_rect(colour = "black", size=3)) 
    
  }
  # # # if brush is used highlight brushed points
  if (!is.null(brushedlines)){
    brushedlines <- brushedlines %>% mutate_(formatCol = formula)
    print(brushedlines)
    g <- g  + 
      geom_point(aes(x= x, y= y),
                 data= brushedlines,
                 color = "black")
    
  }
  
  return(g)
}


getActigraphTimelineRep <- function(user_activity_df,break_time_hr,break_time_min,break_time_sec){
 
  user_activity_df$activity_level <- as.factor(user_activity_df$activity_level)
   
  coloursvec <- brewer.pal(6, "Dark2")
  time_of_ref <- user_activity_df[1,]$start_time
  hour(time_of_ref) <- 0
  minute(time_of_ref) <- 0
  second(time_of_ref) <- 0
  print(time_of_ref)
  
  Sys.setenv(TZ='UTC')
  
  l <- length(unique(user_activity_df$segment_code))
  x_lim_start <- time_of_ref+hours((l-1)*24)
  hour(x_lim_start) <- break_time_hr
  minute(x_lim_start) <- break_time_min
  second(x_lim_start) <- break_time_sec
  
  x_limits <- c(x_lim_start,(x_lim_start+86400))
  print(x_limits)
  y_limits <- c((time_of_ref-86400),(time_of_ref+(l+1)*86400))
  
  g <<- ggplot(data=user_activity_df, aes(x=user_activity_df$start_time,y=user_activity_df$start_time)) + scale_x_datetime(name = "",labels=date_format("%H:%M"),breaks=date_breaks("4 hour"),minor_breaks = date_breaks("2 hour"),limits = x_limits)+ scale_y_datetime(name = "",labels=date_format("%d-%m-%Y"))+theme(legend.position="none")
  
  
  
  for(i in unique(user_activity_df$segment_code))
  {
    shift <- (i-1)*86400
    
    x_min_time= user_activity_df$start_time+shift
    x_max_time= user_activity_df$end_time+shift
    y_min_time= time_of_ref+shift-43200
    y_max_time= time_of_ref+shift+43200
    g <<- g + geom_rect(data=data.frame(x_min_time,x_max_time,y_min_time,y_max_time),aes(xmin=x_min_time,xmax=x_max_time,ymin=y_min_time,ymax=y_max_time,fill=coloursvec[user_activity_df$activity_level]),color="black")#ymin=y_min_time, ymax=y_max_time))
  }
  g
}





 