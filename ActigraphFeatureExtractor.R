library(tidyverse)
#Loads csv files, represents each user based on statistical model for his sleep pattern

#t <-  "/Users/IpsitaPrakash/Documents/QCRI/ClassMat2/ClassMat/input/QUEST-BC_44-2015.csv"
#file_name<-file.path("./Documents/QCRI/ClassMat2/ClassMat/input/QUEST","QUEST-BC_44-2015.csv")
getSummarySleepDataRepForUser <- function(id,file_name)
{
  actigraph_data = read_csv(file_name,col_types = 
                              cols(start_time=col_datetime(),
                                   end_time=col_datetime()))
  sleep_data <- actigraph_data %>% filter(activity_level == "sleep") %>% mutate(duration = as.numeric(end_time - start_time,"hours"), mid_sleep_time = start_time + duration/2)
  if(nrow(sleep_data) %in% c(0,1) )
      NULL
  else
  {
    
    avg_mid_time <- mean(sleep_data$mid_sleep_time)
    shift_mid_time <- sd(sleep_data$mid_sleep_time) #should it be the shift in start times or mid times
    avg_sleep_duration <- mean(sleep_data$duration)
    shift_sleep_duration <- sd(sleep_data$duration) 
    
    c(id=id,avg_mid_time=avg_mid_time,shift_mid_time=shift_mid_time,avg_sleep_duration=avg_sleep_duration,shift_sleep_duration=shift_sleep_duration) 
  }
}

#getSleepDataRepForUser("QUEST-BC_103-2015","./Documents/QCRI/ClassMat2/ClassMat/input/QUEST/QUEST-BC_148-2015.csv")

