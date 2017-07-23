#source("global.R")
#source("./Documents/QCRI/ClassMat2/ClassMat/ActigraphFeatureExtractor.R")
library(lubridate)
source("ActigraphFeatureExtractor.R")
source("utilities.R")

min_eq <- function(date_time)
{
  time <- ymd_hms(date_time)
  min_passed <- hour(time)*60 + minute(time)
  min_passed
}

segment_data_at_time<- function(sleep_data,hr=0,min=0,sec=0){
  
  sleep_data$rep_segment_code <- rep(1,nrow(sleep_data))
  
  for(i in 1:nrow(d))
  {
    break_time <- sleep_data[i,]$start_time
    hour(break_time) <- hr
    minute(break_time) <- min
    second(break_time) <- sec
    if(sleep_data[i,]$start_time<break_time && sleep_data[i,]$end_time>break_time)
    {
      new_df <- sleep_data[i,]
      new_df$start_time <- break_time
      
      sleep_data[i,]$end_time <- break_time - 1
      sleep_data <- insertDF(sleep_data,i+1,new_df)
      sleep_data[(i+1):nrow(sleep_data),]$rep_segment_code <- (sleep_data[i,]$rep_segment_code+1)
    }
  }
  sleep_data
}

load_detail_data_for_actigraph <- function(base_dir = "./input/QUEST",is_labelled = TRUE)
{ 
  files_list <- list.files(base_dir,pattern = "*.csv")
  sleep_data <- lapply(files_list,function(file_name){ 
    getDetailDataForUser(sub(".csv","",file_name),file.path(base_dir,file_name))
  })
  
  #remove NULL
  sleep_data[sapply(sleep_data,is.null)]<-NULL
  
  tuned_sleep_data <- lapply(sleep_data, function(per_user_sleep_data){
    per_user_sleep_data <- per_user_sleep_data %>% mutate(duration=as.numeric(end_time-start_time,"mins"),start_time_min= min_eq(start_time),end_time_min= min_eq(end_time))
    per_user_sleep_data
    #per_user_group_sleep_data <- per_user_sleep_data %>% group_by(segment_code,activity_level) %>% summarise(sum_dur=sum(duration))
  })
  tuned_sleep_data
}

load_sleep_data <- function(base_dir = "./input/QUEST",is_labelled = TRUE)
{ 
  print(base_dir)
  files_list <- list.files(base_dir,pattern = "*.csv")
  
  # get list of user points
  sleep_data <- lapply(files_list,function(file_name){ 
    getSummarySleepDataRepForUser(sub(".csv","",file_name),file.path(base_dir,file_name))})
  
  sleep_data_df <- cleanData(sleep_data)
  if(is_labelled)
    sleep_data_df <- categorizeData(sleep_data_df)
  sleep_data_df
  #dataStdzdf<-standardizeData(sleep_data[,-1])
  #dataStdz<-as.matrix(dataStdzdf[,1:datadim])
  
}
cleanData <- function(data_list){
  #remove NULL
  data_list[sapply(data_list,is.null)]<-NULL
  
  #convert ot data frame
  t <- as.data.frame(matrix(unlist(data_list),ncol=length(data_list[[1]]),byrow = TRUE),stringsAsFactors=FALSE)
  names(t) <- names(data_list[[1]])
  options(digits=15,scipen = 100)
  t$avg_mid_time = as.numeric(t$avg_mid_time)
  t$shift_mid_time = as.numeric(t$shift_mid_time)
  t$avg_sleep_duration = as.numeric(t$avg_sleep_duration)
  t$shift_sleep_duration = as.numeric(t$shift_sleep_duration)
  t
}
categorizeData <- function(dataf)#,method='kmeans')
{
  #remove unnecessary variables from the observations
  t <- kmeans(dataf[,-c(1)],centers = 1, nstart = 20)
  dataf$label <- t$cluster
  dataf
}


#sleepdatadf <- getSleepData("./Documents/QCRI/ClassMat2/ClassMat/input/QUEST")#getSleepData("input/QUEST")
#labeled_sleepdatadf <- categorizeData(sleepdatadf)

#print(sleepdatadf)
#t <- as.data.frame(matrix(unlist(sleepdatadf),ncol=length(sleepdatadf[[1]]),byrow = TRUE),col.names = c( "id","avg_mid_time","shift_mid_time","avg_sleep_duration","shift_sleep_duration"))
#t <- rbind.data.frame(as.matrix(unlist(sleepdatadf)),stringsAsFactors = FALSE)
#names(t) <- names(sleepdatadf[[1]])
#print(t)
#print(unlist(sleepdatadf))

#t <- sleepdatadf[,-which(names(sleepdatadf) %in% c("id"))]
#print(t)
#categorizeData(sleepdatadf)

