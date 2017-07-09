#source("global.R")
#source("./Documents/QCRI/ClassMat2/ClassMat/ActigraphFeatureExtractor.R")
source("ActigraphFeatureExtractor.R")

load_sleep_data <- function(base_dir = "./input/QUEST",is_labelled = TRUE)
{ 
  files_list <- list.files(base_dir,pattern = "*.csv")
  
  # get list of user points
  sleep_data <- lapply(files_list,function(file_name){ 
    getSummarySleepDataRepForUser(sub(".csv","",file_name),file.path(base_dir,file_name))})
  
  sleep_data_df <- cleanData(sleep_data)
  if(is_labelled)
    sleep_data_df <- categorizeData(sleep_data_df)
  print("Struct of sleep data ------")
  print(str(sleep_data_df))
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
    print("T===========")
  print(t)
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
  t <- kmeans(dataf[,-c(1)],centers = 6, nstart = 20)
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

