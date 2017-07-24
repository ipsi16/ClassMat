library(MASS)

source("MNISTFeatureExtractor.R")
source("SleepDataTransformer.R")

get_default_data <-function(datasource_folder=NULL)
{
  # to access input data 
  # inFile <- input$chosenfile
  # if (is.null(chosenfile))
  #   return(NULL)
  # read.csv(chosenfile$datapath, header = input$header)

Sigma <- diag(1,3)
Sigma
n1=20;
n2=20;
n3=20;
n4=20;
n5=20;
data1<-mvrnorm(n = n1, c(0,0,0), Sigma)
data2<-mvrnorm(n = n2, c(0,5,0), Sigma)
data3<-mvrnorm(n = n3, c(10,10,0), Sigma)
data4<-mvrnorm(n = n4, c(10,10,0), Sigma)
#data5<-mvrnorm(n = n5, c(10,10,0), Sigma)

data.df<-data.frame(#index=seq(from=1, to=(n1+n2+n3+n4),by=1),
                    x=c(data1[,1],data2[,1],data3[,1], data4[,1]),
                    y=c(data1[,2],data2[,2],data3[,2], data4[,2]),
                    z=c(data1[,3],data2[,3],data3[,3], data4[,3]),
                    label=c(rep(1,n1),rep(2,n2),rep(3,n3),rep(4,n4)))
#data.df$label<-factor(data.df$label)


return(data.df)

}

getMNISTData <- function(datasource_folder=NULL)
{
  #get N-D data
  data.df <- load_mnist()
  
  
  return(data.df)
}

getSleepData <- function(datasource_folder='./input/QUEST')
{
  data.df <- load_sleep_data(base_dir = datasource_folder)
  return(data.df)
}

getActigraphRepData <- function(datasource_folder=NULL)
{
  if(is.null(datasource_folder))
    load_detail_data_for_actigraph()
  else
    load_detail_data_for_actigraph(datasource_folder)
}

inputTypeToFunction <- c("default"=get_default_data,"mnist"=getMNISTData,"sleepData"=getSleepData )
getdata <- function(datatype,datasource_folder=NULL)
{
  if(is.null(datasource_folder))
    inputTypeToFunction[[datatype]]()
  else
    inputTypeToFunction[[datatype]](datasource_folder)
  
}
inputTypeToRepFunction <- c("sleepData"=getActigraphRepData )
getRepData <- function(datatype,datasource_folder=NULL){
  if(is.null(datasource_folder))
    inputTypeToRepFunction[[datatype]]()
  else
    inputTypeToRepFunction[[datatype]](datasource_folder)
}