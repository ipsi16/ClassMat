library(gridExtra)
library(ggplot2)
library(MASS)

library('RColorBrewer')
library('reshape2')

source('processData.R')

getLDAdata <- function(data.df,l1,l2){
  
  uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  comb_labels <- combn(uniq_labels,2)
  
  dataLDA <- data.df
    #project N-D data on LDA / PCA
    if(length(data.df[,!(names(data.df)  %in% 'label')])>2)
    {
      labels <- data.df[,c('label')]
      dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
      dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
      dataLDA<-ldaClassSPLOM(dataStdz,labels,l1,l2)
      
    }
}

# Returns LIST of LDA plotly objects
getplotlylist <- function(counter, data.df, brushedlines, clickedline)
{
  p <- list()
  uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  if(length(uniq_labels)>1)
  {
    comb_labels <- combn(uniq_labels,2)
    
    #project data in 2D using PCA
    dataLDA <- data.df
    #project N-D data on LDA / PCA
    
    
    # Set palette:
    # NAME; No.OF COLORS
    # Accent 8
    # Dark2 8
    # RColorBrewer 3
    # Paired 12
    # Pastel1 9
    # Pastel2 8
    # Set1 9
    # Set2 8
    # Set3 12
    coloursvec <- brewer.pal(8, "Dark2")
    palette(coloursvec)
    
    # Replace doPlot with function that returns plots given 2 labels
    p <- lapply(1:ncol(comb_labels), function(i)
    {
      l1 <- comb_labels[1,i]
      l2 <- comb_labels[2,i]
      
      dataLDA <- data.df
      #project N-D data on LDA / PCA
      if(length(data.df[,!(names(data.df)  %in% 'label')])>2)
      {
        labels <- data.df[,c('label')]
        dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
        dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
        dataLDA<-ldaClassSPLOM(dataStdz,labels,l1,l2)
        
      }
      doPlotly(counter, dataLDA, comb_labels[1, i], comb_labels[2, i],  brushedlines, clickedline, coloursvec)
    })
  }
  
  
  return(p)
}

# Returns LIST of LDA plot objects
getplotlist <- function(counter, data.df, brushedlines, clickedline)
{
  p <- list()
  uniq_labels <- sort(unique(data.df[,c('label')]))
  if(length(uniq_labels)>1)
  {
    comb_labels <- combn(uniq_labels,2)
    
    # Set palette:
    # NAME; No.OF COLORS
    # Accent 8
    # Dark2 8
    # RColorBrewer 3
    # Paired 12
    # Pastel1 9
    # Pastel2 8
    # Set1 9
    # Set2 8
    # Set3 12
    
    coloursvec <- brewer.pal(8, "Dark2")
    palette(coloursvec)
    
    # Replace doPlot with function that returns plots given 2 labels
    p <- lapply(1:ncol(comb_labels), function(i)
    {
      l1 <- comb_labels[1,i]
      l2 <- comb_labels[2,i]
      
      dataLDA <- data.df
      #project N-D data on LDA / PCA
      if(length(data.df[,!(names(data.df)  %in% 'label')])>2)
      {
        labels <- data.df[,c('label')]
        dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
        dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
        dataLDA<-ldaClassSPLOM(dataStdz,labels,l1,l2)
        
      }
      
      
      doPlot(counter, dataLDA, comb_labels[1, i], comb_labels[2, i],  brushedlines, clickedline, coloursvec)
    })
  }
  t <- getdiaglist(counter,data.df)
  return(append(p,t))
}
getdiaglist <- function(counter, data.df)
{
  uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  coloursvec <- brewer.pal(8, "Dark2")
  palette(coloursvec)
  
  #dataPCA <- data.df[,!(names(data.df)  %in% c('id','label'))]
  dataPCA <- data.df
  if(data.df[,!(names(data.df)  %in% c('id','label'))]>2)
  {
    labels <- data.df[,c('label')]
    print(data.df)
    dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
    print(dataStdzdf)
    dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
    dataPCA <- pcaClassSPLOM(dataStdz)
    dataPCA$label <- labels
  }
  
  p <- lapply(1:length(uniq_labels), function(i) doPlot(counter, dataPCA, uniq_labels[i], uniq_labels[i],  NULL, NULL, coloursvec))
  
  return(p)
}

# Returns diagonal list of plots
# Replace doPlot with LDA generating function
getdiaglylist <- function(counter, data.df)
{
  
  uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  coloursvec <- brewer.pal(8, "Dark2")
  palette(coloursvec)
  
  #dataPCA <- data.df[,!(names(data.df)  %in% c('id','label'))]
  dataPCA <- data.df
  if(data.df[,!(names(data.df)  %in% c('id','label'))]>2)
  {
    labels <- data.df[,c('label')]
    print(data.df)
    dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
    print(dataStdzdf)
    dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
    dataPCA <- pcaClassSPLOM(dataStdz)
    dataPCA$label <- labels
  }
  
  p <- lapply(1:length(uniq_labels), function(i) doPlotly(counter, dataPCA, uniq_labels[i], uniq_labels[i],  NULL, NULL, coloursvec))
  
  return(p)
  
}

 # getauclist Returns trustworthiness plot list
 getauclist <-  function(data.df)
 {
   
   #print(data.df)
   datalabels<-data.df$label
   
   databrutdf <- data.df
   datadim <- ncol(data.df) -1
   databrutdf2D<-data.frame(x=databrutdf[,1],y=databrutdf[,2],label=factor(datalabels))
   ggplot()+geom_point(data=databrutdf2D,aes(x=x,y=y,color=label))
   
   
   nc<-  length(unique(data.df[,ncol(data.df)])) # number of classes
   ndata<-nrow(data.df) # number of data
   data<-as.matrix(data.df[,1:datadim]) # raw data matrix without labels
   
   
   uniq_labels <- unique(datalabels)
   comb_labels <- combn(uniq_labels,2)
   #print(comb_labels)
   colnames(data.df)<-c(1:datadim,"labels")
   dataStdzdf<-data.df #standardizeData(data.df)
   dataStdz<-as.matrix(dataStdzdf[,1:datadim])
   
   #counter <- 1
   #coloursvec <- brewer.pal(12, "Paired")
   #p <- lapply(1:ncol(comb_labels), function(i) doPlot(counter, data.df,comb_labels[1, i], comb_labels[2, i],  NULL, NULL, coloursvec))
   p <- lapply(1:ncol(comb_labels), function(i) LDA_ROC( dataStdz[which(datalabels== comb_labels[1, i]),] , dataStdz[which(datalabels== comb_labels[2, i]),] ,nboot=2))
#  
   return(p)
 }
 
 getActigraphBarReps <- function(actigraph_data)
 {
   actigraphBarReps <- lapply(actigraph_data,function(per_user_actigraph_data){
     getActigraphTimelineRep(per_user_actigraph_data,12,0,0)
     #ggplot(per_user_actigraph_data,aes(x=segment_code,y=sum_dur)) + geom_col(aes(fill=activity_level))
   })
 }