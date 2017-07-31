library(gridExtra)
library(ggplot2)
library(MASS)

library('RColorBrewer')
library('reshape2')

source('processData.R')
source("global.R")

#coloursvec <- brewer.pal(12, "Paired")

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
    dataLDA
}

getPCAdata <- function(data.df){
  dataPCA <- data.df
  if(data.df[,!(names(data.df)  %in% c('id','label'))]>2)
  {
    labels <- data.df[,c('label')]
    dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
    dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
    dataPCA <- pcaClassSPLOM(dataStdz)
    dataPCA$label <- labels
  }
  dataPCA
}

getLDAPlotlist <- function(counter, data.df, brushedlines, clickedline)
{
  p <- list()
  uniq_labels <- sort(unique(data.df[,c('label')]))
  if(length(uniq_labels)>1)
  {
    comb_labels <- combn(uniq_labels,2)
    
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
  
  return(p)
}

# Returns LIST of LDA plotly objects
getplotlylist <- function(counter, data.df,viewOrder, brushedlines = NULL, clickedline=NULL)
{
  p <- list()
  #uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  if(!is.null(viewOrder) & length(viewOrder)>1)
  {
    comb_labels <- viewOrder
    
    #project data in 2D using PCA
    dataLDA <- data.df
    #coloursvec <- brewer.pal(12, "Paired")
    #palette(coloursvec)
    
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
getmiscplotlist <- function(data.df,classDispOrder ,noOfMiscClasses ,brushedlines = NULL, clickedline=NULL)
{
  nonMiscClasses <- head(classDispOrder,-noOfMiscClasses)
  miscClasses <- tail(classDispOrder,noOfMiscClasses)
  
  data.df[data.df$label %in% miscClasses,c('label')] <- min(miscClasses) #assign aggregate the label of the lowest one
  noOfCombn <- length(nonMiscClasses)  #number of non-misc class plus aggregate of misc classes
  comb_labels = matrix(nrow=2,ncol=noOfCombn)
  comb_labels[1,] <- nonMiscClasses
  comb_labels[2,] <- min(miscClasses)
  
  modified_coloursvec = coloursvec
  modified_coloursvec[min(miscClasses)] = "#000000"
  
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
    doPlot(counter, dataLDA, comb_labels[1, i], comb_labels[2, i],  brushedlines, clickedline, modified_coloursvec)
  })
  d <- getdiagplot(counter,data.df,min(miscClasses),modified_coloursvec)
  p[[length(p)+1]] <- d
  return(p)
}

getdiagplot <- function(counter,data.df,inFocusLabel,modified_coloursvec=coloursvec){
  
  dataPCA <- data.df
  if(data.df[,!(names(data.df)  %in% c('id','label'))]>2)
  {
    labels <- data.df[,c('label')]
    dataStdzdf<-standardizeData(data.df[,!(names(data.df)  %in% c('label','id'))])
    dataStdz<-as.matrix(dataStdzdf[,1:length(dataStdzdf)])
    dataPCA <- pcaClassSPLOM(dataStdz)
    dataPCA$label <- labels
  }
  doPlot(counter, dataPCA, inFocusLabel, inFocusLabel,  NULL, NULL, modified_coloursvec)
}


# Returns LIST of LDA and PCA plot objects for timeline's snapshots
gettimelineplotlist <- function(counter, data.df, brushedlines, clickedline)
{
  p <- list()
  uniq_labels <- sort(unique(data.df[,c('label')]))
  if(length(uniq_labels)>1)
  {
    comb_labels <- combn(uniq_labels,2)
    
    #coloursvec <- brewer.pal(12, "Paired")
    #palette(coloursvec)
    
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

getdiaglist <- function(counter, data.df,modified_coloursvec=coloursvec)
{
  uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  #coloursvec <- brewer.pal(12, "Paired")
  #palette(coloursvec)
  
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
  
  p <- lapply(1:length(uniq_labels), function(i) doPlot(counter, dataPCA, uniq_labels[i], uniq_labels[i],  NULL, NULL, modified_coloursvec))
  
  return(p)
}

# Returns diagonal list of plots
# Replace doPlot with LDA generating function
getdiaglylist <- function(counter=1, data.df,uniq_labels=NULL)
{
  if(is.null(uniq_labels))
    uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  #coloursvec <- brewer.pal(12, "Paired")
  
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

getActigraphBarReps <- function(actigraph_data)
{
   actigraphBarReps <- lapply(actigraph_data,function(per_user_actigraph_data){
     getActigraphTimelineRep(per_user_actigraph_data,12,0,0)
     #ggplot(per_user_actigraph_data,aes(x=segment_code,y=sum_dur)) + geom_col(aes(fill=activity_level))
   })
}

getplotlylistfromplots <- function(plot_list){
  plotlylist <- lapply(plot_list,function(plot){
    getPlotlyFromPlot(plot)
  })
  plotlylist
}