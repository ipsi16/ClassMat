library(gridExtra)
library(ggplot2)
library(MASS)

library('RColorBrewer')
library('reshape2')

source('processData.R')


coloursvec <- brewer.pal(12, "Paired")
palette(coloursvec)

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
getplotlylist <- function(counter, data.df,viewOrder, brushedlines = NULL, clickedline=NULL)
{
  p <- list()
  #uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  if(!is.null(viewOrder) & length(viewOrder)>1)
  {
    comb_labels <- viewOrder
    
    #project data in 2D using PCA
    dataLDA <- data.df
    coloursvec <- brewer.pal(12, "Paired")
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
getmiscplotlylist <- function(data.df,classDispOrder ,noOfMiscClasses ,brushedlines = NULL, clickedline=NULL)
{
  print("noOfMiscClasses")
  print(noOfMiscClasses)
  nonMiscClasses <- head(classDispOrder,-noOfMiscClasses)
  print(nonMiscClasses)
  miscClasses <- tail(classDispOrder,noOfMiscClasses)
  print(miscClasses)
  
  data.df[data.df$label %in% miscClasses,c('label')] <- min(miscClasses) #assign aggregate the label of the lowest one
  noOfCombn <- length(nonMiscClasses)  #number of non-misc class plus aggregate of misc classes
  comb_labels = matrix(nrow=2,ncol=noOfCombn)
  comb_labels[1,] <- nonMiscClasses
  comb_labels[2,] <- min(miscClasses)
  
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
  d <- getdiaglylist(counter,data.df,min(miscClasses)) 
  p <- append(p,d)
  
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
    # Paired 8
    # RColorBrewer 3
    # Paired 12
    # Pastel1 9
    # Pastel2 8
    # Set1 9
    # Set2 8
    # Paired 12
    
    coloursvec <- brewer.pal(12, "Paired")
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
  coloursvec <- brewer.pal(12, "Paired")
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
getdiaglylist <- function(counter=1, data.df,uniq_labels=NULL)
{
  if(is.null(uniq_labels))
    uniq_labels <- sort(unique(data.df[,ncol(data.df)]))
  coloursvec <- brewer.pal(12, "Paired")
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

getActigraphBarReps <- function(actigraph_data)
{
   actigraphBarReps <- lapply(actigraph_data,function(per_user_actigraph_data){
     getActigraphTimelineRep(per_user_actigraph_data,12,0,0)
     #ggplot(per_user_actigraph_data,aes(x=segment_code,y=sum_dur)) + geom_col(aes(fill=activity_level))
   })
}