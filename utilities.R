getPlotCardinal <- function(uniq_label,class1, class2){
  n <- length(uniq_label)
  noLowerTriangle <- (n^2-n)/2
  k <-  1:noLowerTriangle
  orderMatrix <- matrix(nrow = n, ncol = n)
  orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
  orderMatrix[max(class1,class2),min(class1,class2)]
  
}

getDiagPlotDataPoints<- function(diag_plotly_list, class){
  attr(diag_plotly_list[[class]],'data')
}

getDataPoints <- function(plotly_list, uniq_label,class1,class2){
  n <- getPlotCardinal(uniq_label,class1,class2)
  attr(plotly_list[[n]],'data')
}


insertDF <- function(data_frame,pos,new_df){
  data_frame[(pos+nrow(new_df)):(nrow(data_frame)+nrow(new_df)),]<- data_frame[pos:nrow(data_frame),]
  data_frame[pos:(pos+nrow(new_df)-1),]<- new_df
  data_frame
}