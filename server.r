library(ggplot2)
library(gridExtra)
library(ggplot2)
library(MASS)
library(shiny)
library(shinyjs)
library(MASS)
library(RColorBrewer)
library(grid)
library(plotly)
library(shinyBS)

source("getdata.R")
source("processData.R")
source("plist.R")
source("doPlot.R")
source("utilities.R")

function(input, output, session) {
  
  #get data from getdata function  
  #data.df <- getdata()
  
  version1 <- 1
  
  #reactive variables v: n.col = numer of classes, counter, selected
  v <<- reactiveValues(version = 0, max_version=1,  selected = list(),focusedPlotCoords = c(1,1)) #, focusedPlotCoords = c(1,1)) ,n.col = nrow(unique(data.df["label"])),
  
  #reactive values for df: df, labels, sel
  df <- list(  sel= list(),labels=NULL,grp_names=NULL) #Df = data.df,labels = getdata(),
  
  #for loading screen
  loading <- reactiveValues(flag = 0)
  
  diag <- NULL
  p <- NULL
  uniq_labels <- NULL
  inputFolderPath <- 'input'
  datatypeInputFolderName <- c('Generated'="default",'Sleep Data'="sleepData",'MNIST'="mnist")
  inputmap <- c('Generated'="default",'Sleep Data'="sleepData",'MNIST'="mnist")
  
  observeEvent(input$submitBtn,{
    
    inFiles <- input$datasourceFiles
    userInputDataSourceFolderPath <- NULL
    if(!is.null(inFiles))
    {
      userInputDataSourceFolderPath <- file.path(inputFolderPath,'user_input','QUEST')
      if(!file.exists(userInputDataSourceFolderPath))
        dir.create(file.path(userInputDataSourceFolderPath),recursive = TRUE)
      for(i in 1:length(inFiles$datapath))
      {
        
        file.copy(inFiles$datapath[i],file.path(userInputDataSourceFolderPath,inFiles$name[i]),overwrite = TRUE)
      }
    }
    
    df$Df <<- getdata(inputmap[[input$datasourceType]],userInputDataSourceFolderPath)
    #assign group name
    
    
    if(input$datasourceType=="Sleep Data")
    {
      actigraphDetailData <- getRepData(inputmap[[input$datasourceType]],userInputDataSourceFolderPath)
      df$actigraphRep <<- getActigraphBarReps(actigraphDetailData)
      for(i in 1:length(df$actigraphRep))
        ggsave(sprintf("dp-%d.jpg",i),df$actigraphRep[[i]])
    }
    
    v$version <<- 0
    v$version <<- 1
    v$max_version <<- 1
  })
  
  reinitializeGrpNames <- function(data.df)
  {
    lbl <- sort(unique(df$Df$label))
    if(length(df$grp_names)==0)
    {
      df$grp_names <<- paste("Group ",lbl)
      names(df$grp_names) <<- lbl
      print(df$grp_names)
    } 
    else
    {
      missing_labels <- lbl[!(lbl %in% names(df$grp_names))]
      missing_grp_names <<- paste("Group ",missing_labels)
      names(missing_grp_names) <- missing_labels
      df$grp_names <<- c(df$grp_names,missing_grp_names)
      print(df$grp_names)
    }
  }
  
  observeEvent(v$version,{
    
    print(paste("Version:",v$version))
    if(v$version==0)
      df$grp_names <- list()
    if(v$version>0){
      
      p <<- getplotlylist( 1, df$Df , NULL , NULL)
      diag <<- getdiaglylist(1, df$Df)
      uniq_labels <<- sort(unique(df$Df$label))
      comblabels <<- list()
      reinitializeGrpNames(df$Df)
      
      if(length(uniq_labels)>1)
        comblabels <<-combn(uniq_labels,2)
      
      # Assign output names for created plots
      for (i in 1:length(p)) {
        local({
          n <- i # Make local variable
          plotname <- paste("plot", n , sep="")
          output[[plotname]] <<- renderPlotly({
            p[[n]]
          })
        })
      }
      
      for (i in 1:length(diag)) {
        local({
          n <- i # Make local variable
          plotname <- paste("diag", n , sep="")
          output[[plotname]] <<- renderPlotly({
            diag[[n]]
          })
        })
      }
      
      if(v$version>0 & v$version==v$max_version)
      {
        i <- v$version
        snapshotName <- sprintf("snapshot%d",i)
        
        output[[snapshotName]] <- renderImage({
          
          force(i)
          outfile <<- sprintf("version%d.jpg",i)
          
          
          #generate snapshot and store as file
          if(i== v$version)
          {
            p2 <<- getplotlist( 1, df$Df , NULL , NULL)
            uniq_labels <<- sort(unique(df$Df$label))
            n <- length(uniq_labels)
            noLowerTriangle <- (n^2-n)/2
            k <-  1:noLowerTriangle
            orderMatrix <- matrix(nrow = n, ncol = n)
            orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
            diag(orderMatrix) <- (noLowerTriangle+1):(noLowerTriangle+n)
            #png(outfile)
            #arrangeGrob(grobs = p2,layout_matrix = orderMatrix)
            #dev.off
            g <- arrangeGrob(grobs = p2,layout_matrix = orderMatrix)
            ggsave(sprintf("version%d.jpg",i),g)
          }
          #render snapshot on timeline
          list(src = outfile,
               contentType = 'image/jpg',
               width = 150,
               height = 150,
               alt = "This is alternate text")
          #ggsave(sprintf("version%d.jpg",v$version),g1)
          #filename = sprintf("version%d.jpg",v$version)
          #list(src = filename,alt = paste("Image number", v$version))
        }, deleteFile = FALSE)
        #}
      }
    }
    
    #output$snapshot <- renderPlot({g1})
  })
  
  # merge function
  observeEvent(input$merge,
               { 
                 label1 <- v$focusedPlotCoords[1] 
                 label2 <- v$focusedPlotCoords[2]
                 
                 
                  loading$flag <- 0
                  if(v$max_version==1)
                  {
                    df$labels <<- data.frame(label1=df$Df[,c('label')])
                  }
                    
                  old_col <- paste("label", v$max_version, sep="")
                  new_col <- paste("label", v$max_version + 1  , sep="")
                  df$labels[new_col] <<-  ifelse((df$labels[[old_col]] == max(v$focusedPlotCoords)) , 
                                              min(v$focusedPlotCoords), df$labels[[old_col]])
                  v$max_version <<- v$max_version + 1
                  v$version <<- v$max_version
               
               df$Df['label'] <<- df$labels[, ncol(df$labels)]
               
               print("After manipulation")
               print(df$Df)
               
               #update using df$Df
               p <<- getplotlylist( v$version, df$Df , NULL , NULL)
               diag <<- getdiaglylist(v$version, df$Df)
               #auc <- getauclist(df$Df)
               
               for (i in 1:length(p)) {
                 local({
                   n <- i # Make local variable
                   plotname <- paste("plot", n , sep="")
                   output[[plotname]] <<- renderPlotly({
                     p[[n]]
                    })
                 })
               }
               
               for (i in 1:length(diag)) {
                 local({
                   n <- i # Make local variable
                   plotname <- paste("diag", n , sep="")
                   output[[plotname]] <<- renderPlotly({
                     diag[[n]]
                   })
                 })
               }
               
               loading$flag <- 1
               
               uniq_labels <<- sort(unique(df$labels[,ncol(df$labels)]))
               #v$n.col <- length(uniq_labels)
               
               }
  )
  
  #split function
  observeEvent(input$split,{ 
                   if(!is.null(event_data("plotly_selected",source = "focusedPlot")))
                   {
                     # display selected rows
                     d <- event_data("plotly_selected",source = "focusedPlot")
                     selected_classes <- v$focusedPlotCoords
                     
                     if(v$focusedPlotCoords[1]==v$focusedPlotCoords[2])
                     {
                       plotData <- getDiagPlotDataPoints(diag,v$focusedPlotCoords[1])
                       focusedPoints <- (plotData$label==v$focusedPlotCoords[1])
                     } 
                     else
                       plotData <- getDataPoints(p,uniq_labels,v$focusedPlotCoords[1],v$focusedPlotCoords[2])
                        focusedPoints <- (plotData$label %in% v$focusedPlotCoords)
                     #plotCardinal <- getPlotCardinal(uniq_labels,v$focusedPlotCoords[1],v$focusedPlotCoords[2])

                    
                     df$sel <<- d[,c("x","y")]
                     #ldaData <- getLDAdata(df$Df,v$focusedPlotCoords[1],v$focusedPlotCoords[2])
                     df$sel[,c("x")] <- round(df$sel[,c("x")],digits=6)
                     df$sel[,c("y")] <- round(df$sel[,c("y")],digits=6)
                     #df$Df %>% mutate(x = round(x, 5) )
                     #xlist <- lapply(df$Df[,"x"], round, digits = 6)
                     plotData[,c("x")] <- round(plotData[,c("x")],digits=6)
                     plotData[,c("y")] <- round(plotData[,c("y")],digits=6)

                     #presenceRoster <- data.frame(x=(df$Df$x %in% df$sel$x),y=(df$Df$y %in% df$sel$y))
                     presenceRoster <- data.frame(x=(plotData$x %in% df$sel$x),y=(plotData$y %in% df$sel$y))
                     to <- df$Df[(presenceRoster$x & presenceRoster$y & focusedPoints),]
  
                     df$Df[(presenceRoster$x & presenceRoster$y),]$label <<- max(df$Df$label)+1   #levels(df$Df$label)[length(levels(df$Df$label))]
                     if(v$max_version==1)
                     {
                       df$labels <<- data.frame(label1=df$Df[,c('label')])
                     }
                       
                     old_col <- paste("label", v$max_version, sep="")
                     new_col <- paste("label", v$max_version + 1  , sep="")
                     df$labels[new_col] <<- df$Df$label 
                     v$max_version <<- v$max_version + 1
                     v$version <<- v$max_version
                     
                     print("Altered Data points")
                     print(df$Df)
                     df$sel <- NULL
                   }
                   else
                     showNotification("Please select datapoints to split.")
                 
                 
               }
               
  )
  
  onTimelineItemClickListener <- function(i)
  {
    function()
    {
        onclick(sprintf("snapshot%d",i),function(e){ 
        
        print(sprintf("label%d",i))
        df$Df[,c("label")] <<- df$labels[sprintf("label%d",i)]
        v$version <<- i
        #reset labels to one in the version 
        #
        #v$version <<- i 
        #TODO: reset labels to selected version
        #workout a way that any split/merge operation must produce ith version where ith version is max_count(label columns)+1 and not selected_version+1
        
        })
    }
  }
  
  observeEvent(input$renameBtn,{
    lbl <- v$focusedPlotCoords[1]
    df$grp_names[lbl] <<- input$renameTxt
    print(df$grp_names)
  })
  
  output$timeline <- renderUI({
    
      lis <<- lapply(1:v$max_version,function(i){  
        tags$li(div(class="timeline-item",plotOutput(sprintf("snapshot%d",i),height=150,width=150)))       
        })
    
      timelineItemClickListeners <<-  lapply(1:v$max_version,function(i){
      force(i)
      onTimelineItemClickListener(i)
    }) 
    for(i in 1:length(timelineItemClickListeners))
    {
      timelineItemClickListeners[[i]]()
    }
      
    t <- tags$ul(class="timeline-list",
      lis
    )
    t
  })
  
  observeEvent(df$Df$label, {
    uniq_labels <<- order(unique(df$Df[,c('label')]))
    
  })
  
  #Printing in grid
  output$plots <- renderUI({
    if(v$version>0)
    {
      n <- length(unique(df$Df[,c('label')]))   #number of classes/catgeories
      loading$flag <- 0
      col.width <- round(12/n) # Calculate bootstrap column width
      cnter <- 0 # Counter variable ------ counter for what?
      
      
      #assigning order of printing plots in triangle
      noLowerTriangle <- (n^2-n)/2
      k <-  1:noLowerTriangle
      orderMatrix <<- matrix(nrow = n, ncol = n)
      orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
      order <- c(t(orderMatrix))
      order <- order[!is.na(order)]
      
      #'global' variables as counters
      g <- 1 # for lda plot
      z <- length(order) # for roc-auc plot
      
      # Create fluidRows with columns
      rows  <- lapply(1:n,function(row_num){
        
        #LDA plots
        if(row_num==1)
        {
          lda_cols <- list()
        }
        else
        {
          lda_cols <- lapply(1:(row_num-1), function(col_num){
            
            plotname <- paste("plot", order[g], sep="")
            g <<- g+1
            column(col.width, plotlyOutput(plotname, height = "100%", width = "100%"))
          })
        }
        
        #diagonal
        diagname <- paste("diag", row_num, sep="")
        diag <- column(col.width, plotlyOutput(diagname, height = "100%", width = "100%"))
        
        #AUC plot
        #auc_cols <- lapply(1:(n-row_num), function(col_num) {
          #plotname <- paste("auc", order[z], sep="")
          #z <<- z-1
          ##})
        
        # attach lda_cols, diag and cols2
        lda_cols[[length(lda_cols)+1]] <- diag
        cols <- lda_cols#append(lda_cols,auc_cols)
        fixedRow( style='height:auto',do.call(tagList, cols ) )
      })
      
      
      lvl <- c("plot","diag")
      eventSourceType <- factor(x=lvl)
      
      plotEventListener <- function(i) {
          force(i)
          function()
          {
            onevent("mouseenter",paste("plot",i,sep=""),{
              v$focusedPlotCoords <- c(comblabels[1,i], comblabels[2,i])
              hide("renameDiv")
            })
          }
      }
      
      
      plotEventListeners <- lapply(1:length(p),function(i){ 
        force(i)
        plotEventListener(i) }) 
      
      for(i in 1:length(plotEventListeners))
      {
        plotEventListeners[[i]]()
      }
      
      diagEventListener <- function(j) {
        function()
        {
          onevent("mouseenter",paste("diag",j,sep=""),{
            v$focusedPlotCoords <- c(j, j)
            updateTextInput(session,"renameTxt",value=df$grp_names[[j]])
            showElement("renameDiv")
            })
        }
      }
      
      
      diagEventListeners <- lapply(1:length(diag),function(j){ 
        force(j)
        diagEventListener(j) 
      }) 
      
      for(j in 1:length(diagEventListeners))
      {
        diagEventListeners[[j]]()
      }
      
      #d<- div(style="height:200;width:200;position:absolute;right:0;top:0;background:red;z-index:2;","Hole in wall")#plotlyOutput("palette",height = 200,width=200))
      
      # change screen from loading
      loading$flag <- 1
      
      # Calling the rows that contain cols
      
      r <- do.call(tagList, rows)
      #div(r,d)
      r
    }
  })
 
  
  
  
  output$selectedPlot <- renderPlotly(
    {
      if(v$version>0)
      {
          if(length(which(v$focusedPlotCoords %in% uniq_labels))<2)
            v$focusedPlotCoords <<- c(1,1)
            
          label1 = v$focusedPlotCoords[1]
          label2 =  v$focusedPlotCoords[2]
          
          if(label1 == label2)
          {
            focusedPlot <- diag[[label1]]
          }
          else
          {  
              comb_labels <- combn(uniq_labels,2)
              n <- intersect(which(comblabels[1,] == label1) , which(comblabels[2,] == label2))
              focusedPlot <- p[[n]]
          }
          
        
          #t$layout$width <- 300
          #t$layout$height <- 300
          focusedPlot <- focusedPlot %>% layout(width=270,height=270,dragmode ="lasso")
          focusedPlot$x$source <- "focusedPlot"
          focusedPlot
      }
      else{
        plotly_empty()
      }
      
      
    })
  
  output$selectedDataPoints <- renderUI({
    d <- event_data("plotly_selected",source = "focusedPlot")
    if(!is.null(d)| length(d)!=0)
    {
      selected_classes <- v$focusedPlotCoords
      
      if(selected_classes[1]==selected_classes[2])
      {
        plotData <- getDiagPlotDataPoints(diag,selected_classes[1])
        focusedPoints <- (plotData$label==selected_classes[1])
      }
      else{
        plotData <- getDataPoints(p,uniq_labels,selected_classes[1],selected_classes[2])
        focusedPoints <- (plotData$label %in% selected_classes)
      }
        
      
      print("Plot Data")
      print(plotData)
      
      df$sel <<- d[,c("x","y")]
      #print(df$sel[,c("x","y")])
      df$sel[,c("x")] <- round(df$sel[,c("x")],digits=6)
      df$sel[,c("y")] <- round(df$sel[,c("y")],digits=6)
      
      plotData[,c("x")] <- round(plotData[,c("x")],digits=6)
      plotData[,c("y")] <- round(plotData[,c("y")],digits=6)
      
      presenceRoster <- data.frame(x=(plotData$x %in% df$sel$x),y=(plotData$y %in% df$sel$y))
      print(which(presenceRoster$x & presenceRoster$y))
      selected_actigraph_plots_idx <- which(presenceRoster$x & presenceRoster$y)#df$actigraphRep[which(presenceRoster$x & presenceRoster$y)]
      rows <- lapply(1:length(selected_actigraph_plots_idx), function(i){
        imgFilePath <- sprintf('dp-%d.jpg',selected_actigraph_plots_idx[[i]]) 
        p <- renderImage({
                              list(src = imgFilePath,
                               contentType = 'image/jpg',
                               width = 150,
                               height = 80,
                               alt = "This is alternate text")},deleteFile = FALSE)
        #p <- renderPlot(selected_actigraph_plot+theme(legend.position = "none"),height = 200)
        fixedRow(column(12,p))
      })
      tagList(rows)
    }
    else
      fixedRow()
  })
  
  
  
  # To display loading page
  observe({
    if(loading$flag == 1){
      hide("loading_page")
      show("main_content")
    }
    else
    {
      hide("main_content")
      show("loading_page")
    }
  })
  
  # get plot number (plot4) from hovered plot
  observeEvent(v$selected, {
    print("v$selected")
    if(length(v$selected)!=0)
    {
      label1 = v$selected[1]
      label2 =  v$selected[2]
      
      if(label1 == label2)
      {
        v$plotno <- diag[[label1]]
      }
        
      else
      {
        uniq_labels <- order(unique(df$Df[,ncol(df$Df)]))
        comb_labels <- combn(uniq_labels,2)
        n <- intersect(which(comb_labels[1,] == v$selected[1]) , which(comb_labels[2,] == v$selected[2]))
        v$plotno <- p[[n]]
      }
    }
  })
  
  
  observeEvent(input$generate,{
    k <- as.integer(input$k)
    if(!is.null(k) && k<nrow(df$Df))
    {
      t <- kmeans(df$Df[,!(names(df$Df) %in% c('label','id'))],centers = k, nstart = 20)
      
      if(v$max_version==1)
      {
        df$labels <<- data.frame(label1=df$Df[,c('label')])
      }
      
      old_col <- paste("label", v$max_version, sep="")
      new_col <- paste("label", v$max_version + 1  , sep="")
      
      df$Df$label <<-  t$cluster
      print(df$Df$label)
      df$labels[new_col] <- df$Df$label
      
      v$max_version <<- v$max_version + 1
      v$version <<- v$max_version
    }
  })
  
  output$download <- downloadHandler(filename = function(){
    paste("data-",v$version,".csv")
  },
  content <- function(file){
    datadf <- df$Df
    datadf$grp_name <- df$grp_names[df$Df$label]
    write.csv(datadf,file)
  }
  )
  
  output$hoveredDpView <- renderUI({
    
    eventdata <- event_data("plotly_hover", source = "focusedPlot")
    #validate(need(!is.null(eventdata), "Hover over the time series chart to populate this heatmap"))
    if(!is.null(eventdata))
    {
      pointIdx <- as.numeric(eventdata$pointNumber)[1]
      print(pointIdx)
      
      
      outfile <- sprintf("dp-%d.jpg",pointIdx+1)
      #list(src = outfile,
           #contentType = 'image/jpg',
           #width = 150,
           #height = 150,
           #alt = "")
      t <- renderImage({
        list(src = outfile,
             contentType = 'image/jpg',
             width = 150,
             height = 150,
             alt = "")},
        deleteFile = FALSE)
      tags$img(t)
      
    }
    
  })
  
}



