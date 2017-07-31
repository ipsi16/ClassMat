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
  v <<- reactiveValues(version = 0, max_version=1,  selected = list(),focusedPlotCoords = c(1,1),priorityClass=1) #, focusedPlotCoords = c(1,1)) ,n.col = nrow(unique(data.df["label"])),
  dispSettings <- reactiveValues(ldaViewOrder=NULL,pcaViewOrder=NULL)
  dispRowLimit <- 6
  exceedsDispClsLt <- FALSE
  
  #reactive values for df: df, labels, sel
  df <- list(  sel= list(),labels=NULL,grp_names=NULL) #Df = data.df,labels = getdata(),
  
  
  #for loading screen
  loading <- reactiveValues(flag = 0)
  diag_plt_list <- NULL
  p <- NULL
  uniq_labels <- NULL
  focusedPlot <- NULL
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
    reset("datasourceFiles")
    df$Df <<- getdata(inputmap[[input$datasourceType]],userInputDataSourceFolderPath)
    
    
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
    } 
    else
    {
      missing_labels <- lbl[!(lbl %in% names(df$grp_names))]
      missing_grp_names <<- paste("Group ",missing_labels)
      names(missing_grp_names) <- missing_labels
      df$grp_names <<- c(df$grp_names,missing_grp_names)
    }
  }
  
  observe({
    print("Re-rendering overview as per the new order")
    print(dispSettings$pcaOrder)
    print(dispSettings$ldaOrder)
    print(sprintf("Rendering version %d",v$version))
    print(df$labels)
    if(v$version>0)
    {
        
        for (i in 1:length(p)) {
          local({
            n <- i # Make local variable
            plotname <- paste("plot", n , sep="")
            print(paste("Rendering...",plotname))
            output[[plotname]] <<- renderPlot({ p[[n]] })
          })
        }
      
        for (diag_indx in 1:length(diag_plt_list)) 
        {
          local({
            n <- diag_indx # Make local variable
            plotname <- paste("diag", n , sep="")
            print(paste("Rendering...",plotname))
            print("PCA order")
            print(dispSettings$pcaOrder)
            output[[plotname]] <<- renderPlot({ diag_plt_list[[which(uniq_labels==dispSettings$pcaOrder[[n]])]] },width=100,height=100)
          })
        }
      
        if(exceedsDispClsLt)
        {
          for(i in 1:dispRowLimit)
          {
            local({
              n <- i # Make local variable
              plotname <- paste("misc", n , sep="")
              output[[plotname]] <<- renderPlot({
                misc[[n]]
              })
            })
          }
        }
    }
    
  })
  
  observeEvent(v$version,{
    print(paste("Version:",v$version))
    if(v$version==0)
    {
      df$grp_names <- list()
      hideElement("viewControl")
      v$focusViewLock$state=FALSE
    }
      
    if(v$version>0){
      
      v$focusViewLock$state=FALSE
      uniq_labels <<- sort(unique(df$Df$label))
      showElement("viewControl")
      updateSelectInput(session,"viewOrderSlct",choices = uniq_labels)
      reinitializeGrpNames(df$Df)
      
      print("Does the number of classes exceed the row limit?")
      if(length(uniq_labels)>dispRowLimit)
        exceedsDispClsLt <<- TRUE
      else
        exceedsDispClsLt <<- FALSE
      print(exceedsDispClsLt)
      
      comblabels <<- list()
      if(length(uniq_labels)>1)
        comblabels <<-combn(uniq_labels,2)
      n <- length(uniq_labels)
      noLowerTriangle <- (n^2-n)/2
      if(noLowerTriangle==0)
        k <- NULL
      else
        k <<- 1:noLowerTriangle
      
      dispSettings$ldaOrder <<- k
      dispSettings$pcaOrder <<- uniq_labels
      print("PCA order")
      print(dispSettings$pcaOrder)
      print("LDA order")
      print(dispSettings$ldaOrder)
     
      print("Fetching Plot Lists....")
      p <<- getLDAPlotlist( 1, df$Df,NULL)
      py <<- getplotlylistfromplots(p)
      diag_plt_list <<- getdiaglist(1, df$Df)
      print(sprintf("Fetched %d LDA plots, %d PCA plots",length(p),length(diag_plt_list)))
      
      if(exceedsDispClsLt)
      {
        print("Fetching Miscellaneous Plot Lists....")
        misc <<- getmiscplotlist(df$Df,dispSettings$pcaOrder,(length(uniq_labels)-dispRowLimit)+1)
        print(sprintf("Fetched %d miscellaneous plots",length(misc)))
      }
      
      if(v$version>0 & v$version==v$max_version)
      {
        print(sprintf("Generating snapshot for version %d",v$max_version))
        i <- v$max_version
        snapshotName <- sprintf("snapshot%d",i)
        output[[snapshotName]] <- renderImage({
          
          force(i)
          
          #generate snapshot and store as file
          #if(i==v$version)
          #{
            outfile <<- sprintf("version%d.jpg",i)
            
            print(paste("Rendering file :",outfile))
            p2 <<- gettimelineplotlist( 1, df$Df , NULL , NULL)
            uniq_labels <<- sort(unique(df$Df$label))
            
            orderMatrix <- matrix(nrow = n, ncol = n)
            orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
            diag(orderMatrix) <- (noLowerTriangle+1):(noLowerTriangle+n)
            g <- arrangeGrob(grobs = p2,layout_matrix = orderMatrix)
            ggsave(sprintf("version%d.jpg",i),g)
          #}
          #render snapshot on timeline
          print(paste("Rendering file ",outfile," to snapshot ",snapshotName))
          list(src = outfile,
               contentType = 'image/jpg',
               width = 150,
               height = 150,
               alt = "This is alternate text")
        }, deleteFile = FALSE)
      }
      print(sprintf("Generated snapshot for version %d",v$version))
    }
  })
  
  # merge function
  observeEvent(input$merge,
               {
                 label1 <- v$focusedPlotCoords[1] 
                 label2 <- v$focusedPlotCoords[2]
                 print(sprintf("Begin merging classes (%d,%d)...",label1,label2))
                 
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
                  
                 df$Df['label'] <<- df$labels[,ncol(df$labels)]
                 
                 print("After manipulation")
                 
                 loading$flag <- 1
                 
                 uniq_labels <<- sort(unique(df$labels[,ncol(df$labels)]))
               }
  )
  
  #split function
  observeEvent(input$split,{
                    print("Splitting selected data points...")
                   if(!is.null(event_data("plotly_selected",source = "focusedPlot")))
                   {
                     # display selected rows
                     d <- event_data("plotly_selected",source = "focusedPlot")
                     #plotData <- focusedPlot$data
                       
                     pos1 = v$focusedPlotCoords[1]
                     pos2 = v$focusedPlotCoords[2]
                     if(pos1==pos2)
                     {
                         label_in_focus = dispSettings$pcaOrder[pos1]
                         plotData <- getPCAdata(df$Df)
                         focusedPoints <- (plotData$label==label_in_focus)
                         
                     } 
                     else
                     {
                         label1 = dispSettings$pcaOrder[pos1]
                         label2 = dispSettings$pcaOrder[pos2]
                         plotData <- getLDAdata(df$Df,label1,label2)
                         focusedPoints <- (plotData$label %in% c(label1,label2))
                     }
                     
                     df$sel <<- d[,c("x","y")]
                     df$sel[,c("x")] <- round(df$sel[,c("x")],digits=6)
                     df$sel[,c("y")] <- round(df$sel[,c("y")],digits=6)
                     
                     plotData[,c("x")] <- round(plotData[,c("x")],digits=6)
                     plotData[,c("y")] <- round(plotData[,c("y")],digits=6)
                     
                     print("Plot data")
                     print(plotData)
                     
                     print("Selected Data Points")
                     print(df$sel)
                     

                     #presenceRoster <- data.frame(x=(df$Df$x %in% df$sel$x),y=(df$Df$y %in% df$sel$y))
                     presenceRoster <- data.frame(x=(plotData$x %in% df$sel$x),y=(plotData$y %in% df$sel$y))
                     lbls <- df$Df$label
                     
                     
                     print("Split indexes:")
                     print(which(presenceRoster$x & presenceRoster$y & focusedPoints))
  
                     if(v$max_version==1)
                     {
                       df$labels <<- data.frame(label1=df$Df[,c('label')])
                     }
                     lbls[(presenceRoster$x & presenceRoster$y & focusedPoints)] <- max(df$Df$label)+1
                     df$Df$label <<- lbls
                     #df$Df[(presenceRoster$x & presenceRoster$y & focusedPoints),]$label <<- max(df$Df$label)+1   #levels(df$Df$label)[length(levels(df$Df$label))]
                     
                       
                     old_col <- paste("label", v$max_version, sep="")
                     new_col <- paste("label", v$max_version + 1  , sep="")
                     df$labels[new_col] <<- lbls 
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
          
          #change version
          df$Df[,c("label")] <<- df$labels[sprintf("label%d",i)]
          v$version <<- i
          
          #send custom message for highlight
        })
    }
  }
  
  observeEvent(input$renameBtn,{
    lbl <- v$focusedPlotCoords[1]
    df$grp_names[lbl] <<- input$renameTxt
    print(df$grp_names)
  })
  
  output$timeline <- renderUI({
    
      print(sprintf("Rendering time line with max version %d",v$max_version))
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
      noOfRows <- n
      loading$flag <- 0
      col.width <- round(12/n) # Calculate bootstrap column width
      cnter <- 0 # Counter variable ------ counter for what?
      
      
      #assigning order of printing plots in triangle
      noLowerTriangle <- (n^2-n)/2
      k <-  1:noLowerTriangle
      orderMatrix <<- matrix(nrow = n, ncol = n)
      orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <<- dispSettings$ldaOrder
      if(exceedsDispClsLt) 
      {
        noOfRows <- dispRowLimit-1
        orderMatrix <<- orderMatrix[1:noOfRows,1:noOfRows]
      }
      print(orderMatrix)
      order <- c(t(orderMatrix))
      order <- order[!is.na(order)]
      
      #'global' variables as counters
      g <- 1 # for lda plot
      z <- length(order) # for roc-auc plot
      
      # Create fluidRows with columns
      
      rows  <- lapply(1:noOfRows,function(row_num){
        
        #LDA plots
        if(row_num==1)
        {
          lda_cols <- list()
        }
        else
        {
          lda_cols <- lapply(1:(row_num-1), function(col_num){
            
            plotname <- paste("plot", order[g],sep="")
            print(plotname)
            g <<- g+1
            column(col.width, plotOutput(plotname, height = "100px", width = "100px"))
          })
        }
        
        #diagonal
        diagname <- paste("diag", row_num, sep="")
        print(diagname)
        diag <- column(col.width, plotOutput(diagname, height = "100px", width = "100px"))
        
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
              if(!v$focusViewLock$state)
                v$focusedPlotCoords <- c(comblabels[1,i], comblabels[2,i])
              hide("renameDiv")
            })
            onclick(paste("plot",i,sep=""),{
              if(!is.null(v$focusViewLock$id) && v$focusViewLock$id==paste("plot",i,sep=""))
              {
                v$focusViewLock$state=!v$focusViewLock$state
              }
              else
              {
                v$focusViewLock$state=TRUE
                v$focusViewLock$id=paste("plot",i,sep="")
              }
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
            if(!v$focusViewLock$state)
            {
              #diag_pos = which(dispSettings$pcaOrder==j)
              v$focusedPlotCoords <- c(j,j)
              cls <- dispSettings$pcaOrder[j]
              updateTextInput(session,"renameTxt",value=df$grp_names[[cls]])
              showElement("renameDiv")
            }
              
            })
          onclick(paste("diag",i,sep=""),{
            if(!is.null(v$focusViewLock$id) && v$focusViewLock$id==paste("diag",j,sep=""))
            {
              v$focusViewLock$state=!v$focusViewLock$state
            }
            else
            {
              v$focusViewLock$state=TRUE
              v$focusViewLock$id=paste("diag",j,sep="")
            }
            
            })
        }
      }
      
      
      diagEventListeners <- lapply(1:length(diag_plt_list),function(j){ 
        force(j)
        diagEventListener(j) 
      }) 
      
      for(j in 1:length(diagEventListeners))
      {
        diagEventListeners[[j]]()
      }
      
      # change screen from loading
      loading$flag <- 1
      
      # Calling the rows that contain cols
      if(exceedsDispClsLt)
      {
        misc_cols <- lapply(1:dispRowLimit,function(ctr){
          misc_plotname <- paste("misc",ctr, sep="")
          column(col.width, plotOutput(misc_plotname, height = "80px", width = "80px"))
        })
        misc_row <- (fixedRow(class="miscRow",do.call(tagList, misc_cols)))
        
        #add listener
        onevent("mouseenter",paste("misc",dispRowLimit,sep=""),{
          v$focusedPlotCoords <- c(dispRowLimit,dispRowLimit)
          hide("renameDiv")
        })
        rows[[dispRowLimit]] <- misc_row
      }
      
      r <- do.call(tagList, rows)
      r
    }
  })
  
  output$selectedPlot <- renderPlotly(
    {
      print(sprintf("Rendering Focused Plot for plot [%d,%d] .....",v$focusedPlotCoords[1],v$focusedPlotCoords[2]))
      if(v$version>0)
      {
          if(length(which(v$focusedPlotCoords %in% uniq_labels))<2)
            v$focusedPlotCoords <<- c(1,1)
          
          pos1 = v$focusedPlotCoords[1]
          pos2 =  v$focusedPlotCoords[2]
          if(exceedsDispClsLt && pos1==dispRowLimit && pos2==dispRowLimit)
          {
            miscClasses <- dispSettings$pcaOrder[dispRowLimit:length(dispSettings$pcaOrder)]
            #get data for the selected plot
            dimReducedData <- misc[[dispRowLimit]]$data[,c('x','y')]
            #assign each point to their original classes (not aggregated classes)
            dimReducedData$label <- df$Df$label
            focusedPlotly <<- getMiscPlotly(dimReducedData,miscClasses) 
            focusedPlotly$x$source <<- "miscFocusedPlot"
          }  
          else 
          {
            if(pos1 == pos2)
            {
              label = dispSettings$pcaOrder[pos1]
              focusedPlot <<- diag_plt_list[[which(uniq_labels==label)]]
              focusedPlotly <<- getPlotlyFromPlot(diag_plt_list[[which(uniq_labels==label)]])
            }
            else
            {
              label1 = pos1
              label2 = pos2
              n <- intersect(which(comblabels[1,] == label1) , which(comblabels[2,] == label2))
              focusedPlot <<- p[[n]]
              focusedPlotly <<- getPlotlyFromPlot(p[[n]])
            }
            focusedPlotly$x$source <<- "focusedPlot"
          }
          
          #t$layout$width <- 300
          #t$layout$height <- 300
          focusedPlotly <<- focusedPlotly %>% layout(width=270,height=270,dragmode ="lasso")
          focusedPlotly
      }
      else{
        plotly_empty()
      }
      
    })
  
  output$selectedDataPoints <- renderUI({
    
    #temporary fix for data sources without representation
    d <- event_data("plotly_selected",source = "focusedPlot")
    
    if(input$datasourceType!='Sleep Data')
      d <- NULL
    if(!is.null(d)| length(d)!=0)
    {
      
      pos1 = v$focusedPlotCoords[1]
      pos2 = v$focusedPlotCoords[2]
      if(pos1==pos2)
      {
        label_in_focus = dispSettings$pcaOrder[pos1]
        plotData <- getPCAdata(df$Df)
        focusedPoints <- (plotData$label==label_in_focus)
        
      } 
      else
      {
        label1 = dispSettings$pcaOrder[pos1]
        label2 = dispSettings$pcaOrder[pos2]
        plotData <- getLDAdata(df$Df,label1,label2)
        focusedPoints <- (plotData$label %in% c(label1,label2))
      }
      
      df$sel <<- d[,c("x","y")]
      df$sel[,c("x")] <- round(df$sel[,c("x")],digits=6)
      df$sel[,c("y")] <- round(df$sel[,c("y")],digits=6)
      
      plotData$x <- round(plotData$x,digits=6)
      plotData$y <- round(plotData$y,digits=6)
      
      print("Selected Data Points")
      print(df$sel)
      print("Plot data")
      print(plotData)
      
      presenceRoster <- data.frame(x=(plotData$x %in% df$sel$x),y=(plotData$y %in% df$sel$y))
      selected_actigraph_plots_idx <- which(presenceRoster$x & presenceRoster$y & focusedPoints)#df$actigraphRep[which(presenceRoster$x & presenceRoster$y)]
      print("data point indexes")
      print(selected_actigraph_plots_idx)
      rows <- lapply(1:length(selected_actigraph_plots_idx), function(i){
        imgFilePath <- sprintf('dp-%d.jpg',selected_actigraph_plots_idx[[i]]) 
        im <- imageOutput(sprintf('dp-%d',selected_actigraph_plots_idx[[i]]), height="80px" )
        im <- renderImage({
                              list(src = imgFilePath,
                               contentType = 'image/jpg',
                               width = 190,
                               height = 80,
                               alt = "This is alternate text")},deleteFile = FALSE)
        #p <- renderPlot(selected_actigraph_plot+theme(legend.position = "none"),height = 200)
        fixedRow(column(12,im,style="padding:0px;height:80px"),height=85)
      })
      tagList(rows)
    }
    else
      fixedRow()
  })
  
  observeEvent(input$viewOrderSlct,{
    v$priorityClass <<- as.integer(input$viewOrderSlct)
  })
  
  observeEvent(v$priorityClass,{
    
    if(!is.na(v$priorityClass))
    {
      print(paste("Priority class:",v$priorityClass))
      print("Before :")
      print(dispSettings$ldaOrder)
      if(!is.null(uniq_labels) && length(uniq_labels)!=0)
      {
        if(is.null(dispSettings$ldaOrder))
        {
          n <- length(uniq_labels)
          noLowerTriangle <- (n^2-n)/2
          if(noLowerTriangle==0)
            noLowerTriangle <- 1
          k <<- 1:noLowerTriangle
          
          dispSettings$ldaOrder <<- k
          dispSettings$pcaOrder <<- uniq_labels
        }
        else
        {
          dispSettings$ldaOrder <<- prioritizeClassDisplay(v$priorityClass,dispSettings$ldaOrder,comblabels)
          dispSettings$pcaOrder <<- c(v$priorityClass,dispSettings$pcaOrder[dispSettings$pcaOrder!=v$priorityClass])
          print(dispSettings$ldaOrder)
          print(dispSettings$pcaOrder)
        }
      }
    }
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
    if(length(v$selected)!=0)
    {
      label1 = v$selected[1]
      label2 =  v$selected[2]
      
      if(label1 == label2)
      {
        v$plotno <- diag_plt_list[[label1]]
      }
        
      else
      {
        n <- intersect(which(comblabels[1,] == v$selected[1]) , which(comblabels[2,] == v$selected[2]))
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
      df$labels[new_col] <<- df$Df$label
      
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
    if(input$datasourceType!='Sleep data')
      eventdata <- NULL
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
  
  observeEvent(event_data("plotly_click",source="miscFocusedPlot"),{
    print(event_data("plotly_click",source="miscFocusedPlot"))
    e <- event_data("plotly_click",source="miscFocusedPlot")
    plotData <- misc[[dispRowLimit]]$data[,c('x','y','label')]
    plotData$label <- df$Df$label
    
    e[,c("x")] <- round(e[,c("x")],digits=7)
    e[,c("y")] <- round(e[,c("y")],digits=7)
    
    plotData[,c("x")] <- round(plotData[,c("x")],digits=7)
    plotData[,c("y")] <- round(plotData[,c("y")],digits=7)
    print(plotData)
    
    presenceRoster <- data.frame(x=(plotData$x %in% e$x),y=(plotData$y %in% e$y))
    selected_pt_idx <- which(presenceRoster$x & presenceRoster$y)
    print(selected_pt_idx)
    print(plotData[selected_pt_idx,])
    v$priorityClass <<- plotData[selected_pt_idx,c("label")]
  })
}



