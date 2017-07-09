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

function(input, output, session) {
  
  #get data from getdata function  
  #data.df <- getdata()
  
  version1 <- 1
  
  #reactive variables v: n.col = numer of classes, counter, selected
  v <<- reactiveValues(version = 0, max_version=1,  selected = list(),focusedPlotCoords = c(1,1)) #, focusedPlotCoords = c(1,1)) ,n.col = nrow(unique(data.df["label"])),
  
  #reactive values for df: df, labels, sel
  df <- list(  sel= list(),labels=NULL) #Df = data.df,labels = getdata(),
  
  #for loading screen
  loading <- reactiveValues(flag = 0)
  
  diag <- NULL
  p <- NULL
  uniq_labels <- NULL
  #outputOptions(output, "timeline", priority = 1)
  
  
  observeEvent(input$datasource,{
               inputmap <- c('Generated'="default",'Sleep Data'="sleepData",'MNIST'="mnist")
               df$Df <<- getdata(inputmap[[input$datasource]])
               v$version <<- 0
               v$version <<- 1
              }
  )
  
  observeEvent(v$version,{
    
    print(paste("Version:",v$version))
    if(v$version>0){
      
      print(df$Df)
      p <<- getplotlylist( 1, df$Df , NULL , NULL)
      diag <<- getdiaglist(1, df$Df)
      uniq_labels <<- sort(unique(df$Df$label))
      
      comblabels <<- combn(uniq_labels,2)
      #print(paste("Inc version",v$version))
      #v$version <<- v$version + 1
      # Assign output names for created plots
      for (i in 1:length(p)) {
        local({
          n <- i # Make local variable
          plotname <- paste("plot", n , sep="")
          print(plotname)
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
            print(uniq_labels)
            n <- length(uniq_labels)
            noLowerTriangle <- (n^2-n)/2
            k <-  1:noLowerTriangle
            orderMatrix <- matrix(nrow = n, ncol = n)
            orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
            print(orderMatrix)
            
            #png(outfile)
            #arrangeGrob(grobs = p2,layout_matrix = orderMatrix)
            #dev.off()
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
                 
                 print(df$Df[,c('label')])
                 print(df$labels)
                  loading$flag <- 0
                  if(v$max_version==1)
                  {
                    df$labels <<- data.frame(label1=df$Df[,c('label')])
                     print(df$labels)
                  }
                  print(df$labels['label1'])
                    
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
               diag <<- getdiaglist(v$version, df$Df)
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
  #get selected points
  #identify selected points and assign new class
  #reload graphs
  
  
  #split function
  observeEvent(input$split,{ 
                   if(!is.null(event_data("plotly_selected",source = "focusedPlot")))
                   {
                     # display selected rows
                     d <- event_data("plotly_selected",source = "focusedPlot")
                    
                     df$sel <- d[,c("x","y")]
                     print(df$Df)
                     print(df$sel[,c("x")])
                     df$sel[,c("x")] <- round(df$sel[,c("x")],digits=6)
                     df$sel[,c("y")] <- round(df$sel[,c("y")],digits=6)
                     #df$Df %>% mutate(x = round(x, 5) )
                     #xlist <- lapply(df$Df[,"x"], round, digits = 6)
                     df$Df[,c("x")] <- round(df$Df[,c("x")],digits=6)
                     df$Df[,c("y")] <- round(df$Df[,c("y")],digits=6)

                     presenceRoster <- data.frame(x=(df$Df$x %in% df$sel$x),y=(df$Df$y %in% df$sel$y))
                     to <- df$Df[(presenceRoster$x & presenceRoster$y),]
                     print(to)
                     
                     #print(df$Df$label)
                     #lvl <- levels(df$Df$label)
                     #print(lvl)
                     #levels(df$Df$label) <- c(lvl,as.character(as.integer(lvl[length(lvl)])+1)) 
                     #print(levels(df$Df$label))
                     
                     df$Df[(presenceRoster$x & presenceRoster$y),]$label <- max(df$Df$label)+1   #levels(df$Df$label)[length(levels(df$Df$label))]
                     if(v$max_version==1)
                     {
                       df$labels['label1'] <- df$Df[,c('label')]
                     }
                       
                     old_col <- paste("label", v$max_version, sep="")
                     new_col <- paste("label", v$max_version + 1  , sep="")
                     df$labels[new_col] <<- df$Df$label 
                     v$max_version <<- v$max_version + 1
                     v$version <<- v$max_version
                     
                     print("Altered Data points")
                     print(df$Df)
                   }
                   else
                     showNotification("Please select datapoints to split.")
                 
                 
               }
               
  )
  
  
  # undo function
  observeEvent(input$undo,
               { 
                 loading$flag <- 0
                 last_col <- paste("label", v$version, sep="")
                 # ADDING DYNAMIC COLUMNS
                 df$labels[last_col] <<-  NULL
                 v$version <<- v$version - 1
                 updateSelectizeInput(session, "variable",
                                      choices = unique(df$Df["label"]))
                 
                 df$Df['label'] <- data.df[, ncol(df$labels)]
                 p <- getplotlylist( v$version, df$Df , NULL , NULL)
                 #print(p)
                 
                 for (i in 1:length(p)) {
                   local({
                     n <- i # Make local variable
                     plotname <- paste("plot", n , sep="")
                     #print(plotname)
                     output[[plotname]] <<- renderPlotly({
                       p[[n]]
                       
                     })
                   })
                 }
                 
                 loading$flag <- 1
                 uniq_labels <- unique(df$Df[,ncol(df$labels)])
                 #v$n.col <- length(uniq_labels)
                 
               }
  )
  
  onTimelineItemClickListener <- function(i)
  {
    function()
    {
        onclick(sprintf("snapshot%d",i),function(e){ 
        
        print(sprintf("label%d",i))
        df$Df[,c("label")] <<- df$labels[sprintf("label%d",i)]
        print(df$Df$label)
        v$version <<- i
        #reset labels to one in the version 
        #
        #v$version <<- i 
        #TODO: reset labels to selected version
        #workout a way that any split/merge operation must produce ith version where ith version is max_count(label columns)+1 and not selected_version+1
        
        })
    }
  }
  
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
    ver <- v$version
    n <- length(unique(df$Df[,c('label')]))   #number of classes/catgeories
    print(paste("No of labels :",n))
    loading$flag <- 0
    col.width <- round(12/n) # Calculate bootstrap column width
    cnter <- 0 # Counter variable ------ counter for what?
    
    
    #assigning order of printing plots in triangle
    noLowerTriangle <- (n^2-n)/2
    k <-  1:noLowerTriangle
    orderMatrix <<- matrix(nrow = n, ncol = n)
    orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
    print(orderMatrix)
    print(uniq_labels)
    order <- c(t(orderMatrix))
    order <- order[!is.na(order)]
    print(order)
    
    
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
          onevent("mouseenter",paste("plot",i,sep=""),v$focusedPlotCoords <- c(comblabels[1,i], comblabels[2,i]))
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
        onevent("mouseenter",paste("diag",j,sep=""),v$focusedPlotCoords <- c(j, j))
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
  })
 
  
  
  
  output$selectedPlot <- renderPlotly(
    {
      if(v$version>0)
      {
        print("Uniq labels")
        print(uniq_labels)
          if(length(which(v$focusedPlotCoords %in% uniq_labels))<2)
            v$focusedPlotCoords <<- c(1,1)
            
          label1 = v$focusedPlotCoords[1]
          label2 =  v$focusedPlotCoords[2]
            
          if(label1 == label2)
          {
            print("Here")
              focusedPlot <- diag[[label1]]
              print("End")
          }
          else
          {  
              comb_labels <- combn(uniq_labels,2)
              n <- intersect(which(comblabels[1,] == label1) , which(comblabels[2,] == label2))
              print(label1)
              print(label2)
              print(n)
              focusedPlot <- p[[n]]
          }
          
        
          #t$layout$width <- 300
          #t$layout$height <- 300
          focusedPlot <- focusedPlot %>% layout(width=300,height=300,dragmode ="lasso")
          focusedPlot$x$source <- "focusedPlot"
          focusedPlot
      }
      else{
        plotly_empty()
      }
      
      
    })
  
  #output$selected <- renderPrint({
    #d <- event_data("plotly_selected",source = "focusedPlot")
    #if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d[, c("x","y")]
    
  #})
  
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
  
  # on clicking split
  observeEvent(input$split, {
    show("col", anim = T, time = 2)
    show("okay", anim = T, time = 1)
  })
  
  # get plot number (plot4) from hovered plot
  observeEvent(v$selected, {
    if(length(v$selected)!=0)
    {
      label1 = v$selected[1]
      label2 =  v$selected[2]
      print(paste(label1,label2,sep=" "))
      
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
  
  # To display bigger plot of hovered plot
  # output$plot <-  renderUI({
  #   cnter <- v$plotno
  #   plotname <- paste("plot", cnter, sep="")
  #   print("gotta display")
  #   plotlyOutput(plotname, height = 500, width = 500)})
  
  
  
  
}



