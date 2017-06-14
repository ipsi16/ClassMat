library("shiny")
library("shinyjs")
library("colourpicker")
library(shiny)
library(shinyBS)
library(plotly)

source("getdata.R")

controlPanel <- fluidPage(
           verbatimTextOutput("selected"),
           actionButton("merge",'merge'),
           actionButton("split",'split'),
           actionButton('undo','undo')
)
detailUi <- tags$div(
  
)

fluidPage(
  
  useShinyjs(),
  
  #head
  tags$head(
    tags$link(rel="stylesheet",type="text/css",href="style.css")
  ),
  
  
  #body
  fluidRow(
      column(7,class='panel panel-default',style='overflow-x:scroll;',div(class='panel-body',uiOutput('plots')) ), #style="background-color:#AA9C99;height:100%",plotlyOutput('splom')),
      column(4,class='panel panel-default',
             div(class='panel-heading','Main View'),
             div(class='panel-body',plotlyOutput('selectedPlot'),controlPanel),
      column(1,detailUi)
    )
  )
)

















