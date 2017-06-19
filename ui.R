library("shiny")
library("shinyjs")
library("colourpicker")
library(shiny)
library(shinyBS)
library(plotly)

source("getdata.R")

controlPanel <- fluidPage(
           verbatimTextOutput("selected"),
           actionButton("merge",'Merge'),
           actionButton("split",'Split'),
           actionButton('generate','Generate'),
           tags$input(id='k',type='text',size='2',hint='k')
)
detailUi <- tags$div(
  
)

fluidPage(
  
  useShinyjs(),
  
  #head
  tags$head(
    tags$link(rel="stylesheet",type="text/css",href="style.css")
  ),
  
  #tags$nav(class='navbar navbar-inverse',
           #tags$a(class="navbar-brand","ClassMat")),
  
  h1("ClassMat",class="title-panel"),
  hr(),
  
  #body
  div(id='content',
    inputPanel(fileInput('file',label=h3("File input"))),#,bsButton("submit",label = "Submit")),
    div(id='display-panel',class='panel panel-default',
        fluidRow(
          uiOutput(class='timeline bg-faded','timeline')
        ),
        fluidRow(
          column(7,id='overview',class='panel panel-default',div(class='panel-body',uiOutput('plots')) ), #style="background-color:#AA9C99;height:100%",plotlyOutput('splom')),
          column(4,class='panel panel-default',
                        div(class='panel-heading','Main View'),
                        div(class='panel-body',plotlyOutput('selectedPlot',height=300),controlPanel),
                        column(1,detailUi)
                )
              )
      )
  )
)

















