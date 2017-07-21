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
    inputPanel(
      fileInput('datasourceFiles',label=h3("Data Source"),multiple = TRUE, 
                accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      selectInput('datasourceType',label=h3('Data Source Type'),choices = c('Generated','Sleep Data','MNIST')),
      submitButton(text = "Apply Changes", icon = NULL, width = NULL)
      ),
    div(id='display-panel',class='panel panel-default',
        fluidRow(
          uiOutput(class='timeline bg-faded','timeline')
        ),
        fluidRow(style='padding-top:10px',
            column(6,id='overview',class='panel panel-default',div(class='panel-body',uiOutput('plots')) ), #style="background-color:#AA9C99;height:100%",plotlyOutput('splom')),
            column(3,id='focusedview',class='panel panel-default',
                          div(class='panel-heading','Main View'),
                          div(class='panel-body',plotlyOutput('selectedPlot',height=300),controlPanel)
                  ),
            column(2,class='panel panel-default',style='height:400px;overflow:scroll',uiOutput('selectedDataPoints',height=300))
              )
      )
  )
)

















