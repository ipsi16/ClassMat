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
           numericInput(inputId='k',label=NULL,value=1,min=1),
           div(id='renameDiv',
             textInput('renameTxt',label="",value=""),
             actionButton('renameBtn','Rename')
           )
           
           
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
      actionButton('submitBtn', "Submit"),
      downloadButton('download',"Save")
      #actionButton('save','Save',icon=icon("save",lib="glyphicon"))
      ),
    div(id='display-panel',class='panel panel-default',
        fluidRow(
          uiOutput(class='timeline bg-faded','timeline')
        ),
        fluidRow(style='padding-top:10px',
            column(6,id='overview',class='panel panel-default',div(class='panel-body',uiOutput('plots')),div(id='viewControl',selectInput('viewOrderSlct','',''))),#,div(id='try',selectInput('tryInp','',c(1,2,3))) ), #style="background-color:#AA9C99;height:100%",plotlyOutput('splom')),
            column(3,id='focusedview',class='panel panel-default',
                          div(class='panel-heading','Main View'),
                          div(class='panel-body',plotlyOutput('selectedPlot',height=300),
                              uiOutput('hoveredDpView',width="100px",height="100px"),
                              controlPanel)
                  ),
            column(2,class='panel panel-default',style='height:400px;overflow:scroll',uiOutput('selectedDataPoints',height=300))
              )
      )
  )
)

















