library(shiny)
tabPanelAbout <- source("documentation.r")$value

shinyUI(fluidPage(
  
  
  headerPanel(
    HTML(
      '<div>
        Probabilistic Distribution Functions
      </div>'
    )
  ),
  
  
  sidebarPanel(
    
    wellPanel( 
      selectInput(
        "dist.type",
        label="Distribution type:",
        choices=list("Discrete","Continuous"),
        selected="Discrete") 
    ),
    
    wellPanel(  
      uiOutput("distName") 
    ),
    
    wellPanel(
      numericInput(
        "n",
        label="Sample size:",
        value=5000),
            
        uiOutput("dist1"),
        uiOutput("dist2"),
        uiOutput("dist3")
    ),
    
    wellPanel(
      uiOutput("sampDens"),
      uiOutput("BW"),
      downloadButton("dlCurPlot", "Download Plot"),
      downloadButton('dldat', 'Download Data')
    )
    
  ),
  
  
  mainPanel(
      
    tabsetPanel(
      tabPanel("Plot",plotOutput("plot",height="auto")),
      tabPanel("Summary",verbatimTextOutput("summary")),
      tabPanel("Table",tableOutput("table")),
      tabPanelAbout()
    )
        
  )
  
  
))
