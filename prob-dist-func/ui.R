library(shiny)

shinyUI(fluidPage(
  
  
  headerPanel(
    HTML('Probabilistic Distribution Functions')
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
      tabPanel("Documentation",
               h4('Description'),
               
               p(style="text-align:justify",
                 'This ',a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"),' application is targeted at data analysts,
                 math enthusiasts and statisticians. Like the name mentions, this app is all about probabilistic distribution functions. 
                 There are two types of distributions shown here, as you can see from the `Distribution Type` dropdown on the left panel.
                 They are as follows,'),
               
               
               HTML('<ul>
                    <li> Discrete Distribution Functions </li>
                    <li> Continuous Distribution Functions </li>
                    </ul>
                    '),
               
               p(style="text-align:justify",'For each of the above options, a wide variety of distribution functions are presented in front 
                 of you in the form of radio buttons on the left hand panel. For each distribution function, on selecting it, the app performs
                 random sampling based on the sample size and plots the distribution on the right hand section. For each distribution function, 
                 the mathematical representation of the function is presented in the top of the plot on the right side, along with the mean and 
                 standard deviation of the distribution. If the distribution is continuous, you can also plot the density curve on the plot. 
                 Each distribution comes with a set of parameters which you can tweak and tinker with based on the function and if you wish to,
                 you can download the plot and the dataset using the buttons provided.'),
               
               br(),
               
               h4('Operating Instructions'),
               
               p(style="text-align:justify",
                 'You can follow these steps mentioned below to get a feel of how things work,'),
               
               
               HTML('<ul>
                    <li> Select a `Distribution Type` from the dropdown on the left panel. </li>
                    <li> Select a `Ditribution function` of your choice from the options presented. </li>
                    <li> View the plot along with other details on the right side. </li>
                    <li> View the summary statistics and raw data by clicking on the tabs on the top right. </li>
                    <li> Tweak and tinker with parameters like `Sample size` and others based on the distribution function. </li>
                    <li> Notice how the plot changes with change in parameters. </li>
                    <li> Save the plot and the data set using the download buttons on the left panel if you feel like. </li>
                    <li> Plot a density curve on the plot using the checkbox on the left panel if it is a continuous plot. </li>
                    </ul>
                    '),
               
               br(),
               
               h4('Important Points'),
               
               p(style="text-align:justify",
                 'Some important points regarding the app are mentioned below,'),
               
               HTML('<ul>
                    <li> You are most welcome to tinker with the parameters but illegal values will throw an error. </li>
                    <li> While selecting a huge sample size, viewing the raw dataset might take some time. </li>
                    <li> Refresh the page in case of any unexpected errors and kindly notify me since its still in development phase. </li>
                    </ul>
                    '),
               
               
               br(),
               
               h4('References'), 
               
               p(HTML('<ul>'),
                 HTML('<li>'),a('R', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
                 HTML('<li>'),a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
                 HTML('<li>'),a('VGAM', href="http://cran.r-project.org/web/packages/VGAM/index.html", target="_blank"),HTML('</li>'),
                 HTML('</ul>')
               ),
               
               br(),
               h5('View Source Code'),
               a('Github Repo', href="https://github.com/dipanjanS/MyShinyApps/tree/master/prob-dist-func", target="_blank"),
               br()
               
               )
    )
        
  )
  
  
))
