library(shinydashboard)
library(rhandsontable)

sidebar <- dashboardSidebar(
  sidebarMenu(
    textInput("treat1","Treatment 1:",value="Treatment 1"),
    textInput("treat2","Treatment 2:", value="Treatment 2"),
    textInput("var1","Variable A:", value= "A"),
    textInput("var2","Variable B:",value= "B")
    
  ) 
)

body <- dashboardBody(
            fluidRow(
              box(title="Using this app",width=3,status="primary",
                  p("This app demonstrates the comparison of 2 treatment groups, for which the proportion of 'Variable A' between the treatments is the measure of interest."),
                  p("For the 2 treatments and variables, use the left-hand side to rename what you are measuring then enter your data in the tables provided."),br(),
                  p("Pressing 'Run Data' will calculate the proportion of variable A and its' sumamry statistics, along with a plot showing mean values. Use inputs to change error values."),br(),
                  p("In the bottom right corner, a one-way ANOVA will be run comparing your two treatment groups."),br(),
                  actionButton("getdata","Run Data"),br(),
                  p("To replicate these analyses in R, download the data and code to produce the same outputs"),br(),
                  downloadButton("downloadData","Download Data"),
                  actionButton("opencode","See R code",icon=icon("calculator",lib="font-awesome"),
                               onclick="window.open('https://raw.githubusercontent.com/collnell/beans/master/beans_DIY','_blank')")),
              
              box(title = "Data Entry", width=5,solidHeader = TRUE,status="primary",
                  column(width=5,textOutput("tr1"), rHandsontableOutput("table1")),
                  column(width=4,textOutput("tr2"),rHandsontableOutput("table2"))),
              
              box(title='Plot', width=4, plotOutput('barplot'),
              radioButtons("errortype", "Error bars:", 
                           choices=c("95% Confidence interval"="ci","Standard error (SE)"="se","Standard deviation (S)"="sd")))
              ),
            fluidRow(
              box(title="Data Summary", width = 6,status="primary", 
               rHandsontableOutput("summary_table"),br(),br(),br()
              ),
              box(title="One-way ANOVA", width=6, status="primary",
                  verbatimTextOutput("anovatable")))
)

# Put them together into a dashboardPage
dashboardPage(skin = "red",
              dashboardHeader(title = "beans"),
              sidebar,
              body
)
