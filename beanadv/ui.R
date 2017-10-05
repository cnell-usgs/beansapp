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
              tabBox(width=3,
                     tabPanel('Using this app', 
                               p("This app demonstrates the comparison of 2 treatment groups, for which the proportion of 'Variable A' between the treatments is the measure of interest."),
                               p("For the 2 treatments and variables, use the left-hand side to rename what you are measuring then enter your data in the tables provided."),br(),
                               p("Pressing 'Run Data' will calculate the proportion of variable A and its' sumamry statistics, along with a plot showing mean values. Use inputs to change error values."),br(),
                               p("In the bottom right corner, a one-way ANOVA will be run comparing your two treatment groups."),br(),
                               actionButton("getdata","Run Data"),br(),
                               p("To replicate these analyses in R, download the data and code to produce the same outputs"),br(),
                               downloadButton("downloadData","Download Data"),
                               actionButton("opencode","See R code",icon=icon("calculator",lib="font-awesome"),
                               onclick="window.open('https://raw.githubusercontent.com/collnell/beans/master/beans_DIY','_blank')")),
                     tabPanel('Calculations',p('How to calculate stats for variable `x`'), tags$code('with R commands'),br(),br(),
                              tags$b('N = the sample size'),br(),tags$code('     length(x)'),br(),
                              tags$b(HTML(paste('S',tags$sup(2), ' =  variance', sep=''))),br(),tags$code('     var(x)'), br(), 
                              tags$b('mean  = sum(x)/N)'),br(),tags$code('     mean(x)'),br(),
                              tags$b('SD = standard deviation '), br(), tags$code('     sd(x)'),br(),
                              tags$b('SE = standard error'), br(), tags$code('     sqrt(var(x)/length(x))'), br(),
                              tags$b('df = degrees of freedom; N - 1'), br(), tags$code('     length(x)-1'), br(),
                              tags$b('T = t statistic, the number of SE for 95% confidence intervals at a given N'), br(), tags$code('     qt(.975, length(x)-1)'), br(),
                              tags$b('CI = confidence interval; T*SE'),br(),tags$code('     qt(.975, length(x)-1)*sqrt(var(x)/length(x))'), br(),
                              tags$b('lower.CI = mean - CI'), br(), tags$code('     mean(x) - qt(.975, length(x)-1)*sqrt(var(x)/length(x))'), br(),
                              tags$b('upper.CI = mean + CI'), br(), tags$code('     mean(x) + qt(.975, length(x)-1)*sqrt(var(x)/length(x))'), br()
                              )
                              ),
              box(title = "Data Entry", width=5,solidHeader = TRUE,status="primary",
                  column(width=5,textOutput("tr1"), rHandsontableOutput("table1")),column(width=1),
                  column(width=5,textOutput("tr2"),rHandsontableOutput("table2"))),
              tabBox(id = "plots",width = 4,
                     tabPanel("Plotting means", 
                              plotOutput('barplot'), 
                              radioButtons("errortype", "Error bars:",
                                           choices=c("95% Confidence interval"="ci","Standard error (SE)"="se","Standard deviation (S)"="sd"))),
                     tabPanel("Histogram",plotOutput("histo"),
                              checkboxInput("showmean","Show means", value=FALSE))
              )),
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
