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
                              tags$b('N = the sample size'),br(),tags$code('     N <- length(x)'),br(),
                              tags$b(HTML(paste('S',tags$sup(2), ' =  variance', sep=''))),br(),tags$code('     var(x)'), br(), 
                              tags$b('mean  = sum(x)/N)'),br(),tags$code('     mean(x)'),br(),
                              tags$b('SD = standard deviation '), br(), tags$code('     SD <- sd(x)'),br(),
                              tags$b('SE = standard error'), br(), tags$code('     SE <- sqrt(var(x)/length(x))'), br(),
                              tags$b('df = degrees of freedom; N - 1'), br(), tags$code('     DF <- N-1'), br(),
                              tags$b('alpha = significance level'),br(),tags$code('alpha <- 0.05'),br(),
                              tags$b('T = t statistic, number of SE for CIs of given alpha and DF'), br(), tags$code('     Tstat <- qt(1-(alpha/2), DF)'), br(),
                              tags$b('CI = confidence interval; T*SE'),br(),tags$code('     CI <- Tstat*SE)'), br(),
                              tags$b('lower.CI = mean - CI'), br(), tags$code('     lower.CI <- mean(x) - CI'), br(),
                              tags$b('upper.CI = mean + CI'), br(), tags$code('     upper.CI <- mean(x) + CI'), br()
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
                              checkboxInput("showmean","Show means", value=FALSE)),
                     tabPanel("t distribution", plotOutput('tdist'), 
                              column(width=6, numericInput('dfs', 'Set DF:', value=20, min=0, step=1)),
                              column(width=6, numericInput('alpha','Set alpha (0 - 1)', value=0.05, min=0, max=1, step=0.01)),br(), 
                              p('The t-value is dependent on sample size and alpha. Alpha is the significance level, or 1 - CI. For a significance level (alpha) of 0.05, we use a 95% CI.'))
              )),
            fluidRow(
              tabBox(id='stats', width=6,
                tabPanel("Data Summary", rHandsontableOutput("summary_table"),br(),br(),br()),
                tabPanel('By hand', radioButtons('Select one:', 'handcalc', choices=c('N','DF','S','mean','SD','SE','T','CI'), selected='N'),
                         uiOutput('calcs'))
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
