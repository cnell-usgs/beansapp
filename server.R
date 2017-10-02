library(shiny)
library(ggplot2)
library(rhandsontable)
library(dplyr)
library(devtools)
library(readr)
library(RCurl)

ttable<-read.csv("https://raw.githubusercontent.com/collnell/beans/master/ttable.csv")#ttable
source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme

##functions
se <- function(x) sqrt(var(x)/length(x))
ci95 <-function(x) se(x)*ttable[ttable$n == length(x),2]

shinyServer(function(input,output){
  
  ###reactive variable names & variables
  output$tr1<-renderText({input$treat1})
  output$tr2<-renderText({input$treat2})
  output$var1<-renderText({input$var1})
  output$var2<-renderText({input$var2})
  
  ##generate data
  values <- reactiveValues(df1 = data.frame(variable1 = as.numeric(rep(NA, 50)),
                                                 variable2 = as.numeric(rep(NA, 50)),
                                            Treatment = rep('B', 50),
                                            Proportion1=rep(NA,50)),
                           df2 =data.frame(variable1 = as.numeric(rep(NA, 50)),
                                                variable2 = as.numeric(rep(NA, 50)),
                                           Treatment = rep('A',50),
                                           Proportion1 = rep(NA,50)),
                           df_summary = data.frame(Treatment = NA,
                                                   mean=NA,
                                                   se=NA))
  
  ##generate input tables
  output$table1 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
    rhandsontable(values$df1%>%dplyr::select(variable1, variable2, Proportion1),width=500,height=485,colHeaders =c(input$var1,input$var2,pvar1))
  })
  output$table2 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
    rhandsontable(values$df2%>%dplyr::select(variable1, variable2, Proportion1),width=500,height=485,colHeaders =c(input$var1,input$var2,pvar1))
  })
  
  observeEvent(input$getdata, { 
    values$df1<-hot_to_r(input$table1)%>%mutate(Proportion1 = variable1/(variable1+variable2))##update df
    values$df2<-hot_to_r(input$table2)%>%mutate(Proportion1 = variable1/(variable1+variable2))
    
    values$df1$Treatment<-rep(input$treat1,length(values$df1$variable1))#make treatment var
    values$df2$Treatment<-rep(input$treat2,length(values$df2$variable1))
    
    values$df_data_all<-rbind(values$df1,values$df2)#bind
    values$df_data_all$Proportion1<-values$df_data_all$variable1/(values$df_data_all$variable1+values$df_data_all$variable2)#calc proportions
    values$df_data_all$Proportion2<-values$df_data_all$variable2/(values$df_data_all$variable1+values$df_data_all$variable2)
    
    ##summarize data for plotting, output table
    values$df_summary<-values$df_data_all%>%na.omit()%>%
      group_by(Treatment)%>%
      summarize(mean=mean(Proportion1), se= se(Proportion1))
    
    values$previous<-values$df_data_all
    values$df_data<-values$df_data_all%>%na.omit()
    
    tresultvar1<-t.test(x = values$df1$Proportion1)
    tresultvar2<-t.test(x=values$df2$Proportion1)
    
    values$aov.model<-aov(Proportion1~Treatment,data=values$df_data)
    conout<-confint(values$aov.model)
    
    ci.df<-data.frame(LCL = conout[1,1], conout[2,1],
                      UCL = conout[1,2], conout[2,2])
  
    
    values$summaryoutput<-values$df_data_all%>%na.omit()%>%
      group_by(Treatment)%>%
      summarize(mean=mean(Proportion1),
                N = length(Proportion1),
                SD = sd(Proportion1),
                SE= se(Proportion1))%>%
      mutate(LCL = c(tresultvar1$conf.int[1], tresultvar2$conf.int[1]),
             UCL = c(tresultvar1$conf.int[2], tresultvar2$conf.int[2]), 
             CI = UCL - LCL, CI.fig =CI/2)#%>%
      #mutate(lower.CI = mean - qt(1 - (0.05 / 2), N - 1) * SE,
       #      upper.CI = mean + qt(1 - (0.05 / 2), N - 1) * SE)
  })
  
  ##barplot
  output$barplot<-renderPlot({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' to visualize treatment means")
    )
    
    error<-switch(input$errortype,##reactive error bars
                  se=values$summaryoutput$SE,
                  sd=values$summaryoutput$SD,
                  ci=values$summaryoutput$CI.fig)
    #plot
    if (error == 'ci'){
      ggplot(values$summaryoutput, aes(x=Treatment, y=mean))+
        geom_bar(stat='identity', fill='grey')+
        theme_mooney()+theme(legend.position='none')+
        geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.2)
      
    } else {
    ggplot(values$summaryoutput, aes(x=Treatment, y=mean))+
      geom_bar(stat='identity', fill='grey')+
      theme_mooney()+theme(legend.position='none')+
      geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.2)
    }
  })
  
  ##summary stats
  output$summary_table<-renderRHandsontable({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' to generate summary data")
    )
    rhandsontable(values$summaryoutput%>%dplyr::select(-CI.fig), readOnly=TRUE)
  })
  
  output$anovatable<-renderPrint({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' for ANOVA test results")
    )
    v1<-paste0(input$var1)
    v2<-paste0(input$var2)
    p1<-paste0("Proportion (",input$var1,")")
    p2<-paste0("Proportion (",input$var2,")")
  
    aov.model<-aov(Proportion1~Treatment,data=values$df_data)
    br()
    print(summary(aov.model))
  })
  ##download csv of data
  output$downloadData <-downloadHandler(
    filename = function() {
      paste("data-",Sys.Date(),".csv", sep="")
    },
    content = function(file){
      write.csv(values$df_data,file, row.names=FALSE)
    }
  )
  output$downloadplotr <-downloadHandler(##this should be redone
    filename=function(){
      paste("ggplot_bar.r")
    },
    content =function(file){
      write_rds(bar.plot,file,compress="none")
    }
  )
  
 
})