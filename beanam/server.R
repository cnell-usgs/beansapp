library(reshape2)
library(shiny)
library(ggplot2)
library(rhandsontable)
library(dplyr)
library(devtools)
library(readr)
library(RCurl)
library(shinydashboard)

##functions
se <- function(x) sqrt(var(x)/length(x))

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
    pvar1<-paste0("Prop(",input$var1,")")
    rhandsontable(values$df1%>%dplyr::select(variable1, variable2, Proportion1),width=500,height=485,colHeaders =c(input$var1,input$var2,pvar1))
  })
  output$table2 = renderRHandsontable({
    pvar1<-paste0("Prop(",input$var1,")")
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
    values$df_data<-values$df_data_all%>%na.omit(Proportion1)
    
    #tresultvar1<-t.test(x = values$df1$Proportion1)
    #tresultvar2<-t.test(x=values$df2$Proportion1)
    
    values$aov.model<-aov(lm(Proportion1~Treatment,data=values$df_data))
    #conout<-confint(values$aov.model)
    
    #ci.df<-data.frame(LCL = conout[1,1], conout[2,1],
                     # UCL = conout[1,2], conout[2,2])
  
    #values$ttout<-t.test(values$df1$Proportion1, values$df2$Proportion1)
    
    values$summaryoutput<-values$df_data_all%>%
      na.omit()%>%
      group_by(Treatment)%>%
      summarize(N = length(Proportion1),
                VAR=var(Proportion1), 
                mean=mean(Proportion1),
                SD = sd(Proportion1),
                SE= se(Proportion1))%>%
      mutate(df = N-1, 
            sed = SD/sqrt(N),
            `T` = qt(.975, df),
            error=sed*`T`,
            CI = `T`*SE, 
            lower.CI = mean-CI, 
            upper.CI = mean+CI)
    
    values$pvar1<-paste0("Prop(",input$var1,")")
    
  })
  
  ##barplot
  output$barplot<-renderPlot({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' to visualize treatment means")
    )
    
    values$summaryoutput$error<-switch(input$errortype,##reactive error bars
                  se=values$summaryoutput$SE,
                  sd=values$summaryoutput$SD,
                  ci=values$summaryoutput$CI)
    err.melt<-melt(values$summaryoutput%>%mutate(SEm=mean+SE, SDm=mean+SD)%>%dplyr::select(SEm,SDm,upper.CI))
    
    #plot
    ggplot(values$summaryoutput, aes(x=Treatment, y=mean))+
      geom_bar(stat='identity', fill='grey')+theme_minimal()+
      theme(legend.position='none')+
      geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.2)+
      ylim(NA,max(err.melt$value)*1.1)+
      labs(y=paste0('Mean ', values$pvar1))

  })
  
  ##histogram of entered data
  output$histo<-renderPlot({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' to see data distribution")
    )
    
    if(input$showmean ==TRUE){ ##add line for means
      mean1<-values$summaryoutput$mean[1]
      mean2<-values$summaryoutput$mean[2]
      
      ggplot(data=values$df_data_all, aes(group=Treatment, x=Proportion1))+
        geom_density(data=values$df_data_all,aes(fill=Treatment),alpha=0.5)+theme_minimal()+
        theme(legend.position='none')+
        geom_vline(aes(xintercept=mean1), lty='dashed')+
        geom_vline(aes(xintercept=mean2), lty='dotted')+
        labs(x=values$pvar1, y= 'Frequency')
      
    } else {
      ggplot(values$df_data_all, aes(group=Treatment, x=Proportion1))+
        geom_density(aes(fill=Treatment),alpha=0.5)+theme_minimal()+
        theme(legend.position='none')+labs(x=values$pvar1, y= 'Frequency')
    }
  })
  ##student's t distribution
  output$tdist<-renderPlot({
    #x<-seq(-4,4, length=100)
    #hx<-dnorm(x)
    Tval<-qt(.975, input$dfs)
    Tval<-qt(1-(input$alpha/2), input$dfs)
    CI<-1-input$alpha
    CI.end<-(1-CI)/2
  
    
    ggplot(data.frame(x=c(-4,4)), aes(x=x))+
      stat_function(fun=dt, args=list(df=input$dfs), size=2, color='red')+
      theme_minimal()+labs(x='Dependent variable',y='Frequency')+
      geom_vline(aes(xintercept=0+Tval), lty='dashed', size=1)+
      geom_vline(aes(xintercept=0-Tval), lty='dashed', size=1)+
      geom_segment(aes(x=0-Tval, xend=0+Tval, y=.45, yend=.45), size=1, arrow = arrow(length = unit(0.4, "cm")))+
      geom_segment(aes(x=0+Tval, xend=0-Tval, y=.45, yend=.45), size=1, arrow = arrow(length = unit(0.4, "cm")))+
      ylim(0,.5)+theme(axis.text=element_blank())+
      annotate('text', x=0, y=0.47, label='T x SE', size=7)+
      annotate('text', x=0, y=0.05, label=paste(CI), size=6)+
      annotate('text', x=0-Tval, y=0.05, label=paste(CI.end), size=6, hjust=1.5)+
      annotate('text', x=0+Tval, y=0.05, label=paste(CI.end), size=6, hjust=-.5)+
      annotate('text', x=0, y=0.42, label=paste0('T = ',round(Tval,2)), size=6)
    
    
  })
  ##summary stats
  output$summary_table<-renderRHandsontable({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' to generate summary data")
    )
    siggy<-expression(paste(sigma))
    rhandsontable(values$summaryoutput%>%dplyr::select(-error, -sed, -CI, -Treatment),
                  colHeaders=c('N', 'S', 'mean', 'SD', 'SE', 'df', 'T', 'lower.CI', 'upper.CI'),
                  rowHeaders=c(values$summaryoutput$Treatment[1],values$summaryoutput$Treatment[2]),rowHeaderWidth=130,
                  readOnly=TRUE)
  })
  
  ##equations for calculations
  output$calcs<-renderUI({
    withMathJax(
      helpText($$\( \frac{1}{n} \sum_{i=i}^{n} x_{i} \))
    )
    
  })
  
  
  ##anova table
  output$anovatable<-renderPrint({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' for ANOVA test results")
    )
    v1<-paste0(input$var1)
    v2<-paste0(input$var2)
    p1<-paste0("Proportion (",input$var1,")")
    p2<-paste0("Proportion (",input$var2,")")
    
    aov.model<-aov(lm(Proportion1~Treatment,data=values$df_data))
    print(aov.model)
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