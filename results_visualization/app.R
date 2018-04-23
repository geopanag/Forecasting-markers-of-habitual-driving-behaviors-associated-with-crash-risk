libs = c("dplyr","ggplot2","tidyr","gridExtra","ROCR","shiny")
lapply(libs, require, character.only = TRUE)


dat = read.csv("data_5.csv")

dat$drive[dat$drive==1]  = "Normal"
dat$drive[dat$drive==2]  = "Cognitive"
dat$drive[dat$drive==3]  = "Sensorimotor"
dat$drive[dat$drive==4]  = "Emotional"
dat$drive[dat$drive==5]  = "Failure"


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Prediction visualization of on road distraction and aggressiveness"),
  
  fluidPage(
    fluidRow(
      column(3,
             selectInput("subject",
                         "Subjects:",
                         unique(dat$subject)),
             selectInput("positive",
                         "Show only Positive class?",
                         c(FALSE,TRUE)),
             tableOutput('acc_table')),
      column(9,
             plotOutput("subject_plot"))
    )
 
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  sdat = reactive({
    dat %>% filter(subject==input$subject)
  })
  
  output$subject_plot <- renderPlot({
    sdat = sdat()
    
    #---------------  Set Distances
    sdat$distraction_prediction = ifelse(sdat$distraction_prediction>=0.5,0.7,0.1)
    
    sdat$distraction = ifelse(sdat$distraction>=0.5,0.6,0)
    
    sdat$aggressiveness_prediction = ifelse(sdat$aggressiveness_prediction>=0.5,1.1,0.4)
    sdat$aggressiveness = ifelse(sdat$aggressiveness>=0.5,1,0.3)
    
    if(input$positive){
      
      sdat %>% 
        select(distraction,distraction_prediction,aggressiveness,aggressiveness_prediction,drive) %>% 
        mutate(Frames = 1:n()) %>% gather(., Variable, Value, -Frames,-drive) %>% 
        ggplot(.,aes(y=Value,x=Frames,color=Variable))+geom_point(aes(shape=Variable),size=1)+ylab("")+xlab("")+
        theme(axis.text.y = element_blank(),axis.text.x = element_blank(),
              axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),
              legend.title=element_blank(),
              legend.position="bottom",strip.text.x = element_text(size = 15),
              legend.text=element_text(size=15))+
        scale_shape_manual(values = c(15, 3, 15, 3),
                           labels=c("Aggressiveness","Prediction of Aggressiveness","Distraction","Prediction of Distraction"))+
        scale_color_manual(values=c("#56B4E9","#56B4E9","#E69F00","#E69F00"),
                           labels=c("Aggressiveness","Prediction of Aggressiveness","Distraction","Prediction of Distraction"))+
        ylim(0.5,1.2)+facet_wrap(~drive,scales = "free_x")+
        guides(colour = guide_legend(override.aes = list(size=6)))
      
    }else{
      sdat %>% 
        select(aggressiveness,aggressiveness_prediction,distraction,distraction_prediction,drive) %>% 
        mutate(Frames = 1:n()) %>% gather(., Variable, Value, -Frames,-drive) %>% 
        ggplot(.,aes(y=Value,x=Frames,color=Variable))+geom_point(aes(shape=Variable),size=1)+ylab("")+xlab("")+
        theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),legend.title=element_blank(),
              legend.text=element_text(size=15),
              legend.position="bottom",strip.text.x = element_text(size = 15))+
        scale_shape_manual(values = c(15, 3, 15, 3),
                           labels=c("Aggressiveness","Prediction of Aggressiveness","Distraction","Prediction of Distraction"))+
        scale_color_manual(values=c("#56B4E9","#56B4E9","#E69F00","#E69F00"),
                           labels=c("Aggressiveness","Prediction of Aggressiveness","Distraction","Prediction of Distraction"))+
        geom_line(y=0.5,size=1,colour="black")+
        facet_wrap(~drive,scales = "free_x")+
        guides(colour = guide_legend(override.aes = list(size=6)))
    }

  })
  
  output$acc_table <- renderTable({
    sdat = sdat()
    acc_agg = format(round(sum(ifelse(sdat$aggressiveness_prediction>=0.5,1,0)==sdat$aggressiveness)/nrow(sdat), 3), nsmall = 3)
    acc_str = format(round(sum(ifelse(sdat$distraction_prediction>=0.5,1,0)==sdat$distraction)/nrow(sdat), 3), nsmall = 3)
    
    if(all(sdat$aggressiveness==sdat$aggressiveness[1])){
      auc_agg = "-"
    }else{
      pred_agg = prediction(sdat$aggressiveness_prediction,sdat$aggressiveness)
      auc_agg = format(round(unlist(slot( performance(pred_agg,"auc"),"y.values")),3),nsmall=3)
    }
    
    if(all(sdat$distraction==sdat$distraction[1])){
      auc_str  = "-"
    }else{
      pred_str = prediction(sdat$distraction_prediction,sdat$distraction)
      auc_str  = format(round(unlist(slot( performance(pred_str,"auc"),"y.values")),3),nsmall=3)
    }
    
    #--------------- Count Accuracy
    cbind(Variable = c("Aggresiveness","Distraction"),
                      Accuracy = c(acc_agg,acc_str),
                      AUC = c(auc_agg,auc_str))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

