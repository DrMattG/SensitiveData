library(shiny)
library(tidyverse)
library(here)
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Risk of harm",radioButtons(inputId="Q1", label="Is the taxon subject to harmful activity?", 
                            choices=c("Yes","No")),
               
               conditionalPanel(condition = "input.Q1=='Yes'",
                                radioButtons(inputId="Q2", label=" Is there established evidence of current or recent occurrences of the harmful human activity?", 
                                             choices=c("Yes","No")),
                                radioButtons(inputId="Q3", label="  Will availability of related biodiversity data increase the likelihood of the harmful human activity taking place?", 
                                             choices=c("Yes","No"))),
               htmlOutput(outputId = "R_o_H",container = div,
                          inline = FALSE),
               actionButton("do1", "Finished answering, click here before moving to the next tab")
               ),
      tabPanel("Impact of harm",
               radioButtons(inputId="Q4", label=" Does the taxon have characteristics that make it significantly vulnerable to the harmful human activity?  ", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q5", label="Is the taxon vulnerable to harmful human activity over its total range, or are there areas (such as in conservation zones, or other parts of the world) where the taxon is not at the same level of risk?   ", 
                            choices=c("Yes","No"))
               ),
      tabPanel("Sensitivity of data",
               radioButtons(inputId="Q6", label="Is the content and detail of the biodiversity data such that their release would enable someone to carry out a harmful activity upon the taxon or attribute?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q7", label="Is information already in the public domain, or already known to those individuals or groups likely to undertake the harmful activity?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q8", label="Would disclosure damage a partnership or relationship (especially where the maintenance of which is essential to helping achieve a specific conservation objective)?",
                            choices=c("Yes","No")),
               radioButtons(inputId="Q9", label=" Would disclosure allow the locations of sensitive features to be derived through combination with other publicly available information sources?",
                            choices=c("Yes","No"))),
      tabPanel("Decision on release",
               radioButtons(inputId="Q10", label="On balance, considering criteria 1 to 3 above and any important wider context, will releasing the information increase the risk of environmental harm or harm to a living person?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q11", label="Is the taxon distinctive and of high biological significance, under high threat from exploitation/ disease or other identifiable threat where even general locality information may threaten the taxon? Or could the release of any part of the record cause irreparable harm to the environment or to an individual?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q12", label="Is the taxon such that the provision of precise locations at finer than 0.1 degrees (~10 km) would subject the taxon to threats such as disturbance and exploitation? Or does the record include highly sensitive information, the release of which could cause extreme harm to an individual or the environment?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q13", label=" Is the taxon such that the provision of precise locations at finer than 0.01 degrees (~1 km) would subject the species to threats such as collection or deliberate damage? Or does the record include sensitive information, the release of which could cause harm to an individual or the environment?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q14", label="Is the taxon subject to low to medium threat if precise locations (i.e. locations with a precision greater than 0.001 degrees or 100m) become publicly available and where there is some risk of collection or deliberate damage?", 
                            choices=c("Yes","No"))
               )
      )
      )
    )


GBIF_cat <- read_excel(here("/data/GBIF_cat.xlsx"))


server <- function(input, output) {
  DataQ1 <- reactive({
    if(input$Q1=="Yes"){
      out<-GBIF_cat[1,4]
      out<-rename(out, nm= 1)
    }else{
      out<-GBIF_cat[1,5]
      out<-rename(out, nm= 1)
    }
    as.data.frame(out)
     })
  
  DataQ2<-reactive({
    if(input$Q1=="Yes" & input$Q2=="Yes"){
      out<-GBIF_cat[2,4]
      out<-rename(out, nm= 1)
    }else{
      if(input$Q1=="Yes" & input$Q2=="No"){
      out<-GBIF_cat[2,5]
      out<-rename(out, nm= 1)
    }else{
      out<-data.frame("nm"=c(""))}
      }
    as.data.frame(out)
    
  })
  
  DataQ3<-reactive({
    if(input$Q1=="Yes" & input$Q3=="Yes"){
      out<-GBIF_cat[3,4]
      out<-rename(out, nm= 1)
    }else{
      if(input$Q1=="Yes" & input$Q3=="No"){
        out<-GBIF_cat[3,5]
        out<-rename(out, nm= 1)
      }else{
        out<-data.frame("nm"=c(""))}
    }
    as.data.frame(out)
    
  })
  
 output$R_o_H<-renderUI({HTML(paste(DataQ1()$nm, DataQ2()$nm, DataQ3()$nm, sep='<br/>'))})
 observeEvent(input$do1, {
   resultData1 <<- paste(DataQ1()$nm, DataQ2()$nm, DataQ3()$nm)
   
 })
 }
shinyApp(ui, server)
