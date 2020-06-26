library(shiny)
library(readxl)
library(tidyverse)
library(here)
#clear out previous iterations of results
rm(list = ls(pattern = "res."))

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
                            choices=c("Yes","No")),
               htmlOutput(outputId = "I_o_H",container = div,
                          inline = FALSE),
               actionButton("do2", "Finished answering, click here before moving to the next tab")
               ),
      tabPanel("Sensitivity of data",
               radioButtons(inputId="Q6", label="Is the content and detail of the biodiversity data such that their release would enable someone to carry out a harmful activity upon the taxon or attribute?", 
                            choices=c("Yes","No")),
               conditionalPanel(condition = "input.Q6=='Yes'", 
               radioButtons(inputId="Q7", label="Is information already in the public domain, or already known to those individuals or groups likely to undertake the harmful activity?", 
                            choices=c("Yes","No")),
               radioButtons(inputId="Q8", label="Would disclosure damage a partnership or relationship (especially where the maintenance of which is essential to helping achieve a specific conservation objective)?",
                            choices=c("Yes","No")),
               radioButtons(inputId="Q9", label=" Would disclosure allow the locations of sensitive features to be derived through combination with other publicly available information sources?",
                            choices=c("Yes","No"))),
               htmlOutput(outputId = "S_o_D",container = div,
                          inline = FALSE),
               actionButton("do3", "Finished answering, click here before moving to the next tab")
               ),
      tabPanel("Decision on release",
               radioButtons(inputId="Q10", label="On balance, considering criteria 1 to 3 above and any important wider context, will releasing the information increase the risk of environmental harm or harm to a living person?", 
                            choices=c("Yes","No")),
               conditionalPanel(condition = "input.Q10=='Yes'",
                                radioButtons(inputId="Q11", label="Is the taxon distinctive and of high biological significance, under high threat from exploitation/ disease or other identifiable threat where even general locality information may threaten the taxon? Or could the release of any part of the record cause irreparable harm to the environment or to an individual?", 
                            choices=c("Yes","No"))),
               conditionalPanel(condition = "input.Q10=='No'",
                                radioButtons(inputId="Q14", label="Is the taxon subject to low to medium threat if precise locations (i.e. locations with a precision greater than 0.001 degrees or 100m) become publicly available and where there is some risk of collection or deliberate damage?", 
                                             choices=c("Yes","No"))),
               # conditionalPanel(condition = "input.Q11=='No'",
               #                  radioButtons(inputId="Q12", label="Is the taxon such that the provision of precise locations at finer than 0.1 degrees (~10 km) would subject the taxon to threats such as disturbance and exploitation? Or does the record include highly sensitive information, the release of which could cause extreme harm to an individual or the environment?", 
               #                               choices=c("Yes","No"))),
               # conditionalPanel(condition = "input.Q12=='No'",
               # radioButtons(inputId="Q13", label=" Is the taxon such that the provision of precise locations at finer than 0.01 degrees (~1 km) would subject the species to threats such as collection or deliberate damage? Or does the record include sensitive information, the release of which could cause harm to an individual or the environment?", 
               #              choices=c("Yes","No"))),
               # conditionalPanel(condition = "input.Q13=='No'",
               # radioButtons(inputId="Q14", label="Is the taxon subject to low to medium threat if precise locations (i.e. locations with a precision greater than 0.001 degrees or 100m) become publicly available and where there is some risk of collection or deliberate damage?", 
               #              choices=c("Yes","No"))),
               htmlOutput(outputId = "D_o_R",container = div,
                          inline = FALSE),
               actionButton("do4", "All finished? click here")
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
  
  DataQ4 <- reactive({
    if(input$Q4=="Yes"){
      out<-GBIF_cat[4,4]
      out<-rename(out, nm= 1)
    }else{
      out<-GBIF_cat[4,5]
      out<-rename(out, nm= 1)
    }
    as.data.frame(out)
  })
  
  
  DataQ5 <- reactive({
    if(input$Q5=="Yes"){
      out<-GBIF_cat[5,4]
      out<-rename(out, nm= 1)
    }else{
      out<-GBIF_cat[5,5]
      out<-rename(out, nm= 1)
    }
    as.data.frame(out)
  })
  
  DataQ6 <- reactive({
    if(input$Q6=="Yes"){
      out<-GBIF_cat[6,4]
      out<-rename(out, nm= 1)
    }else{
      out<-GBIF_cat[6,5]
      out<-rename(out, nm= 1)
    }
    as.data.frame(out)
  })
  
  DataQ7<-reactive({
    if(input$Q6=="Yes" & input$Q7=="Yes"){
      out<-GBIF_cat[7,4]
      out<-rename(out, nm= 1)
    }else{
      if(input$Q6=="Yes" & input$Q7=="No"){
        out<-GBIF_cat[7,5]
        out<-rename(out, nm= 1)
      }else{
        out<-data.frame("nm"=c(""))}
    }
    as.data.frame(out)  
  })
  
  DataQ8<-reactive({
    if(input$Q6=="Yes" & input$Q8=="Yes"){
      out<-GBIF_cat[8,4]
      out<-rename(out, nm= 1)
    }else{
      if(input$Q6=="Yes" & input$Q8=="No"){
        out<-GBIF_cat[8,5]
        out<-rename(out, nm= 1)
      }else{
        out<-data.frame("nm"=c(""))}
    }
    as.data.frame(out)  
  })
  
  DataQ9<-reactive({
    if(input$Q6=="Yes" & input$Q9=="Yes"){
      out<-GBIF_cat[9,4]
      out<-rename(out, nm= 1)
    }else{
      if(input$Q6=="Yes" & input$Q9=="No"){
        out<-GBIF_cat[9,5]
        out<-rename(out, nm= 1)
      }else{
        out<-data.frame("nm"=c(""))}
    }
    as.data.frame(out)  
  })
  
  DataQ10<-reactive({
        if(input$Q10=="Yes"){
        out<-GBIF_cat[10,4]
        out<-rename(out, nm= 1)
      }else{
        out<-GBIF_cat[10,5]
        out<-rename(out, nm= 1)
      }
      as.data.frame(out)
    })
  DataQ11<-reactive({
    if(input$Q11=="Yes"){
      out<-GBIF_cat[11,4]
      out<-rename(out, nm= 1)
    }else{
        out<-data.frame("nm"=c(""))}
    as.data.frame(out)  
  })
  
  DataQ12<-reactive({
    if(input$Q12=="Yes"){
      out<-GBIF_cat[12,4]
      out<-rename(out, nm= 1)
    }else{
      out<-data.frame("nm"=c(""))}
    as.data.frame(out)  
  })
  
  DataQ13<-reactive({
    if(input$Q13=="Yes"){
      out<-GBIF_cat[13,4]
      out<-rename(out, nm= 1)
    }else{
      out<-data.frame("nm"=c(""))}
    as.data.frame(out)  
  })
  
  DataQ14<-reactive({
    if(input$Q10=="No" | input$Q13=="No" & input$Q14=="Yes"){
      out<-GBIF_cat[14,4]
      out<-rename(out, nm= 1)
    }else{
      if(input$Q10=="No" | input$Q13=="No" & input$Q14=="No"){
        out<-GBIF_cat[14,5]
        out<-rename(out, nm= 1)
      }else{
        out<-data.frame("nm"=c(""))}
    }
    as.data.frame(out)  
  })
 output$R_o_H<-renderUI({HTML(paste(DataQ1()$nm, DataQ2()$nm, DataQ3()$nm, sep='<br/>'))})
 output$I_o_H<-renderUI({HTML(paste(DataQ4()$nm, DataQ5()$nm, sep='<br/>'))})
 output$S_o_D<-renderUI({HTML(paste(DataQ6()$nm, DataQ7()$nm, DataQ8()$nm, DataQ9()$nm, sep='<br/>'))})
 output$D_o_R<-renderUI({HTML(paste(DataQ10()$nm, DataQ11()$nm, DataQ12()$nm, DataQ13()$nm, DataQ14()$nm, sep='<br/>'))})
 
 
 observeEvent(input$do1, {
   resultData1 <<- paste(DataQ1()$nm, DataQ2()$nm, DataQ3()$nm)
   
 })
 observeEvent(input$do2, {
   resultData2 <<- paste(DataQ4()$nm, DataQ5()$nm)
   
 })
 observeEvent(input$do3, {
   resultData3 <<- paste(DataQ6()$nm, DataQ7()$nm, DataQ8()$nm, DataQ9()$nm)
   
 })
 observeEvent(input$do4, {
   resultData4 <<- paste(DataQ10()$nm, DataQ11()$nm, DataQ12()$nm, DataQ13()$nm, DataQ14()$nm)
   
 })
 
 }
shinyApp(ui, server)
