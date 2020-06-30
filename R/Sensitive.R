#
#
#

Sensitive_locations<-function(data, XField, YField,
                              
                              Cat="NS"){
  
  library(tidyverse)
  
  library(encryptr)
  
  out<-data
  
  
  ### Switch: Cats CHECK THE ROUNDING
  
  switch(Cat,
         
         "2"={
           XField<-enquo(XField)
           YField<-enquo(YField)
           out<-data %>% 
             dplyr::mutate(!!XField := round(as.numeric(!!XField),-5)) %>% 
             dplyr::mutate(!!YField := round(as.numeric(!!YField),-5))
         },
         
         "3"={
           XField<-enquo(XField)
           YField<-enquo(YField)
           out<-data %>% 
             dplyr::mutate(!!XField := round(!!XField,-3)) %>% 
             dplyr::mutate(!!YField := round(!!YField,-3))
         },
         "4"={
           XField<-enquo(XField)
           YField<-enquo(YField)
           out<-data %>% 
             dplyr::mutate(!!XField := round(!!XField,-1)) %>% 
             dplyr::mutate(!!YField :=round(!!YField,-1))
         },
         "1"={### Highest category - encrypt the data
           XField<-enquo(XField)
           YField<-enquo(YField)
           encryptr::genkeys()
           # Keys == Ptarmigan
           encrypted = data %>% 
             encryptr::encrypt(!!XField, !!YField)
           
           out<-encrypted},
         
  )
  
  return(out)
}


#############################

Full_decrypt<-function(data=data,target_cols=c(x, y)){
  library(encryptr)
  library(dplyr)
  decrypted = data %>% 
    encryptr::decrypt(data$x, data$y)
  
  return(decrypted)
  
}