#
#
#

library(LivingNorwayData)
library(dplyr)
data=LivingNorwayData::Rock_ptarmigan %>% 
  mutate(X.koordinat=gsub(",",".", X.koordinat)) %>% 
  mutate(Y.koordinat=gsub(",",".", Y.koordinat)) %>% 
  mutate(X.koordinat=as.numeric(X.koordinat)) %>% 
  mutate(Y.koordinat=as.numeric(Y.koordinat))

sensitive_locations<-function(data, target_cols=c(x,y), cat="NS"){
  if (cat=="1") {
    library(encryptr)
    library(dplyr)
    encryptr::genkeys()
    # Keys == Ptarmigan
    data=data %>% 
      encryptr::encrypt(data$x, data$y)
    return(data) 
  } else {
    x= round(data$x,-4)
    y=round(data$y,-4)
    data=cbind(x,y)
    return(data)}
  }


sensitive_locations(data,c("X.koordinat", "Y.koordinat"), cat="NS")

round(data$X.koordinat,-4)


#pt_encrypt<-Full_encrypt(data = data, target_cols=c(X.koordinat, Y.koordinat))

Sensitive_locations<-function(data, XField="X", YField="Y",
                         
                         Cat="Not_sensitive"){
  
  library(tidyverse)
  
  library(encryptr)
  
  out<-data
  
  
### Switch: Cats CHECK THE ROUNDING
  
  switch(Cat,
         
         "2"={out<-data %>% 
             dplyr::mutate(X= round(as.numeric(X),-3)) %>% 
             dplyr::mutate(Y=round(as.numeric(Y),-3))
         },
         
         "3"={out<-data %>% 
             dplyr::mutate(X= round(as.numeric(X),-2)) %>% 
             dplyr::mutate(Y=round(as.numeric(Y),-2))
         },
         "4"={out<-data %>% 
             dplyr::mutate(X= round(as.numeric(X),-1)) %>% 
             dplyr::mutate(Y=round(as.numeric(Y),-1))
         },
         "1"={### Highest category - encrypt the data
           
           encryptr::genkeys()
           # Keys == Ptarmigan
           encrypted = data %>% 
             encryptr::encrypt(data$X, data$Y)
           
           out<-encrypted},
         
  )
  
  return(out)
}


Sensitive_locations(data=data, XField = "X.koordinat", YField="Y.koordinat", Cat=2)




Full_decrypt<-function(data=data,target_cols=c(x, y)){
  library(encryptr)
  library(dplyr)
  decrypted = data %>% 
    encryptr::decrypt(data$x, data$y)
  
  return(decrypted)
  
}

Full_decrypt(data = pt_encrypt, target_cols=c(X.koordinat, Y.koordinat))



  low<-data %>% 
    dplyr::mutate(X.koordinat= round(X.koordinat,-4)) %>% 
    dplyr::mutate(Y.koordinat=round(Y.koordinat,-4))
  
low






library(leaflet)
Original<-LivingNorwayR::get_NOR_geographic_extent(data,"X.koordinat", "Y.koordinat",Code = 3045)
Low<-LivingNorwayR::get_NOR_geographic_extent(low,"X.koordinat", "Y.koordinat",Code = 3045)

Original
Low

