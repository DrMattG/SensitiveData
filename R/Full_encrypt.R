library(LivingNorwayData)
data=LivingNorwayData::Rock_ptarmigan


Full_encrypt<-function(data=data,target_cols=c(x, y)){
  library(encryptr)
  library(dplyr)
 encryptr::genkeys()
# Keys == Ptarmigan
encrypted = data %>% 
  encryptr::encrypt(data$x, data$y)

return(encrypted)

}

pt_encrypt<-Full_encrypt(data = data, target_cols=c(X.koordinat, Y.koordinat))


Full_decrypt<-function(data=data,target_cols=c(x, y)){
  library(encryptr)
  library(dplyr)
  decrypted = data %>% 
    encryptr::decrypt(data$x, data$y)
  
  return(decrypted)
  
}

Full_decrypt(data = pt_encrypt, target_cols=c(X.koordinat, Y.koordinat))
