# Code to score manipulation check string
# Last Modified : 5/22/2020

library(readxl)
library(stringr)
library(tm)
Data <- read_excel("C:/Users/hemra/Downloads/Data.xlsx")
View(Data)


#manipulation check scorer function
#Respondent gets a point if they correctly identified the issues discussed
#They also get a point if they did not mention the issues not discussed
scorer <- function(text){
  s <- 0
  if(is.na(text)){return(NA)}
    else{
        if(str_detect(text,"1")){s <- s+1}
        if(str_detect(text,"2")){s <- s+1}   
        if(str_detect(text,"3")==FALSE){s <- s+1}  
        if(str_detect(text,"4")){s <- s+1}
        if(str_detect(text,"5")==FALSE){s <- s+1}
        if(str_detect(text,"6")){s <- s+1}
        if(str_detect(text,"7")==FALSE){s <- s+1}
        return(s)}
}

Data$mcscore <- sapply(Data$Q13,scorer)
