library(plyr)
library(dplyr)
source("scripts/functions.R")


dfall <- read.csv("outputdata/df_all.csv", sep=",", stringsAsFactors=FALSE)


dfall$maxPosition <- apply(dfall[,3:7], 1, max) 

dfall$posGroup = ""
dfall$totalPos = 1
dfall$range = ""
dfall$type = ""

shiftCol = 2
for(i in 1:nrow(dfall)) {
  
  col = floor(dfall[i,]$maxPosition*10)*10
  
  dfall[i,]$range = col
  totalPos = 0
  colnumber = 1
  pos = ""
  #colnames(dfall)
  while (colnumber <= 5){
    if (dfall[i,][,colnumber+shiftCol]  >= 0.20){
      
      totalPos = totalPos + 1
      posCurto = ""
      if (names(dfall)[colnumber+shiftCol] =="Ala"){
        posCurto = "SF"
      }
      
      if (names(dfall)[colnumber+shiftCol] =="AlaArmador"){
        posCurto = "SG"
      }
      
      if (names(dfall)[colnumber+shiftCol] =="Armador"){
        posCurto = "PG"
      }
      
      if (names(dfall)[colnumber+shiftCol] =="AlaPivo"){
        posCurto = "PF"
      }
      if (names(dfall)[colnumber+shiftCol] =="Pivo"){
        posCurto = "C"
      }
      pos = paste(pos, posCurto, sep =",")
    }
    
    colnumber = colnumber + 1
    
  }
  pos = substring(pos, 2)
  
  write.csv(dfall, "outputdata/classified_players.csv")
  
  