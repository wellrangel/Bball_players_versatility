library(sqldf)
#reading csv file with 9 seasons in nbb
df_cp <- read.csv("outputdata/classified_players.csv", sep=",", stringsAsFactors=FALSE, encoding = "UTF-8")


#calculating the player's experience
p_exp <- as.data.frame(table(df_cp$id_jogador, df_cp$Temporada))
p_exp$Freq <- as.numeric(p_exp$Freq) 
p_exp$Var2 = NULL
p_exp <- sqldf("SELECT Var1, sum(Freq) as Freq from p_exp group by Var1")


df_cp$exp = 0
for(i in 1:nrow(df_cp)) {
  # print(i)
  idJogador = df_cp[i,"id_jogador"]
  #print(idJogador)
  if ( is.na(idJogador)==0){
    
    #get experience
    varsql = paste("SELECT Freq FROM p_exp where Var1 = '", idJogador, sep="" )
    varsql = paste(varsql, "' limit  1", sep="" )
    linha <- sqldf( varsql)
    if ( nrow(linha) > 0){
      print(linha$Freq)
      df_cp[i,"exp"] =  linha$Freq;
      
    }
  }
}

#save file
write.csv(df_cp, file = "outputdata/classified_players.csv")


