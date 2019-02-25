library(sqldf)
#reading csv file with 9 seasons in nbb
dfstats <- read.csv("inputdata/all-stats-nbb.csv", sep = "," ,stringsAsFactors=FALSE, encoding = "UTF-8")

#changing NA values to 0
dfstats[is.na(dfstats)] <- 0
#changing comma by dot in minutes column
dfstats$played_minutes = scan(text=dfstats$played_minutes, dec=",", sep=".")

# as numeric
dfstats <- transform(dfstats, played_minutes = as.numeric(played_minutes))
dfstats <- transform(dfstats, plus_minus = as.numeric(plus_minus))



dfstats_by_years <- sqldf("SELECT distinct(jogador), id_jogador, jogador, jogador_apelido, Temporada, id_equipe, equipe,  
  sum(played_minutes) as played_minutes, sum(tried_points) as tried_points, sum(lost_points) as lost_points, sum(total_points) as total_points,
sum(offensive_rebounds) as offensive_rebounds, sum(defensive_rebounds) as defensive_rebounds, sum(total_assists) as total_assists,
sum(steals) as steals, 
  sum(tried_two_points) as tried_two_points, sum(made_two_points) as made_two_points,  sum(lost_two_points) as lost_two_points, sum(two_points) as two_points,
  sum(tried_three_points) as tried_three_points,  sum(made_three_points) as made_three_points,  sum(lost_three_points) as lost_three_points,sum(three_points) as three_points,
  sum(tried_free_throw_points) as tried_free_throw_points, sum(made_free_throw_points) as made_free_throw_points, sum(lost_free_throw_points) as lost_free_throw_points, 
   sum(free_throw_points) as free_throw_points,  sum(correct_two_points_throws) as correct_two_points_throws, sum(wrong_two_points_throws) as wrong_two_points_throws, 
sum(two_points_throws_total) as two_points_throws_total, sum(wrong_throws) as wrong_throws,sum(tried_dunk_points) as tried_dunk_points, sum(dunk_points) as dunk_points, 
                 sum(correct_three_points_throws) as correct_three_points_throws, sum(three_points_throws_total) as three_points_throws_total,
                 sum(wrong_three_points_throws) as wrong_three_points_throws, sum(correct_free_throws) as correct_free_throws, sum(wrong_free_throws) as wrong_free_throws,
                 sum(total_free_throws) as total_free_throws,sum(total_rebounds) as total_rebounds,
                  sum(total_of_blocks) as total_of_blocks,
                 sum(commited_fouls) as commited_fouls, sum(received_fouls) as received_fouls, sum(fouls) as fouls,
                 sum(total_fouls) as total_fouls,  
                 sum(total_of_errors) as total_of_errors, sum(mistakes) as mistakes,
                 sum(total_of_violations) as total_of_violation 
                 FROM dfstats group by Temporada, jogador ")





dfstats_by_years <- dfstats_by_years[dfstats_by_years$played_minutes > 1,]

dfstats_by_years$made_two_points =  dfstats_by_years$made_two_points/dfstats_by_years$played_minutes
dfstats_by_years$made_three_points =  dfstats_by_years$made_three_points/dfstats_by_years$played_minutes
dfstats_by_years$made_free_throw_points =  dfstats_by_years$made_free_throw_points/dfstats_by_years$played_minutes
dfstats_by_years$total_points<- dfstats_by_years$total_points/dfstats_by_years$played_minutes
dfstats_by_years$lost_points<- dfstats_by_years$lost_points/dfstats_by_years$played_minutes
dfstats_by_years$tried_points<- dfstats_by_years$tried_points/dfstats_by_years$played_minutes
dfstats_by_years$two_points<- dfstats_by_years$two_points/dfstats_by_years$played_minutes
dfstats_by_years$lost_two_points<- dfstats_by_years$lost_two_points/dfstats_by_years$played_minutes
dfstats_by_years$tried_two_points<- dfstats_by_years$tried_two_points/dfstats_by_years$played_minutes
dfstats_by_years$three_points<- dfstats_by_years$three_points/dfstats_by_years$played_minutes
dfstats_by_years$lost_three_points<- dfstats_by_years$lost_three_points/dfstats_by_years$played_minutes
dfstats_by_years$tried_three_points<- dfstats_by_years$tried_three_points/dfstats_by_years$played_minutes
dfstats_by_years$dunk_points<- dfstats_by_years$dunk_points/dfstats_by_years$played_minutes
dfstats_by_years$tried_dunk_points<- dfstats_by_years$tried_dunk_points/dfstats_by_years$played_minutes
dfstats_by_years$wrong_throws<- dfstats_by_years$wrong_throws/dfstats_by_years$played_minutes
dfstats_by_years$correct_two_points_throws<- dfstats_by_years$correct_two_points_throws/dfstats_by_years$played_minutes
dfstats_by_years$two_points_throws_total<- dfstats_by_years$two_points_throws_total/dfstats_by_years$played_minutes
dfstats_by_years$wrong_two_points_throws<- dfstats_by_years$wrong_two_points_throws/dfstats_by_years$played_minutes
dfstats_by_years$correct_three_points_throws<- dfstats_by_years$correct_three_points_throws/dfstats_by_years$played_minutes
dfstats_by_years$three_points_throws_total<- dfstats_by_years$three_points_throws_total/dfstats_by_years$played_minutes
dfstats_by_years$wrong_three_points_throws<- dfstats_by_years$wrong_three_points_throws/dfstats_by_years$played_minutes
dfstats_by_years$correct_free_throws<- dfstats_by_years$correct_free_throws/dfstats_by_years$played_minutes
dfstats_by_years$wrong_free_throws<- dfstats_by_years$wrong_free_throws/dfstats_by_years$played_minutes
dfstats_by_years$total_free_throws<- dfstats_by_years$total_free_throws/dfstats_by_years$played_minutes
#dfstats_by_years$total_of_taps<- dfstats_by_years$total_of_taps/dfstats_by_years$played_minutes
#dfstats_by_years$wrong_taps<- dfstats_by_years$wrong_taps/dfstats_by_years$played_minutes
dfstats_by_years$total_rebounds<- dfstats_by_years$total_rebounds/dfstats_by_years$played_minutes
dfstats_by_years$offensive_rebounds<- dfstats_by_years$offensive_rebounds/dfstats_by_years$played_minutes
dfstats_by_years$defensive_rebounds<- dfstats_by_years$defensive_rebounds/dfstats_by_years$played_minutes
dfstats_by_years$total_assists<- dfstats_by_years$total_assists/dfstats_by_years$played_minutes
dfstats_by_years$steals<- dfstats_by_years$steals/dfstats_by_years$played_minutes
dfstats_by_years$total_of_blocks<- dfstats_by_years$total_of_blocks/dfstats_by_years$played_minutes
dfstats_by_years$commited_fouls<- dfstats_by_years$commited_fouls/dfstats_by_years$played_minutes
dfstats_by_years$received_fouls<- dfstats_by_years$received_fouls/dfstats_by_years$played_minutes
dfstats_by_years$fouls<- dfstats_by_years$fouls/dfstats_by_years$played_minutes
dfstats_by_years$total_fouls<- dfstats_by_years$total_fouls/dfstats_by_years$played_minutes
dfstats_by_years$total_of_errors<- dfstats_by_years$total_of_errors/dfstats_by_years$played_minutes
dfstats_by_years$mistakes<- dfstats_by_years$mistakes/dfstats_by_years$played_minutes
dfstats_by_years$total_of_violations<- dfstats_by_years$total_of_violations/dfstats_by_years$played_minutes
dfstats_by_years$free_throw_points<- dfstats_by_years$free_throw_points/dfstats_by_years$played_minutes
dfstats_by_years$lost_free_throw_points<- dfstats_by_years$lost_free_throw_points/dfstats_by_years$played_minutes
dfstats_by_years$tried_free_throw_points<- dfstats_by_years$tried_free_throw_points/dfstats_by_years$played_minutes

dfstats_by_years <- dfstats_by_years[dfstats_by_years$played_minutes > 100,]



#calculating the player's experience
p_exp <- as.data.frame(table(dfstats_by_years$id_jogador, dfstats_by_years$Temporada))
p_exp$Freq <- as.numeric(p_exp$Freq) 
p_exp$Var2 = NULL
p_exp <- sqldf("SELECT Var1, sum(Freq) as Freq from p_exp group by Var1")

#adding position for each player
dfstats_by_yearsPos <- read.csv("inputdata/atletasPosicaoDistinct.csv", sep = "," ,stringsAsFactors=FALSE, encoding = "UTF-8")
dfstats_by_yearsPos <- dfstats_by_yearsPos[, c("id_jogador", "altura", "posicao")]


dfstats_by_yearsPosTemp <- read.csv("inputdata/dadosPreAutoML.csv", sep = "," ,stringsAsFactors=FALSE, encoding = "UTF-8")
dfstats_by_yearsPosTemp <- dfstats_by_yearsPosTemp[, c("id_jogador", "posicao")]

dfstats_by_years$pos = ""
dfstats_by_years$exp = 0
for(i in 1:nrow(dfstats_by_years)) {
  # print(i)
  idJogador = dfstats_by_years[i,"id_jogador"]
  #print(idJogador)
  if ( is.na(idJogador)==0){
    
    varsql = paste("SELECT id_jogador, posicao FROM dfstats_by_yearsPosTemp where id_jogador = '", idJogador, sep="" )
    varsql = paste(varsql, "' limit  1", sep="" )
    linha <- sqldf( varsql)
    if ( nrow(linha) > 0){
      print(linha$posicao)
      dfstats_by_years[i,"pos"] =  linha$posicao;
      
    }
    
    #get experience
    varsql = paste("SELECT Freq FROM p_exp where Var1 = '", idJogador, sep="" )
    varsql = paste(varsql, "' limit  1", sep="" )
    linha <- sqldf( varsql)
    if ( nrow(linha) > 0){
      print(linha$Freq)
      dfstats_by_years[i,"exp"] =  linha$Freq;
      
    }
  }
}

#note few null positions were obtained directly from the web site http://lnb.com.br/
#NA cases
dfstats_by_years[is.na(dfstats_by_years)] <- 0

dfstats_by_years <- dfstats_by_years[dfstats_by_years$id_jogador > 0,]

dfstats_by_years$jogador.1 <- NULL
dfstats_by_years$jogador.2 <- NULL

#save file
write.csv(dfstats_by_years, file = "outputdata/aggregateData.csv")


