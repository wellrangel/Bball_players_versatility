
library(stringr)
library(cluster) #biblioteca para teste do numero de clusters
library(reshape)
library(reshape2)
library(ggfortify) # lib para o grafico geometrico autoplot
library(sqldf)
library(xtable)
library(plyr)
Sys.setlocale("LC_ALL","pt_BR.UTF-8")

source("scripts/functions.R")

df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)

df_cp <- df_cp[df_cp$Temporada == "NBB 9",]


Especialistas <- df_cp[df_cp$type == "Specialist",]
Versateis <- df_cp[df_cp$type == "Versatile",]

#jogadores jogo das estrelas
final_jogo_estrelas <- df_cp[
        df_cp$jogador_apelido == "Marcelinho" |
                               df_cp$jogador_apelido == "Davi" | 
                              df_cp$jogador_apelido == "Deryk" |
                               df_cp$jogador == "Alex Ribeiro Garcia" |
                               df_cp$jogador_apelido == "Marquinhos" |
                               df_cp$jogador_apelido == "Giovannoni" |
                               df_cp$jogador == "Jefferson William Andrade da Silva Antônio" |
                               df_cp$jogador_apelido == "Olivinha" |
                               df_cp$jogador_apelido == "Betinho" |
                               df_cp$jogador == "Lucas Fernandes Mariano" |
                               df_cp$jogador_apelido == "De Paula" |
                               df_cp$jogador_apelido == "Fúlvio" |
                               df_cp$jogador == "Tyrone Denell Curnell" |
                               df_cp$jogador == "Kendall Lamont Anthony" |
                               df_cp$jogador_apelido == "Rollins" |
                               df_cp$jogador_apelido == "Bennett" |
                               df_cp$jogador_apelido == "Rodgers" |
                               df_cp$jogador_apelido == "Maynard" |
                               df_cp$jogador == "Kenny Montrell Dawkins" |
                               df_cp$jogador_apelido == "Holloway" |
                               df_cp$jogador_apelido == "Shamell" |
                               df_cp$jogador_apelido == "Jackson" |
                               df_cp$jogador_apelido == "Laws" |
                               df_cp$jogador_apelido == "Hure" |
                               df_cp$jogador_apelido == "Cafferata"
                             ,]


final_jogo_estrelas <- final_jogo_estrelas[,c( "jogador", "pos","pos1","predict", "PG" , "SG","SF", "PF","C" )]

final_jogo_estrelas$PG = round(final_jogo_estrelas$PG,2)
final_jogo_estrelas$SG = round(final_jogo_estrelas$SG,2)
final_jogo_estrelas$SF = round(final_jogo_estrelas$SF,2)
final_jogo_estrelas$PF = round(final_jogo_estrelas$PF,2)
final_jogo_estrelas$C = round(final_jogo_estrelas$C,2)


round((mean(final_jogo_estrelas$predict==final_jogo_estrelas$pos)*100),2)

20/24

#xtable::xtable(final_jogo_estrelas)

