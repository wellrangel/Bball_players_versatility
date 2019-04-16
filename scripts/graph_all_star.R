
library(stringr)
library(cluster) #biblioteca para teste do numero de clusters
library(reshape)
library(reshape2)
library(ggfortify) # lib para o grafico geometrico autoplot
library(sqldf)
library(ggplot2)
library(plyr)
Sys.setlocale("LC_ALL","pt_BR.UTF-8")

source("scripts/functions.R")

df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)

df_cp <- df_cp[df_cp$Temporada == "NBB 9",]

Especialistas <- df_cp[df_cp$type == "Specialist",]
Versateis <- df_cp[df_cp$type == "Versatile",]

#jogadores jogo das estrelas
final_jogo_estrelas <- df_cp[df_cp$jogador_jogador_apelido == "Marcelinho" |
                                     df_cp$jogador_jogador_apelido == "Davi" | 
                                     df_cp$jogador_jogador_apelido == "Deryk" |
                                     df_cp$jogador == "Alex Ribeiro Garcia" |
                                     df_cp$jogador_jogador_apelido == "Marquinhos" |
                                     df_cp$jogador_jogador_apelido == "Giovannoni" |
                                     df_cp$jogador == "Jefferson William Andrade da Silva Antônio" |
                                     df_cp$jogador_jogador_apelido == "Olivinha" |
                                     df_cp$jogador_jogador_apelido == "Betinho" |
                                     df_cp$jogador == "Lucas Fernandes Mariano" |
                                     df_cp$jogador_jogador_apelido == "De Paula" |
                                     df_cp$jogador_jogador_apelido == "Fúlvio" |
                                     df_cp$jogador == "Tyrone Denell Curnell" |
                                     df_cp$jogador == "Kendall Lamont Anthony" |
                                     df_cp$jogador_jogador_apelido == "Rollins" |
                                     df_cp$jogador_jogador_apelido == "Bennett" |
                                     df_cp$jogador_jogador_apelido == "Rodgers" |
                                     df_cp$jogador_jogador_apelido == "Maynard" |
                                     df_cp$jogador == "Kenny Montrell Dawkins" |
                                     df_cp$jogador_jogador_apelido == "Holloway" |
                                     df_cp$jogador_jogador_apelido == "Shamell" |
                                     df_cp$jogador_jogador_apelido == "Jackson" |
                                     df_cp$jogador_jogador_apelido == "Laws" |
                                     df_cp$jogador_jogador_apelido == "Hure" |
                                     df_cp$jogador_jogador_apelido == "Cafferata"
                                   ,]


final_jogo_estrelas$Ala = round(final_jogo_estrelas$Ala,2)
final_jogo_estrelas$Pivo = round(final_jogo_estrelas$Pivo,2)
final_jogo_estrelas$AlaArmador = round(final_jogo_estrelas$AlaArmador,2)
final_jogo_estrelas$AlaPivo = round(final_jogo_estrelas$AlaPivo,2)
final_jogo_estrelas$Armador = round(final_jogo_estrelas$Armador,2)

temp <- final_jogo_estrelas[final_jogo_estrelas$posicao == "Ala",]

BrasilEspecialistas <-Especialistas[Especialistas$jogador_apelido == "Marcelinho" |
                                      Especialistas$jogador_apelido == "Davi" | 
                                      Especialistas$jogador_apelido == "Deryk" |
                                      Especialistas$jogador == "Alex Ribeiro Garcia" |
                                      Especialistas$jogador_apelido == "Marquinhos" |
                                      Especialistas$jogador_apelido == "Giovannoni" |
                                      Especialistas$jogador == "Jefferson William Andrade da Silva Antônio" |
                                      Especialistas$jogador_apelido == "Olivinha" |
                                      Especialistas$jogador_apelido == "Betinho" |
                                      Especialistas$jogador == "Lucas Fernandes Mariano" |
                                      Especialistas$jogador_apelido == "De Paula" |
                                      Especialistas$jogador_apelido == "Fúlvio",]

BrasilVersateis <- Versateis[Versateis$jogador_apelido == "Marcelinho" |
                               Versateis$jogador_apelido == "Davi" | 
                               Versateis$jogador_apelido == "Deryk" |
                               Versateis$jogador == "Alex Ribeiro Garcia" |
                               Versateis$jogador_apelido == "Marquinhos" |
                               Versateis$jogador_apelido == "Giovannoni" |
                               Versateis$jogador == "Jefferson William Andrade da Silva Antônio" |
                               Versateis$jogador_apelido == "Olivinha" |
                               Versateis$jogador_apelido == "Betinho" |
                               Versateis$jogador == "Lucas Fernandes Mariano" |
                               Versateis$jogador_apelido == "De Paula" |
                               Versateis$jogador_apelido == "Fúlvio",]

MundoEspecialistas <- Especialistas[Especialistas$jogador == "Tyrone Denell Curnell" |
                                      Especialistas$jogador == "Kendall Lamont Anthony" |
                                      Especialistas$jogador_apelido == "Rollins" |
                                      Especialistas$jogador_apelido == "Bennett" |
                                      Especialistas$jogador_apelido == "Rodgers" |
                                      Especialistas$jogador_apelido == "Maynard" |
                                      Especialistas$jogador == "Kenny Montrell Dawkins" |
                                      Especialistas$jogador_apelido == "Holloway" |
                                      Especialistas$jogador_apelido == "Shamell" |
                                      Especialistas$jogador_apelido == "Jackson" |
                                      Especialistas$jogador_apelido == "Hure" |
                                      Especialistas$jogador_apelido == "Cafferata",]

MundoVersateis <- Versateis[Versateis$jogador == "Tyrone Denell Curnell" |
                              Versateis$jogador == "Kendall Lamont Anthony" |
                              Versateis$jogador_apelido == "Rollins" |
                              Versateis$jogador_apelido == "Bennett" |
                              Versateis$jogador_apelido == "Rodgers" |
                              Versateis$jogador_apelido == "Maynard" |
                              Versateis$jogador == "Kenny Montrell Dawkins" |
                              Versateis$jogador_apelido == "Holloway" |
                              Versateis$jogador_apelido == "Shamell" |
                              Versateis$jogador_apelido == "Jackson" |
                              Versateis$jogador_apelido == "Hure" |
                              Versateis$jogador_apelido == "Cafferata",]


round((14/24)*100,2)

dataGraph <- data.frame(Team=character(),
                        Classification=character(),
                        Total=integer(),
                        stringsAsFactors=FALSE) 

myRow <- dataGraph[1,]
myRow$Team <- "Brazil"
myRow$Classification <- "Specialists"
myRow$Total <- nrow(BrasilEspecialistas)

dataGraph <- rbind(dataGraph, myRow)

myRow$Team <- "Brazil"
myRow$Classification <- "Versatile"
myRow$Total <- nrow(BrasilVersateis)

dataGraph <- rbind(dataGraph,  myRow)
#Mundo
myRow <- dataGraph[1,]
myRow$Team <- "Foreign"
myRow$Classification <- "Specialists"
myRow$Total <- nrow(MundoEspecialistas)

dataGraph <- rbind(dataGraph, myRow)

myRow$Team <- "Foreign"
myRow$Classification <- "Versatile"
myRow$Total <- nrow(MundoVersateis)

dataGraph <- rbind(dataGraph,  myRow)


ggplot(dataGraph, aes(Team, Total, fill = Classification)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Teams") +
  ylab("Number of players") +
  scale_y_continuous(name ="Players' frequency", 
                   limits=c(0,8),
                  breaks=c(0,3,5,7)) +
  scale_fill_manual(values=c( "#9999CC", "#66CC99"))+
  theme(axis.text=element_text(size=40),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title=element_text(size=35,face="bold"),
        legend.position="top", legend.title=element_text(size=30), 
        legend.text=element_text(size=40)
        ) + theme(legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))

ggsave("graph/figure6.png")


