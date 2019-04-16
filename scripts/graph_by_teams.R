library(stringr)
library(reshape)
library(reshape2)
library(sqldf)
library(ggplot2)
library(plyr)

source("scriptsnovoscampos/functions.R")

rotatedAxisElementText = function(angle,posicaoition='x'){
  angle     = angle[1]; 
  posicaoition  = posicaoition[1]
  posicaoitions = list(x=0,y=90,top=180,right=270)
  if(!posicaoition %in% names(posicaoitions))
    stop(sprintf("'posicaoition' must be one of [%s]",paste(names(posicaoitions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - posicaoitions[[ posicaoition ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust, size=36)
}


df_cp <- read.csv2("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)

df_cp<- df_cp[df_cp$Temporada == "NBB 9",]


table(df_cp$equipe, df_cp$type)
table(df_cp$equipe)
 
bauru <- df_cp[df_cp$equipe == "Bauru",]
bauru <- bauru[,c("posicao", "jogador")]

Especialistas <- df_cp[df_cp$type == "Specialist",]
Versateis <- df_cp[df_cp$type == "Versatile",]




dataGraph <- data.frame(Team=character(),
                        Classification=character(),
                        Total=integer(),
                        Percentual=integer(),
                        stringsAsFactors=FALSE) 

#times <- as.data.frame(unique(Especialistas$equipe))

times <- c("Flamengo","Mogi", "Franca",  "Brasília",  
           "Bauru","Paulistano", "Vitória","Pinheiros",
           "Vasco da Gama","Campo Mourão", "Basq, Cearense", "Macaé Basquete",
           "Minas", "L, Sorocabana", "Caxias do Sul"
)
times <- as.data.frame(times)
for(i in 1:nrow(times)) {
  
  TimeEspecialistas <- Especialistas[Especialistas$equipe==times[i,],]
  TimeVersateis <- Versateis[Versateis$equipe==times[i,],]
  
  myRow <- dataGraph[1,]
  myRow$Team <- times[i,]
  myRow$Classification <- "Versatile"
  myRow$Total <- nrow(TimeVersateis)
  myRow$Percentual <- round((nrow(TimeVersateis)/(nrow(TimeEspecialistas) + nrow(TimeVersateis))) *100,0)
  
  dataGraph <- rbind(dataGraph, myRow)
  
  myRow <- dataGraph[1,]
  myRow$Team <- times[i,]
  myRow$Classification <- "Specialists"
  myRow$Total <- nrow(TimeEspecialistas)
  myRow$Percentual <- round((nrow(TimeEspecialistas)/(nrow(TimeEspecialistas) + nrow(TimeVersateis))) *100,0)
  
  dataGraph <- rbind(dataGraph, myRow)
  
  
  
  
}

dataGraph$Team <- factor(dataGraph$Team, levels=c("Bauru","Paulistano", "Pinheiros", "Vitória",
                                                  "Flamengo","Mogi", "Franca",  "Brasília",  
                                                  "Vasco da Gama","Campo Mourão", "Basq, Cearense", "Macaé Basquete",
                                                  "Minas", "L, Sorocabana", "Caxias do Sul"))


dataGraph$Team <- ifelse(dataGraph$Team=="Bauru", "Team 01", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Paulistano", "Team 02", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Pinheiros", "Team 03", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Vitória", "Team 04", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Flamengo", "Team 05", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Mogi", "Team 06", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Franca", "Team 07", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Brasília", "Team 08", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Vasco da Gama", "Team 09", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Campo Mourão", "Team 10", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Basq, Cearense", "Team 11", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Macaé Basquete", "Team 12", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Minas", "Team 13", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="L, Sorocabana", "Team 14", as.character(dataGraph$Team))
dataGraph$Team <- ifelse(dataGraph$Team=="Caxias do Sul", "Team 15", as.character(dataGraph$Team))

#dataGraph$Team <- factor(dataGraph$Team, levels=c("Team 01","Paulistano", "Pinheiros", "Vitória",
 #                                                 "Flamengo","Mogi", "Franca",  "Brasília",  
  #                                                "Vasco da Gama","Campo Mourão", "Basq. Cearense", "Macaé Basquete",
   #                                               "Minas", "Liga Sorocabana", "Caxias do Sul"))

#dataGraph[dataGraph$Classification=="Specialists",]$labelposicao <- dataGraph[dataGraph$Classification=="Specialists",]$Percentual+5
#dataGraph[dataGraph$Classification=="Versatile",]$labelposicao <- dataGraph[dataGraph$Classification=="Versatile",]$Percentual-5


dataGraph <- ddply(dataGraph, .(Team),
                   transform, posicao = cumsum(Percentual) - (0.5 * Percentual))

ggplot(dataGraph, aes(Team, Percentual, fill = Classification))  +
  theme_bw() +
  geom_bar(stat="identity") +
  geom_text(data=dataGraph, aes(x = Team, y = posicao, label = paste0(Percentual,"%")), size=10, vjust=-.5) +
  xlab("Teams") +
  
  ylab("Players' frequency (in percentage, %)") +
  scale_fill_manual(values=c( "#9999CC", "#66CC99"))+
  
  theme(
       #axis.text.x=element_text(size=38),
       axis.text.y=element_text(size=40),
        axis.title=element_text(size=35,face="bold"),
        legend.position="top", 
        legend.title=element_text(size=25), 
        legend.text=element_text(size=40),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(hjust=0.85, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text.x = rotatedAxisElementText(45,'y')
  ) + theme(legend.title=element_blank(), legend.spacing.x = unit(0.5, 'cm')) 

ggsave("graph/figure4.png")

