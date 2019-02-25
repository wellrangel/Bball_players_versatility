library(stringr)
library(reshape)
library(reshape2)
library(sqldf)
library(ggplot2)
library(plyr)

source("scripts/functions.R")

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
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}


df_cp <- read.csv("outputdata/classified_players.csv", sep=",", stringsAsFactors=FALSE, encoding = "UTF-8")

df_cp<- df_cp[df_cp$Temporada == "NBB 9",]
table(df_cp$equipe, df_cp$posicaoicao)
table(df_cp$equipe)

bauru <- df_cp[df_cp$equipe == "Bauru",]
bauru <- bauru[,c("posicaoicao", "jogador")]

Especialistas <- df_cp[df_cp$type == "Specialist",]
Versateis <- df_cp[df_cp$type == "Versatile",]

temp <- sqldf("SELECT posicaoGroup, count(*) FROM Versateis  group by posicaoGroup")

temp <- count(Especialistas, "posicaoicao")
temp <- count(Versateis, "posicaoicao")
temp <- count(Versateis, "totalposicao")

temp <- count(Versateis, "posicaoGroup")



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

#dataGraph[dataGraph$Classification=="Specialists",]$labelposicao <- dataGraph[dataGraph$Classification=="Specialists",]$Percentual+5
#dataGraph[dataGraph$Classification=="Versatile",]$labelposicao <- dataGraph[dataGraph$Classification=="Versatile",]$Percentual-5


dataGraph <- ddply(dataGraph, .(Team),
                   transform, posicao = cumsum(Percentual) - (0.5 * Percentual))

ggplot(dataGraph, aes(Team, Percentual, fill = Classification)) + theme_bw() +
  geom_bar(stat="identity") + 
  geom_text(data=dataGraph, aes(x = Team, y = posicao, label = paste0(Percentual,"%")), size=8, vjust=-.5) +
  xlab("Teams") +
  ylab("Percentage of players") +
  scale_fill_manual(values=c( "#9999CC", "#66CC99"))+
  
  theme(axis.text=element_text(size=35),
        axis.title=element_text(size=35,face="bold"),
        legend.position="top", legend.title=element_text(size=30), 
        legend.text=element_text(size=27),
        axis.text.x = rotatedAxisElementText(45,'y')
  ) 



