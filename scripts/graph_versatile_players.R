library(stringr)
library(cluster) #biblioteca para teste do numero de clusters
library(reshape)
library(reshape2)
library(ggfortify) # lib para o grafico geometrico autoplot
library(sqldf)
library(ggplot2)
library(plyr)

source("scripts/functions.R")

df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE, encoding = "UTF-8")

Especialistas <- df_cp[df_cp$type == "Specialist",]
Versateis <- df_cp[df_cp$type == "Versatile",]

temp <- sqldf("SELECT pos as Position, Temporada, count(*) as total FROM Versateis  group by pos, Temporada order by Temporada, pos")

temp$Position <- ifelse(temp$Position == "Ala", "Small Forward", temp$Position)
temp$Position <- ifelse(temp$Position == "AlaArmador", "Shooting Guard", temp$Position)
temp$Position <- ifelse(temp$Position == "AlaPivo", "Power Forward", temp$Position)
temp$Position <- ifelse(temp$Position == "Pivo", "Center", temp$Position)
temp$Position <- ifelse(temp$Position == "Armador", "Point Guard", temp$Position)


temp$ano[temp$Temporada == "NBB 1"] <- 2009
temp$ano[temp$Temporada == "NBB 2"] <- 2010
temp$ano[temp$Temporada == "NBB 3"] <- 2011
temp$ano[temp$Temporada == "NBB 4"] <- 2012
temp$ano[temp$Temporada == "NBB 5"] <- 2013
temp$ano[temp$Temporada == "NBB 6"] <- 2014
temp$ano[temp$Temporada == "NBB 7"] <- 2015
temp$ano[temp$Temporada == "NBB 8"] <- 2016
temp$ano[temp$Temporada == "NBB 9"] <- 2017

set.seed(45)
df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                 variable=rep(paste0("category", 1:9), each=5))


# plot
temp$Position <- as.factor(temp$Position)

temp$Position <- factor(temp$Position, levels = c("Point Guard", "Shooting Guard", "Small Forward", "Power Forward", "Center"))

p <- ggplot(data = temp, aes(x=ano , y=total)) + 
  geom_line(aes(colour=Position), size=2 ) +labs(col="") 

p + ylab("Frequency of versatile players") + scale_x_discrete(name ="Seasons", limits=c(2009,2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  theme(axis.text.x=element_text(size=40),axis.text.y=element_text(size=40),
        axis.title=element_text(size=35,face="bold"),
        legend.position="top", legend.title=element_text(size=34), 
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.text=element_text(size=34)) +
  
  
        geom_point(size=5, aes(color=Position)) +
      #scale_color_manual(values = c( "#9999CC", "#66CC99")) +
  
  geom_text(aes(label=total), size=10, vjust=-0.3)

ggsave("graph/figure3.png")


