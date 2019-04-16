library(igraph)
library(sqldf)
set.seed(111)

orderCol<-function(i, splitted)
{
  lista <- ""
  #splitted = c("SF", "SG", "PG")
  if (NROW(splitted) == 3){
  
    maxPosition <- apply(df_time[i,][,splitted], 1, max) 
    
    if (df_time[i,][,c(splitted[1])] == maxPosition){
      maxPosition <- apply(df_time[i,][,splitted[-1]], 1, max) 
      if (df_time[i,][,c(splitted[2])] == maxPosition){
        lista = c(splitted[1], splitted[2], splitted[3])
      }else{
        lista = c(splitted[1], splitted[3], splitted[2])
      }
    
    }else if (df_time[i,][,c(splitted[2])] == maxPosition){
      maxPosition <- apply(df_time[i,][,splitted[-2]], 1, max) 
      if (df_time[i,][,c(splitted[1])] == maxPosition){
        lista = c(splitted[2], splitted[1], splitted[3])
      }else{
        lista = c(splitted[2], splitted[3], splitted[1])
      }
    }else if (df_time[i,][,c(splitted[3])] == maxPosition){
      maxPosition <- apply(df_time[i,][,splitted[-3]], 1, max) 
      if (df_time[i,][,c(splitted[1])] == maxPosition){
        lista = c(splitted[3], splitted[1], splitted[2])
      }else{
        lista = c(splitted[3], splitted[2], splitted[1])
      }
    }
  }else{
      lista <- ""
      if (df_time[i,][,c(splitted[1])] > df_time[i,][,c(splitted[2])]){
        lista = c(splitted[1], splitted[2])
      }else{
        lista = c(splitted[2], splitted[1])
      }
    
  }
  
  return (lista)
}


df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)
unique(df_cp$equipe)
time = "Minas"
titulo = ""

df_cp <- df_cp[df_cp$Temporada == "NBB 9",]
df_time <- df_cp[df_cp$equipe == time,]

df_time <- df_time[df_time$jogador != "Jordan Emerson Montgomery",]
df_time <- df_time[df_time$jogador != "Pedro Henrique Santos de Souza",]
i =1
source<-c()
target<-c()
importance<-c()
player<-c()
type <-c()

#for(i in 1:5) {
for(i in 1:nrow(df_time)) {
  
  if (df_time[i,]$type == "Specialist"){
    source<-c(source,df_time[i,]$pos1)
    target<-c(target,df_time[i,]$pos1)
    maxPosition <- apply(df_time[i,][,c("Ala","AlaArmador","AlaPivo","Armador","Pivo")], 1, max) 
    importance <-c(importance,maxPosition)
    player <-c(player,i)
    type<-c(type,df_time[i,]$totalPos)
  }else{
    
    
    
    splitted <- strsplit(df_time[i,]$posGroup, ',')[[1]]
    splitted <- orderCol(i, splitted)
    
    #splittedLoop <- strsplit(splitted," ")
    postemp = df_time[i,]$pos1
    for(j in 1: NROW(splitted)) {
      entry = splitted[j]
      
      if (entry != df_time[i,]$pos1){
        type<-c(type,df_time[i,]$totalPos)
        source<-c(source,postemp)
        target<-c(target,entry)
        maxPosition <- df_time[i,][,c(entry)]
        importance <-c(importance,maxPosition)
        player <-c(player,i)
        
        postemp = entry
      }
      
    }
    
  }
}

type <- ifelse(type<=2, 1, type)
type <- ifelse(type>2, 6, type)

links=data.frame(source,target, player, importance, type)


nodes=data.frame(
  name=c("PG","SG", "SF", "PF", "C" ),
  carac=c("PG","SG", "SF", "PF", "C" ),
  x= c(0, 0, 1, 2, 2),
  y= c(5, 3, 4, 3, 5)
  
)

# Turn it into igraph object
network=graph_from_data_frame(d=links, vertices = nodes, directed=T) 

# You can now plot it!
par(bg="white")
#plot(network)
color = c("#D82B03","#78161D","#06C4F4",
          "#8CC13F","#011140","#1A4858",
          "#5A3204","#702C8E","#FE70DA",
          "#FF6000","#FF0292","#CC850C")

plot(network, vertex.label.cex=3.2, edge.label.color = "black", edge.label.cex= 2.5,  
     #edge.label=paste0("P ",E(network)$player),  
     edge.loop.angle = E(network)$player,
     edge.curved = E(network)$player/7,
     edge.arrow.size=1.5,
     edge.lty = E(network)$type,
     edge.label.dist=1.5,
     vertex.color="#F2F2F2",
     vertex.size=60,
     label.cex = "yellow",
     edge.color=color[E(network)$player], 
     edge.width=E(network)$importance*10 )
#legend("bottom",  legend=as.factor(paste0("P ",E(network)$player)), 
#       col = color[E(network)$player] , bty = "n", 
#       pch=20 , pt.cex = 3, cex = 2, text.col=color[E(network)$player] ,
#       horiz = T)
title("",cex.main=3,col.main="Black", line = 1)
