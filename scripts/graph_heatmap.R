#0.734939759 - 
library(stringr)
library(cluster) #biblioteca para teste do numero de clusters
library(reshape)
library(reshape2)
library(ggfortify) # lib para o grafico geometrico autoplot
library(sqldf)
library(ggplot2)
library(plyr)
library(ggalt)

source("scripts/functions.R")


df_cp <- read.csv("outputdata/classified_players.csv", sep=",", stringsAsFactors=FALSE)
#last season
df_cp <- df_cp[df_cp$Temporada== "NBB 9",]

round ( (NROW(df_cp[df_cp$type== "Specialist",])/NROW(df_cp) ) * 100 ,2)

round ( (NROW(df_cp[df_cp$type== "Versatile",])/NROW(df_cp) ) * 100 ,2)



#####Temporada 9 

head(df_cp)


mat <- criaMatrizMapaV2(df_cp)

mat$label = ""
for(i in 1:nrow(mat)) {
  
  mat[i,]$label = GetDetalheMapa (DetalheMapa, mat[i,]$X1, mat[i,]$X2, mat[i,]$value, NROW(df_cp))
  #  print(mat[i,]$value)
}

mat$Var1 <- as.factor(mat$Var1)
mat$Var2 <- as.factor(mat$Var2)
mat$value <- as.factor(mat$value)

ggplot(data = mat, aes(x=Var2, y=Var1)) + 
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = label ) ,col='black',cex=6) + 
  #scale_x_discrete(limits=c("3 Positions","2 Positions","1 Position")) +
  #scale_fill_gradient2(low="red4", high="red", guide="colorbar")+
  #labs(title = "Distribution of player percentages - season 2016", fill="Number of players") +
  xlab("Positions") +
  ylab("Probability Range (%)") +
  guides(fill=FALSE) + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=35,face="bold")) +
  # geom_encircle(aes(x=1.5, y=5), 
  #           color="blue", 
  #          size=2, 
  #         expand=0.35) +
  scale_fill_manual(
    values =  c("white", rev(heat.colors(10)) )) 



