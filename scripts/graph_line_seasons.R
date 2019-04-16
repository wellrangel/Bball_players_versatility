#heat map
library(stringr)
library(sqldf)
library(ggplot2)
library(plyr)
library(reshape)
source("scripts/functions.R")

df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)
table(df_cp$type)




df_cpTemp = df_cp[,c("pos","predict", "Ala" , "AlaArmador","AlaPivo", "Armador","Pivo", "jogador" )]
round ( (NROW(df_cp[df_cp$type== "Specialist",])/NROW(df_cp) ) * 100 ,2)

round ( (NROW(df_cp[df_cp$type== "Versatile",])/NROW(df_cp) ) * 100 ,2)


nbb1 <- df_cp[df_cp$Temporada== "NBB 1",]
nbb2 <- df_cp[df_cp$Temporada== "NBB 2",]
nbb3 <- df_cp[df_cp$Temporada== "NBB 3",]
nbb4 <- df_cp[df_cp$Temporada== "NBB 4",]
nbb5 <- df_cp[df_cp$Temporada== "NBB 5",]
nbb6 <- df_cp[df_cp$Temporada== "NBB 6",]
nbb7 <- df_cp[df_cp$Temporada== "NBB 7",]
nbb8 <- df_cp[df_cp$Temporada== "NBB 8",]
nbb9 <- df_cp[df_cp$Temporada== "NBB 9",]



ver <- c(
  round ( (NROW(nbb1[nbb1$type== "Versatile",])/NROW(nbb1) ) * 100 ,2),
  round ( (NROW(nbb2[nbb2$type== "Versatile",])/NROW(nbb2) ) * 100 ,2),
  round ( (NROW(nbb3[nbb3$type== "Versatile",])/NROW(nbb3) ) * 100 ,2),
  round ( (NROW(nbb4[nbb4$type== "Versatile",])/NROW(nbb4) ) * 100 ,2),
  round ( (NROW(nbb5[nbb5$type== "Versatile",])/NROW(nbb5) ) * 100 ,2),
  round ( (NROW(nbb6[nbb6$type== "Versatile",])/NROW(nbb6) ) * 100 ,2),
  round ( (NROW(nbb7[nbb7$type== "Versatile",])/NROW(nbb7) ) * 100 ,2),
  round ( (NROW(nbb8[nbb8$type== "Versatile",])/NROW(nbb8) ) * 100 ,2),
  round ( (NROW(nbb9[nbb9$type== "Versatile",])/NROW(nbb9) ) * 100 ,2))


esp <- c(
  round ( (NROW(nbb1[nbb1$type== "Specialist",])/NROW(nbb1) ) * 100 ,2),
  round ( (NROW(nbb2[nbb2$type== "Specialist",])/NROW(nbb2) ) * 100 ,2),
  round ( (NROW(nbb3[nbb3$type== "Specialist",])/NROW(nbb3) ) * 100 ,2),
  round ( (NROW(nbb4[nbb4$type== "Specialist",])/NROW(nbb4) ) * 100 ,2),
  round ( (NROW(nbb5[nbb5$type== "Specialist",])/NROW(nbb5) ) * 100 ,2),
  round ( (NROW(nbb6[nbb6$type== "Specialist",])/NROW(nbb6) ) * 100 ,2),
  round ( (NROW(nbb7[nbb7$type== "Specialist",])/NROW(nbb7) ) * 100 ,2),
  round ( (NROW(nbb8[nbb8$type== "Specialist",])/NROW(nbb8) ) * 100 ,2),
  round ( (NROW(nbb9[nbb9$type== "Specialist",])/NROW(nbb9) ) * 100 ,2))


group <- c("2009(*)", "2010", "2011", "2012", "2013", "2014", "2015", "2016(*)", "2017(*)")

df <- data.frame(n_versateis=ver,n_especialistas=esp, group=group)
#df$perc <- (df$n_versateis/df$n_especialistas) *100

colnames(df)[1] <- "Versatile"
colnames(df)[2] <- "Specialist"

chart_data <- melt(df, id='group')

chart_data$value<-format(chart_data$value, nsmall = 2) 


chart_data$group = as.factor(chart_data$group)
colnames(chart_data)[2] <- "Classification"
format(chart_data$value, nsmall = 2) 

chart_data$value_n = chart_data$value
chart_data$value_n = as.numeric(chart_data$value_n)

p <- ggplot(data = chart_data, aes(x=group, y=value, group= Classification )) +
  geom_line(aes(color=Classification), size=2) +labs(col="")

p + ylab("Players' frequency (in percentage, %)") +
  xlab("Seasons") +
  theme(axis.text.x=element_text(size=38),axis.text.y=element_text(size=33),
        axis.title=element_text(size=35,face="bold"),
        legend.position="top", legend.title=element_text(size=25), 
        legend.text=element_text(size=40), #size titulo legenda
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0))
        ) +
  geom_point(size =5, aes(color=Classification) ) +
  geom_text(aes(label=value), size=10, vjust=.3) 


#write.csv(df, "outputdata/anosVvsE.csv")

ggsave("graph/figure2.png")





