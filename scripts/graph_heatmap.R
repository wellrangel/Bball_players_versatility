  library(stringr)
  library(cluster) #biblioteca para teste do numero de clusters
  library(reshape)
  library(reshape2)
  library(sqldf)
  library(ggplot2)
  library(plyr)
  
  
  source("scriptsnovoscampos/functions.R")
  
  
  df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)
  #last season
  df_cp <- df_cp[df_cp$Temporada== "NBB 9",]
  
  round ( (NROW(df_cp[df_cp$type== "Specialist",])/NROW(df_cp) ) * 100 ,2)
  
  round ( (NROW(df_cp[df_cp$type== "Versatile",])/NROW(df_cp) ) * 100 ,2)
  
  
  
  #####Temporada 9 
  
  head(df_cp)
  
  
  mat <- criaMatrizMapaV2(df_cp)
  
  mat$label = ""
  for(i in 1:nrow(mat)) {
    
    mat[i,]$label = GetDetalheMapa (DetalheMapa, mat[i,]$Var1, mat[i,]$Var2, mat[i,]$value, NROW(df_cp))
    #  print(mat[i,]$value)
  }
  
  mat$Var1 <- as.factor(mat$Var1)
  mat$Var2 <- as.factor(mat$Var2)
  
  perc_col1 <- (sum(mat[mat$Var2 == "1 Position",]$value))/sum(mat$value)
  perc_col1 <- round(perc_col1*100,2)
  perc_col1 <- paste0(perc_col1, "%")
  perc_col1 <- "52.16%"
  
  perc_col2 <- (sum(mat[mat$Var2 == "2 Positions",]$value))/sum(mat$value)
  perc_col2 <- round(perc_col2*100,2)
  #perc_col2 <- paste0(perc_col2, "%")
  perc_col2 <- "39.88%"
  
  perc_col3 <- (sum(mat[mat$Var2 == "3 Positions",]$value))/sum(mat$value)
  perc_col3 <- round(perc_col3*100,2)
  #perc_col3 <- paste0(perc_col3, "%")
  perc_col3 <- "7.97%"
  
  
  
  
  mat$value <- as.factor(mat$value)
  
  
  p <- ggplot(data = mat, aes(x=Var2, y=Var1)) + 
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = label ) ,col='black',size=10) + 
    xlab("Positions") +
    ylab("Frequency (in percentage, %)") +
    guides(fill=FALSE) + 
    theme(axis.text.x=element_text(size=40),axis.text.y=element_text(size=40),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title=element_text(size=35,face="bold")) +
    
    scale_fill_manual(
      values =  c("white", rev(heat.colors(12)) )) 
  
  
  p + annotate("text", color="black", size = 10, x = 1:3, y = 10, fontface =2, label = c(perc_col1, perc_col2, perc_col3))
  
  ggsave("graph/figure1.png")
  
  
  
  
