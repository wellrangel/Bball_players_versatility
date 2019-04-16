library(stringr)
library(sqldf)
library(ggplot2)
library(plyr)
source("scriptsnovoscampos/functions.R")

df_cp <- read.csv("outputdata/classified_players_novos_atributos.csv", sep=",", stringsAsFactors=FALSE)
table(df_cp$type,df_cp$Temporada)
df_cp$exp <- as.numeric(df_cp$exp)
df_cp <- df_cp[df_cp$Temporada=="NBB 9", ]

#ve_low_exp 1 and 2
df_le <- df_cp[df_cp$exp<=3, ]

df_le_ver <- df_le[df_le$type== "Versatile", ]

table(df_le_ver$pos1)

perc_esp_le <- round ( (NROW(df_le[df_le$type== "Specialist",])/NROW(df_le) ) * 100 ,2)
perc_ver_le <- round ( (NROW(df_le[df_le$type== "Versatile",])/NROW(df_le) ) * 100 ,2)

#ve_middle_exp 2 to 5
df_me <- df_cp[df_cp$exp>3 & df_cp$exp < 7, ]

df_me_ver <- df_me[df_me$type== "Versatile", ]

table(df_me_ver$pos1)

perc_esp_me <- round ( (NROW(df_me[df_me$type== "Specialist",])/NROW(df_me) ) * 100 ,2)
perc_ver_me <- round ( (NROW(df_me[df_me$type== "Versatile",])/NROW(df_me) ) * 100 ,2)

#ve_middle_exp 6 to 9
df_he <- df_cp[df_cp$exp>6, ]

df_he_ver <- df_he[df_he$type== "Versatile", ]

table(df_he_ver$pos1)



perc_esp_he <- round ( (NROW(df_he[df_he$type== "Specialist",])/NROW(df_he) ) * 100 ,2)
perc_ver_he <- round ( (NROW(df_he[df_he$type== "Versatile",])/NROW(df_he) ) * 100 ,2)

exp <- data.frame(exp="le", p_esp = perc_esp_le, p_ver= perc_ver_le)
exp <- rbind(exp, data.frame(exp="me", p_esp = perc_esp_me, p_ver= perc_ver_me))
exp <- rbind(exp, data.frame(exp="he", p_esp = perc_esp_he, p_ver= perc_ver_he))
                  



