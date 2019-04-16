df_cp <- read.csv("outputdata/classified_players.csv", sep=",", stringsAsFactors=FALSE)

df_cp <- df_cp[df_cp$Temporada == "NBB 8",]
round((mean(df_cp$predict==df_cp$pos)*100),2)

26/33
PG <- df_cp[df_cp$pos1 == "PG",] #33
table(PG$predict)
round((mean(PG$predict==PG$pos)),2)

SG <- df_cp[df_cp$pos1 == "SG",] # 18
table(SG$predict)
round((mean(SG$predict==SG$pos)),2)

SF <- df_cp[df_cp$pos1 == "SF",] # 43
round((mean(SF$predict==SF$pos)),2)

PF <- df_cp[df_cp$pos1 == "PF",] # 26
round((mean(PF$predict==PF$pos)),2)

C <- df_cp[df_cp$pos1 == "C",] #41
table(C$predict)
round((mean(C$predict==C$pos)),2)



table(df_cp$pos)
