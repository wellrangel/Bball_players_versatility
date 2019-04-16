
#reading csv file with aggregate 9 seasons in nbb
dfstats <- read.csv("outputdata/classified_players.csv", sep = "," ,stringsAsFactors=FALSE)

unique(dfstats$pos)

dfstats$posingles <- dfstats$pos

dfstats$posingles[dfstats$posingles == "Armador"] <- "Point Guard"

dfstats$posingles[dfstats$posingles == "AlaArmador"] <- "Shooting Guard"

dfstats$posingles[dfstats$posingles == "Ala"] <- "Small Forward"

dfstats$posingles[dfstats$posingles == "AlaPivo"] <- "Power Forward"

dfstats$posingles[dfstats$posingles == "Pivo"] <- "Center"

dfstats$pos1 = dfstats$posingles
dfstats$PG = dfstats$Armador
dfstats$SG = dfstats$AlaArmador
dfstats$SF = dfstats$Ala
dfstats$PF = dfstats$AlaPivo
dfstats$C = dfstats$Pivo
dfstats$pos1[dfstats$pos1 == "Point Guard"] <- "PG"
dfstats$pos1[dfstats$pos1 == "Shooting Guard"] <- "SG"
dfstats$pos1[dfstats$pos1 == "Small Forward"] <- "SF"
dfstats$pos1[dfstats$pos1 == "Power Forward"] <- "PF"
dfstats$pos1[dfstats$pos1 == "Center"] <- "C"


dfstats$jogador.1 = NULL
dfstats$X.1 = NULL
dfstats$X = NULL
#save file
write.csv(dfstats, "outputdata/classified_players.csv", fileEncoding = "UTF-8")

