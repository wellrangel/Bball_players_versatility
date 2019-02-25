
#reading csv file with aggregate 9 seasons in nbb
dfstats <- read.csv("outputdata/df_all.csv", sep = "," ,stringsAsFactors=FALSE)

unique(dfstats$pos)

dfstats$pos[dfstats$pos == "Armador"] <- "Point Guard"

dfstats$pos[dfstats$pos == "AlaArmador"] <- "Shooting Guard"

dfstats$pos[dfstats$pos == "Ala"] <- "Small Forward"

dfstats$pos[dfstats$pos == "AlaPivo"] <- "Power Forward"

dfstats$pos[dfstats$pos == "Pivo"] <- "Center"

dfstats$pos1 = dfstats$pos
dfstats$pos1[dfstats$pos1 == "Point Guard"] <- "PG"
dfstats$pos1[dfstats$pos1 == "Shooting Guard"] <- "SG"
dfstats$pos1[dfstats$pos1 == "Small Forward"] <- "SF"
dfstats$pos1[dfstats$pos1 == "Power Forward"] <- "PF"
dfstats$pos1[dfstats$pos1 == "Center"] <- "C"


dfstats$jogador.1 = NULL
dfstats$X.1 = NULL
dfstats$X = NULL
#save file
write.csv(dfstats, file = "outputdata/aggregateData.csv")

