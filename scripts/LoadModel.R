# required packages
# install vip from github repo: devtools::install_github("koalaverse/vip")
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(stringr)      # ML model building
library(h2o)        # ML model building

# initialize h2o
h2o.init()

source("scriptsnovoscampos/functions.R")
aml <- h2o.loadModel("ml_model/model/StackedEnsemble_BestOfFamily_0_AutoML_Winner_0.70_0.71")

aml@model

dfstats <- readData();

NROW(dfstats)
dfstats <-dfstats[dfstats$played_minutes > 100,]

dfstats$pos <- str_replace(dfstats$pos, "/" ,"") 
dfstats$pos <- str_replace_all(dfstats$pos, "ô", "o")
dfstats$pos <- as.factor(dfstats$pos)

dfstats$pos <- as.factor(dfstats$pos)
unique(dfstats$pos)
options(digits=10)

training <- getTreinoData(dfstats)
validation <- getValidacaoData(dfstats)
test <- getTesteData(dfstats)

training.hex  <- as.h2o(training) # convertendo df para formato h2o
validation.hex  <- as.h2o(validation) 
test.hex  <- as.h2o(test)

alldata.hex  <- as.h2o(dfstats) 

pred <- h2o.predict(aml, validation.hex)   # predict(aml, test) also works
mean(pred$predict==validation.hex$pos)


pred <- h2o.predict(aml, test.hex)   # predict(aml, test) also works
mean(pred$predict==test.hex$pos)


pred_all <- h2o.predict(aml, alldata.hex)   # predict(aml, test) also works
mean(pred_all$predict==alldata.hex$pos)

dataset_pred_all <- as.data.frame(pred_all)

dfall <- cbind(dataset_pred_all,  dfstats) 

write.csv(dfall, file = "outputdata/df_all_predicted.csv", fileEncoding = "UTF-8")
1-(47 / 161)
0.7168674699

round((100*0.7080745342),2)

round((100*0.7116564417),2)
