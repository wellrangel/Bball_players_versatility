
# required packages
# install vip from github repo: devtools::install_github("koalaverse/vip")
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretatio
library(h2o)        # ML model building

# initialize h2o
h2o.init()


x <- c("predict",
       "made_two_points", "lost_two_points",
       "made_three_points", "lost_three_points",
       "made_free_throw_points", "lost_free_throw_points", 
       "dunk_points", "steals","total_assists","offensive_rebounds",
       "defensive_rebounds","commited_fouls", "received_fouls")


x1 <- c("predict", "pos", "jogador",
       "made_two_points", "lost_two_points",
       "made_three_points", "lost_three_points",
       "made_free_throw_points", "lost_free_throw_points", 
       "dunk_points", "steals","total_assists","offensive_rebounds",
       "defensive_rebounds","commited_fouls", "received_fouls")

model_type.H2OMultinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
  
}
#predict_model.H2OMultinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  #pred <- h2o.predict(x, as.h2o(newdata))
  
  # return classification probabilities only
  #return(as.data.frame(pred[,-1]))
  
#}

source("scriptsnovoscampos/functions.R")
#aml <- h2o.loadModel("ml_model/model/StackedEnsemble_BestOfFamily_0_AutoML_20190306_202706_BOM")

aml <- h2o.loadModel("ml_model/model/StackedEnsemble_AllModels_0_AutoML_Winner_0.70_0.71")
#aml <- h2o.loadModel("ml_model/model/StackedEnsemble_BestOfFamily_0_AutoML_20190306_215319_BOMM")

#aml <- h2o.loadModel("ml_model/model/StackedEnsemble_BestOfFamily_0_AutoML_20190306_220846")


dfstats <- readData();

NROW(dfstats)
dfstats <-dfstats[dfstats$played_minutes > 100,]

dfstats$pos <- str_replace(dfstats$pos, "/" ,"") 
dfstats$pos <- str_replace_all(dfstats$pos, "ô", "o")
dfstats$pos <- as.factor(dfstats$pos)

dfstats$pos <- as.factor(dfstats$pos)
unique(dfstats$pos)
options(digits=10)


alldata.hex  <- as.h2o(dfstats) 


pred_all <- h2o.predict(aml, alldata.hex)   # predict(aml, test) also works
mean(pred_all$predict==alldata.hex$pos)

dataset_pred_all <- as.data.frame(pred_all)

dfall <- cbind(dataset_pred_all,  dfstats) 

training <- dfall[dfall$Temporada != "NBB 9",]
training <- training[,x]
training <- training[,-1]
head(training)
# Lime is used to get explain on the train data
explainer <- lime::lime(training, aml, bin_continuous = TRUE, n_bins = 5, n_permutations = 1000)
class(explainer)
#?lime::lime
summary(explainer)

nbb9 <- dfall[dfall$Temporada == "NBB 9",]
nbb9 <- nbb9[,x1]

test <- dfall[dfall$Temporada == "NBB 9",]
test <-test[test$jogador_apelido == "Rollins" ,]
  #test$jogador_apelido == "Marcelinho" |
  #test$jogador_apelido == "Davi" | 
  #test$jogador_apelido == "Deryk" |
  #test$jogador == "Alex Ribeiro Garcia" |
  #test$jogador_apelido == "Marquinhos" |
  #test$jogador_apelido == "Giovannoni" |
  #test$jogador == "Jefferson William Andrade da Silva Antônio" |
  #test$jogador_apelido == "Olivinha" |
  #test$jogador_apelido == "Betinho" |
  #test$jogador == "Lucas Fernandes Mariano" |
  #test$jogador_apelido == "De Paula" |
  #test$jogador_apelido == "Fúlvio",]

#Mundotest <- test[test$jogador == "Tyrone Denell Curnell" |
#                                      test$jogador == "Kendall Lamont Anthony" |
#                                      test$jogador_apelido == "Rollins" |
#                                      test$jogador_apelido == "Bennett" |
#                                      test$jogador_apelido == "Rodgers" |
#                                      test$jogador_apelido == "Maynard" |
#                                      test$jogador == "Kenny Montrell Dawkins" |
#                                      test$jogador_apelido == "Holloway" |
#                                      test$jogador_apelido == "Shamell" |
#                                      test$jogador_apelido == "Jackson" |
#                                      test$jogador_apelido == "Hure" |
#                                      test$jogador_apelido == "Cafferata",]

test <- test[,x]
test <- test[,-1]
#?lime::explain
explanation <- lime::explain(
  x = test, 
  explainer = explainer, 
  #n_permutations = 5000,
  #dist_fun = "gower",
  #kernel_width = .75,
  n_features = 13, 
  feature_select = "highest_weights",
  #label = test$predict
  n_label=1
)

#mean(nbb9$total_assists)

#mean(nbb9[nbb9$predict=="Ala",]$total_assists)
#mean(nbb9[nbb9$predict=="Armador",]$total_assists)
#mean(nbb9[nbb9$predict=="Pivo",]$total_assists)
#mean(nbb9[nbb9$predict=="AlaArmador",]$total_assists)
#mean(nbb9[nbb9$predict=="AlaPivo",]$total_assists)

#test$total_assists

plot_features(explanation)
#plot_explanations(explanation)

