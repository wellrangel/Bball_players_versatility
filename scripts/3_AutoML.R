#model_id mean_per_class_error
#1    DeepLearning_grid_0_AutoML_20180517_075319_model_2         0.3080599689
#2 StackedEnsemble_BestOfFamily_0_AutoML_20180517_075319         0.3113437440
#3             GBM_grid_0_AutoML_20180517_075319_model_7         0.3267283594
#4    DeepLearning_grid_0_AutoML_20180517_075319_model_0         0.3621708116
#35             GLM_grid_0_AutoML_20180517_075319_model_0         0.3680728972
#6             GBM_grid_0_AutoML_20180517_075319_model_3         0.3722498554

library(h2o)

source("scripts/functions.R")

h2o.init(nthreads = -1)
options(scipen=20)

dfstats <- readData();
dfstats <-dfstats[dfstats$played_minutes > 100,]
unique(dfstats$posicao)


dfstats$posicao <- str_replace(dfstats$posicao, "/" ,"") 
dfstats$posicao <- str_replace_all(dfstats$posicao, "ô", "o")
dfstats$posicao <- as.factor(dfstats$posicao)



dfstats$posicao <- as.factor(dfstats$posicao)

options(digits=10)

training <- getTreinoData(dfstats)
validation <- getValidacaoData(dfstats)
test <- getTesteData(dfstats)

training.hex  <- as.h2o(training) # convertendo df para formato h2o
validation.hex  <- as.h2o(validation) 
test.hex  <- as.h2o(test)

table(test$equipe,test$posicao)

alldata.hex  <- as.h2o(dfstats) 




x <- c(
  "tried_two_points", "two_points", "lost_two_points", "wrong_throws", 
  "tried_three_points", "three_points","dunk_points","tried_free_throw_points", "free_throw_points", 
  "steals","total_assists","offensive_rebounds","defensive_rebounds","commited_fouls", "received_fouls")



y <- "posicao"


aml <- h2o.automl(x = x, y = y,
                  training_frame = training.hex,
                  seed = 123,
                  validation_frame=validation.hex, 
                  leaderboard_frame = test.hex, # com 0.7045454545, sem esse parametro 0.7196969697
                  #stopping_metric = "mean_per_class_error",
                  max_runtime_secs = 60)



# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

# The leader model is stored here
aml@leader
class(aml@leader)

# save the model
model_path <- h2o.saveModel(object=aml@leader, path="ml_model/model", force=TRUE)

my_varimpRF <- h2o.varimp(aml@leader)

print(my_varimpRF)


# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, validation.hex)   # predict(aml, test) also works
mean(pred$predict==validation.hex$posicao)


perf <- h2o.performance(aml@leader, test.hex)
h2o.confusionMatrix(perf)

pred_all <- h2o.predict(aml, alldata.hex)   # predict(aml, test) also works
mean(pred_all$predict==alldata.hex$posicao)

dataset_pred_all <- as.data.frame(pred_all)

dfall <- cbind(dataset_pred_all,  dfstats) 

#write.csv(dfall, file = "outputdata/df_all.csv")
#h2o.shutdown(prompt=FALSE)
