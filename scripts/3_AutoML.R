    library(h2o)
    library(stringr)
    
    #seed to reproducibility
    set.seed(123122); 
    
    #import my own library functions
    source("scripts/functions.R")
    
    #init h2o framework
    h2o.init(nthreads=-1)
    h2o.removeAll()
    #read all data
    dfstats <- readData();
    
    NROW(dfstats)
    
    #reading csv file with aggregate 9 seasons in nbb
    dfstats <-dfstats[dfstats$played_minutes > 100,]
    
    #removing special characters
    dfstats$pos <- str_replace(dfstats$pos, "/" ,"") 
    dfstats$pos <- str_replace_all(dfstats$pos, "ô", "o")
    dfstats$pos <- as.factor(dfstats$pos)
    
    #to factor type
    dfstats$pos <- as.factor(dfstats$pos)
    
    options(digits=10)
    
    #split in all data in training, validation and test dataset
    training <- getTreinoData(dfstats)
    validation <- getValidacaoData(dfstats)
    test <- getTesteData(dfstats)
    
    #to h2o type
    training.hex  <- as.h2o(training) # convertendo df para formato h2o
    validation.hex  <- as.h2o(validation) 
    test.hex  <- as.h2o(test)
    alldata.hex  <- as.h2o(dfstats) 
    
    #to define the columns for the analysis
    x <- c(
      "made_two_points", "lost_two_points",
      "made_three_points", "lost_three_points",
      "made_free_throw_points", "lost_free_throw_points", 
      "dunk_points", "steals","total_assists","offensive_rebounds",
      "defensive_rebounds","commited_fouls", "received_fouls")
    
    #set the position column for the prediction
    y <- "pos"
    
    #run automl predict
    aml <- h2o.automl(x = x, y = y,
                      training_frame = training.hex,
                      seed = 123122,
                      max_models = 8,
                      exclude_algos = "DeepLearning",
                      validation_frame=validation.hex,
                      stopping_metric = "mean_per_class_error")
    
    
    # View the AutoML Leaderboard
    lb <- aml@leaderboard
    lb
    
    # The leader model is stored here
    aml@leader
    class(aml@leader)
    

    # model object directly
    
    pred <- h2o.predict(aml, validation.hex)   # predict(aml, test) also works
    #validation accuracy
    mean(pred$predict==validation.hex$pos)
    
    perfv <- h2o.performance(aml@leader, validation.hex)
    h2o.confusionMatrix(perfv)
    
    #test accuracy
    pred_teste <- h2o.predict(aml, test.hex)   # predict(aml, test) also works
    mean(pred_teste$predict==test.hex$pos)
    
    perf <- h2o.performance(aml@leader, test.hex)
    h2o.confusionMatrix(perf)
    
    
    pred_all <- h2o.predict(aml, alldata.hex)   # predict(aml, test) also works
    mean(pred_all$predict==alldata.hex$pos)
    
    dataset_pred_all <- as.data.frame(pred_all)
    dfall <- cbind(dataset_pred_all,  dfstats) 
    
    
    #save in file
    write.csv(dfall, file = "outputdata/df_all_predicted.csv", fileEncoding = "UTF-8")
    
    
    # save the model
    model_path <- h2o.saveModel(object=aml@leader, path="ml_model/model", force=TRUE)
    
    #turnoff h2o
    h2o.shutdown(prompt=FALSE)
  
    