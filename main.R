aMicroclimate_2_hours = read.csv("./Data/Microclimate_2_hours.csv",header = T, stringsAsFactors = FALSE)
Microclimate_2_hours_summary = summary(Microclimate_2_hours)
Macroclimate_Sidi_Ifni_Weather_Station = read.csv("./Data/Macroclimate_Sidi_Ifni_Weather_Station.csv",header = T, stringsAsFactors = FALSE)
Macroclimate_Sidi_Ifni_Weather_Station_summary = summary(Macroclimate_Sidi_Ifni_Weather_Station)

missingDataColumnCountDF = data.frame(do.call(rbind, lapply(1:ncol(Macroclimate_Sidi_Ifni_Weather_Station), function(x){
  missingDataColumnCount = length(which(is.na(Macroclimate_Sidi_Ifni_Weather_Station[,x])))
  if(missingDataColumnCount > 0)
    c(colnames(Macroclimate_Sidi_Ifni_Weather_Station)[x], missingDataColumnCount)
})))

colnames(missingDataColumnCountDF) <- c("Column", "NA_Count")

Microclimate_2_hours_summary = data.frame(do.call(rbind, lapply(1:ncol(Microclimate_2_hours), function(x){summary(Microclimate_2_hours[,x])})))

Macroclimate_Agadir = read.csv("./Data/Macroclimate_Agadir.csv",header = T, stringsAsFactors = FALSE)

Macroclimate_Guelmim_Airport = read.csv("./Data/Macroclimate_Guelmim_Airport.csv",header = T, stringsAsFactors = FALSE)

factorColumnIndexes_Train = c()
numericColumnIndexes_Train = c()

# skipping the 1st column as target
for(col in 2:ncol(Macroclimate_Sidi_Ifni_Weather_Station)){
  if(class(Macroclimate_Sidi_Ifni_Weather_Station[,col]) != "numeric")
    factorColumnIndexes_Train = c(factorColumnIndexes_Train, col)
  else
    numericColumnIndexes_Train = c(numericColumnIndexes_Train, col)
}

Microclimate_2_hours = read.csv("./Data/Macroclimate_Agadir.csv",header = T, stringsAsFactors = FALSE)


# list of type of clouds
transform_c_attr <- function(df){
  cloud_list = c("Few clouds", "Scattered clouds","Broken clouds", "Overcast")
  col_names = c(cloud_list, "cumulonimbus clouds", "cumulus congestus of great vertical extent")
  cloud_df = data.frame(matrix(0, nrow = nrow(df), ncol = length(col_names)))
  colnames(cloud_df) = col_names
  invisible(sapply(1:nrow(df), function(index){
    #print(index)
    x = df[,"c"][index]
    invisible(sapply(unlist(strsplit(x, split= ",")), function(y){
      # check the types
      found = TRUE
      cloudType = ""
      for(cloud in cloud_list){
        if(grepl(cloud, y, ignore.case = TRUE)){
          cloudType <- cloud
          found = FALSE
          break
        }
      }
      #print(cloudType)
      if(found){
        if(grepl("cumulonimbus clouds", y, ignore.case = TRUE))
          cloud_df[index,"cumulonimbus clouds"] <<- 1
        else if(grepl("cumulus congestus of great vertical extent", y, ignore.case = TRUE))
          cloud_df[index,"cumulus congestus of great vertical extent"] <<- 1
        #else
        #  cloud_df[index,"other"] <<- y
      }else{
        val = unlist(strsplit(regmatches(y, regexpr("[0-9]+ m", y))," "))[1]
        if(!is.null(val))
          cloud_df[index, cloudType] <<- unlist(strsplit(regmatches(y, regexpr("[0-9]+ m", y))," "))[1]
      }
    }))
    }))
  #do.call(rbind())
  return(cloud_df)
}


cloud_df = transform_c_attr(Macroclimate_Agadir)
Macroclimate_Agadir <- cbind(Macroclimate_Agadir, cloud_df)
write.csv(Macroclimate_Agadir, file = "Macroclimate_Agadir_c_attr_transformed.csv")

cloud_df = transform_c_attr(Macroclimate_Guelmim_Airport)
Macroclimate_Guelmim_Airport <- cbind(df, cloud_df)
write.csv(Macroclimate_Guelmim_Airport, file = "Macroclimate_Guelmim_Airport_c_attr_transformed.csv")


##################### DATA LOADING #####################

## TXT
Microclimate_2_hours_test = read.csv("./Data/Microclimate_Test_2_hours.txt",sep = "\t", 
                                     header = T, stringsAsFactors = FALSE)


# TXT
Microclimate_2_hours_train = read.table("./Data/training_set_microclimate(2 hour).txt",sep = "\t", 
                                        header = T, stringsAsFactors = FALSE)


# TXT
submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)


######################### BASE MODEL #################################################
library("Metrics", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

Microclimate_2_hours_train = read.csv("./Data/training_set_microclimate(2 hour)_with_yield.csv",header = T, stringsAsFactors = FALSE)

Microclimate_2_hours_train = read.table("./Data/training_set_microclimate(2 hour).txt", header = T, stringsAsFactors = FALSE)


## fixing data - done! ##

## Linear regression
LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Microclimate_2_hours_train[3:ncol(Microclimate_2_hours_train)-1]),
                                   collapse = "+"), sep = "~")), Microclimate_2_hours_train)



pr = predict(LM_all_features_model, 
             Microclimate_2_hours_train[3:ncol(Microclimate_2_hours_train)-1])

## train on train - MSE = 2.695621
rms_train_lm_all_features = rmse(Microclimate_2_hours_train$yield, predict(LM_all_features_model, 
                                                                          Microclimate_2_hours_train[3:ncol(Microclimate_2_hours_train)-1]))

merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],Microclimate_2_hours_test)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                              merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm = predict(LM_all_features_model, 
        submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index)){
    if(predicted_lm[target_index] > 0)
      submission_format[index,2] = predicted_lm[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/timbuktoo_lm.csv", 
          row.names = FALSE)


###################### BASE MODEL - END ###################### 

library("earth", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

MARS_all_features_model = earth(as.formula(paste("yield",paste(colnames(Microclimate_2_hours_train[3:ncol(Microclimate_2_hours_train)-1]), 
                                      collapse = "+"), sep = "~")),
                                data = Microclimate_2_hours_train, degree = 2, 
                                nfold = 5, ncross = 6, keepxy = T)

pr = predict(MARS_all_features_model)

rms_mars_all_features = rmse(Microclimate_2_hours_train$yield, predict(MARS_all_features_model))

MARS_all_features_model.summary = summary(MARS_all_features_model)

# TXT
submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],
                          Microclimate_2_hours_test)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_mars = predict(MARS_all_features_model, 
                       submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index)){
    if(predicted_mars[target_index] > 0 && predicted_mars[target_index] < 60)
      submission_format[index,2] = predicted_mars[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/fog_mars.csv", 
          row.names = FALSE)

#### #####



### Cross validation ### 

# http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
# Apply log tranformation to continous values, 

log.Microclimate_2_hours_train <- log(Microclimate_2_hours_train[, 3:ncol(Microclimate_2_hours_train) - 1])

Microclimate_2_hours_train.pca <- prcomp(~ ., 
                  data=Microclimate_2_hours_train[, 3:ncol(Microclimate_2_hours_train) - 1],
                 center = TRUE,
                 scale. = TRUE) 

summary(Microclimate_2_hours_train.pca)

# biplot code - 

vari = varimax(Microclimate_2_hours_train.pca$rotation)




best_attr = c("humidity", "leafwet_lwscnt","leafwet450_min", "gusts_ms", "wind_ms")

bad_attr = c("temp", "percip_mm", "leafwet_lwscnt", "wind_dir")

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Microclimate_2_hours_train[best_attr]),
                                                           collapse = "+"), sep = "~")), Microclimate_2_hours_train)

my_Microclimate_2_hours_train.pca = Microclimate_2_hours_train.pca

biplot(my_Microclimate_2_hours_train.pca, cex = c(1,0.7))

pr = predict(LM_all_features_model, 
             Microclimate_2_hours_train[best_attr])


library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(my_Microclimate_2_hours_train.pca, obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Microclimate_2_hours_train[best_attr]),
                                                           collapse = "+"), sep = "~")), Microclimate_2_hours_train)

## train on train - MSE = 2.69841
rms_train_lm_all_features = rmse(Microclimate_2_hours_train$yield, predict(LM_all_features_model, 
                                                                           Microclimate_2_hours_train[best_attr]))

merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],Microclimate_2_hours_test)
submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm = predict(LM_all_features_model, 
                       submission_input[best_attr])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index)){
    if(predicted_lm[target_index] > 0)
      submission_format[index,2] = predicted_lm[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/pca_timbucktoo_lm.csv", 
          row.names = FALSE)


# k-fold cross validation - http://robjhyndman.com/hyndsight/crossvalidation/

# https://www.researchgate.net/post/How_to_apply_the_k-cross_validation_technique_when_the_data_is_in_the_form_time_series

# reading a lot on k-fold cross validation on time series data,
# I am going to implement in the following way
# Get the dates, and take care to not to predict from the future!!!!
# Initially get the days and then shift the window by few more days
# Say, 1 to 100 days
# 70% train to 30% test
# 1 to 70 days - train and 71 to 100 - test
# 2 to 71 days - train and 72 to 100 - test
# 3 to 72 days - train and 73 to 100 - test 

someenv<-new.env()
#someenv[["1"]]<- c(2,4)

# get all days in the training set
days = c()
dayCount = 1
someenv[[toString(dayCount)]] = c(1)
for(rowIndex in 2:nrow(Microclimate_2_hours_train)){
  if(unlist(strsplit(Microclimate_2_hours_train[rowIndex,1], split = " "))[1] > 
     unlist(strsplit(Microclimate_2_hours_train[rowIndex - 1,1], split = " "))[1]){
    someenv[[toString(dayCount)]] = c(someenv[[toString(dayCount)]], rowIndex - 1)
    dayCount = dayCount + 1
    days = c(days, dayCount)
    someenv[[toString(dayCount)]] = c(rowIndex)
  }
}

  
k_fold_cross_validation <- function(k_fold, train_perc, dataCount, someenv){
  cross_validated_list <- new.env()
  jumpSteps = round((dataCount * (100 - train_perc)/100)/k_fold)
  print(jumpSteps)
  startWindow = 1
  trainWindow = round(dataCount * train_perc/100)
  print(trainWindow)
  for(i in 1:k_fold){
    cross_validated_list[[toString(i)]] = c(someenv[[toString(startWindow)]][1], 
                                             someenv[[toString(startWindow + trainWindow)]][2])
    print(cross_validated_list[[toString(i)]])
    startWindow = startWindow + jumpSteps
  }  
  return(cross_validated_list)
}

k_fold = 5
train_perc = 70
k_fold_CV_list = k_fold_cross_validation(k_fold,train_perc,length(days),someenv)

for(i in 1:k_fold){
  LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Microclimate_2_hours_train[best_attr]),
                                                           collapse = "+"), sep = "~")), 
                             Microclimate_2_hours_train[k_fold_CV_list[[toString(i)]][1]:k_fold_CV_list[[toString(i)]][2],])
  
  
  ## train on train - MSE = 2.69841
  rms_train_lm_all_features = rmse(Microclimate_2_hours_train[(k_fold_CV_list[[toString(i)]][2]+1):nrow(Microclimate_2_hours_train),]$yield, predict(LM_all_features_model, 
                            Microclimate_2_hours_train[(k_fold_CV_list[[toString(i)]][2]+1):nrow(Microclimate_2_hours_train),best_attr]))
  
  print(rms_train_lm_all_features)
}


merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],Microclimate_2_hours_test)
submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm = predict(LM_all_features_model, 
                       submission_input[best_attr])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index)){
    if(predicted_lm[target_index] > 0)
      submission_format[index,2] = predicted_lm[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/k_fold_pca_timbucktoo_lm.csv", 
          row.names = FALSE)




##### RANDOM FORREST ####

### WITH PCA ### 
   
rf = randomForest(yield ~ humidity+leafwet_lwscnt + leafwet450_min + gusts_ms + wind_ms,
                  data = Microclimate_2_hours_train, importance = TRUE, ntree = 2000)

pr = predict(rf, Microclimate_2_hours_train[,2:ncol(Microclimate_2_hours_train) - 1])

#  1.191716
rms_train_lm_all_features = rmse(Microclimate_2_hours_train$yield, pr)

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],
                          Microclimate_2_hours_test)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_rf = predict(rf, 
                         submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index)){
    if(predicted_rf[target_index] > 0 && predicted_rf[target_index] < 60)
      submission_format[index,2] = predicted_rf[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/rf_timbuktoo.csv", 
          row.names = FALSE)

### WITHOUT PCA ### 

rf = randomForest(yield ~ percip_mm+humidity+temp+leafwet450_min+leafwet_lwscnt+gusts_ms+wind_dir+wind_ms,
                  data = Microclimate_2_hours_train, importance = TRUE, ntree = 2000)

pr = predict(rf, Microclimate_2_hours_train[,2:ncol(Microclimate_2_hours_train) - 1])

# 0.7727516
rms_train_lm_all_features = rmse(Microclimate_2_hours_train$yield, pr)

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],
                          Microclimate_2_hours_test)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_rf = predict(rf, 
                       submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index)){
    if(predicted_rf[target_index] > 0 && predicted_rf[target_index] < 60)
      submission_format[index,2] = predicted_rf[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/rf_timbuktoo_without_pca.csv", 
          row.names = FALSE)

###### http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/ 

##### NUERAL NET

library(neuralnet)
n <- names(Microclimate_2_hours_train)[2:ncol(Microclimate_2_hours_train)]
n <- c(best_attr, "yield")
f <- as.formula(paste("yield ~", paste(n[!n %in% "yield"], collapse = " + ")))

nn <- neuralnet(f,data=Microclimate_2_hours_train,hidden=c(5,3),linear.output=T,threshold = 0.02,
                stepmax = 2e+05)

nn <- neuralnet(f,data=Microclimate_2_hours_train[,n],hidden=3,linear.output=T,threshold = 0.01)

pr.nn <- compute(nn1,Microclimate_2_hours_train[,best_attr])

rmse_neural_net = rmse(Microclimate_2_hours_train$yield, pr.nn$net.result)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

which(is.na(Microclimate_2_hours_train[,2]))



###### MACRO-CLIMATE ######



# load the file

Macroclimate_Guelmim_Airport = read.csv("/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Data/gulTheFinal.csv",
         header = T, stringsAsFactors = FALSE)

Macroclimate_Agadir_Airport = read.csv("/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Data/agadir.csv",
                                       header = T, stringsAsFactors = FALSE)

Microclimate_2_hours_Train = read.table("/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Data/training_set_microclimate(2 hour).txt",                                     
                                      header = T, sep = "\t",stringsAsFactors = FALSE)

####### Macroclimate_Guelmim_Airport_Merged #############

Macroclimate_Guelmim_Airport_Merged = data.frame(stringsAsFactors=TRUE)
timeStamps = c()
for(index in 1:nrow(Microclimate_2_hours_Train)){
  startTimeStamp = Microclimate_2_hours_Train[index,1]
  targetIndex = which(Macroclimate_Guelmim_Airport[,1] == startTimeStamp)
  if(length(targetIndex) > 0){
    attrVal = c()
    emptyRow = data.frame()
    for(attr in 2:ncol(Macroclimate_Guelmim_Airport)){
      attrVal = c(attrVal, mean(c(Macroclimate_Guelmim_Airport[targetIndex,attr], 
                                  Macroclimate_Guelmim_Airport[targetIndex-1,attr]), na.rm = TRUE))
    }
    attrVal = c(attrVal, Microclimate_2_hours_Train[index,"yield"])
    #print(Microclimate_2_hours_Train[index,"yield"])
    timeStamps = c(timeStamps, startTimeStamp)
    Macroclimate_Guelmim_Airport_Merged <- rbind(Macroclimate_Guelmim_Airport_Merged,
                                                attrVal)
  }
}
Macroclimate_Guelmim_Airport_Merged = cbind(data.frame(timeStamps),Macroclimate_Guelmim_Airport_Merged)
colnames(Macroclimate_Guelmim_Airport_Merged) <- c(colnames(Macroclimate_Guelmim_Airport), "yield")
# Macroclimate_Guelmim_Airport_Merged <- subset( Macroclimate_Guelmim_Airport_Merged, select = -Ff)

## EXPORTING TO CSV

write.csv(Macroclimate_Guelmim_Airport_Merged, 
          file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/Macroclimate_Guelmim_Airport_Merged.csv", 
          row.names = FALSE)


## LM WITHOUT PCA ON Macroclimate_Guelmim_Airport_Merged

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Macroclimate_Guelmim_Airport_Merged[3:ncol(Macroclimate_Guelmim_Airport_Merged)-1]),
                                                           collapse = "+"), sep = "~")), Macroclimate_Guelmim_Airport_Merged)

## train on train - MSE = 3.063255599
rms_train_lm_all_features = rmse(Macroclimate_Guelmim_Airport_Merged$yield, predict(LM_all_features_model, 
                                                                                    Macroclimate_Guelmim_Airport_Merged[1,2:ncol(Macroclimate_Guelmim_Airport_Merged) - 1]))


#merged_train_test = rbind(Macroclimate_Guelmim_Airport_Merged[,1:ncol(Macroclimate_Guelmim_Airport_Merged)-1],
#                          Microclimate_2_hours_test)

#merged_train_test = Macroclimate_Guelmim_Airport_Merged

merged_train_test = Macroclimate_Guelmim_Airport

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index)){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm_guelmin_without_pca = predict(LM_all_features_model, 
                       submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index) > 0 && !is.na(predicted_lm_guelmin_without_pca[target_index])){
    #print(target_index)
    if(predicted_lm_guelmin_without_pca[target_index] > 0 && predicted_lm_guelmin_without_pca[target_index] < 60)
      submission_format[index,2] = predicted_lm_guelmin_without_pca[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/lm_gul_timbuktoo_without_pca.csv", 
          row.names = FALSE)




####### Macroclimate_Agadir_Airport_Merged #############

Macroclimate_Agadir_Airport_Merged = data.frame(stringsAsFactors=TRUE)
timeStamps = c()
for(index in 1:nrow(Microclimate_2_hours_Train)){
  startTimeStamp = Microclimate_2_hours_Train[index,1]
  targetIndex = which(Macroclimate_Agadir_Airport[,1] == startTimeStamp)
  if(length(targetIndex) > 0){
    attrVal = c()
    emptyRow = data.frame()
    for(attr in 2:ncol(Macroclimate_Agadir_Airport)){
      attrVal = c(attrVal, mean(c(Macroclimate_Agadir_Airport[targetIndex,attr], 
                                  Macroclimate_Agadir_Airport[targetIndex-1,attr]), na.rm = TRUE))
    }
    attrVal = c(attrVal, Microclimate_2_hours_Train[index,"yield"])
    #print(Microclimate_2_hours_Train[index,"yield"])
    timeStamps = c(timeStamps, startTimeStamp)
    Macroclimate_Agadir_Airport_Merged <- rbind(Macroclimate_Agadir_Airport_Merged,
                                                 attrVal)
  }
}

for(index in 1:ncol(Macroclimate_Agadir_Airport_Merged)){
  indices = which(is.na(Macroclimate_Agadir_Airport_Merged[,index]))
  if(length(indices) > 0){
    print(index)
    Macroclimate_Agadir_Airport_Merged[indices,index] = mean(Macroclimate_Agadir_Airport_Merged[,index], na.rm = TRUE)
  }
}

colnames(Macroclimate_Agadir_Airport_Merged) <- c(colnames(Macroclimate_Agadir_Airport), "yield")

## EXPORTING TO CSV

write.csv(Macroclimate_Agadir_Airport_Merged, 
          file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/Macroclimate_Agadir_Airport_Merged.csv", 
          row.names = FALSE)


## LM WITHOUT PCA ON Macroclimate_Agadir_Airport_Merged

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Macroclimate_Agadir_Airport_Merged[3:ncol(Macroclimate_Agadir_Airport_Merged)-1]),
                                                           collapse = "+"), sep = "~")), Macroclimate_Agadir_Airport_Merged)

## train on train - MSE = 3.727140953
rms_train_lm_all_features = rmse(Macroclimate_Agadir_Airport_Merged$yield, predict(LM_all_features_model, 
                                                                                   Macroclimate_Agadir_Airport_Merged[1,2:ncol(Macroclimate_Agadir_Airport_Merged) - 1]))

#merged_train_test = rbind(Macroclimate_Guelmim_Airport_Merged[,1:ncol(Macroclimate_Guelmim_Airport_Merged)-1],
#                          Microclimate_2_hours_test)

#merged_train_test = Macroclimate_Guelmim_Airport_Merged

merged_train_test = Macroclimate_Agadir_Airport

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  
  target_index = which(Macroclimate_Agadir_Airport_Merged[,1] == submission_format[index,1])
  if(length(target_index) > 0){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    Macroclimate_Agadir_Airport_Merged[target_index,2:ncol(merged_train_test)]))
    next
  }
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index) > 0){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm_agadir_without_pca = predict(LM_all_features_model, 
                                           submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index) > 0 && !is.na(predicted_lm_agadir_without_pca[target_index])){
    #print(target_index)
    if(predicted_lm_agadir_without_pca[target_index] > 0 && predicted_lm_agadir_without_pca[target_index] < 60)
      submission_format[index,2] = predicted_lm_agadir_without_pca[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/lm_agadir_timbuktoo_without_pca.csv", 
          row.names = FALSE)

###### PCA MACRO-CLIMATE ####

##### PCA - AGADIR - Macroclimate_Agadir_Airport_Merged ###

Macroclimate_Agadir_Airport_Merged.pca <- prcomp(~ ., 
                                         data=Macroclimate_Agadir_Airport_Merged[, 3:ncol(Macroclimate_Agadir_Airport_Merged) - 1],
                                         center = TRUE,
                                         scale. = TRUE) 

best_attr = c("P0", "T", "DD", "U", "Ff", "cumulonimbus.clouds", "Overcast")

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Macroclimate_Agadir_Airport_Merged[best_attr]),
                                                           collapse = "+"), sep = "~")), Macroclimate_Agadir_Airport_Merged)

## train on train - MSE = 3.538866446
rms_train_lm_all_features = rmse(Macroclimate_Agadir_Airport_Merged$yield, predict(LM_all_features_model, 
                                                                                   Macroclimate_Agadir_Airport_Merged[best_attr]))

#merged_train_test = rbind(Macroclimate_Agadir_Airport[,1:ncol(Microclimate_2_hours_train)-1],Microclimate_2_hours_test)

merged_train_test = Macroclimate_Agadir_Airport

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  
  target_index = which(Macroclimate_Agadir_Airport_Merged[,1] == submission_format[index,1])
  if(length(target_index) > 0){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    Macroclimate_Agadir_Airport_Merged[target_index,2:ncol(merged_train_test)]))
    next
  }
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index) > 0){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm_agadir_without_pca = predict(LM_all_features_model, 
                                          submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index) > 0 && !is.na(predicted_lm_agadir_without_pca[target_index])){
    #print(target_index)
    if(predicted_lm_agadir_without_pca[target_index] > 0 && predicted_lm_agadir_without_pca[target_index] < 60)
      submission_format[index,2] = predicted_lm_agadir_without_pca[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/lm_agadir_timbuktoo_with_pca.csv", 
          row.names = FALSE)

##### PCA - GULEMIN - Macroclimate_Guelmim_Airport_Merged #####

Macroclimate_Guelmim_Airport_Merged.pca <- prcomp(~ ., 
                                                 data=Macroclimate_Guelmim_Airport_Merged[, 3:ncol(Macroclimate_Guelmim_Airport_Merged) - 1],
                                                 center = TRUE,
                                                 scale. = TRUE) 


best_attr = c("P0", "U", "Td", "DD", "VV", "Overcast", "Broken.clouds")

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(Macroclimate_Guelmim_Airport_Merged[best_attr]),
                                                           collapse = "+"), sep = "~")), Macroclimate_Guelmim_Airport_Merged)

## train on train - MSE = 2.85099266
rms_train_lm_all_features = rmse(Macroclimate_Guelmim_Airport_Merged$yield, predict(LM_all_features_model, 
                                                                                    Macroclimate_Guelmim_Airport_Merged[best_attr]))

#merged_train_test = rbind(Macroclimate_Agadir_Airport[,1:ncol(Microclimate_2_hours_train)-1],Microclimate_2_hours_test)

merged_train_test = Macroclimate_Guelmim_Airport

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  
  target_index = which(Macroclimate_Guelmim_Airport_Merged[,1] == submission_format[index,1])
  if(length(target_index) > 0){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    Macroclimate_Guelmim_Airport_Merged[target_index,2:ncol(merged_train_test)]))
    next
  }
  target_index = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index) > 0){
    submission_input = rbind(submission_input,cbind(c(submission_format[index,1]),
                                                    merged_train_test[target_index,2:ncol(merged_train_test)]))
  }else{
    missingIndex = c(missingIndex,index)
  }
}

predicted_lm_gul_with_pca = predict(LM_all_features_model, 
                                          submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index) > 0 && !is.na(predicted_lm_gul_with_pca[target_index])){
    #print(target_index)
    if(predicted_lm_gul_with_pca[target_index] > 0 && predicted_lm_gul_with_pca[target_index] < 60)
      submission_format[index,2] = predicted_lm_gul_with_pca[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/lm_gul_timbuktoo_with_pca.csv", 
          row.names = FALSE)


#### MERGE - Best_attr_gul + Best_attr_agadir + Best_attr_micro

#### LM
best_attr_gul = c("Td", "DD", "VV", "Broken.clouds")
best_attr_aga = c("P0", "T", "U", "Ff", "cumulonimbus.clouds", "Overcast")
best_attr_micro = c("humidity", "leafwet_lwscnt","leafwet450_min", "gusts_ms", "wind_ms")
  
all_time_stamps = c()

time_stamp_agadir = c()

time_stamp_gul = c()

time_stamp_micro = c()

# time stamp for micro
for(index in 1:nrow(Microclimate_2_hours_train)){
  time_stamp_micro = c(time_stamp_micro, Microclimate_2_hours_train[index,1])
}

# time stamp for gulmelin
for(index in 1:nrow(Macroclimate_Guelmim_Airport_Merged)){
  time_stamp_gul = c(time_stamp_gul, as.character(Macroclimate_Guelmim_Airport_Merged[index,1]))
}

# time stamp for agadir
for(index in 1:nrow(Macroclimate_Agadir_Airport_Merged)){
  time_stamp_agadir = c(time_stamp_agadir, as.character(Macroclimate_Agadir_Airport_Merged[index,1]))
}

all_time_stamps = intersect(intersect(time_stamp_agadir,time_stamp_gul),time_stamp_micro)

merge_all_macro_micro = data.frame()
for(timeStamp in all_time_stamps){
  col_gul = Macroclimate_Guelmim_Airport_Merged[which(Macroclimate_Guelmim_Airport_Merged[,1] == timeStamp),
                                                c("Time",best_attr_gul)]
  col_aga = Macroclimate_Agadir_Airport_Merged[which(Macroclimate_Agadir_Airport_Merged[,1] == timeStamp),
                                                c(best_attr_aga)]
  col_micro = Microclimate_2_hours_train[which(Microclimate_2_hours_train[,1] == timeStamp),
                                               c(best_attr_micro, "yield")]
  
  df = cbind(col_gul, col_aga, col_micro)
  merge_all_macro_micro = rbind(merge_all_macro_micro, df)
}

LM_all_features_model = lm(as.formula(paste("yield", paste(colnames(merge_all_macro_micro[3:ncol(merge_all_macro_micro) - 1]),
                                                           collapse = "+"), sep = "~")), merge_all_macro_micro)


## train on train - MSE = 2.216083547
rms_train_lm_all_features = rmse(merge_all_macro_micro$yield, predict(LM_all_features_model, 
                                                                      merge_all_macro_micro[c(best_attr_micro, best_attr_aga, best_attr_gul)]))


#merged_train_test = Macroclimate_Guelmim_Airport
merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],
                          Microclimate_2_hours_test)

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  
  # check if in micro train n test
  target_index_1 = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index_1) == 0){
    missingIndex = c(missingIndex,index)
    next
  }
  # check in gulmil
  
  target_index_2 = which(Macroclimate_Guelmim_Airport[,1] == submission_format[index,1])
  if(length(target_index_2) == 0){
    missingIndex = c(missingIndex,index)
    next
  }
  
  target_index_3 = which(Macroclimate_Agadir_Airport[,1] == submission_format[index,1])
  if(length(target_index_3) == 0){
    missingIndex = c(missingIndex,index)
    next
  }
  submission_input = rbind(submission_input,cbind(submission_format[index,1],
                                                  merged_train_test[target_index_1,best_attr_micro],
                                                  Macroclimate_Guelmim_Airport[target_index_2,best_attr_gul],
                                                  Macroclimate_Agadir_Airport[target_index_3,best_attr_aga]
                                                  ))
}

predicted_lm_all_pca = predict(LM_all_features_model, 
                                    submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index) > 0 && !is.na(predicted_lm_all_pca[target_index])){
    #print(target_index)
    if(predicted_lm_all_pca[target_index] > 0 && predicted_lm_all_pca[target_index] < 60)
      submission_format[index,2] = predicted_lm_all_pca[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/lm_all_timbuktoo_with_pca.csv", 
          row.names = FALSE)


#### MERGE - Best_attr_gul + Best_attr_agadir + Best_attr_micro

#### Random Forrest
best_attr_gul = c("Td", "DD", "VV", "Broken.clouds")
best_attr_aga = c("P0", "T", "U", "Ff", "cumulonimbus.clouds", "Overcast")
best_attr_micro = c("humidity", "leafwet_lwscnt","leafwet450_min", "gusts_ms", "wind_ms")

all_time_stamps = c()

time_stamp_agadir = c()

time_stamp_gul = c()

time_stamp_micro = c()

# time stamp for micro
for(index in 1:nrow(Microclimate_2_hours_train)){
  time_stamp_micro = c(time_stamp_micro, Microclimate_2_hours_train[index,1])
}

# time stamp for gulmelin
for(index in 1:nrow(Macroclimate_Guelmim_Airport_Merged)){
  time_stamp_gul = c(time_stamp_gul, as.character(Macroclimate_Guelmim_Airport_Merged[index,1]))
}

# time stamp for agadir
for(index in 1:nrow(Macroclimate_Agadir_Airport_Merged)){
  time_stamp_agadir = c(time_stamp_agadir, as.character(Macroclimate_Agadir_Airport_Merged[index,1]))
}

all_time_stamps = intersect(intersect(time_stamp_agadir,time_stamp_gul),time_stamp_micro)

merge_all_macro_micro = data.frame()
for(timeStamp in all_time_stamps){
  col_gul = Macroclimate_Guelmim_Airport_Merged[which(Macroclimate_Guelmim_Airport_Merged[,1] == timeStamp),
                                                c("Time",best_attr_gul)]
  col_aga = Macroclimate_Agadir_Airport_Merged[which(Macroclimate_Agadir_Airport_Merged[,1] == timeStamp),
                                               c(best_attr_aga)]
  col_micro = Microclimate_2_hours_train[which(Microclimate_2_hours_train[,1] == timeStamp),
                                         c(best_attr_micro, "yield")]
  
  df = cbind(col_gul, col_aga, col_micro)
  merge_all_macro_micro = rbind(merge_all_macro_micro, df)
}

rf_all_features_model = randomForest(yield ~ Td + DD + VV + Broken.clouds + P0 + T + U + Ff + cumulonimbus.clouds + 
                                       Overcast + humidity + leafwet_lwscnt + leafwet450_min + gusts_ms + 
                                       wind_ms, data = merge_all_macro_micro, importance = TRUE, ntree = 2000)

## train on train - MSE = 0.6220679328
rms_train_lm_all_features = rmse(merge_all_macro_micro$yield, predict(rf_all_features_model, 
                                                                      merge_all_macro_micro[c(best_attr_micro, best_attr_aga, best_attr_gul)]))


#merged_train_test = Macroclimate_Guelmim_Airport
merged_train_test = rbind(Microclimate_2_hours_train[,1:ncol(Microclimate_2_hours_train)-1],
                          Microclimate_2_hours_test)

submission_format = read.csv("./Data/submission_format.csv",
                             header = T, stringsAsFactors = FALSE)

submission_input = data.frame()
missingIndex = c()
for(index in 1:nrow(submission_format)){
  
  # check if in micro train n test
  target_index_1 = which(merged_train_test[,1] == submission_format[index,1])
  if(length(target_index_1) == 0){
    missingIndex = c(missingIndex,index)
    next
  }
  # check in gulmil
  
  target_index_2 = which(Macroclimate_Guelmim_Airport[,1] == submission_format[index,1])
  if(length(target_index_2) == 0){
    missingIndex = c(missingIndex,index)
    next
  }
  
  target_index_3 = which(Macroclimate_Agadir_Airport[,1] == submission_format[index,1])
  if(length(target_index_3) == 0){
    missingIndex = c(missingIndex,index)
    next
  }
  submission_input = rbind(submission_input,cbind(submission_format[index,1],
                                                  merged_train_test[target_index_1,best_attr_micro],
                                                  Macroclimate_Guelmim_Airport[target_index_2,best_attr_gul],
                                                  Macroclimate_Agadir_Airport[target_index_3,best_attr_aga]
  ))
}

predicted_rf_all_pca = predict(rf_all_features_model, 
                               submission_input[2:ncol(submission_input)])

for(index in 1:nrow(submission_format)){
  target_index = which(submission_input[,1] == submission_format[index,1])
  if(length(target_index) > 0 && !is.na(predicted_rf_all_pca[target_index])){
    #print(target_index)
    if(predicted_rf_all_pca[target_index] > 0 && predicted_rf_all_pca[target_index] < 60)
      submission_format[index,2] = predicted_rf_all_pca[target_index]
  }
}

write.csv(submission_format, file = "/Users/chiteshtewani/Desktop/Courses/Spring-2016/Data Mining/From Fog Nets To Nueral Nets/Results/rf_all_timbuktoo_with_pca.csv", 
          row.names = FALSE)



