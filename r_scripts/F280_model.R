library(data.table)
library(nnet)
library(rfUtilities)
library(dplyr)

F280_clean <- fread("Q:/Team Drives/APHIS  Private Data/Pathways/F280_clean.csv", stringsAsFactors = T)
F280_model <- F280_clean[!Outcome == "CC"]
F280_model <- droplevels(F280_model)
F280_model <- F280_model[, c(9, 10, 18, 27, 38, 39, 40, 41, 42, 43, 47)]

F280_model$Protocol <- ""
F280_model[!Outcome == "NP"]$Protocol <- "No"
F280_model[Protocol==""]$Protocol <- "Yes"
F280_model$Protocol <- as.factor(F280_model$Protocol)
F280_model <- na.omit(F280_model)
F280_model <- droplevels(F280_model)

F280_model_calib <- F280_model[!FY==2018]
F280_model_test <- F280_model[FY==2018]

# Summarize data
dplyr::count(F280_model_calib, Outcome) %>% mutate(freq = n/sum(n))
dplyr::count(F280_model_calib, Protocol) %>% mutate(freq = n/sum(n))
count(F280_model, FY)
count(F280_model, PATHWAY)
count(F280_model, State)
count(F280_model, season)
count(F280_model, Origin_Model)



# Sample data

# x - the original vector, matrix or data.frame.
# y - a vector, what to balance.
# p - proportion of x to choose.

createSets <- function(x, y, p){
  nr <- NROW(x)
  size <- (nr * p) %/% length(unique(y))
  idx <- lapply(split(seq_len(nr), y), function(.x) sample(.x, size, replace=T))
  unlist(idx)
}
ind <- createSets(F280_model_calib,F280_model_calib$Outcome, 0.01)
balanced_outcome_sample <- F280_model_calib[ind,]
summary(balanced_outcome_sample)
balanced_outcome_sample <- na.omit(balanced_outcome_sample)
summary(balanced_outcome_sample)

ind <- createSets(F280_model_calib,F280_model_calib$Protocol, 0.04)
balanced_protocol_sample <- F280_model_calib[ind,]
summary(balanced_protocol_sample)
balanced_protocol_sample <- na.omit(balanced_protocol_sample)
summary(balanced_protocol_sample)

# ANN
set.seed(8)

train_data_indices <- rep(FALSE, nrow(balanced_outcome_sample))
train_data_indices[sample(1:nrow(balanced_outcome_sample), round(0.8 * nrow(balanced_outcome_sample)))] <- TRUE
ann_F280 <- nnet(Outcome ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_outcome_sample[train_data_indices,], size = 10, decay = 5e-04, maxit = 700, MaxNWts = 2000)


# validation
# F280_validation <- balanced_outcome_sample[!train_data_indices,]
# validate_data_indices <- rep(FALSE, nrow(F280_validation))
# validate_data_indices[sample(1:nrow(F280_validation), round(0.4 * nrow(F280_validation)))] <- TRUE
ann_validate <- predict(ann_F280, balanced_outcome_sample[!train_data_indices,])
confusion_ann <- table(max.col(ann_validate), as.integer(balanced_outcome_sample$Outcome[!train_data_indices]))
accuracy(confusion_ann)

ann_pred <- predict(ann_F280, F280_model_test)
ann_outcome_2018 <- as.data.table(ann_pred)
confusion_ann <- table(max.col(ann_pred), as.integer(F280_model_test$Outcome))
accuracy(confusion_ann)

ann_outcome_2018$actual <- as.integer(F280_model_test$Outcome)
ann_outcome_2018$decision <- ""
ann_outcome_2018[`NP`>0.5]$decision <- "2"
ann_outcome_2018[decision == ""]$decision <- "1"
ann_outcome_2018[actual==decision][actual==1] #-/- Right decision to inspect, intercept bad shipment
ann_outcome_2018[actual!=decision][actual==1] #+/- Wrong decision not to inspect, missed bad shipments
ann_outcome_2018[actual==decision][actual==2] #+/+ Right decision to not inspect, saved labor
ann_outcome_2018[actual!=decision][actual==2] #-/+ Wrong decision to inspect, status quo
ann_outcome_2018[actual==1]


# ANN binary outcome
set.seed(8)

train_data_indices <- rep(FALSE, nrow(balanced_protocol_sample))
train_data_indices[sample(1:nrow(balanced_protocol_sample), round(0.8 * nrow(balanced_protocol_sample)))] <- TRUE
ann_binary <- nnet(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_protocol_sample[train_data_indices,], size = 10, decay = 5e-04, maxit = 700, MaxNWts = 2000)

# validation
# F280_validation <- balanced_outcome_sample[!train_data_indices,]
# validate_data_indices <- rep(FALSE, nrow(F280_validation))
# validate_data_indices[sample(1:nrow(F280_validation), round(0.4 * nrow(F280_validation)))] <- TRUE
ann_binary_validate <- predict(ann_binary, balanced_protocol_sample[!train_data_indices,])
confusion_ann_binary <- table(max.col(ann_binary_validate), as.integer(balanced_protocol_sample$Protocol[!train_data_indices]))
accuracy(confusion_ann_binary)

ann_pred_binary <- predict(ann_binary, F280_model_test)
confusion_ann_binary <- table(max.col(ann_pred_binary), as.integer(F280_model_test$Protocol))
accuracy(confusion_ann_binary)

ann_binary_2018 <- as.data.table(ann_pred_binary)
ann_binary_2018$actual <- as.integer(F280_model_test$Protocol)
ann_binary_2018$decision <- ""
ann_binary_2018["No">0.5]$decision <- "1"
ann_binary_2018[decision == ""]$decision <- "2"
ann_binary_2018[actual==decision][actual==2] #-/- Right decision to inspect, intercept bad shipment
ann_binary_2018[actual!=decision][actual==2] #+/- Wrong decision not to inspect, missed bad shipments
ann_binary_2018[actual==decision][actual==1] #+/+ Right decision to not inspect, saved labor
ann_binary_2018[actual!=decision][actual==1] #-/+ Wrong decision to inspect, status quo
ann_binary_2018[actual==1]

# Random forest

library(randomForest)
library(reprtree)

# Multi outcome classification
rf_F280 <- randomForest(Outcome ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_outcome_sample, importance=T, do.trace = T)
varImpPlot(rf_F280)
rf_F280
confusion_rf <- as.table(rf_F280$confusion)[, 1:5]
accuracy(confusion_rf)

# plot an example tree
reprtree:::plot.getTree(rf_F280)

# predict test data with default class assignment
pred_outcome_rf <- predict(rf_F280, F280_model_test, type = "class") # predict the outcome as majority wins
confusion_pred_outcome <- table(pred_outcome_rf, F280_model_test$Outcome)
accuracy(confusion_pred_outcome)

# predict test data with probabilities, apply decision thresholds
pred_outcome_rf <- predict(rf_F280, F280_model_test, type = "prob") # predict the outcome as probabilities
outcome_prob_2018 <- as.data.table(pred_outcome_rf)
outcome_prob_2018$actual <- as.integer(F280_model_test$Outcome)
outcome_prob_2018$decision <- ""
outcome_prob_2018[`NP`>0.5]$decision <- "2"
outcome_prob_2018[decision == ""]$decision <- "1"
outcome_prob_2018[actual==decision][!actual==2] #-/- Right decision to inspect, intercept bad shipment
outcome_prob_2018[actual==decision][actual==2] #+/+ Right decision to not inspect, saved labor
outcome_prob_2018[actual!=decision][actual==2] #-/+ Wrong decision to inspect, status quo
outcome_prob_2018[actual!=decision][!actual==2] #+/- Wrong decision not to inspect, missed bad shipments
outcome_prob_2018[actual==2]


# Binary outcome classification
rf_F280_binary <- randomForest(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_protocol_sample, importance=T, do.trace = T)
varImpPlot(rf_F280_binary)
rf_F280_binary
confusion_rf_binary <- as.table(rf_F280_binary$confusion)[, 1:2]
accuracy(confusion_rf_binary)

# plot an example tree
reprtree:::plot.getTree(rf_F280_binary)

# predict test data with default class assignment
pred_binary_rf <- predict(rf_F280_binary, F280_model_test, type = "class") # predict the outcome as majority wins
confusion_pred_binary <- table(pred_binary_rf, F280_model_test$Protocol)
accuracy(confusion_pred_binary)

# predict test data with probabilities, apply decision thresholds
pred_binary_rf <- predict(rf_F280_binary, F280_model_test, type = "prob") # predict the outcome as probabilities
protocol_prob_2018 <- as.data.table(pred_binary_rf)
protocol_prob_2018$actual <- as.integer(F280_model_test$Protocol)
protocol_prob_2018$decision <- ""
protocol_prob_2018[`No`>0.98]$decision <- "1"
protocol_prob_2018[decision == ""]$decision <- "2"
protocol_prob_2018[actual==decision][actual==2] #-/- Right decision to inspect, intercept bad shipment
protocol_prob_2018[actual!=decision][actual==2] #+/- Wrong decision not to inspect, missed bad shipments
protocol_prob_2018[actual==decision][actual==1] #+/+ Right decision to not inspect, saved labor
protocol_prob_2018[actual!=decision][actual==1] #-/+ Wrong decision to inspect, status quo
protocol_prob_2018[actual==2]


# Random Forest with cost sensitive learning

# Calculate weights, cost of misclassification
# class_weights <- ifelse(F280_model_calib$Protocol == 1,
#                         (1/table(F280_model_calib$Protocol)[1]) * 0.5,
#                         (1/table(F280_model_calib$Protocol)[2]) * 0.5)

# Break trianing data into 1/4's - hit memory allocation limit
rf_clwt_binary_1 <- randomForest(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=F280_model_calib[(1:500000)], classwt=c(0.00005, 1000), importance=T, do.trace=T, parallel=T)
rf_clwt_binary_2 <- randomForest(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=F280_model_calib[(500001:1000000)], classwt=c(0.00005, 1000), importance=T, do.trace=T, parallel=T)
rf_clwt_binary_3 <- randomForest(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=F280_model_calib[(1000001:1500000)], classwt=c(0.00005, 1000), importance=T, do.trace=T, parallel=T)
rf_clwt_binary_4 <- randomForest(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=F280_model_calib[(1500001:2000000)], classwt=c(0.00005, 1000), importance=T, do.trace=T, parallel=T)

rf_clwt_binary <- randomForest::combine(rf_clwt_binary_1, rf_clwt_binary_2, rf_clwt_binary_3, rf_clwt_binary_4)
confusion_rf_clwt_binary <- as.table(rf_clwt_binary_1$confusion)[, 1:2]
accuracy(confusion_rf_clwt_binary)

tree <- getTree(rf_clwt_binary_1)


reprtree:::plot.tree

pred_protocol_rf <- predict(rf_clwt_binary, F280_model_test, type = "class") # predict the outcome as majority wins
confusion_pred_protocol <- table(pred_protocol_rf, F280_model_test$Protocol)
accuracy(confusion_pred_protocol)

pred_protocol_rf <- predict(rf_clwt_binary, F280_model_test, type = "prob") # predict the outcome as probabilities
protocol_prob_cw_2018 <- as.data.table(pred_protocol_rf)
protocol_prob_cw_2018$actual <- as.integer(F280_model_test$Protocol)
protocol_prob_cw_2018$decision <- ""
protocol_prob_cw_2018[`Don't Inspect`>0.65]$decision <- "2"
protocol_prob_cw_2018[decision == ""]$decision <- "1"
protocol_prob_cw_2018[actual==decision][actual==1] #-/- Right decision to inspect, intercept bad shipment
protocol_prob_cw_2018[actual!=decision][actual==1] #+/- Wrong decision not to inspect, missed bad shipments
protocol_prob_cw_2018[actual==decision][actual==2] #+/+ Right decision to not inspect, saved labor
protocol_prob_cw_2018[actual!=decision][actual==2] #-/+ Wrong decision to inspect, status quo

protocol_prob_cw_2018[actual==2]


tmp <- outcome_prob_2018[!actual==1]
tmp$maxProb <- max.col(tmp[,c(1:2)])
tmp[actual != maxProb]
confusion_rf <- table(as.integer(pred_outcome), as.integer(F280_model[FY==2018]$Outcome))
accuracy(confusion_rf)

rf_binary <- rf_F280
rf_binary
varImpPlot(rf_clwt_binary)
pred_binary_rf
confusion_binary_rf <- table(as.integer(pred_binary_rf), as.integer(F280_model[FY==2018]$Protocol))
accuracy(confusion_binary_rf)
