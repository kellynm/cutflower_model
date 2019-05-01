library(data.table)
library(nnet)
library(rfUtilities)

F280_clean <- fread("Q:/Team Drives/APHIS  Private Data/Pathways/F280_clean.csv", stringsAsFactors = T)
F280_model <- F280_clean[!Outcome == "CC"]
F280_model <- droplevels(F280_model)
F280_model <- F280_model[, c(9, 10, 18, 27, 38, 39, 40, 41, 42, 43, 47)]

F280_model$Protocol <- ""
F280_model[!Outcome == "NP"]$Protocol <- "Inspect"
F280_model[Protocol==""]$Protocol <- "Don't Inspect"
F280_model$Protocol <- as.factor(F280_model$Protocol)
F280_model <- na.omit(F280_model)
F280_model <- droplevels(F280_model)

# Summarize data
count(F280_model, Outcome)
count(F280_model, Protocol)
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
ind <- createSets(F280_model,F280_model$Outcome, 0.1)
balanced_outcome_sample <- F280_model[ind,]
summary(balanced_outcome_sample)
balanced_outcome_sample <- na.omit(balanced_outcome_sample)
summary(balanced_outcome_sample)

# ANN
set.seed(8)

train_data_indices <- rep(FALSE, nrow(balanced_outcome_sample))
train_data_indices[sample(1:nrow(balanced_outcome_sample), round(0.8 * nrow(balanced_outcome_sample)))] <- TRUE
ann_F280 <- nnet(Outcome ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_outcome_sample[train_data_indices,], size = 10, decay = 5e-04, maxit = 700, MaxNWts = 2000)
#Outcome ~ Origin_Model+PATHWAY+QUANTITY+State+season+avg_temp+avg_precip+Order

ann_F280$fitted.values 

# validation
# F280_validation <- balanced_outcome_sample[!train_data_indices,]
# validate_data_indices <- rep(FALSE, nrow(F280_validation))
# validate_data_indices[sample(1:nrow(F280_validation), round(0.4 * nrow(F280_validation)))] <- TRUE
ann_validate <- predict(ann_F280, balanced_outcome_sample[!train_data_indices,])
confusion_ann <- table(max.col(ann_validate), as.integer(balanced_outcome_sample$Outcome[!train_data_indices]))
accuracy(confusion_ann)

ann_pred <- predict(ann_F280, F280_model[FY==2018])
ann_outcome_2018 <- as.data.table(ann_pred)
ann_outcome_2018$actual <- as.integer(F280_model[FY==2018]$Outcome)
ann_outcome_2018$decision <- ""
ann_outcome_2018[`NP`>0.7]$decision <- "2"
ann_outcome_2018[decision == ""]$decision <- "1"
ann_outcome_2018[actual==decision][actual==1] #-/- Right decision to inspect, intercept bad shipment
ann_outcome_2018[actual!=decision][actual==1] #+/- Wrong decision not to inspect, missed bad shipments
ann_outcome_2018[actual==decision][actual==2] #+/+ Right decision to not inspect, saved labor
ann_outcome_2018[actual!=decision][actual==2] #-/+ Wrong decision to inspect, status quo
ann_outcome_2018[actual==2]

confusion_ann <- table(max.col(ann_pred), F280_model[FY==2018])


# ANN binary outcome
set.seed(8)

train_data_indices <- rep(FALSE, nrow(balanced_outcome_sample))
train_data_indices[sample(1:nrow(balanced_outcome_sample), round(0.8 * nrow(balanced_outcome_sample)))] <- TRUE
ann_binary <- nnet(Protocol ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_outcome_sample[train_data_indices,], size = 10, decay = 5e-04, maxit = 700, MaxNWts = 2000)
#Outcome ~ Origin_Model+PATHWAY+QUANTITY+State+season+avg_temp+avg_precip+Order

ann_binary$fitted.values 

# validation
# F280_validation <- balanced_outcome_sample[!train_data_indices,]
# validate_data_indices <- rep(FALSE, nrow(F280_validation))
# validate_data_indices[sample(1:nrow(F280_validation), round(0.4 * nrow(F280_validation)))] <- TRUE
ann_binary_validate <- predict(ann_binary, balanced_outcome_sample[!train_data_indices,])
confusion_ann_binary <- table(max.col(ann_binary_validate), as.integer(balanced_outcome_sample$Protocol[!train_data_indices]))
accuracy(confusion_ann_binary)

ann_pred_binary <- predict(ann_binary, F280_model[FY==2018])
ann_binary_2018 <- as.data.table(ann_pred_binary)
ann_binary_2018$actual <- as.integer(F280_model[FY==2018]$Protocol)
ann_binary_2018$decision <- ""
ann_binary_2018[`Don't Inspect`>0.6]$decision <- "1"
ann_binary_2018[decision == ""]$decision <- "2"
ann_binary_2018[actual==decision][actual==2] #-/- Right decision to inspect, intercept bad shipment
ann_binary_2018[actual!=decision][actual==2] #+/- Wrong decision not to inspect, missed bad shipments
ann_binary_2018[actual==decision][actual==1] #+/+ Right decision to not inspect, saved labor
ann_binary_2018[actual!=decision][actual==1] #-/+ Wrong decision to inspect, status quo
ann_binary_2018[actual==1]

# Random forest

rf_F280 <- randomForest(Outcome ~ Origin_Model+QUANTITY+State+Month+avg_temp+avg_precip+Order, data=balanced_outcome_sample, importance=T, do.trace = T)
varImpPlot(rf_F280)
rf_F280
confusion_rf <- as.table(rf_F280$confusion)[, 1:2]
accuracy(confusion_rf)

pred_outcome_rf <- predict(rf_F280, F280_model[FY==2018], type = "prob") # predict the outcome
outcome_prob_2018 <- as.data.table(pred_outcome_rf)
outcome_prob_2018$actual <- as.integer(F280_model[FY==2018]$Outcome)
outcome_prob_2018$decision <- ""
outcome_prob_2018[`NP`>0.9]$decision <- "2"
outcome_prob_2018[decision == ""]$decision <- "1"
outcome_prob_2018[actual==decision][!actual==2] #-/- Right decision to inspect, intercept bad shipment
outcome_prob_2018[actual==decision][actual==2] #+/+ Right decision to not inspect, saved labor
outcome_prob_2018[actual!=decision][actual==2] #-/+ Wrong decision to inspect, status quo
outcome_prob_2018[actual!=decision][!actual==2] #+/- Wrong decision not to inspect, missed bad shipments
outcome_prob_2018[!actual==2]

pred_binary_rf <- predict(rf_binary, F280_model[FY==2018], type = "class") # predict the outcome
outcome_prob_2018 <- as.data.table(pred_outcome)
outcome_prob_2018$actual <- as.integer(F280_model[FY==2018]$Protocol)
outcome_prob_2018$decision <- ""
outcome_prob_2018[`Don't Inspect`>0.9]$decision <- "1"
outcome_prob_2018[decision == ""]$decision <- "2"
outcome_prob_2018[actual==decision][actual==2] #-/- Right decision to inspect, intercept bad shipment
outcome_prob_2018[actual!=decision][actual==2] #+/- Wrong decision not to inspect, missed bad shipments
outcome_prob_2018[actual==decision][actual==1] #+/+ Right decision to not inspect, saved labor
outcome_prob_2018[actual!=decision][actual==1] #-/+ Wrong decision to inspect, status quo

outcome_prob_2018[actual==1]


tmp <- outcome_prob_2018[!actual==1]
tmp$maxProb <- max.col(tmp[,c(1:2)])
tmp[actual != maxProb]
confusion_rf <- table(as.integer(pred_outcome), as.integer(F280_model[FY==2018]$Outcome))
accuracy(confusion_rf)

rf_binary <- rf_F280
rf_binary
varImpPlot(rf_binary)
pred_binary_rf
confusion_binary_rf <- table(as.integer(pred_binary_rf), as.integer(F280_model[FY==2018]$Protocol))
accuracy(confusion_binary_rf)
