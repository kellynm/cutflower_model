library(data.table)
library(nnet)
library(rfUtilities)


F280_clean <- fread("/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/F280_clean.csv", stringsAsFactors = T)

# ANN
set.seed(8)

train_data_indices <- rep(FALSE, nrow(F280_clean))
train_data_indices[sample(1:nrow(F280_clean), round(0.2 * nrow(F280_clean)))] <- TRUE
ann_F280 <- nnet(Outcome ~ Subregion+season+avg_temp+avg_precip+Order, data=F280_clean[train_data_indices,], size = 10, decay = 5e-04, maxit = 700, MaxNWts = 2000)
#Outcome ~ Origin_Model+PATHWAY+QUANTITY+State+season+avg_temp+avg_precip+Order
ann_F280$fitted.values # note that these are not factors!

# Subset for validation
F280_validation <- F280_clean[!train_data_indices,]
validate_data_indices <- rep(FALSE, nrow(F280_validation))
validate_data_indices[sample(1:nrow(F280_validation), round(0.4 * nrow(F280_validation)))] <- TRUE
F280_predict <- predict(ann_F280, F280_validation[validate_data_indices,])

confusion_ann <- table(max.col(F280_predict), as.integer(F280_validation$Outcome[validate_data_indices]))
accuracy(confusion_ann)
