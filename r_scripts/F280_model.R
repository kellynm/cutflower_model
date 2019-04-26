library(data.table)
library(nnet)
library(rfUtilities)


F280_clean <- fread("Q:/My Drive/Research/APHIS_Pathways/analysis/F280_clean.csv", stringsAsFactors = T)

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
ind <- createSets(F280_clean,F280_clean$Outcome, 0.1)
balanced_outcome_sample <- F280_clean[ind,]
summary(balanced_outcome_sample)

# ANN
set.seed(8)

train_data_indices <- rep(FALSE, nrow(balanced_outcome_sample))
train_data_indices[sample(1:nrow(balanced_outcome_sample), round(0.6 * nrow(balanced_outcome_sample)))] <- TRUE
ann_F280 <- nnet(Outcome ~ Origin_Model+PATHWAY+QUANTITY+State+season+avg_temp+avg_precip+Order, data=balanced_outcome_sample[train_data_indices,], size = 10, decay = 5e-04, maxit = 700, MaxNWts = 2000)
#Outcome ~ Origin_Model+PATHWAY+QUANTITY+State+season+avg_temp+avg_precip+Order

ann_F280$fitted.values 

# validation
# F280_validation <- balanced_outcome_sample[!train_data_indices,]
# validate_data_indices <- rep(FALSE, nrow(F280_validation))
# validate_data_indices[sample(1:nrow(F280_validation), round(0.4 * nrow(F280_validation)))] <- TRUE
F280_predict <- predict(ann_F280, balanced_outcome_sample[!train_data_indices,])

confusion_ann <- table(max.col(F280_predict), as.integer(balanced_outcome_sample$Outcome[!train_data_indices]))
accuracy(confusion_ann)
