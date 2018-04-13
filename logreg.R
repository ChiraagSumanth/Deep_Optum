library(data.table)
library(tidyverse)
library(glmnet)
library(ROCR)
library(InformationValue)
library(mice)
library(ResourceSelection)

select <- dplyr::select
prediction <- ROCR::prediction

#Read
hcc_train_data <- fread('./old_csv/hcc_train_data.csv', sep = ",", header = T, drop = 1)
hcc_val_data <- fread('./old_csv/hcc_val_data.csv', sep = ",", header = T, drop = 1)
hcc_test_data <- fread('./old_csv/hcc_test_data.csv', sep = ",", header = T, drop = 1)
hcc_test_16_data <- fread('./old_csv/hcc_test_16_data.csv', sep = ",", header = T, drop = 1)

hcc_sdh_train_data <- fread('./old_csv/hcc_sdh_train_data.csv', sep = ",", header = T, drop = 1)
hcc_sdh_val_data <- fread('./old_csv/hcc_sdh_val_data.csv', sep = ",", header = T, drop = 1)
hcc_sdh_test_data <- fread('./old_csv/hcc_sdh_test_imputed_data.csv', sep = ",", header = T, drop = 1)
hcc_sdh_test_16_data <- fread('./old_csv/hcc_sdh_test_16_imputed_data.csv', sep = ",", header = T, drop = 1)


#Logistic regression without SDH
mdlX <- data.matrix(hcc_train_data %>% select(-c(utilize)))
mdlY <- as.numeric(hcc_train_data$utilize)
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", parallel = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]

non_sdh_lr <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
save(non_sdh_lr, file = "non_sdh_lr_orig.RData")


# Year 2015 Non SDH
mdlX_test <- data.matrix(hcc_test_data %>% select(-c(utilize)))
mdlY_test <- as.numeric(hcc_test_data$utilize)
preds_non_sdh <- predict(non_sdh_lr, mdlX_test, type = "response")
non_sdh_co <- optimalCutoff(mdlY_test, preds_non_sdh, returnDiagnostics = T)[1]
lwr_90_non_sdh <- sum(preds_non_sdh < non_sdh_co & mdlY_test == 0)/sum(mdlY_test == 0)
upr_10_non_sdh <- sum(preds_non_sdh >= non_sdh_co & mdlY_test == 1)/sum(mdlY_test == 1)

tp <- sum(preds_non_sdh >= non_sdh_co & mdlY_test == 1)
tn <- sum(preds_non_sdh < non_sdh_co & mdlY_test == 0)
fp <- sum(preds_non_sdh >= non_sdh_co & mdlY_test == 0)
fn <- sum(preds_non_sdh < non_sdh_co & mdlY_test == 1)
tpr <- tp/(tp + fn)
tnr <- tn/(tn + fp)
metrics_non_sdh <- c(tpr,tnr)

rocr_non_sdh <- prediction(preds_non_sdh, mdlY_test)
auc_non_sdh <- performance(rocr_non_sdh, measure = "auc")

# Year 2016 Non SDH 
mdlX_test_16 <- data.matrix(hcc_test_16_data %>% select(-c(utilize)))
mdlY_test_16 <- as.numeric(hcc_test_16_data$utilize)
preds_non_sdh_16 <- predict(non_sdh_lr, mdlX_test_16, type = "response")
non_sdh_co_16 <- optimalCutoff(mdlY_test_16, preds_non_sdh_16)[1]
lwr_90_non_sdh_16 <- sum(preds_non_sdh_16 < non_sdh_co_16 & mdlY_test_16 == 0)/sum(mdlY_test_16 == 0)
upr_10_non_sdh_16 <- sum(preds_non_sdh_16 >= non_sdh_co_16 & mdlY_test_16 == 1)/sum(mdlX_test_16 == 1)

tp <- sum(preds_non_sdh_16 >= non_sdh_co_16 & mdlY_test_16 == 1)
tn <- sum(preds_non_sdh_16 < non_sdh_co_16 & mdlY_test_16 == 0)
fp <- sum(preds_non_sdh_16 >= non_sdh_co_16 & mdlY_test_16 == 0)
fn <- sum(preds_non_sdh_16 < non_sdh_co_16 & mdlY_test_16 == 1)
tpr <- tp/(tp + fn)
tnr <- tn/(tn + fp)
metrics_non_sdh_16 <- c(tpr,tnr)

rocr_non_sdh_16 <- prediction(preds_non_sdh_16, mdlY_test_16)
auc_non_sdh_16 <- performance(rocr_non_sdh_16, measure = "auc")


#Logistic regression with SDH
mdlX <- data.matrix(hcc_sdh_train_data %>% select(-c(utilize)))
impute_mdlX <- data.matrix(mdlX)
mdlX  <- mice(impute_mdlX, m = 1)
mdlX <- data.matrix(complete(mdlX, "long") %>% select(-c(.imp,.id)))
mdlY <- as.numeric(hcc_sdh_train_data$utilize)
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", parallel = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]

sdh_lr <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
save(sdh_lr, file = "sdh_lr_orig.RData")

# SDH Year 2015
mdlX_test <- data.matrix(hcc_sdh_test_data %>% select(-c(utilize)))
mdlY_test <- as.numeric(hcc_sdh_test_data$utilize)
preds_sdh <- predict(sdh_lr, mdlX_test, type = "response")
sdh_co <- optimalCutoff(mdlY_test, preds_sdh)[1]
lwr_90_sdh <- sum(preds_sdh < sdh_co & mdlY_test == 0)/sum(mdlY_test == 0)
upr_10_sdh <- sum(preds_sdh >= sdh_co & mdlY_test == 1)/sum(mdlY_test == 1)

tp <- sum(preds_sdh >= sdh_co & mdlY_test == 1)
tn <- sum(preds_sdh < sdh_co & mdlY_test == 0)
fp <- sum(preds_sdh >= sdh_co & mdlY_test == 0)
fn <- sum(preds_sdh < sdh_co & mdlY_test == 1)
tpr <- tp/(tp + fn)
tnr <- tn/(tn + fp)
metrics_sdh <- c(tpr,tnr)

rocr_sdh <- prediction(preds_sdh, mdlY_test)
auc_sdh <- performance(rocr_sdh, measure = "auc")

# SDH Year 2016
mdlX_test_16 <- data.matrix(hcc_sdh_test_16_data %>% select(-c(utilize)))
mdlX_test_imp_16 <- mice(mdlX_test_16, m = 1)
mdlX_test_16 <- data.matrix(complete(mdlX_test_imp_16, "long") %>% select(-c(.imp,.id)))
mdlY_test_16 <- as.numeric(hcc_sdh_test_16_data$utilize)
preds_sdh_16 <- predict(sdh_lr, mdlX_test_16, type = "response")
sdh_co_16 <- optimalCutoff(mdlY_test_16, preds_sdh_16)[1]
lwr_90_sdh_16 <- sum(preds_sdh_16 < sdh_co_16 & mdlY_test_16 == 0)/sum(mdlY_test_16 == 0)
upr_10_sdh_16 <- sum(preds_sdh_16 >= sdh_co_16 & mdlY_test_16 == 1)/sum(mdlX_test_16 == 1)

tp <- sum(preds_sdh_16 >= sdh_co_16 & mdlY_test_16 == 1)
tn <- sum(preds_sdh_16 < sdh_co_16 & mdlY_test_16 == 0)
fp <- sum(preds_sdh_16 >= sdh_co_16 & mdlY_test_16 == 0)
fn <- sum(preds_sdh_16 < sdh_co_16 & mdlY_test_16 == 1)
tpr <- tp/(tp + fn)
tnr <- tn/(tn + fp)
metrics_sdh_16 <- c(tpr,tnr)

rocr_sdh_16 <- prediction(preds_sdh_16, mdlY_test_16)
auc_sdh_16 <- performance(rocr_sdh_16, measure = "auc")