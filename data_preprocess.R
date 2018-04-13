library(data.table)
library(tidyverse)
library(lubridate)
library(icd)
library(MASS)
library(ROCR)
library(FNN)
library(ggpubr)
library(InformationValue)

select <- dplyr::select
summarise <- dplyr::summarise
prediction <- ROCR::prediction


#Read year 2015 data
q1_15 <- fread('/home/chiraagsumanth/optum/data/zip5_m2015q1.csv', sep = ",", header = T, nrows = 8000000)
q2_15 <- fread('/home/chiraagsumanth/optum/data/zip5_m2015q2.csv', sep = ",", header = T, nrows = 8000000)
q3_15 <- fread('/home/chiraagsumanth/optum/data/zip5_m2015q3.csv', sep = ",", header = T, nrows = 8000000)
q4_15 <- fread('/home/chiraagsumanth/optum/data/zip5_m2015q4.csv', sep = ",", header = T, nrows = 8000000)
m_15 <- rbind(q1_15, q2_15, q3_15, q4_15)

#For 2016 replce above with correct files

######################### Data Pre-processing ##############################
m_df <- m_15
m_df$Fst_Dt = as.Date(m_df$Fst_Dt, "%Y-%m-%d")
m_df$Lst_Dt = as.Date(m_df$Lst_Dt, "%Y-%m-%d")
m_df <- m_df[complete.cases(m_df[,"Tos_Cd"]),]
op_vec <- c("FAC_OP.ER","FAC_OP.FO_DIA","FAC_OP.FO_SUR","FAC_OP.FO_OTH","FAC_OP.FO_LAB","FAC_OP.FO_RAD","FAC_IP.ACUTE")
m_df_op <- m_df %>% filter(Tos_Cd %in% op_vec)

#Convert from wide to tall dataframe 
hcc_icd_format <- m_df_op %>% select(Patid, Fst_Dt, Diag1:Diag25, Proc1:Proc25, Std_Cost)
hcc_icd_format_long <- hcc_icd_format %>% gather(Diag, icd9, Diag1:Proc25, factor_key = TRUE)
hcc_icd_format_long <- hcc_icd_format_long[!duplicated(hcc_icd_format_long[,c('Patid','Diag','icd9')]),]

#HCC Coding
#Read ICD-HCC Crosswalk and merge with tall dataframe from above
#For 2016 replace above with correct icd10 to hcc mapping
icd_hcc <- read.csv('/home/chiraagsumanth/optum/icd_hcc.csv', header = TRUE)
icd_hcc <- na.omit(icd_hcc)
colnames(icd_hcc) <- c("icd9", "hcc")
hcc_comb <- merge(hcc_icd_format_long, icd_hcc, by = "icd9")
hcc_comb <- hcc_comb[!duplicated(hcc_comb[,c('Patid','hcc')]),]

#Obtain total cost for each patient
hcc_comb$Std_Cost <- as.double(hcc_comb$Std_Cost)
hcc_comb <- hcc_comb %>% filter(Std_Cost >= 0.0)
hcc_comb_cost <- hcc_comb %>% group_by(Patid) %>% summarise(tot_cost = sum(Std_Cost))
hcc_comb_cost$tot_cost = (hcc_comb_cost$tot_cost/max(hcc_comb_cost$tot_cost)) * 100

#Get deciles of cost
x_hcc <- hcc_comb_cost$tot_cost
deciles_hcc <- quantile(x_hcc, prob = seq(0, 1, length = 11), type = 5)

#Convert to classification problem, settig True for top10 percent  and False otherwise
hcc_cls_cost <- hcc_comb_cost %>% mutate(utilize = ifelse(tot_cost < deciles_hcc[10], FALSE, TRUE)) %>% 
  select (-c(tot_cost))

#COnverting back to wide dataframe, where each patient has a binary indicator for each of the 79 HCC codes
testing <- aggregate(hcc ~ Patid, data = hcc_comb %>% select(Patid,hcc), c)

#Final HCC Mappings
hcc_list <- c(1,2,6,8,9,10,11,12,17,18,19,21,22,23,27,28,29,33,34,35,39,40,46,47,48,54,55,57,58,70,71,72,73,74,75,76,77,78,79,80,82,83,84,85,86,87,88,96,99,100,103,104,106,107,108,110,111,112,114,115,122,124,134,135,136,137,157,158,161,162,166,167,169,170,173,176,186,188,189)
hcc_comor <- as.data.frame(matrix(0, ncol = 80))

ix <- length(testing[,1])
for (i in 1:ix){
  row <- rep(0,80)
  row[1] <- testing$Patid[i]
  for (j in testing[i,2]){
    for (key in (1:79)){
      if (j == hcc_list[key]){
        row[key + 1] = 1
        break
      }
    }
  }
  hcc_comor <- rbind(hcc_comor,row)
}
colnames(hcc_comor)[1] <- "Patid"
#################### End of Data Pre-processing #############################

#################### Generate train/test/validate files ##################

# Combine HCC features and binary outcome
# hcc_comor_data <- merge(hcc_comor, hcc_cls_cost, by = "Patid")

#Data Preparation
mbr_all <- fread(file = "/home/chiraagsumanth/optum/zip5_mbr.csv", header = T, sep = ',')
colnames(mbr_all)[11] <- "Yob"
colnames(mbr_all)[7] <- "Sex"
mbr_all <- mbr_all[complete.cases(mbr_all[,"Eligeff"]),]
mbr_all <- mbr_all[complete.cases(mbr_all[,"Eligend"]),]
mbr_all$Eligeff = as.Date(mbr_all$Eligeff, "%Y-%m-%d")
mbr_all$Eligend = as.Date(mbr_all$Eligend, "%Y-%m-%d")

mbr_all_15 <- mbr_all %>% filter(format(Eligend,"%Y") >= 2015 & format(Eligeff,"%Y") < 2016)
mbr_15 <- mbr_all_15 %>% filter(as.numeric(Eligend - as.Date("2015-01-01")) > 335)

#Add Zipcode and Demographics data
mbr_zip_all <- mbr_all %>% select(c(Patid, Sex, Yob, Zipcode_5))
mbr_zip <- mbr_15 %>% select(c(Patid, Sex, Yob, Zipcode_5))
mbr_zip$Yob <- 2015 - as.numeric(mbr_zip$Yob)
mbr_zip$Sex <- ifelse(as.character(mbr_zip$Sex) == 'M', 1, 0)
mbr_non_zip <- mbr_zip %>% select(-c(Zipcode_5))

#Non-SDH data files generation
hcc_comor_non_zip <- merge(hcc_comor, mbr_non_zip, by = "Patid")
hcc_comor_data <- merge(hcc_comor_non_zip, hcc_cls_cost, by = "Patid")
hcc_comor_data <- hcc_comor_data[!duplicated(hcc_comor_data[,c('Patid')]),]

hcc_train_ind <- sample(seq_len(nrow(hcc_comor_data)), size = floor(0.8 * nrow(hcc_comor_data)))
hcc_train_data <- hcc_comor_data %>% select(-c(Patid))
hcc_train_data <- hcc_train_data[hcc_train_ind,]
hcc_test_data <- hcc_train_data[-hcc_train_ind,]
hcc_val_ind <- sample(seq_len(nrow(hcc_test_data)), size = floor(0.5 * nrow(hcc_test_data))) 
hcc_val_data <- hcc_test_data[hcc_val_ind,]
hcc_test_data <- hcc_test_data[-hcc_val_ind,]

write.csv(hcc_train_data, file = "hcc_train_data.csv")
write.csv(hcc_val_data, file = "hcc_val_data.csv")
write.csv(hcc_test_data, file = "hcc_test_data.csv")

#SDH data files generation

#Impute missing SDH values
sdh_df <- read.csv(file = '/home/chiraagsumanth/optum/sdh.csv', header = FALSE)
colnames(sdh_df) <- c("Zipcode_5","v1","v2","v3","v4","v5","v6")
impute_dm <- data.matrix(sdh_df)
impute_sdh_df <- mice(impute_dm, m = 1)
sdh_df <- as.data.frame(complete(impute_sdh_df, "long") %>% select(-c(.imp,.id)))
sdh_df$v1 <- as.character(sdh_df$v1)
sdh_df$v2 <- as.character(sdh_df$v2)
sdh_df$v3 <- as.character(sdh_df$v3)
sdh_df$v4 <- as.character(sdh_df$v4)
sdh_df$v5 <- as.character(sdh_df$v5)
sdh_df$v6 <- as.character(sdh_df$v6)

write.csv(sdh_df, file = "sdh_df_imputed.csv")

hcc_comor_zip <- merge(hcc_comor, mbr_zip, by = "Patid")

countchar <- function(s) {
  s2 <- gsub("_","",s)
  return (nchar(s) - nchar(s2))
}

hcc_comor_zip$mul_hom <- countchar(hcc_comor_zip$Zipcode_5)
hcc_comor_zip$mul_hom <- ifelse(hcc_comor_zip$mul_hom >= 2, 1, 0)
Zipcode_5 <- sapply(strsplit(hcc_comor_zip$Zipcode_5,'_'), `[`, 1)
hcc_comor_zip$Zipcode_5 <- Zipcode_5

hcc_comor_sdh_zip <- merge(hcc_comor_zip, sdh_df, by = "Zipcode_5")

hcc_comor_sdh_data <- merge(hcc_comor_sdh_zip, hcc_cls_cost, by = "Patid")
hcc_comor_sdh_data <- hcc_comor_sdh_data[!duplicated(hcc_comor_sdh_data[,c('Patid')]),]

hcc_comor_sdh_data$v1 <- as.numeric(hcc_comor_sdh_data$v1)
hcc_comor_sdh_data$v2 <- as.numeric(hcc_comor_sdh_data$v2)
hcc_comor_sdh_data$v3 <- as.numeric(hcc_comor_sdh_data$v3)
hcc_comor_sdh_data$v4 <- as.numeric(hcc_comor_sdh_data$v4)
hcc_comor_sdh_data$v5 <- as.numeric(hcc_comor_sdh_data$v5)
hcc_comor_sdh_data$v6 <- as.numeric(hcc_comor_sdh_data$v6)
hcc_comor_sdh_data <- hcc_comor_sdh_data[complete.cases(hcc_comor_sdh_data),]

hcc_sdh_train_ind <- sample(seq_len(nrow(hcc_comor_sdh_data)), size = floor(0.8 * nrow(hcc_comor_sdh_data)))
hcc_sdh_train_data <- hcc_comor_sdh_data %>% select(-c(Patid, Zipcode_5))
hcc_sdh_train_data <- hcc_sdh_train_data[hcc_sdh_train_ind,]
hcc_sdh_test_data <- hcc_sdh_train_data[-hcc_sdh_train_ind,]
hcc_sdh_val_ind <- sample(seq_len(nrow(hcc_sdh_test_data)), size = floor(0.5 * nrow(hcc_sdh_test_data))) 
hcc_sdh_val_data <- hcc_sdh_test_data[hcc_sdh_val_ind,]
hcc_sdh_test_data <- hcc_sdh_test_data[-hcc_sdh_val_ind,]

hcc_sdh_train_data <- na.omit(hcc_sdh_train_data)
hcc_sdh_test_data <- na.omit(hcc_sdh_test_data)
hcc_sdh_val_data <- na.omit(hcc_sdh_val_data)

write.csv(hcc_sdh_train_data, file = "hcc_train_sdh_data.csv")
write.csv(hcc_sdh_val_data, file = "hcc_val_sdh_data.csv")
write.csv(hcc_sdh_test_data, file = "hcc_test_sdh_data.csv")
