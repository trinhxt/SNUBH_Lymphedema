# Load library
library(openxlsx)
library(svDialogs)
library(data.table)
library(caret)
library(dplyr)
library(purrr)
library(doMC)
library(ROCit)


#-------------------------------------------------------------------------------
# Read data excel file

DataFile <- dlg_open(title = "Select excel data file:", filters = dlg_filters[c("All"), ])$res
DataFolder <- dirname(DataFile)

DataTable <- read.xlsx(DataFile, sheet = 1, startRow = 1, colNames = TRUE,
                       rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                       skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                       namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)



#-------------------------------------------------------------------------------
# Select variables + endpoint
Table1 <- DataTable %>%
  #select(-c("id", "opd", "nam.y", "int", "le"))
  select(-c("id", "opd", "nam.y", "le"))
  #select(-c("id", "opd", "nam.y", "tax", "lnn","axi","int", "che", "fx", "Gy", "recon", "le"))

Table1$Endpoint <- factor(DataTable$le)

randomseed <- 365#3890#1675#1165#2030#


set.seed(randomseed)
  
  
  # Split data into train/test with ratio 8/2 of the sample size
  in_rows <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
  train <- Table1[in_rows, ]
  test <- Table1[-in_rows, ]
  
  
#-------------------------------------------------------------------------------
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
  registerDoMC(cores=6)
  XGBmodel <- train(Endpoint~., data=train, method="xgbTree", trControl=control, tuneLength=5)
# summarize the model
  print(XGBmodel)
# Save model for later use
  save(XGBmodel, file = paste(DataFolder,"/XGBmodel_220604_13.Rdata", sep = ""))
 
#-------------------------------------------------------------------------------
# Find Cut-off value for probability to maximize balanced accuracy

# Get probability
  pred_all_prob <- as.data.frame(XGBmodel %>% predict(Table1, type = "prob"))
  
  Table_cutoff <- data.frame( "Cutoff"             = seq(0.01, 1, by= 0.01),
                              "Balanced_Accuracy"  = 0)    
  for (i in (1:100)) {
    pred.LE <- as.factor(ifelse(pred_all_prob$`1`>Table_cutoff$Cutoff[i],"1","0"))
    Table2 <- table(factor(pred.LE, levels = c("0","1")), Table1$Endpoint)
    ConfMat <- confusionMatrix(Table2)
    Performance <- setDT(as.data.frame(ConfMat$byClass), keep.rownames = TRUE)[]
    Table_cutoff$Balanced_Accuracy[i] <- Performance[11,2]
}

cutoff <- Table_cutoff$Cutoff[which.max(Table_cutoff$Balanced_Accuracy)]

#-------------------------------------------------------------------------------
# Make prediction on test set 
  pred_train_prob <- as.data.frame(XGBmodel %>% predict(train, type = "prob"))
  pred_test_prob <- as.data.frame(XGBmodel %>% predict(test, type = "prob"))
  pred_all_prob <- as.data.frame(XGBmodel %>% predict(Table1, type = "prob"))
  
  pred_train <- as.factor(ifelse(pred_train_prob$`1`>cutoff,"1","0"))
  pred_test <- as.factor(ifelse(pred_test_prob$`1`>cutoff,"1","0"))
  pred_all <- as.factor(ifelse(pred_all_prob$`1`>cutoff,"1","0"))
  
  
#-------------------------------------------------------------------------------
# Confusion matrix and model performance in Train+Test
  Table2 <- table(pred_all, Table1$Endpoint)
  ConfMat <- confusionMatrix(Table2)
  Performance <- setDT(as.data.frame(ConfMat$byClass), keep.rownames = TRUE)[]
  colnames(Performance) <- c("Parameter", "Value")
  Performance

#-------------------------------------------------------------------------------
# Confusion matrix and model performance in Train
  Table21 <- table(pred_train, train$Endpoint)
  ConfMat_train <- confusionMatrix(Table21)
  Performance_train <- setDT(as.data.frame(ConfMat_train$byClass), keep.rownames = TRUE)[]
  colnames(Performance_train) <- c("Parameter", "Value")
  Performance_train
  
  
#-------------------------------------------------------------------------------
# Confusion matrix and model performance in Test
  Table22 <- table(pred_test, test$Endpoint)
  ConfMat_test <- confusionMatrix(Table22)
  Performance_test <- setDT(as.data.frame(ConfMat_test$byClass), keep.rownames = TRUE)[]
  colnames(Performance_test) <- c("Parameter", "Value")
  Performance_test

  
#-------------------------------------------------------------------------------  
# Estimate Descriptor importance
  DescImportance <- data.frame(Descriptor = row.names(varImp(XGBmodel, scale=TRUE)$importance),
                               Value = varImp(XGBmodel, scale=TRUE)$importance)
  colnames(DescImportance) <- c("Descriptor", "Value")
  DescImportance <- DescImportance[order(DescImportance$Value, decreasing = TRUE),]
  
# Combine original data (DataTable) with Probability
  Table3 <- DataTable
  Table3$Prediction <- pred_all
  Table3$Prediction_prob_0 <- pred_all_prob$`0`
  Table3$Prediction_prob_1 <- pred_all_prob$`1`
  
# Add Train/Test label to the combined data
  Table3$Train_Test <- ifelse(do.call(paste0, Table1) %in% do.call(paste0, train) == TRUE, "Train", "Test")
  

#-------------------------------------------------------------------------------     
# ROC and AUC data  
  ROCit_obj_all <- rocit(score=Table3$Prediction_prob_1,class=Table3$le); plot(ROCit_obj_all)
  ROCit_obj_test <- rocit(score=pred_test_prob$`1`,class=test$Endpoint); plot(ROCit_obj_test)
  ROCit_obj_train <- rocit(score=pred_train_prob$`1`,class=train$Endpoint); plot(ROCit_obj_train)
  
  ROC_data_all <- data.frame(FPR = ROCit_obj_all$FPR,
                             TPR = ROCit_obj_all$TPR)
  ROC_data_train <- data.frame(FPR = ROCit_obj_train$FPR,
                               TPR = ROCit_obj_train$TPR)
  ROC_data_test <- data.frame(FPR = ROCit_obj_test$FPR,
                              TPR = ROCit_obj_test$TPR)
  ROCit_obj_train$AUC
  ROCit_obj_test$AUC
  
  
  
  
#-------------------------------------------------------------------------------    
# Save data to excel file
  ExcelFile <- createWorkbook("Lymphedema")
  addWorksheet(ExcelFile, "DataTable")
  writeData(ExcelFile, sheet = 1, Table3)
  addWorksheet(ExcelFile, "Descriptors")
  writeData(ExcelFile, sheet = 2, DescImportance)
  addWorksheet(ExcelFile, "ModelInfo")
  writeData(ExcelFile, sheet = 3, capture.output(XGBmodel))
  addWorksheet(ExcelFile, "Performance_train")
  writeData(ExcelFile, sheet = 4, Performance_train)
  addWorksheet(ExcelFile, "CfMatrix_train")
  writeData(ExcelFile, sheet = 5, Table21)
  addWorksheet(ExcelFile, "Performance_test")
  writeData(ExcelFile, sheet = 6, Performance_test)
  addWorksheet(ExcelFile, "CfMatrix_test")
  writeData(ExcelFile, sheet = 7, Table22)
  addWorksheet(ExcelFile, "ROC_train")
  writeData(ExcelFile, sheet = 8, ROC_data_train)
  addWorksheet(ExcelFile, "AUC_train")
  writeData(ExcelFile, sheet = 9, ROCit_obj_train$AUC)
  addWorksheet(ExcelFile, "ROC_test")
  writeData(ExcelFile, sheet = 10, ROC_data_test)
  addWorksheet(ExcelFile, "AUC_test")
  writeData(ExcelFile, sheet = 11, ROCit_obj_test$AUC)
  addWorksheet(ExcelFile, "Cutoff")
  writeData(ExcelFile, sheet = 12, cutoff)
  addWorksheet(ExcelFile, "Seed")
  writeData(ExcelFile, sheet = 13, randomseed)
  saveWorkbook(ExcelFile, paste(DataFolder,"/XGB_output_220604_13.xlsx", sep = ""), overwrite = TRUE)
  
  
  