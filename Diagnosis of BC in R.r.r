# Set working directory and load the data
setwd("C:/Users/dilip/Downloads")
df = read.csv("BC_Diagnostics.csv")

# Convert diagnosis column to a factor
df$diagnosis = as.factor(df$diagnosis)
df$id = NULL

# Display summary and structure of the data
summary(df)
str(df)

# Calculate proportion of each diagnosis category
prop.table(table(df$diagnosis))

# Load required libraries
library(corrplot)
library(mgcv)
library(caret)
library(randomForest)

# Create a correlation plot
library(corrplot)
corrplot(cor(df[,2:31]), method = "circle", type = "upper", tl.col = "black")
cor_matrix <- cor(df[, 2:31])

# Create histograms for various features
par(mfrow = c(2, 3))
for (i in 2:21) {
  hist(df[, i], main = paste("Histogram of", colnames(df)[i]), xlab = colnames(df)[i])
}

# Create the count plot
plot(df$diagnosis, xlab = "Diagnosis (0 - B, 1 - M)", ylab = "Count")

# Create spline plots for selected features
par(mfrow = c(2, 2))
# Radius Mean
model1 <- gam(diagnosis ~ s(radius_mean), data = df, family=binomial())
#summary(model1)
#plot(model1)
plot(model1, select = 1, scheme = 1, se = TRUE, xlab = "Radius Mean", ylab = "Log(Odds)")
# Texture Mean
model2 <- gam(diagnosis ~ s(texture_mean), data = df, family=binomial())
#summary(model2)
#plot(model2, ylab = "Log(Odds)")
plot(model2, select = 1, scheme = 1, se = TRUE, xlab = "Texture Mean", ylab = "Log(Odds)")

# Perimeter Mean
model3 <- gam(diagnosis ~ s(perimeter_mean), data = df, family=binomial())
#summary(model3)
#plot(model3, ylab = "Log(Odds)")
plot(model3, select = 1, scheme = 1, se = TRUE, xlab = "Perimeter Mean", ylab = "Log(Odds)")

# Area Mean
model4 <- gam(diagnosis ~ s(area_mean), data = df, family=binomial())
#summary(model4)
#plot(model4, ylab = "Log(Odds)")
plot(model4, select = 1, scheme = 1, se = TRUE, xlab = "Area Mean", ylab = "Log(Odds)")

# Smoothness Mean
model5 <- gam(diagnosis ~ s(smoothness_mean), data = df, family=binomial())
#summary(model5)
#plot(model5, ylab = "Log(Odds)")
plot(model5, select = 1, scheme = 1, se = TRUE, xlab = "Smoothness Mean", ylab = "Log(Odds)")

# Compactness Mean
model6 <- gam(diagnosis ~ s(compactness_mean), data = df, family=binomial())
#summary(model6)
#plot(model6, ylab = "Log(Odds)")
plot(model6, select = 1, scheme = 1, se = TRUE, xlab = "Compactness Mean", ylab = "Log(Odds)")

# Concavity Mean
model7 <- gam(diagnosis ~ s(concavity_mean), data = df, family=binomial())
#summary(model7)
#plot(model7, ylab = "Log(Odds)")
plot(model7, select = 1, scheme = 1, se = TRUE, xlab = "Concavity Mean", ylab = "Log(Odds)")

# Concave Points Mean
model8 <- gam(diagnosis ~ s(concave.points_mean), data = df, family=binomial())
#summary(model8)
#plot(model8, ylab = "Log(Odds)")
plot(model8, select = 1, scheme = 1, se = TRUE, xlab = "Concave Pts Mean", ylab = "Log(Odds)")

# Symmentry Mean
model9 <- gam(diagnosis ~ s(symmetry_mean), data = df, family=binomial())
#summary(model9)
#plot(model9, ylab = "Log(Odds)")
plot(model9, select = 1, scheme = 1, se = TRUE, xlab = "Symmetry Mean", ylab = "Log(Odds)")

# Fractal Dimension Mean
model10 <- gam(diagnosis ~ s(fractal_dimension_mean), data = df, family=binomial())
#summary(model10)
#plot(model10, ylab = "Log(Odds)")
plot(model10, select = 1, scheme = 1, se = TRUE, xlab = "Fractal Dim. Mean", ylab = "Log(Odds)")


# Radius SE
model11 <- gam(diagnosis ~ s(radius_se), data = df, family=binomial())
#summary(model11)
#plot(model11, ylab = "Log(Odds)")
plot(model11, select = 1, scheme = 1, se = TRUE, xlab = "Radius SE", ylab = "Log(Odds)")

# Texture SE
model12 <- gam(diagnosis ~ s(texture_se), data = df, family=binomial())
#summary(model12)
#plot(model12, ylab = "Log(Odds)")
plot(model12, select = 1, scheme = 1, se = TRUE, xlab = "Texture SE", ylab = "Log(Odds)")

# Perimeter SE
model13 <- gam(diagnosis ~ s(perimeter_se), data = df, family=binomial())
#summary(model13)
#plot(model13, ylab = "Log(Odds)")
plot(model13, select = 1, scheme = 1, se = TRUE, xlab = "Perimeter SE", ylab = "Log(Odds)")

# Area SE
model14 <- gam(diagnosis ~ s(area_se), data = df, family=binomial())
#summary(model14)
#plot(model14, ylab = "Log(Odds)")
plot(model14, select = 1, scheme = 1, se = TRUE, xlab = "Area SE", ylab = "Log(Odds)")

# Smoothness SE
model15 <- gam(diagnosis ~ s(smoothness_se), data = df, family=binomial())
#summary(model15)
#plot(model15, ylab = "Log(Odds)")
plot(model15, select = 1, scheme = 1, se = TRUE, xlab = "Smoothness SE", ylab = "Log(Odds)")

# Compactness SE
model16 <- gam(diagnosis ~ s(compactness_se), data = df, family=binomial())
#summary(model16)
#plot(model16, ylab = "Log(Odds)")
plot(model16, select = 1, scheme = 1, se = TRUE, xlab = "Compactness SE", ylab = "Log(Odds)")

# Concavity SE
model17 <- gam(diagnosis ~ s(concavity_se), data = df, family=binomial())
summary(model17)
#plot(model17, ylab = "Log(Odds)")
plot(model17, select = 1, scheme = 1, se = TRUE, xlab = "Concavity SE", ylab = "Log(Odds)")

# Concave Points SE
model18 <- gam(diagnosis ~ s(concave.points_se), data = df, family=binomial())
summary(model18)
#plot(model18, ylab = "Log(Odds)")
plot(model18, select = 1, scheme = 1, se = TRUE, xlab = "Concave Pts. SE", ylab = "Log(Odds)")

# Symmentry SE
model19 <- gam(diagnosis ~ s(symmetry_se), data = df, family=binomial())
#summary(model19)
#plot(model19, ylab = "Log(Odds)")
plot(model19, select = 1, scheme = 1, se = TRUE, xlab = "Symmetry SE", ylab = "Log(Odds)")

# Fractal Dimension SE
model20 <- gam(diagnosis ~ s(fractal_dimension_se), data = df, family=binomial())
#summary(model20)
#plot(model20, ylab = "Log(Odds)")
plot(model20, select = 1, scheme = 1, se = TRUE, xlab = "Fractal Dim. SE", ylab = "Log(Odds)")


# Radius Worst
model21 <- gam(diagnosis ~ s(radius_worst), data = df, family=binomial())
#summary(model21)
#plot(model21, ylab = "Log(Odds)")
plot(model21, select = 1, scheme = 1, se = TRUE, xlab = "Radius Worst", ylab = "Log(Odds)")
# Texture Worst
model22 <- gam(diagnosis ~ s(texture_worst), data = df, family=binomial())
#summary(model22)
#plot(model22, ylab = "Log(Odds)")
plot(model22, select = 1, scheme = 1, se = TRUE, xlab = "Texture Worst", ylab = "Log(Odds)")

# Perimeter Worst
model23 <- gam(diagnosis ~ s(perimeter_worst), data = df, family=binomial())
#summary(model23)
#plot(model23, ylab = "Log(Odds)")
plot(model23, select = 1, scheme = 1, se = TRUE, xlab = "Perimeter Worst", ylab = "Log(Odds)")

# Area Worst
model24 <- gam(diagnosis ~ s(area_worst), data = df, family=binomial())
#summary(model24)
#plot(model24, ylab = "Log(Odds)")
plot(model24, select = 1, scheme = 1, se = TRUE, xlab = "Area Worst", ylab = "Log(Odds)")

# Smoothness Worst
model25 <- gam(diagnosis ~ s(smoothness_worst), data = df, family=binomial())
#summary(model25)
#plot(model25, ylab = "Log(Odds)")
plot(model25, select = 1, scheme = 1, se = TRUE, xlab = "Smoothness Worst", ylab = "Log(Odds)")

# Compactness Worst
model26 <- gam(diagnosis ~ s(compactness_worst), data = df, family=binomial())
#summary(model26)
#plot(model26, ylab = "Log(Odds)")
plot(model26, select = 1, scheme = 1, se = TRUE, xlab = "Compactness Worst", ylab = "Log(Odds)")

# Concavity Worst
model27 <- gam(diagnosis ~ s(concavity_worst), data = df, family=binomial())
#summary(model27)
#plot(model27, ylab = "Log(Odds)")
plot(model27, select = 1, scheme = 1, se = TRUE, xlab = "Concavity Worst", ylab = "Log(Odds)")

# Concave Points Worst
model28 <- gam(diagnosis ~ s(concave.points_worst), data = df, family=binomial())
#summary(model28)
#plot(model28, ylab = "Log(Odds)")
plot(model28, select = 1, scheme = 1, se = TRUE, xlab = "Concave Pts Worst", ylab = "Log(Odds)")

# Symmentry Worst
model29 <- gam(diagnosis ~ s(symmetry_worst), data = df, family=binomial())
#summary(model29)
#plot(model29, ylab = "Log(Odds)")
plot(model29, select = 1, scheme = 1, se = TRUE, xlab = "Symmetry Worst", ylab = "Log(Odds)")

# Fractal Dimension Worst
model30 <- gam(diagnosis ~ s(fractal_dimension_worst), data = df, family=binomial())
#summary(model30)
#plot(model30, ylab = "Log(Odds)")
plot(model30, select = 1, scheme = 1, se = TRUE, xlab = "Fractal Dim. Worst", ylab = "Log(Odds)")


# Create a model for feature selection using randomForest
model <- randomForest(diagnosis~., data=train_data)
importance(model)
varImpPlot(model)

# Feature selection using Recursive Feature Elimination (RFE)
set.seed(1000)
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

rfe_result <- rfe(x = df[,-1], 
                  y = df$diagnosis, 
                  sizes = c(1:30), 
                  rfeControl = ctrl)
selected_features <- predictors(rfe_result)
print(selected_features)

# Data for selected features
cancer_data = df[,c("diagnosis","area_worst","concave.points_worst","perimeter_worst","radius_worst","texture_worst","concave.points_mean","area_se","texture_mean","concavity_worst","smoothness_worst","concavity_mean","area_mean")]

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(cancer_data$diagnosis, p = 0.8, list = FALSE)
train_data <- cancer_data[train_indices, ]
test_data <- cancer_data[-train_indices, ]

# Convert the data into matrix format required by glmnet
x_train <- as.matrix(train_data[, -1])
y_train <- train_data$diagnosis
x_test <- as.matrix(test_data[, -1])

# Define the control parameters for model training
ctrl <- trainControl(method = "cv", number = 5)

# Fit Support Vector Machine (SVM) model
svm_model <- train(diagnosis ~ ., data = train_data, method = "svmRadial",
                   trControl = ctrl)

# Fit Random Forest model
rf_model <- train(diagnosis ~ ., data = train_data, method = "rf",
                  trControl = ctrl)

# Fit Logistic Regression model
logreg_model <- train(diagnosis ~ ., data = train_data, method = "glm",
                      trControl = ctrl, family = "binomial")

# Fit K-Nearest Neighbors model
knn_model <- train(diagnosis ~ ., data = train_data, method = "knn",
                   trControl = ctrl)

# Fit XGBoost model
xgb_model <- train(diagnosis ~ ., data = train_data, method = "xgbTree",
                   trControl = ctrl)

# Print the model results
print(svm_model)
print(rf_model)
print(logreg_model)
print(knn_model)

# Predict on the test data using the trained models
svm_pred <- predict(svm_model, newdata = test_data)
rf_pred <- predict(rf_model, newdata = test_data)
logreg_pred <- predict(logreg_model, newdata = test_data)
knn_pred <- predict(knn_model, newdata = test_data)
xgb_pred <- predict(xgb_model, newdata = test_data)

# Evaluate model performance
# Accuracy: The percentage of tumors whose malignancy was correctly predicted
svm_accuracy <- mean(svm_pred == test_data$diagnosis)
rf_accuracy <- mean(rf_pred == test_data$diagnosis)
logreg_accuracy <- mean(logreg_pred == test_data$diagnosis)
knn_accuracy <- mean(knn_pred == test_data$diagnosis)
xgb_accuracy <- mean(xgb_pred == test_data$diagnosis)

# Confusion Matrix
svm_cm <- confusionMatrix(svm_pred, test_data$diagnosis, positive = "1" )
rf_cm <- confusionMatrix(rf_pred, test_data$diagnosis, positive = "1")
logreg_cm <- confusionMatrix(logreg_pred, test_data$diagnosis, positive = "1")
knn_cm <- confusionMatrix(knn_pred, test_data$diagnosis, positive = "1")
xgb_cm <- confusionMatrix(xgb_pred, test_data$diagnosis, positive = "1")

accuracy = cbind(svm_accuracy, rf_accuracy, logreg_accuracy, knn_accuracy, xgb_accuracy)
colnames(accuracy) = c("SVM","RF","Log. Reg.", "KNN", "XGB")
round(accuracy, 4)

# Precision: TP / (TP + FP)
svm_precision <- svm_cm$byClass['Precision']
rf_precision <- rf_cm$byClass['Precision']
logreg_precision <- logreg_cm$byClass['Precision']
knn_precision <- knn_cm$byClass['Precision']
xgb_precision <- xgb_cm$byClass['Precision']

precision = cbind(svm_precision, rf_precision, logreg_precision, knn_precision, xgb_precision)
colnames(precision) = c("SVM","RF","Log. Reg.", "KNN", "XGB")
round(precision, 4)

# Recall: TP / (TP + FN)
svm_recall <- svm_cm$byClass['Sensitivity']
rf_recall <- rf_cm$byClass['Sensitivity']
logreg_recall <- logreg_cm$byClass['Sensitivity']
knn_recall <- knn_cm$byClass['Sensitivity']
xgb_recall <- xgb_cm$byClass['Sensitivity']

recall = cbind(svm_recall, rf_recall, lasso_recall, knn_recall, xgb_recall)
colnames(recall) = c("SVM","RF","Log. Reg.","KNN", "XGB")
round(recall, 4)

# F1 Score: (Precision * Recall) / (Precision + Recall)
svm_f1 <- svm_cm$byClass['F1']
rf_f1 <- rf_cm$byClass['F1']
logreg_f1 <- logreg_cm$byClass['F1']
knn_f1 <- knn_cm$byClass['F1']
xgb_f1 <- xgb_cm$byClass['F1']

f1 = cbind(svm_f1, rf_f1, logreg_f1, knn_f1, xgb_f1)
colnames(f1) = c("SVM","RF","Log. Reg.", "KNN", "XGB")
round(f1, 4)

# Convert the predicted labels to a prediction object
pred_svm <- prediction(as.numeric(svm_pred), as.numeric(test_data$diagnosis))
pred_rf <- prediction(as.numeric(rf_pred), as.numeric(test_data$diagnosis))
pred_logreg <- prediction(as.numeric(logreg_pred), as.numeric(test_data$diagnosis))
pred_knn <- prediction(as.numeric(knn_pred), as.numeric(test_data$diagnosis))
pred_xgb <- prediction(as.numeric(xgb_pred), as.numeric(test_data$diagnosis))

# Create the performance object
perf_svm <- performance(pred_svm, "tpr", "fpr")
perf_rf <- performance(pred_rf, "tpr", "fpr")
perf_logreg <- performance(pred_logreg, "tpr", "fpr")
perf_knn <- performance(pred_knn, "tpr", "fpr")
perf_xgb <- performance(pred_xgb, "tpr", "fpr")

# Calculate the AUC ROC
auc_svm <- performance(pred_svm, "auc")@y.values[[1]]
auc_rf <- performance(pred_rf, "auc")@y.values[[1]]
auc_logreg <- performance(pred_logreg, "auc")@y.values[[1]]
auc_knn <- performance(pred_knn, "auc")@y.values[[1]]
auc_xgb <- performance(pred_xgb, "auc")@y.values[[1]]

auc_roc = cbind(auc_svm, auc_rf, auc_logreg, auc_lasso, auc_knn, auc_xgb)
colnames(auc_roc) = c("SVM","RF","Log. Reg.", "KNN", "XGB")
round(auc_roc, 4)

par(mfrow=c(2,3))
# Plot the ROC curve
plot(perf_svm, col = "red", main = "ROC Curve - SVM", xlab = "1-Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")
legend("bottomright", legend = paste("AUC =", round(auc_svm, 2)), col = "black", bg = "white", cex = 1)
abline(0, 1, lty = 2, col = "black")

plot(perf_rf, col = "red", main = "ROC Curve - RF", xlab = "1-Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")
legend("bottomright", legend = paste("AUC =", round(auc_rf, 2)), col = "black", bg = "white", cex = 0.8)
abline(0, 1, lty = 2, col = "black")

plot(perf_logreg, col = "red", main = "ROC Curve - Log. Reg.", xlab = "1-Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")
legend("bottomright", legend = paste("AUC =", round(auc_logreg, 2)), col = "black", bg = "white", cex = 0.8)
abline(0, 1, lty = 2, col = "black")

plot(perf_knn, col = "red", main = "ROC Curve - KNN", xlab = "1-Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")
legend("bottomright", legend = paste("AUC =", round(auc_knn, 2)), col = "black", bg = "white", cex = 0.8)
abline(0, 1, lty = 2, col = "black")

plot(perf_xgb, col = "red", main = "ROC Curve - XGB", xlab = "1-Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")
legend("bottomright", legend = paste("AUC =", round(auc_xgb, 2)), col = "black", bg = "white", cex = 0.8)
abline(0, 1, lty = 2, col = "black")


# Print the AUC ROC values
print(paste("SVM AUC:", auc_svm)) # 0.9740
print(paste("Random Forest AUC:", auc_rf)) # 0.9929
print(paste("Logistic Regression AUC:", auc_logreg)) # 0.9669
print(paste("k-NN AUC:", auc_knn)) # 0.9110
print(paste("XGB AUC:", auc_xgb)) # 0.9740

auc = cbind(auc_svm, auc_rf, auc_logreg,  auc_knn, auc_xgb)
colnames(auc) = c("SVM","RF","Log. Reg.", "KNN", "XGB")
round(auc, 4)

# Print the performance metrics
print("Evaluation Metrics:")

print(paste("SVM Accuracy:", svm_accuracy)) # 0.9735
print(paste("Random Forest Accuracy:", rf_accuracy)) # 0.9911
print(paste("Logistic Regression Accuracy:", logreg_accuracy)) # 0.9646
print(paste("k-NN Accuracy:", knn_accuracy)) # 0.9115

print(paste("SVM Precision:", svm_precision)) # 0.9857
print(paste("Random Forest Precision:", rf_precision)) # 1
print(paste("Logistic Regression Precision:", logreg_precision)) # 0.9855
print(paste("k-NN Regression Precision:", knn_precision)) # 0.9855

print(paste("SVM Recall:", svm_recall)) # 0.9718
print(paste("Random Forest Recall:", rf_recall)) # 0.9859
print(paste("Logistic Regression Recall:", logreg_recall)) # 0.9577
print(paste("k-NN Recall:", knn_recall)) # 0.9154

