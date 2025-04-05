library(readxl)
library(cluster)
library(psych)
library(ggcorrplot)
library(factoextra)
library(caret)
library(e1071)
library(caTools)
library(rpart)
library(rstudioapi)
library(rpart.plot)
library(ROCR)
library(pROC)

run <- function() {
  eval(parse(text = rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())$text))
}

df_heart <- read_xlsx("heart.xlsx", sheet = "heart_failure_clinical_records",
                      col_names = T, range = "A1:M5001")

string_to_numeric <- function(x) {
  if (is.character(x) || is.factor(x)) {
    as.numeric(as.character(x)) # Convert to numeric values if possible
  }
  else {
    x
  }
}

# Original column names
cnames <- c("Wiek", "Anemia", "Kinaza_fosfokreatyninowa", "Cukrzyca", "Frakcja_wyrzutowa", "Wysokie_ciśnienie_krwi",
            "Płytki_krwi", "Keratynina_w_surowicy", "Stężenie_sodu_w_surowicy", "Płeć", "Palenie", "Czas_w_dniach", "Przypadek_śmiertelny")
# Column names used for descriptive statistics
cnames_X <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13")
df_heart <- as.data.frame(lapply(df_heart, string_to_numeric))
colnames(df_heart) <- cnames
print("df_heart:")
print(head(df_heart))

# Descriptive statistics
df_heart_X <- df_heart
colnames(df_heart_X) <- cnames_X
stat = describe(df_heart_X, type = 2, quant = c(.25, .75))
print(stat[, -c(1, 2, 6, 7)])
wsp_zm <- round( (stat$sd/stat$mean)*100, 3)
which(wsp_zm < 10) # quasi-constant
wsp_zm <- as.data.frame( t(c(wsp_zm)) )
colnames(wsp_zm) <- cnames_X
rownames(wsp_zm) <- 'wsp_zm '
wsp_zm

# Bar plots for variables showing value distribution
num_cols = ncol(df_heart)
for (i in 1:num_cols) {
  counts <- table(df_heart[, i]) # Number of occurrences for each value in a column
  if (i == 3 || i == 7) {
    # For columns 3 and 7, there's an extreme outlier distorting the Y-axis,
    # so we manually set ylim to 100
    ylim_max = 100
  } else {
    # For other columns, set ylim based on max count * 1.2
    ylim_max = 1.2 * max(counts)
  }
  barplot(counts, ylab = "Number of observations", ylim = c(0, ylim_max), 
          main = paste0("X", i, ' - ', colnames(df_heart)[i]))
}

# Boxplots for continuous variables
continous_value_columns = c(1, 3, 5, 7, 8, 9, 12)
for (i in continous_value_columns) {
  boxplot(df_heart[,i], main = paste0("X", i, ' - ', colnames(df_heart)[i]))
}

# Correlation matrix
ggcorrplot(cor(df_heart), lab = T, lab_size = 2)

# Remove column with coefficient of variation less than 10
df_heart <- df_heart[, -9]

print("================================================================================================================")
print("df_heart after removing column 9:")
print(head(df_heart))

# Outliers
# for (i in 1:(num_cols-1)) {
#   print(paste0(i, ": ", boxplot.stats(df_heart[,i])$out))
# }
print(df_heart[df_heart[, 3] > 7000, 3])

transform_outlier_data <- function(df) {
  for (i in 1:ncol(df)) {
    whiskers = boxplot.stats(df[,i])$stats[c(1, 5)] # Lower and upper whiskers
    
    # If value is below lower whisker, set to lower whisker
    df[ df[, i] < whiskers[1], i] <- whiskers[1] 
    
    # If value is above upper whisker, set to upper whisker
    df[ df[, i] > whiskers[2], i] <- whiskers[2]
  }
  
  return(df)
}

# Transform outlier values
df_heart <- transform_outlier_data(df_heart)

set.seed(7267166)
# Split into training and test set (80/20 split)
trainIndex = createDataPartition(df_heart$Przypadek_śmiertelny, p = 0.8)$Resample1

# Variable transformations – normalization
normalizacja <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Normalized and split data to be used in further classification methods
df_heart_NORM <- as.data.frame(lapply(df_heart, normalizacja))
train_NORM = df_heart_NORM[trainIndex, ]
test_NORM = df_heart_NORM[-trainIndex, ]

######################################################################################
# UNTIL THIS POINT, DATA WAS LOADED, VISUALIZED, AND SPLIT INTO TRAINING AND TEST SETS #
# BELOW ARE APPLICATIONS OF DIFFERENT CLASSIFICATION METHODS                         #                              
######################################################################################

### NAIVE BAYES CLASSIFIER
# Train classifier and make predictions on test data
classifier_NB_NORM <- naiveBayes(Przypadek_śmiertelny ~ ., data = train_NORM)
predictions_testdata_NORM <- predict(classifier_NB_NORM, newdata = test_NORM)
prob_testdata_NB = predict(classifier_NB_NORM, newdata = test_NORM, type = "raw")[, 2]
# Confusion matrix
print("")
print("================================================================================================================")
print("")
print("NAIVE BAYES")
print("CONFUSION MATRIX AND PREDICTION METRICS FOR TEST DATA:")
cm_object_testdata_NORM = confusionMatrix(reference = as.factor(test_NORM$Przypadek_śmiertelny), 
                                          data = as.factor(predictions_testdata_NORM), positive = '0')

# Transpose matrix to match presentation layout (actual values in rows, predictions in columns)
cm_testdata_NORM_transposed <- t(cm_object_testdata_NORM$table)
print(cm_testdata_NORM_transposed)

# Function to display quality metrics using confusion matrix
# Some metrics are built-in to confusionMatrix, this function also calculates F1-score
prediction_quality <- function(cm) {
  precision = cm['0', '0'] / (cm['0', '0'] + cm['1', '0'])
  NPV =  cm['1', '1'] / (cm['1', '1'] + cm['0', '1'])
  specificity = cm['1', '1'] / (cm['1', '1'] + cm['1', '0'])
  sensitivity = cm['0', '0'] / (cm['0', '0'] + cm['0', '1'])
  accuracy = (cm['0', '0'] + cm['1', '1']) / sum(cm)
  F1 = 2 * (precision * sensitivity) / (precision + sensitivity)
  
  print( sprintf("Precision: %s", round(precision, 3)) )
  print( sprintf("NPV: %s", round(NPV, 3)) )
  print( sprintf("Specificity: %s", round(specificity, 3)) )
  print( sprintf("Sensitivity: %s", round(sensitivity, 3)) )
  print( sprintf("Accuracy: %s", round(accuracy, 3)) )
  print( sprintf("F1-score: %s", round(F1, 3)) )
}

prediction_quality(cm_testdata_NORM_transposed)

# testing for newly generated data
new_test_NORM = as.data.frame(matrix(nrow = 1000, ncol = ncol(df_heart)))
colnames(new_test_NORM) <- cnames[-9] # Skip the 9th column ("Serum sodium concentration")

# generating new records
for (i in 1:nrow(new_test_NORM)) {
  for (j in 1:ncol(new_test_NORM)) {
    new_test_NORM[i, j] = sample(df_heart_NORM[, j], 1)
  }
}

# Predictions and confusion matrix for new data
predictions_newdata_NORM = predict(classifier_NB_NORM, newdata = new_test_NORM)
prob_newdata_NB = predict(classifier_NB_NORM, newdata = new_test_NORM, type = "raw")[, 2]

print("")
print("================================================================================================================")
print("")
print("NAIVE BAYES")
print("CONFUSION MATRIX AND PREDICTION STATISTICS FOR NEW DATA:")
cm_object_newdata_NORM = confusionMatrix(reference = as.factor(new_test_NORM$Przypadek_śmiertelny),
                                         data = as.factor(predictions_newdata_NORM), positive = '0')
cm_newdata_NORM_transposed <- t(cm_object_newdata_NORM$table)
print(cm_newdata_NORM_transposed)
prediction_quality(cm_newdata_NORM_transposed)

true_test = test_NORM$Przypadek_śmiertelny # true values for test data
predicted_bayes_test = prob_testdata_NB # predicted values by Bayes for test data

true_new = new_test_NORM$Przypadek_śmiertelny # true values for new data
predicted_bayes_new = prob_newdata_NB # predicted values by Bayes for new data

pred_test_NB <- prediction(as.numeric(predicted_bayes_test), true_test)
perf_test_NB <- performance(pred_test_NB, "tpr", "fpr")

pred_new_NB <- prediction(as.numeric(predicted_bayes_new), true_new)
perf_new_NB <- performance(pred_new_NB, "tpr", "fpr")

plot(perf_test_NB, main = "ROC Curve - Naive Bayes Classifier", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_NB@x.values[[1]], perf_new_NB@y.values[[1]], col = "green", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("Test data", "New data", "Random classifier"), 
       col = c("blue", "green", "red"), lwd = 2, lty = 1)

# visualization for predictions on test data
pca_test <- prcomp(test_NORM, scale = TRUE)
summary(pca_test)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Test data")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predictions_testdata_NORM, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction")

# visualization for predictions on new data
pca_new <- prcomp(new_test_NORM, scale = TRUE)
summary(pca_new)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("New data")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predictions_newdata_NORM, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction")



### DECISION TREE CLASSIFIER
# DT Classifier
classifier_DT = rpart(formula = Przypadek_śmiertelny ~ ., data = train_NORM, method = "class")

# tree visualization
rpart.plot(classifier_DT)
predicted_DT = predict(classifier_DT, newdata = test_NORM, type = "class")
prob_testdata_DT = predict(classifier_DT, newdata = test_NORM, type = "prob")[, 2]
# print(predicted_DT)
# cm_object_testdata_DT = confusionMatrix(reference = as.factor(test_NORM$Przypadek_śmiertelny),
#                                        data = as.factor(predicted_DT), positive = '0')
print("")
print("================================================================================================================")
print("")
print("DECISION TREE")
print("CONFUSION MATRIX AND PREDICTION STATISTICS FOR TEST DATA:")
cm_testdata_DT = table(reference = test_NORM$Przypadek_śmiertelny, predicted = predicted_DT)
print(cm_testdata_DT)
prediction_quality(cm_testdata_DT) # decision tree classification quality indicators
# print(cm_object_testdata_DT)

print("")
print("================================================================================================================")
print("")
print("DECISION TREE")
print("CONFUSION MATRIX AND PREDICTION STATISTICS FOR NEW DATA:")
predicted_newdata_DT = predict(classifier_DT, newdata = new_test_NORM, type = "class")
prob_newdata_DT = predict(classifier_DT, newdata = new_test_NORM, type = "prob")[, 2]
cm_newdata_DT = table(reference = new_test_NORM$Przypadek_śmiertelny, predicted = predicted_newdata_DT)
print(cm_newdata_DT)
prediction_quality(cm_newdata_DT) # decision tree classification quality indicators

# ROC curves for Decision Tree Classifier
predicted_test_DT = prob_testdata_DT # values predicted by the decision tree for test data
predicted_new_DT = prob_newdata_DT # values predicted by the tree for new data

pred_test_DT <- prediction(as.numeric(predicted_test_DT), true_test) # prediction object for test data
perf_test_DT <- performance(pred_test_DT, "tpr", "fpr") # performance object for test data,
# used for plotting the ROC curve

pred_new_DT <- prediction(as.numeric(predicted_new_DT), true_new) # prediction object for new data
perf_new_DT <- performance(pred_new_DT, "tpr", "fpr") # performance object for new data, used for plotting 
# the ROC curve

plot(perf_test_DT, main = "ROC Curve - Decision Tree", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_DT@x.values[[1]], perf_new_DT@y.values[[1]], col = "green", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("Test Data", "New Data", "Random Classifier"), 
       col = c("blue", "green", "red"), lwd = 2, lty = 1)

# visualization for predictions on test data
pca_test <- prcomp(test_NORM, scale = TRUE)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Test Data")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predicted_DT, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction")

# visualization for predictions on new data
pca_new <- prcomp(new_test_NORM, scale = TRUE)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("New Data")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predicted_newdata_DT, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction")



### LOGISTIC REGRESSION CLASSIFIER
# logistic regression
classifier_Logit = glm(formula = Przypadek_śmiertelny ~ ., family = binomial, data = train_NORM)
print(summary(classifier_Logit))

predicted_Logit = predict(classifier_Logit, newdata = test_NORM, type = "response")
prob_testdata_Logit = predicted_Logit
p_kryt1 = 0.5

n = length(test_NORM$Przypadek_śmiertelny)
predicted_Logit1 = 0 # changing probability to y^=1 or y^=0
for(i in 1:n){
  if(predicted_Logit[i] > p_kryt1){
    predicted_Logit1[i] = 1
  }else{
    predicted_Logit1[i] = 0
  }
}

print("")
print("================================================================================================================")
print("")
print("LOGISTIC REGRESSION")
print("CONFUSION MATRIX AND PREDICTION STATISTICS FOR TEST DATA:")
cm_testdata_Logit1 = table(reference = test_NORM$Przypadek_śmiertelny, predicted = predicted_Logit1)
print(cm_testdata_Logit1)
prediction_quality(cm_testdata_Logit1) # logistic regression classification quality indicators

print("")
print("================================================================================================================")
print("")
print("LOGISTIC REGRESSION")
print("CONFUSION MATRIX AND PREDICTION STATISTICS FOR NEW DATA:")
predicted_newdata_Logit = predict(classifier_Logit, newdata = new_test_NORM, type = "response")
prob_newdata_Logit = predicted_newdata_Logit

predicted_newdata_Logit1 = 0 # changing probability to y^=1 or y^=0
for(i in 1:n){
  if(predicted_newdata_Logit[i] > p_kryt1){
    predicted_newdata_Logit1[i] = 1
  }else{
    predicted_newdata_Logit1[i] = 0
  }
}

cm_newdata_Logit1 = table(reference = new_test_NORM$Przypadek_śmiertelny, predicted = predicted_newdata_Logit1)
print(cm_newdata_Logit1)
prediction_quality(cm_newdata_Logit1) # logistic regression classification quality indicators

# ROC curves for Logistic Regression
predicted_test_Logit1 = prob_testdata_Logit # values predicted by logistic regression for test data
predicted_new_Logit1 = prob_newdata_Logit  # values predicted by logistic regression for new data

pred_test_Logit1 <- prediction(as.numeric(predicted_test_Logit1), true_test) # prediction object for test data
perf_test_Logit1 <- performance(pred_test_Logit1, "tpr", "fpr") # performance object for test data,
# used for plotting the ROC curve

pred_new_Logit1 <- prediction(as.numeric(predicted_new_Logit1), true_new) # prediction object for new data
perf_new_Logit1 <- performance(pred_new_Logit1, "tpr", "fpr") # performance object for new data, used for plotting 
# the ROC curve

plot(perf_test_Logit1, main = "ROC Curve - Logistic Regression", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_Logit1@x.values[[1]], perf_new_Logit1@y.values[[1]], col = "green", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("Test Data", "New Data", "Random Classifier"), 
       col = c("blue", "green", "red"), lwd = 2, lty = 1)

# visualization for predictions on test data
pca_test <- prcomp(test_NORM, scale = TRUE)
summary(pca_test)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Test Data")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predicted_Logit1, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction")

# visualization for predictions on new data
pca_new <- prcomp(new_test_NORM, scale = TRUE)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) +
  theme_minimal() +
  ggtitle("New Data")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predicted_newdata_Logit1, addEllipses = T) +
  theme_minimal() +
  ggtitle("Prediction")


#############HYBRID MODEL######################

# Hybrid classifier function
# This function takes the three classifiers defined earlier, data (either test or new),
# weights for each classifier in the form of a vector, and a probability threshold, defaulting to 0.5
hybrid_classifier <- function(cl_NB, cl_DT, cl_LR, data, weights, prob_threshold = 0.5) {
  # Predicted probabilities for each classifier
  prob_nb <- predict(cl_NB, data, type = "raw")[, 2]
  prob_dt <- predict(cl_DT, data, type = "prob")[, 2]
  prob_lr <- predict(cl_LR, data, type = "response")
  
  # Calculate weighted average of probabilities
  ensemble_prob <- (weights[1] * prob_nb + weights[2] * prob_dt + weights[3] * prob_lr) / sum(weights)
  ensemble_prediction <- ifelse(ensemble_prob > prob_threshold, 1, 0)
  
  # Return a list of results
  return(list(probabilities = ensemble_prob, classes = ensemble_prediction))
}

# Function to draw ROC curve
draw_roc_curve <- function(predicted_classes, true_labels, title, color, add = FALSE) {
  pred <- prediction(as.numeric(predicted_classes), true_labels)
  perf <- performance(pred, "tpr", "fpr")
  if (add) {
    lines(perf@x.values[[1]], perf@y.values[[1]], col = color, lwd = 2)
  } else {
    plot(perf, main = title, col = color, lwd = 2,
         xlab = "False Positive Rate (1 - Specificity)",
         ylab = "True Positive Rate (Sensitivity)")
  }
}

# Weights based on F1-score
calculate_model_weights <- function(f1_test, f1_new, weight_test = 0.7, weight_new = 0.3) {
  f1_combined <- (f1_test * weight_test + f1_new * weight_new)
  model_weights <- f1_combined / sum(f1_combined)
  return(model_weights)
}

# F1-score calculations and weights
f1_test <- c(0.887, 0.919, 0.886)  # Bayes, Decision Tree, Logistic Regression (Test Data)
f1_new <- c(0.698, 0.690, 0.704)   # Bayes, Decision Tree, Logistic Regression (New Data)
weights <- calculate_model_weights(f1_test, f1_new)

# Hybrid classifier on test data
predicted_testdata_HC <- hybrid_classifier(
  cl_NB = classifier_NB_NORM,
  cl_DT = classifier_DT,
  cl_LR = classifier_Logit,
  data = test_NORM,
  weights = weights
)

# Hybrid classifier on new data
predicted_newdata_HC <- hybrid_classifier(
  cl_NB = classifier_NB_NORM,
  cl_DT = classifier_DT,
  cl_LR = classifier_Logit,
  data = new_test_NORM,
  weights = weights
)

# Confusion matrices and quality statistics
print("================================================================================================================")
print("HYBRID CLASSIFIER - CONFUSION MATRIX AND STATISTICS FOR TEST DATA:")
cm_testdata_HC <- table(reference = test_NORM$Przypadek_śmiertelny, predicted = predicted_testdata_HC$classes)
print(cm_testdata_HC)
prediction_quality(cm_testdata_HC) # `prediction_quality` function must be defined earlier.

print("================================================================================================================")
print("HYBRID CLASSIFIER - CONFUSION MATRIX AND STATISTICS FOR NEW DATA:")
cm_newdata_HC <- table(reference = new_test_NORM$Przypadek_śmiertelny, predicted = predicted_newdata_HC$classes)
print(cm_newdata_HC)
prediction_quality(cm_newdata_HC)

# Drawing ROC curves
true_test <- test_NORM$Przypadek_śmiertelny
true_new <- new_test_NORM$Przypadek_śmiertelny

draw_roc_curve(predicted_testdata_HC$probabilities, true_test,
               title = "ROC Curve - Hybrid Classifier",
               color = "blue")
draw_roc_curve(predicted_newdata_HC$probabilities, true_new,
               title = "ROC Curve - Hybrid Classifier",
               color = "green", add = TRUE)
abline(a = 0, b = 1, col = "red", lwd = 2)  # Random Classifier
legend("bottomright", legend = c("Test Data", "New Data", "Random Classifier"),
       col = c("blue", "green", "red"), lwd = 2, lty = 1)


# Visualization for predictions on test data
pca_test <- prcomp(test_NORM, scale = TRUE)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Test Data")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predicted_testdata_HC$classes, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction - Hybrid Model")

# Visualization for predictions on new data
pca_new <- prcomp(new_test_NORM, scale = TRUE)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("New Data")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predicted_newdata_HC$classes, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Prediction - Hybrid Model")


###### ROC curves for test and new data - comparison of all classifiers ######
pred_test_HC <- prediction(as.numeric(predicted_testdata_HC$probabilities), true_test)
perf_test_HC <- performance(pred_test_HC, "tpr", "fpr")

pred_new_HC <- prediction(as.numeric(predicted_newdata_HC$probabilities), true_new)
perf_new_HC <- performance(pred_new_HC, "tpr", "fpr")

plot(perf_test_NB, main = "ROC Curve - Test Data", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_test_DT@x.values[[1]], perf_test_DT@y.values[[1]], col = "green", lwd = 2)
lines(perf_test_Logit1@x.values[[1]], perf_test_Logit1@y.values[[1]], col = "red", lwd = 2)
lines(perf_test_HC@x.values[[1]], perf_test_HC@y.values[[1]], col = "orange", lwd = 2)
abline(a = 0, b = 1, col = "black", lwd = 2)
legend("bottomright", legend = c("Naive Bayes Classifier", "Decision Tree Classifier",
                                 "Logistic Regression", "Hybrid Classifier", "Random Classifier"), 
       col = c("blue", "green", "red", "orange", "black"), lwd = 2, lty = 1)


plot(perf_new_NB, main = "ROC Curve - New Data", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_DT@x.values[[1]], perf_new_DT@y.values[[1]], col = "green", lwd = 2)
lines(perf_new_Logit1@x.values[[1]], perf_new_Logit1@y.values[[1]], col = "red", lwd = 2)
lines(perf_new_HC@x.values[[1]], perf_new_HC@y.values[[1]], col = "orange", lwd = 2)
abline(a = 0, b = 1, col = "black", lwd = 2)
legend("bottomright", legend = c("Naive Bayes Classifier", "Decision Tree Classifier",
                                 "Logistic Regression", "Hybrid Classifier", "Random Classifier"), 
       col = c("blue", "green", "red", "orange", "black"), lwd = 2, lty = 1)
