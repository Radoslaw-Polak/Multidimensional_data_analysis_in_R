library(readxl)
library(ggcorrplot)
library(psych)
library(factoextra)
library(caret)
library(glmnet)

df_lifeexp <- read_xlsx("Life Expectancy Data.xlsx", sheet = "Life Expectancy Data", 
                        col_names = T, range = "A1:V2939")
head(df_lifeexp)

## Move the column with outcome values (life expectancy) to the end of the table
Life_expectancy <- df_lifeexp$Life_expectancy
df_lifeexp$Life_expectancy <- NULL
df_lifeexp <- cbind(df_lifeexp, Life_expectancy)

df_lifeexp = df_lifeexp[df_lifeexp$Year == 2013, ] # filter records for the year 2013
print(head(df_lifeexp))

df_lifeexp$Status <- ifelse(df_lifeexp$Status == "Developed", 1, 0) # if status is "Developed"
# then assign value 1, otherwise ("Developing") 0
df_lifeexp = df_lifeexp[, c(-1, -2)] # remove the text column "Country" and the "Year" column
df_lifeexp <- na.omit(df_lifeexp) # remove rows with missing data

df_lifeexp <- as.data.frame(df_lifeexp)

# rounding floating point values for better appearance in distribution plots
# percentage values to 0.5% accuracy and the GDP column to whole numbers
df_lifeexp$percentage_expenditure <- floor(df_lifeexp$percentage_expenditure * 2 + 0.5) / 2
df_lifeexp$Total_expenditure <- floor(df_lifeexp$Total_expenditure * 2 + 0.5) / 2
df_lifeexp$GDP <- round(df_lifeexp$GDP)

# data after transformations
head(df_lifeexp)

cnames_XY <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11",
               "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "Y")
df_lifeexp_XY = df_lifeexp
colnames(df_lifeexp_XY) <- cnames_XY
head(df_lifeexp_XY)

stat = describe(df_lifeexp_XY, type = 2, quant = c(.25, .75))
print(stat[-1, -c(1, 2, 6, 7)])
wsp_zm <- round( (stat$sd/stat$mean)*100, 3)
which(wsp_zm < 10) # quasi-constant variables
wsp_zm <- as.data.frame( t(c(wsp_zm)) )
colnames(wsp_zm) <- colnames(df_lifeexp_XY)
rownames(wsp_zm) <- 'wsp_zm '
wsp_zm

# Number of columns in df_lifeexp
num_cols <- ncol(df_lifeexp)
for (i in 1:num_cols) {
  title_text = paste0(colnames(df_lifeexp_XY)[i], ' - ', colnames(df_lifeexp)[i])
  if (i %in% c(1, 6, 10, 11, 12, 13)) {
    # Draw bar chart for categorical variables
    counts <- table(df_lifeexp[, i])
    ylim_max <- max(counts, na.rm = TRUE)
    barplot(counts, ylab = "Number of observations", ylim = c(0, 1.2 * ylim_max),
            main = title_text)
  }
  
  else {
    # Draw histograms for variables with high variability or wide range of values
    
    # auxiliary dataframe for ggplot
    temp_df <- data.frame(x = df_lifeexp[, i])
    # Dynamic binwidth
    bw <- (max(temp_df$x, na.rm = TRUE) - min(temp_df$x, na.rm = TRUE)) / 30
    
    # draw histogram
    print(ggplot(temp_df, aes(x = x)) +
            geom_histogram(binwidth = bw, fill = "grey", color = "black") +
            labs(title = title_text, x="", y = "Number of observations") +
            theme(plot.title = element_text(hjust = 0.5))
    )
  } 
  
  # boxplots
  boxplot(df_lifeexp[, i], 
          main = paste0(colnames(df_lifeexp_XY)[i], ' - ', colnames(df_lifeexp)[i])) 
}

# Correlation matrix
ggcorrplot(cor(df_lifeexp_XY), lab = T, lab_size = 2, title="Correlation matrix")

## Remove outlier observations
transform_outlier_data <- function(df) {
  for (i in 1:ncol(df)) {
    whiskers = boxplot.stats(df[,i])$stats[c(1, 5)] # lower and upper whisker values
    
    # if an outlier is below the lower whisker, set its value to the lower whisker
    df[ df[, i] < whiskers[1], i] <- whiskers[1]
    
    # if an outlier is above the upper whisker, set its value to the upper whisker
    df[ df[, i] > whiskers[2], i] <- whiskers[2]
  }
  
  return(df)
}

df_lifeexp <- transform_outlier_data(df_lifeexp)

## Split the data into training and test sets with a 0.8 ratio
set.seed(67854)
trainIndex = createDataPartition(df_lifeexp$Life_expectancy, p = 0.8)$Resample1
train = df_lifeexp[trainIndex, ] # training set
test = df_lifeexp[-trainIndex, ] # test set

# There are 106 observations in the training set
print("N ROWS:")
print(nrow(train))

###############################################################################################
# VARIABLE SELECTION

# Prepare the data (training data) for variable selection using LASSO regression
x <- as.matrix(train[, -ncol(train)])  
y <- train$Life_expectancy 

# LASSO Regression (variable selection)
lasso_cv <- cv.glmnet(x, y, alpha = 1, standardize = TRUE, nfolds = 10)  # Cross-validation for LASSO model
print("LAMBDA MIN: ")
print(lasso_cv$lambda.min)
plot(lasso_cv) # Plot error as a function of lambda
lambda_lasso <- lasso_cv$lambda.min  # Choose optimal lambda that minimizes error

lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_lasso, standardize = TRUE)  # Train LASSO model with optimal lambda
print("Coefficients of variables in LASSO regression:")
print(coef(lasso_model))

# selected_features <- which(coef(lasso_model) != 0)  # Choose significant variables in LASSO model

# take variables with numbers 2, 5, 7, 11, 13, 18, 19 (based on the LASSO output)
selected_features = c(2, 5, 7, 11, 13, 18, 19)
print("Selected variables in LASSO:")
print(selected_features)

#############################################################################################################
# Ordinary linear regression on the training set after removing variables

# update the training and test sets by selecting variables chosen by LASSO regression
train = train[, c(selected_features, 20)] # variable number 20 is our Y
df_lifeexp_XY = df_lifeexp_XY[, c(selected_features, 20)]
print(head(train, 2))

ggcorrplot(cor(train[,-8]), lab = T, lab_size = 2,tl.cex = 9,
           title="Correlation matrix of variables selected in Lasso")

# linear model for selected variables (created based on the training set)
# create a standard linear model for variables selected by LASSO regression
print("LINEAR MODEL FOR SELECTED VARIABLES")
linearModel = lm(formula = Life_expectancy ~., data = train[,-7])
summary(linearModel)

# FITTING THE REGRESSION WITH SELECTED VARIABLES TO TRAINING DATA
# recreate x_train and y_train so it's clear
x_train = train[, -ncol(train)]
y_train = train$Life_expectancy

# Fit to training data
y_hat <- predict(linearModel, x_train)  
plot(y_hat, y_train, 
     main = "Linear regression for selected variables (fitting to training data)")  
abline(lm(y_train ~ y_hat), col = "red")  

### PREDICTION FOR TEST DATA
### Prediction for test data based on the linear model built
### for the selected variables

# test data (also select only variables chosen by LASSO regression)
test = test[, c(selected_features, 20)] # variable number 20 is our Y 

# split into test input and output data
x_test = test[, -ncol(test)] # test input variables
y_test = test$Life_expectancy # test output values

y_pred = predict(linearModel, x_test) # prediction for test input data

### Calculate mean squared error (MSE) and mean absolute error (MAE) for predictions on
### test data
MSE_LASSO = mean((y_test - y_pred)^2)
MAE_LASSO = mean(abs(y_test - y_pred))
print(paste0("MSE LASSO: ", MSE_LASSO))
print(paste0("MAE LASSO: ", MAE_LASSO))

### TESTING ASSUMPTIONS
re=resid(linearModel)

##### assumptions
### no multicollinearity
library(car)
vif_values <- vif(linearModel)
vif_values # < 10, meaning no multicollinearity that distorts results. 
# This means the explanatory variables in the model are not strongly correlated with each other.

### expected values of residuals are zero
mean(re)

### residuals are random
plot(re);abline(h=0,lty=2)
library(tseries)
test_serii <- runs.test(factor(sign(re)))
test_serii

### residual component follows a normal distribution
qqnorm(re);qqline(re)
hist(re,prob=TRUE)
curve(dnorm(x,mean(re),sd(re)),xlim=c(-10,10),col=2,add=TRUE)
shapiro.test(re)

### residual variance is constant (homoscedasticity)
library(car)
bp_test <- ncvTest(linearModel)
bp_test
