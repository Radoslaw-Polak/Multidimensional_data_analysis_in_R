library(readxl)
library(ggcorrplot)
library(psych)
library(factoextra)
library(caret)
library(glmnet)


df_lifeexp <- read_xlsx("Life Expectancy Data.xlsx", sheet = "Life Expectancy Data", 
                        col_names = T, range = "A1:V2939")
head(df_lifeexp)

## Przeniesie kolumny z wartościami wyjściowymi (długość życia) na koniec tabeli
Life_expectancy <- df_lifeexp$Life_expectancy
df_lifeexp$Life_expectancy <- NULL
df_lifeexp <- cbind(df_lifeexp, Life_expectancy)

df_lifeexp = df_lifeexp[df_lifeexp$Year == 2013, ] # pobranie rekordów dla roku 2013
print(head(df_lifeexp))

df_lifeexp$Status <- ifelse(df_lifeexp$Status == "Developed", 1, 0) # jeżeli status jest "Developed"
# to mamy wartość 1, a w przeciwnym wypadku ("Developing") 0
df_lifeexp = df_lifeexp[, c(-1, -2)] # pozbywamy się kolumy tekstowej Country i kolumny Year
df_lifeexp <- na.omit(df_lifeexp) # występowały jakieś braki w danych, więc usuwamy te obserwacje

df_lifeexp <- as.data.frame(df_lifeexp)

# zaokrąglanie wartości zmiennoprzecinkowych, żeby lepiej wyglądało na rozkładzie 
# wartości procentowe z dokładnością do 0,5 % a kolumna GDP do całości
df_lifeexp$percentage_expenditure <- floor(df_lifeexp$percentage_expenditure * 2 + 0.5) / 2
df_lifeexp$Total_expenditure <- floor(df_lifeexp$Total_expenditure * 2 + 0.5) / 2
df_lifeexp$GDP <- round(df_lifeexp$GDP)

# dane po przekształceniach
head(df_lifeexp)

cnames_XY <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11",
               "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "Y")
df_lifeexp_XY = df_lifeexp
colnames(df_lifeexp_XY) <- cnames_XY
head(df_lifeexp_XY)

stat = describe(df_lifeexp_XY, type = 2, quant = c(.25, .75))
print(stat[-1, -c(1, 2, 6, 7)])
wsp_zm <- round( (stat$sd/stat$mean)*100, 3)
which(wsp_zm < 10) # quasi-stałe
wsp_zm <- as.data.frame( t(c(wsp_zm)) )
colnames(wsp_zm) <- colnames(df_lifeexp_XY)
rownames(wsp_zm) <- 'wsp_zm '
wsp_zm


# Liczba kolumn w df_lifeexp
num_cols <- ncol(df_lifeexp)
for (i in 1:num_cols) {
  title_text = paste0(colnames(df_lifeexp_XY)[i], ' - ', colnames(df_lifeexp)[i])
  if (i %in% c(1, 6, 10, 11, 12, 13)) {
    # Rysowanie wykresu słupkowego dla zmiennych kategorycznych
    counts <- table(df_lifeexp[, i])
    ylim_max <- max(counts, na.rm = TRUE)
    barplot(counts, ylab = "Liczba obserwacji", ylim = c(0, 1.2 * ylim_max),
            main = title_text)
  }
  
  else {
    # tam gdzie jest duża zmienność lub bardzo szeroki zakres przyjmowanych wartości 
    # są rysowane histogramy
    
    # pomocniczy dataframe do ggplot
    temp_df <- data.frame(x = df_lifeexp[, i])
    # Dynamiczny binwidth
    bw <- (max(temp_df$x, na.rm = TRUE) - min(temp_df$x, na.rm = TRUE)) / 30
    
    # rysowanie histogramu
    print(ggplot(temp_df, aes(x = x)) +
            geom_histogram(binwidth = bw, fill = "grey", color = "black") +
            labs(title = title_text, x="", y = "Liczba obserwacji") +
            theme(plot.title = element_text(hjust = 0.5))
    )
  } 
  
  # boxploty
  boxplot(df_lifeexp[, i], 
          main = paste0(colnames(df_lifeexp_XY)[i], ' - ', colnames(df_lifeexp)[i])) 
}

# Macierz korelacji
ggcorrplot(cor(df_lifeexp_XY), lab = T, lab_size = 2, title="Macierz korelacji")

## usunięcie obserwacji odstających
transform_outlier_data <- function(df) {
  for (i in 1:ncol(df)) {
    whiskers = boxplot.stats(df[,i])$stats[c(1, 5)] # wartości wąsów dolnego i górnego
    
    # jeżeli wartość odstająca jest poniżej dolnego wąsa to ustawiamy jej wartość tego dolnego wąsa
    df[ df[, i] < whiskers[1], i] <- whiskers[1]
    
    # jeżeli wartość odstająca jest powyżej górnego wąsa to ustawiamy jej wartość tego górnego wąsa
    df[ df[, i] > whiskers[2], i] <- whiskers[2]
  }
  
  return(df)
}

df_lifeexp <- transform_outlier_data(df_lifeexp)

## podział na zbiór treningowy i testowy w proporcji 0.8
set.seed(67854)
trainIndex = createDataPartition(df_lifeexp$Life_expectancy, p = 0.8)$Resample1
train = df_lifeexp[trainIndex, ] # zbiór treningowy
test = df_lifeexp[-trainIndex, ] # zbiór testowy

# wychodzi 106 obserwacji dla zbioru uczącego
print("N ROWS:")
print(nrow(train))


###############################################################################################
# DOBÓR ZMIENNYCH

# Przygotowanie danych (dane treningowe) do doboru zmiennych regresją LASSO
x <- as.matrix(train[, -ncol(train)])  
y <- train$Life_expectancy 

# LASSO Regression (selekcja zmiennych)
lasso_cv <- cv.glmnet(x, y, alpha = 1, standardize = TRUE, nfolds = 10)  # Walidacja krzyżowa dla modelu LASSO
print("LAMBDA MIN: ")
print(lasso_cv$lambda.min)
plot(lasso_cv) # Wykres zależności błędu od wartości lambda
lambda_lasso <- lasso_cv$lambda.min  # Wybór optymalnej wartości lambda minimalizującej błąd

lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_lasso, standardize = TRUE)  # Trenowanie modelu LASSO z optymalnym lambda
print("Współczyniki zmiennych w regresji LASSO:")
print(coef(lasso_model))

# selected_features <- which(coef(lasso_model) != 0)  # Wybór zmiennych istotnych w modelu LASSO

# bierzemy zmienne o numerach 2, 5, 7, 11, 13, 18, 19 (na podstawie wydruku dla LASSO)
selected_features = c(2, 5, 7, 11, 13, 18, 19)
print("Wybrane zmienne w LASSO:")
print(selected_features)


#############################################################################################################
# Zwykła regresja liniowa na treningowym zbiorze po odrzuceniu zmiennych 

# aktualizujemy zbiór treningowy i testowy wybierając zmienne wybrane przy regresji LASSO
train = train[, c(selected_features, 20)] # zmienna nr 20 to nasze Y
df_lifeexp_XY = df_lifeexp_XY[, c(selected_features, 20)]
print(head(train, 2))

ggcorrplot(cor(train[,-8]), lab = T, lab_size = 2,tl.cex = 9,
           title="Macierz korelacji zm. dobranych w Lasso")


# model linowy dla dobranych zmiennych (tworzony na podstawie zbioru treningowego)
# tworze zwykły model liniowy dla zmiennych wskazanych w regresji LASSO
print("MODEL LINIOWY DLA DOBRANYCH ZMIENNYCH")
linearModel = lm(formula = Life_expectancy ~., data = train[,-7])
summary(linearModel)

# DOPASOWANIE REGRESJI Z DOBRANYMI ZMIENNYMI DO DANYCH UCZĄCYCH
# jeszcze raz tworze x_train i y_train zeby było wiadomo
x_train = train[, -ncol(train)]
y_train = train$Life_expectancy

# Dopasowanie do danych uczących
y_hat <- predict(linearModel, x_train)  
plot(y_hat, y_train, 
     main = "Regresja liniowa dla dobranych zmiennych (dopasowanie do danych uczących)")  
abline(lm(y_train ~ y_hat), col = "red")  


### PREDYKCJA DLA DANYCH TESTOWYCH
### Predykcja dla danych testowych na podstawie modelu liniowego wyznaczonego
### dla dobranych zmiennych

# dane testowe (też wybieramy tylko zmienne dobrane przy regresji LASSO)
test = test[, c(selected_features, 20)] # zmienna nr 20 oznacza nasz Y 

# podział na testowe dane wejściowe i wyjściowe
x_test = test[, -ncol(test)] # testowe zmienne wejściowe
y_test = test$Life_expectancy # testowe wartości wyjściowe

y_pred = predict(linearModel, x_test) # predykcja dla wejściowych danych testowych

### Obliczenie błędu średniokwadratowego i średniego błędu bezwzględnego dla predykcji dla
### danych testowych
MSE_LASSO = mean((y_test - y_pred)^2)
MAE_LASSO = mean(abs(y_test - y_pred))
print(paste0("MSE LASSO: ", MSE_LASSO))
print(paste0("MAE LASSO: ", MAE_LASSO))


### TESTOWANIE ZAŁOŻEŃ
re=resid(linearModel)

##### założenia
### brak współliniowości
library(car)
vif_values <- vif(linearModel)
vif_values # < 10, czyli brak współliniowości stale zakłócającej wyniki. 
# Oznacza to, że występujące w modelu zm objaśniające nie są ze sobą silnie skorelowane.


### wartości oczekiwane składników losowych są równe zero
mean(re)

### reszty modelu mają charakter losowy
plot(re);abline(h=0,lty=2)
library(tseries)
test_serii <- runs.test(factor(sign(re)))
test_serii

### składnik losowy w modelu ma rozkład normalny
qqnorm(re);qqline(re)
hist(re,prob=TRUE)
curve(dnorm(x,mean(re),sd(re)),xlim=c(-10,10),col=2,add=TRUE)
shapiro.test(re)

### wariancje skł. los. są stałe (homoskedastyczność)
library(car)
bp_test <- ncvTest(linearModel)
bp_test