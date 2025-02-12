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
    as.numeric(as.character(x)) # Konwersja na wartości numeryczne jeśli to możliwe
  }
  else {
    x
  }
}

# oryginalne nazwy kolumn
cnames <- c("Wiek", "Anemia", "Kinaza_fosfokreatyninowa", "Cukrzyca", "Frakcja_wyrzutowa", "Wysokie_ciśnienie_krwi",
             "Płytki_krwi", "Keratynina_w_surowicy", "Stężenie_sodu_w_surowicy", "Płeć", "Palenie", "Czas_w_dniach", "Przypadek_śmiertelny")
# nazwy kolumn wyświetlane dla statystyk opisowych
cnames_X <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13")
df_heart <- as.data.frame(lapply(df_heart, string_to_numeric))
colnames(df_heart) <- cnames
print("df_heart:")
print(head(df_heart))

# statystyki opisowe
df_heart_X <- df_heart
colnames(df_heart_X) <- cnames_X
stat = describe(df_heart_X, type = 2, quant = c(.25, .75))
print(stat[, -c(1, 2, 6, 7)])
wsp_zm <- round( (stat$sd/stat$mean)*100, 3)
which(wsp_zm < 10) # quasi-stałe
wsp_zm <- as.data.frame( t(c(wsp_zm)) )
colnames(wsp_zm) <- cnames_X
rownames(wsp_zm) <- 'wsp_zm '
wsp_zm


# Wykresy słupkowe dla zmiennych pokazujące rozkład wartości
num_cols = ncol(df_heart) # liczba kolumn
for (i in 1:num_cols) {
  counts <- table(df_heart[, i]) # liczba wystąpień poszczególnych wartości dla danej kolumny
  if (i == 3 || i == 7) {
  # dla kolumny 3 ("Kinaza fosfokreatyninowa") i 7 ("Płytki krwi") występuje jedna bardzo odstająca wartość, która niekorzystnie przeskalowuje oś Y
  # dlatego maksymalna wartość ylim jest określona jako 100, co dobrze odpowiada całej reszcie obserwacji
    ylim_max = 100
  }
  else {
    # dla pozostałych kolumn maksymalna wartość na osi Y odpowiada maksymalnej liczbie wystąpień danej wartości przemnożonej przez 1.2
    ylim_max = 1.2 * max(counts)
  }
  barplot(counts, ylab = "Liczba obserwacji", ylim = c(0, ylim_max), 
          main = paste0("X", i, ' - ', colnames(df_heart)[i]))
}

# Boxploty dla zmiennych ciągłych
continous_value_columns = c(1, 3, 5, 7, 8, 9, 12)
for (i in continous_value_columns) {
  boxplot(df_heart[,i], main = paste0("X", i, ' - ', colnames(df_heart)[i]))
}

# macierz korelacji 
ggcorrplot(cor(df_heart), lab = T, lab_size = 2)

# pozbycie się kolumny o wsp. zmienności mnieszym od 10
df_heart <- df_heart[, -9]

print("================================================================================================================")
print("df_heart po usunięciu 9 kolumny:")
print(head(df_heart))

# obserwacje odstające
# for (i in 1:(num_cols-1)) {
#   print(paste0(i, ": ", boxplot.stats(df_heart[,i])$out))
# }
print(df_heart[df_heart[, 3] > 7000, 3])

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

# transformacja wartości odstających
df_heart <- transform_outlier_data(df_heart)

set.seed(7267166)
# podział na zbiór treningowy i testowy w proporcji 0.8
trainIndex = createDataPartition(df_heart$Przypadek_śmiertelny, p = 0.8)$Resample1

### przekształcenia zmiennych - unitaryzacja
normalizacja <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# dane ustandaryzowane i podzielone, można z nich korzystać w kolejnych metodach
df_heart_NORM <- as.data.frame(lapply(df_heart, normalizacja)) # normalizacja kolumn 
train_NORM = df_heart_NORM[trainIndex, ] # zbiór treningowy
test_NORM = df_heart_NORM[-trainIndex, ] # zbiór testowy

######################################################################################
# DO TEGO MOMENTU DANE ZOSTAŁY WCZYTANE, POKAZANE NA WYKRESACH I PODZIELONE NA ZBIÓR #
# TRENINGOWY I TESTOWY, PONIŻEJ JUŻ STOSOWANIE KOLEJNYCH METOD KLASYFIKACJI          #                              
######################################################################################


### KLASYFIKATOR NAIVE-BAYES 
# klasyfikator i dokonanie predykcji dla danych testowych
classifier_NB_NORM <- naiveBayes(Przypadek_śmiertelny ~ ., data = train_NORM)
predictions_testdata_NORM <- predict(classifier_NB_NORM, newdata = test_NORM)
prob_testdata_NB = predict(classifier_NB_NORM, newdata = test_NORM, type = "raw")[, 2]
# Macierz trafień
print("")
print("================================================================================================================")
print("")
print("NAIVE BAYES")
print("MACIERZ TRAFIEŃ I STATYSTYKI PREDYKCJI DLA DANYCH TESTOWYCH:")
cm_object_testdata_NORM = confusionMatrix(reference = as.factor(test_NORM$Przypadek_śmiertelny), 
                                     data = as.factor(predictions_testdata_NORM), positive = '0') # traktuje 0 jako wartość pozytywną (czyli osoba nie zmarła)

# tutaj dokonujemy transpozycji i ten wynik będziemy podawać do zdefiniowanej powyżej funkcji 'prediction_quality',
# ponieważ funkcja ta była definowana dla układu macierzy trafień tak jak w prezentacji, czyli w wierszach wartości
# rzeczywiste a w kolumnach wartości przewidziane
cm_testdata_NORM_transposed <- t(cm_object_testdata_NORM$table) # transpozycja macierzy trafień 
print(cm_testdata_NORM_transposed)

# funkcja do wyświetlania wskaźników jakości wg podanej macierzy trafień/pomyłek, część z tych wskaźników jest liczona
# w funkcji confusionMatrix, a tutaj to jest zrobione dla weryfikacji i do obliczenia F1-score
# jako argument podajemy transponowaną macierz trafień na podstawie obiektu otrzymanego z funkcji confusionMatrix, bo te wzory 
# na wskaźniki są dla układu macierzy trafień takiego jak w prezentacji a ta funkcja confusionMatrix zwraca macierz transponowaną 
# względem tego co w prezentacji jest
prediction_quality <- function(cm) {
  precision = cm['0', '0'] / (cm['0', '0'] + cm['1', '0'])
  NPV =  cm['1', '1'] / (cm['1', '1'] + cm['0', '1'])
  specificity = cm['1', '1'] / (cm['1', '1'] + cm['1', '0'])
  sensitivity = cm['0', '0'] / (cm['0', '0'] + cm['0', '1'])
  accuracy = (cm['0', '0'] + cm['1', '1']) / sum(cm)
  F1 = 2 * (precision * sensitivity) / (precision + sensitivity)
  
  print( sprintf("Precyzja: %s", round(precision, 3)) )
  print( sprintf("NPV: %s", round(NPV, 3)) )
  print( sprintf("Specyficzność: %s", round(specificity, 3)) )
  print( sprintf("Wrażliwość: %s", round(sensitivity, 3)) )
  print( sprintf("Dokładność: %s", round(accuracy, 3)) )
  print( sprintf("F1-score: %s", round(F1, 3)) )
}

prediction_quality(cm_testdata_NORM_transposed)


# przetestowanie dla nowo wygenerowanych danych
new_test_NORM = as.data.frame(matrix(nrow = 1000, ncol = ncol(df_heart)))
colnames(new_test_NORM) <- cnames[-9] # Pomijamy 9 kolumnę ("Stężenie sodu w surowicy")
# generowanie nowych rekordów
for (i in 1:nrow(new_test_NORM)) {
  for (j in 1:ncol(new_test_NORM)) {
    new_test_NORM[i, j] = sample(df_heart_NORM[, j], 1)
  }
}

# print(new_test_NORM)


# Predykcje i macierz trafień dla nowych danych
# print(head(train_NORM))
# print(head(new_test_NORM))
predictions_newdata_NORM = predict(classifier_NB_NORM, newdata = new_test_NORM)
prob_newdata_NB = predict(classifier_NB_NORM, newdata = new_test_NORM, type = "raw")[, 2]
print("")
print("================================================================================================================")
print("")
print("NAIVE BAYES")
print("MACIERZ TRAFIEŃ I STATYSTYKI PREDYKCJI DLA NOWYCH DANYCH:")
cm_object_newdata_NORM = confusionMatrix(reference = as.factor(new_test_NORM$Przypadek_śmiertelny),
                                    data = as.factor(predictions_newdata_NORM), positive = '0')
# print(cm_object_newdata_NORM)
cm_newdata_NORM_transposed <- t(cm_object_newdata_NORM$table)
print(cm_newdata_NORM_transposed)
prediction_quality(cm_newdata_NORM_transposed)

true_test = test_NORM$Przypadek_śmiertelny # wartości rzeczywiste dla danych testowych
predicted_bayes_test = prob_testdata_NB # wartości przewidziane Bayesem dla danych testowych 

true_new = new_test_NORM$Przypadek_śmiertelny # wartości rzeczywiste dla nowych danych
predicted_bayes_new = prob_newdata_NB # wartości przewidziane Bayesem dla nowych danych 

pred_test_NB <- prediction(as.numeric(predicted_bayes_test), true_test)
perf_test_NB <- performance(pred_test_NB, "tpr", "fpr")

pred_new_NB <- prediction(as.numeric(predicted_bayes_new), true_new)
perf_new_NB <- performance(pred_new_NB, "tpr", "fpr")

plot(perf_test_NB, main = "Krzywa ROC - Naiwny Klasyfikator Bayesa", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_NB@x.values[[1]], perf_new_NB@y.values[[1]], col = "green", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("Testowe dane", "Nowe dane", "Klasyfikator losowy"), 
       col = c("blue", "green", "red"), lwd = 2, lty = 1)

# wizualizacja dla predykcji na danych testowych
pca_test <- prcomp(test_NORM, scale = TRUE)
summary(pca_test)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Dane testowe")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predictions_testdata_NORM, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja")

# wizualizacja dla predykcji na nowych danych
pca_new <- prcomp(new_test_NORM, scale = TRUE)
summary(pca_new)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Nowe dane")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predictions_newdata_NORM, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja")

### KLASYFIKATOR DRZEWA DECYJNEGO (Decision Tree)
# Klasyfikator DT
classifier_DT = rpart(formula = Przypadek_śmiertelny ~ ., data = train_NORM, method = "class")

# wizualizacja drzewa
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
print("MACIERZ TRAFIEŃ I STATYSTYKI PREDYKCJI DLA DANYCH TESTOWYCH:")
cm_testdata_DT = table(reference = test_NORM$Przypadek_śmiertelny, predicted = predicted_DT)
print(cm_testdata_DT)
prediction_quality(cm_testdata_DT) # wskaźniki jakości klasyfikacji drzewem decyzyjnym
# print(cm_object_testdata_DT)

print("")
print("================================================================================================================")
print("")
print("DECISION TREE")
print("MACIERZ TRAFIEŃ I STATYSTYKI PREDYKCJI DLA NOWYCH DANYCH:")
predicted_newdata_DT = predict(classifier_DT, newdata = new_test_NORM, type = "class")
prob_newdata_DT = predict(classifier_DT, newdata = new_test_NORM, type = "prob")[, 2]
cm_newdata_DT = table(reference = new_test_NORM$Przypadek_śmiertelny, predicted = predicted_newdata_DT)
print(cm_newdata_DT)
prediction_quality(cm_newdata_DT) # wskaźniki jakości klasyfikacji drzewem decyzyjnym

# Krzywe ROC dla Decyzyjnego Drzewa Klasyfikacyjnego
predicted_test_DT = prob_testdata_DT # wartości przewidziane drzewem klasyfikacyjnym dla danych testowych
predicted_new_DT = prob_newdata_DT # wartości przewidziane drzewem dla nowych danych

pred_test_DT <- prediction(as.numeric(predicted_test_DT), true_test) # obiekt predykcji dla danych testowych
perf_test_DT <- performance(pred_test_DT, "tpr", "fpr") # obiekt performance dla danych testowych,
# który służy do rysowania krzywej ROC

pred_new_DT <- prediction(as.numeric(predicted_new_DT), true_new) # obiekt predykcji dla nowych danych
perf_new_DT <- performance(pred_new_DT, "tpr", "fpr") # obiekt performance dla nowych danych, do rysowania 
# krzywej ROC

plot(perf_test_DT, main = "Krzywa ROC - Drzewo Klasyfikacyjne", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_DT@x.values[[1]], perf_new_DT@y.values[[1]], col = "green", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("Testowe dane", "Nowe dane", "Klasyfikator losowy"), 
       col = c("blue", "green", "red"), lwd = 2, lty = 1)

# wizualizacja dla predykcji na danych testowych
pca_test <- prcomp(test_NORM, scale = TRUE)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Dane testowe")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predicted_DT, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja")

# wizualizacja dla predykcji na nowych danych
pca_new <- prcomp(new_test_NORM, scale = TRUE)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Nowe dane")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predicted_newdata_DT, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja")



### KLASYFIKATOR NA PODSTAWIE REGRESJI LOGISTYCZNEJ
# regresja logistyczna
classifier_Logit=glm(formula = Przypadek_śmiertelny ~ ., family=binomial, data = train_NORM)
print(summary(classifier_Logit))

predicted_Logit=predict(classifier_Logit, newdata = test_NORM, type="response")
prob_testdata_Logit = predicted_Logit
p_kryt1 = 0.5

n = length(test_NORM$Przypadek_śmiertelny)
predicted_Logit1 = 0 # zmiana p-stwa na y^=1 lub y^=0
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
print("REGRESJA LOGISTYCZNA")
print("MACIERZ TRAFIEŃ I STATYSTYKI PREDYKCJI DLA DANYCH TESTOWYCH:")
cm_testdata_Logit1 = table(reference = test_NORM$Przypadek_śmiertelny, predicted = predicted_Logit1)
print(cm_testdata_Logit1)
prediction_quality(cm_testdata_Logit1) # wskaźniki jakości klasyfikacji regresją logistyczną 


print("")
print("================================================================================================================")
print("")
print("REGRESJA LOGISTYCZNA")
print("MACIERZ TRAFIEŃ I STATYSTYKI PREDYKCJI DLA NOWYCH DANYCH:")
predicted_newdata_Logit = predict(classifier_Logit, newdata = new_test_NORM, type = "response")
prob_newdata_Logit = predicted_newdata_Logit

predicted_newdata_Logit1 = 0 # zmiana p-stwa na y^=1 lub y^=0
for(i in 1:n){
  if(predicted_newdata_Logit[i] > p_kryt1){
    predicted_newdata_Logit1[i] = 1
  }else{
    predicted_newdata_Logit1[i] = 0
  }
}

cm_newdata_Logit1 = table(reference = new_test_NORM$Przypadek_śmiertelny, predicted = predicted_newdata_Logit1)
print(cm_newdata_Logit1)
prediction_quality(cm_newdata_Logit1) # wskaźniki jakości klasyfikacji regresją logistyczną

# Krzywe ROC dla Regresji Logistycznej
predicted_test_Logit1 = prob_testdata_Logit # wartości przewidziane regresją logistyczną dla danych testowych
predicted_new_Logit1 = prob_newdata_Logit  # wartości przewidziane regresją logistyczną dla nowych danych

pred_test_Logit1 <- prediction(as.numeric(predicted_test_Logit1), true_test) # obiekt predykcji dla danych testowych
perf_test_Logit1 <- performance(pred_test_Logit1, "tpr", "fpr") # obiekt performance dla danych testowych,
# który służy do rysowania krzywej ROC

pred_new_Logit1 <- prediction(as.numeric(predicted_new_Logit1), true_new) # obiekt predykcji dla nowych danych
perf_new_Logit1 <- performance(pred_new_Logit1, "tpr", "fpr") # obiekt performance dla nowych danych, do rysowania 
# krzywej ROC

plot(perf_test_Logit1, main = "Krzywa ROC - Regresja Logistyczna", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_Logit1@x.values[[1]], perf_new_Logit1@y.values[[1]], col = "green", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("Testowe dane", "Nowe dane", "Klasyfikator Losowy"), 
       col = c("blue", "green", "red"), lwd = 2, lty = 1)


# wizualizacja dla predykcji na danych testowych
pca_test <- prcomp(test_NORM, scale = TRUE)
summary(pca_test)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Dane testowe")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predicted_Logit1, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja")

# wizualizacja dla predykcji na nowych danych
pca_new <- prcomp(new_test_NORM, scale = TRUE)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) +
  theme_minimal() +
  ggtitle("Nowe dane")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predicted_newdata_Logit1, addEllipses = T) +
  theme_minimal() +
  ggtitle("Predykcja")


#############MODEL HYBRYDOWY######################

# Funkcja klasyfikatora hybrydowego
# Do tej funkcji podajemy te 3 klasyfikatory które były zdefiniowane wcześniej, dane (testowe albo nowe),
# wagi dla poszczególnych klasyfikatorów w postaci wektora i próg prawdopodobieństwa, domyślnie 0.5
hybrid_classifier <- function(cl_NB, cl_DT, cl_LR, data, weights, prob_threshold = 0.5) {
  # Prognozowane prawdopodobieństwa dla poszczególnych klasyfikatorów
  prob_nb <- predict(cl_NB, data, type = "raw")[, 2]
  prob_dt <- predict(cl_DT, data, type = "prob")[, 2]
  prob_lr <- predict(cl_LR, data, type = "response")
  
  # Obliczenie średniej ważonej prawdopodobieństw
  ensemble_prob <- (weights[1] * prob_nb + weights[2] * prob_dt + weights[3] * prob_lr) / sum(weights)
  ensemble_prediction <- ifelse(ensemble_prob > prob_threshold, 1, 0)
  
  # Zwracanie listy wyników
  return(list(probabilities = ensemble_prob, classes = ensemble_prediction))
}

# Funkcja rysowania krzywej ROC
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

# Wagi na podstawie F1-score
calculate_model_weights <- function(f1_test, f1_new, weight_test = 0.7, weight_new = 0.3) {
  f1_combined <- (f1_test * weight_test + f1_new * weight_new)
  model_weights <- f1_combined / sum(f1_combined)
  return(model_weights)
}

# Obliczenia F1-score i wag
f1_test <- c(0.887, 0.919, 0.886)  # Bayes, Drzewo, Regresja (Testowe)
f1_new <- c(0.698, 0.690, 0.704)   # Bayes, Drzewo, Regresja (Nowe)
weights <- calculate_model_weights(f1_test, f1_new)

# Klasyfikator hybrydowy na danych testowych
predicted_testdata_HC <- hybrid_classifier(
  cl_NB = classifier_NB_NORM,
  cl_DT = classifier_DT,
  cl_LR = classifier_Logit,
  data = test_NORM,
  weights = weights
)

# Klasyfikator hybrydowy na nowych danych
predicted_newdata_HC <- hybrid_classifier(
  cl_NB = classifier_NB_NORM,
  cl_DT = classifier_DT,
  cl_LR = classifier_Logit,
  data = new_test_NORM,
  weights = weights
)

# Macierze trafień i statystyki jakości
print("================================================================================================================")
print("KLASYFIKATOR HYBRYDOWY - MACIERZ TRAFIEŃ I STATYSTYKI DLA DANYCH TESTOWYCH:")
cm_testdata_HC <- table(reference = test_NORM$Przypadek_śmiertelny, predicted = predicted_testdata_HC$classes)
print(cm_testdata_HC)
prediction_quality(cm_testdata_HC) # Funkcja `prediction_quality` musi być wcześniej zdefiniowana.

print("================================================================================================================")
print("KLASYFIKATOR HYBRYDOWY - MACIERZ TRAFIEŃ I STATYSTYKI DLA NOWYCH DANYCH:")
cm_newdata_HC <- table(reference = new_test_NORM$Przypadek_śmiertelny, predicted = predicted_newdata_HC$classes)
print(cm_newdata_HC)
prediction_quality(cm_newdata_HC)

# Rysowanie krzywych ROC
true_test <- test_NORM$Przypadek_śmiertelny
true_new <- new_test_NORM$Przypadek_śmiertelny

draw_roc_curve(predicted_testdata_HC$probabilities, true_test,
               title = "Krzywa ROC - Klasyfikator Hybrydowy",
               color = "blue")
draw_roc_curve(predicted_newdata_HC$probabilities, true_new,
               title = "Krzywa ROC - Klasyfikator Hybrydowy",
               color = "green", add = TRUE)
abline(a = 0, b = 1, col = "red", lwd = 2)  # Klasyfikator losowy
legend("bottomright", legend = c("Testowe dane", "Nowe dane", "Klasyfikator losowy"),
       col = c("blue", "green", "red"), lwd = 2, lty = 1)


# wizualizacja dla predykcji na danych testowych
pca_test <- prcomp(test_NORM, scale = TRUE)

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Dane testowe")

fviz_pca_ind(pca_test, repel = TRUE, geom = "point", habillage = predicted_testdata_HC$classes, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja - Model Hybrydowy")

# wizualizacja dla predykcji na nowych danych
pca_new <- prcomp(new_test_NORM, scale = TRUE)

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = new_test_NORM$Przypadek_śmiertelny, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Nowe dane")

fviz_pca_ind(pca_new, repel = TRUE, geom = "point", habillage = predicted_newdata_HC$classes, addEllipses = T) + 
  theme_minimal() + 
  ggtitle("Predykcja - Model Hybrydowy")


###### Krzywe ROC dla danych testowych i nowych danych - zestawienie wszystkich klasyfikatorów ######
pred_test_HC <- prediction(as.numeric(predicted_testdata_HC$probabilities), true_test)
perf_test_HC <- performance(pred_test_HC, "tpr", "fpr")

pred_new_HC <- prediction(as.numeric(predicted_newdata_HC$probabilities), true_new)
perf_new_HC <- performance(pred_new_HC, "tpr", "fpr")

plot(perf_test_NB, main = "Krzywa ROC - Dane testowe", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_test_DT@x.values[[1]], perf_test_DT@y.values[[1]], col = "green", lwd = 2)
lines(perf_test_Logit1@x.values[[1]], perf_test_Logit1@y.values[[1]], col = "red", lwd = 2)
lines(perf_test_HC@x.values[[1]], perf_test_HC@y.values[[1]], col = "orange", lwd = 2)
abline(a = 0, b = 1, col = "black", lwd = 2)
legend("bottomright", legend = c("Naiwny Klasyfikator Bayesa", "Klasyfikacyjne Drzewo Decyzyjne",
                                 "Regresja Logistyczna", "Klasyfikator Hybrydowy", "Klasyfikator Losowy"), 
       col = c("blue", "green", "red", "orange", "black"), lwd = 2, lty = 1)


plot(perf_new_NB, main = "Krzywa ROC - Nowe dane", col = "blue", 
     xlab = "False Positive (1 - Specificity)", ylab = "True Positive (Sensitivity)", lwd = 2)
lines(perf_new_DT@x.values[[1]], perf_new_DT@y.values[[1]], col = "green", lwd = 2)
lines(perf_new_Logit1@x.values[[1]], perf_new_Logit1@y.values[[1]], col = "red", lwd = 2)
lines(perf_new_HC@x.values[[1]], perf_new_HC@y.values[[1]], col = "orange", lwd = 2)
abline(a = 0, b = 1, col = "black", lwd = 2)
legend("bottomright", legend = c("Naiwny Klasyfikator Bayesa", "Klasyfikacyjne Drzewo Decyzyjne",
                                 "Regresja Logistyczna", "Klasyfikator Hybrydowy", "Klasyfikator Losowy"), 
       col = c("blue", "green", "red", "orange", "black"), lwd = 2, lty = 1)
