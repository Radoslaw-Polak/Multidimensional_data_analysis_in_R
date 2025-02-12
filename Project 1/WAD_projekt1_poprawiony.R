library(readxl)
library(cluster)
library(psych)
library(ggcorrplot)
library(factoextra)
library(clusterCrit)

### wczytanie danych
d_2008 <- read_xlsx("E:/SGGW-przedmioty/Wielowymiarowa analiza danych/Projekt 1/WAD_dane1.xlsx",
                    sheet = "daneR", col_names = T, range = "A3:P19")
d_2022 <- read_xlsx("E:/SGGW-przedmioty/Wielowymiarowa analiza danych/Projekt 1/WAD_dane1.xlsx",
                    sheet = "daneR", col_names = T, range = "A23:P39")

### przygotowanie danych do wygodnej analizy
dane08 <- NULL
dane22 <- NULL
for (i in 1:15) {
  dane08 <- cbind(dane08, d_2008[[paste0("X", i)]])
  dane22 <- cbind(dane22, d_2022[[paste0("X", i)]])
}

cnames <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15")
rnames <- c("DOLNOŚLĄSKIE",
            "KUJAWSKO-POMORSKIE",
            "LUBELSKIE",
            "LUBUSKIE",
            "ŁÓDZKIE",
            "MAŁOPOLSKIE",
            "MAZOWIECKIE",
            "OPOLSKIE",
            "PODKARPACKIE",
            "PODLASKIE",
            "POMORSKIE",
            "ŚLĄSKIE",
            "ŚWIĘTOKRZYSKIE",
            "WARMIŃSKO-MAZURSKIE",
            "WIELKOPOLSKIE",
            "ZACHODNIOPOMORSKIE")

colnames(dane08) <- cnames; colnames(dane22) <- cnames
rownames(dane08) <- rnames; rownames(dane22) <- rnames


### dobór zmiennych
stat_opis08 <- describe(dane08, type = 2, quant = c(.25,.75))
stat_opis08[,-c(1,2,6,7)]
wsp_zm08 <- (stat_opis08$sd/stat_opis08$mean)*100
which(wsp_zm08 < 10) # quasi-stałe

ggcorrplot(cor(dane08), lab = T, lab_size = 2)


stat_opis22 <- describe(dane22, type = 2, quant = c(.25,.75))
stat_opis22[,-c(1,2,6,7)]
wsp_zm22 <- (stat_opis22$sd/stat_opis22$mean)*100
which(wsp_zm22 < 10) # quasi-stałe

ggcorrplot(cor(dane22), lab = T, lab_size = 2)

### ostateczny dobór zmiennych i zmiana ich nazw
dane08 <- dane08[,-c(2,9,10,12,13,14)]
dane22 <- dane22[,-c(2,9,10,12,13,14)]

# obecne zm  -   X1,  X3,  X4,  X5,  X6,  X7,  X8,  X11, X15
new_cnames <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9")
colnames(dane08) <- new_cnames; colnames(dane22) <- new_cnames


### wstępna analiza danych
## statystyki opisowe
stat_opis08 <- describe(dane08, type = 2, quant = c(.25,.75))
stat_opis08[,-c(1,2,6,7)]

stat_opis22 <- describe(dane22, type = 2, quant = c(.25,.75))
stat_opis22[,-c(1,2,6,7)]


## wizualizacje - wykresy
# X1 - destymulanta 
boxplot(dane08[,1], dane22[,1], names = c("2008", "2022"), main = "X1 - stopa bezrobocia rejestrowanego")

plot(dane08[,1], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X1", ylim = c(2,18), main = "X1 - stopa bezrobocia rejestrowanego")
points(dane22[,1], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X2 - stymulanta
boxplot(dane08[,2], dane22[,2], names = c("2008", "2022"), main = "X2 - przeciętny miesięczny dochód na 1 osobę")

plot(dane08[,2], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X2", ylim = c(780,2400), main = "X2 - przeciętny miesięczny dochód na 1 osobę")
points(dane22[,2], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("left", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X3 - destymulanta
boxplot(dane08[,3], dane22[,3], names = c("2008", "2022"), main = "X3 - przestępstwa stwierdzone na 1000 mieszkańców")

plot(dane08[,3], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X3", ylim = c(12,30), main = "X3 - przestępstwa stwierdzone na 1000 mieszkańców")
points(dane22[,3], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("bottomright", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X4 - stymulanta
boxplot(dane08[,4], dane22[,4], names = c("2008", "2022"), main = "X4 - linie kolejowe ogółem na 100 km2")

plot(dane08[,4], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X4", ylim = c(3,18), main = "X4 - linie kolejowe ogółem na 100 km2")
points(dane22[,4], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X5 - stymulanta
boxplot(dane08[,5], dane22[,5], names = c("2008", "2022"), main = "X5 - udział pow. terenów zieleni w pow. ogółem")

plot(dane08[,5], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X5", ylim = c(0,2), main = "X5 - udział pow. terenów zieleni w pow. ogółem")
points(dane22[,5], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.25, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X6 - destymulanta
boxplot(dane08[,6], dane22[,6], names = c("2008", "2022"), main = "X6 - emisja zanieczyszczeń powietrza")

plot(dane08[,6], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X6", main = "X6 - emisja zanieczyszczeń powietrza")
points(dane22[,6], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X7 - stymulanta
boxplot(dane08[,7], dane22[,7], names = c("2008", "2022"), main = "X7 - absolwenci na 10 tys. ludności")

plot(dane08[,7], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X7", ylim = c(40,140), main = "X7 - absolwenci na 10 tys. ludności")
points(dane22[,7], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.25, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X8 - destymulanta
boxplot(dane08[,8], dane22[,8], names = c("2008", "2022"), main = "X8 - rozwody na 10 tys. ludności")

plot(dane08[,8], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X8", ylim = c(5,25), main = "X8 - rozwody na 10 tys. ludności")
points(dane22[,8], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X9 - destymulanta
boxplot(dane08[,9], dane22[,9], names = c("2008", "2022"), main = "X9 - wypadki drogowe na 100 tys. ludności")

plot(dane08[,9], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X9", ylim = c(25,190), main = "X9 - wypadki drogowe na 100 tys. ludności")
points(dane22[,9], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))


## braki danych - brak, dane były kompletne


## obserwacje odstające
boxplot.stats(dane08[,1])$out; boxplot.stats(dane22[,1])$out 
boxplot.stats(dane08[,2])$out; boxplot.stats(dane22[,2])$out # maz / nic
boxplot.stats(dane08[,3])$out; boxplot.stats(dane22[,3])$out # nic / podkarp, śląskie
boxplot.stats(dane08[,4])$out; boxplot.stats(dane22[,4])$out # śląskie / śląskie
boxplot.stats(dane08[,5])$out; boxplot.stats(dane22[,5])$out # małopol, podkarp, śląskie x2
boxplot.stats(dane08[,6])$out; boxplot.stats(dane22[,6])$out # śląskie / łódzkie
boxplot.stats(dane08[,7])$out; boxplot.stats(dane22[,7])$out 
boxplot.stats(dane08[,8])$out; boxplot.stats(dane22[,8])$out
boxplot.stats(dane08[,9])$out; boxplot.stats(dane22[,9])$out 


### przekształcenia zmiennych - unitaryzacja
normalizacja <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

dane08_norm <- apply(dane08, MARGIN = 2, FUN = normalizacja)
dane22_norm <- apply(dane22, MARGIN = 2, FUN = normalizacja)

print(dane08_norm)
print('-------------------------------------------------------')
print(dane22_norm)

fviz_nbclust(dane08_norm, kmeans, method = "wss") +
  ggtitle("Metoda łokcia")

fviz_nbclust(dane08_norm, kmeans, method = "silhouette") +
  ggtitle("Metoda współczynnika silhouete")

### analiza skupień
## metoda Warda
hc08=hclust(dist(dane08_norm),method="ward")
print(skupienia08<-cutree(hc08,k = 3))
plot(dane08_norm,col=skupienia08,main="metoda Warda 2008", pch = 16)
plot(hc08, main = "metoda Warda 2008")
rect.hclust(hc08, k = 3, border = 2:4)

hc22=hclust(dist(dane22_norm),method="ward")
print(skupienia22<-cutree(hc22, k = 3))
plot(dane22_norm,col=skupienia22,main="metoda Warda 2022", pch = 16)
plot(hc22, main = "metoda Warda 2022")
rect.hclust(hc22, k = 3, border = 2:4)

cbind(skupienia08, skupienia22)

## metoda k-średnich
set.seed(123)
cl08=kmeans(dist(dane08_norm),3, 20)
plot(dane08_norm,col=cl08$cluster, pch = 16, main="k-means-metoda k-średnich 2008")
fviz_cluster(cl08, data = dane08_norm, repel = T) + theme_minimal() + ggtitle("metoda k-średnich - 2008")

cl22=kmeans(dist(dane22_norm),3, 20)
plot(dane22_norm,col=cl22$cluster, pch = 16, main="k-means-metoda k-średnich 2022")
fviz_cluster(cl22, data = dane22_norm, repel = T) + theme_minimal() + ggtitle("metoda k-średnich - 2022")

cbind(cl08$cluster, cl22$cluster)


## porównanie między metodami, ALE UWAGA: k-średnich jest niechierarchiczna
cbind(skupienia08, cl08$cluster) # 2008
cbind(skupienia22, cl22$cluster) # 2022


## Analiza PCA dla danych z 2008
pca08 <- prcomp(dane08_norm, scale = TRUE)
summary(pca08)

fviz_pca_ind(pca08, repel = TRUE, col.ind = cl08$cluster) + 
  theme_minimal() + 
  ggtitle("PCA - 2008")

biplot(pca08,pc.biplot=T,cex=0.6, main = "PCA - 2008")

## Analiza PCA dla danych z 2022
pca22 <- prcomp(dane22_norm, scale = TRUE)
summary(pca22)

fviz_pca_ind(pca22, repel = TRUE, col.ind = cl22$cluster) + 
  theme_minimal() + 
  ggtitle("PCA - 2022")

biplot(pca22,pc.biplot=T,cex=0.6,main = "PCA - 2022")

# Wykres - wyjaśniona wariancja oznacza, jak dużą część całkowitej zmienności 
# w danych tłumaczy każda z głównych składowych

fviz_eig(pca08) + ggtitle("wyjaśniona wariancja - PCA 2008")
fviz_eig(pca22) + ggtitle("wyjaśniona wariancja - PCA 2022")

# Wykresy wyjaśnionej wariancji pokazują, ile procent zmienności jest wyjaśniane 
# przez kolejne składowe, co pomaga ocenić, ile składowych warto zachować 
# w analizie, aby uchwycić jak najwięcej istotnych informacji z danych.

### ocena jakości skupień

## 1. Obliczenie współczynnika Silhouette (szerokości sylwetki)
# Obliczenie szerokości sylwetki dla metody Warda
ward_sil08 <- silhouette(skupienia08, dist(dane08_norm))
rownames(ward_sil08) <- rnames
ward_sil22 <- silhouette(skupienia22, dist(dane22_norm))
rownames(ward_sil22) <- rnames
# Obliczenie szerokości sylwetki dla k-średnich
kmeans_sil08 <- silhouette(cl08$cluster, dist(dane08_norm))
rownames(kmeans_sil08) <- rnames
kmeans_sil22 <- silhouette(cl22$cluster, dist(dane22_norm))
rownames(kmeans_sil22) <- rnames

# Wykresy sylwetki
plot(ward_sil08, main = "Wykres współczynnika Silhouette'a dla metody Warda - 2008")
plot(ward_sil22, main = "Wykres współczynnika Silhouette'a dla metody Warda - 2022")
plot(kmeans_sil08, main = "Wykres współczynnika Silhouette'a dla metody k-means - 2008")
plot(kmeans_sil22, main = "Wykres współczynnika Silhouette'a dla metody k-means - 2022")



