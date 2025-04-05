library(readxl)
library(cluster)
library(psych)
library(ggcorrplot)
library(factoextra)
library(clusterCrit)

### loading data
d_2008 <- read_xlsx("WAD_dane1.xlsx",
                    sheet = "daneR", col_names = T, range = "A3:P19")
d_2022 <- read_xlsx("WAD_dane1.xlsx",
                    sheet = "daneR", col_names = T, range = "A23:P39")

### preparing data for convenient analysis
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


### variable selection
stat_opis08 <- describe(dane08, type = 2, quant = c(.25,.75))
stat_opis08[,-c(1,2,6,7)]
wsp_zm08 <- (stat_opis08$sd/stat_opis08$mean)*100
which(wsp_zm08 < 10) # quasi-constant

ggcorrplot(cor(dane08), lab = T, lab_size = 2)


stat_opis22 <- describe(dane22, type = 2, quant = c(.25,.75))
stat_opis22[,-c(1,2,6,7)]
wsp_zm22 <- (stat_opis22$sd/stat_opis22$mean)*100
which(wsp_zm22 < 10) # quasi-constant

ggcorrplot(cor(dane22), lab = T, lab_size = 2)

### final variable selection and renaming
dane08 <- dane08[,-c(2,9,10,12,13,14)]
dane22 <- dane22[,-c(2,9,10,12,13,14)]

# current variables  -   X1,  X3,  X4,  X5,  X6,  X7,  X8,  X11, X15
new_cnames <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9")
colnames(dane08) <- new_cnames; colnames(dane22) <- new_cnames


### preliminary data analysis
## descriptive statistics
stat_opis08 <- describe(dane08, type = 2, quant = c(.25,.75))
stat_opis08[,-c(1,2,6,7)]

stat_opis22 <- describe(dane22, type = 2, quant = c(.25,.75))
stat_opis22[,-c(1,2,6,7)]


## visualizations - plots
# X1 - de-stimulant 
boxplot(dane08[,1], dane22[,1], names = c("2008", "2022"), main = "X1 - registered unemployment rate")

plot(dane08[,1], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X1", ylim = c(2,18), main = "X1 - registered unemployment rate")
points(dane22[,1], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("topleft", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# X2 - stimulant
boxplot(dane08[,2], dane22[,2], names = c("2008", "2022"), main = "X2 - average monthly income per person")

plot(dane08[,2], xaxt = "n", col = "blue", pch = 16, xlab = "", ylab = "X2", ylim = c(780,2400), main = "X2 - average monthly income per person")
points(dane22[,2], col = "red", pch = 17)
axis(1, at = 1:16, labels = FALSE)
text(x = 1:16, y = par("usr")[3] - 0.5, labels = rnames, srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
legend("left", legend = c("2008", "2022"), col = c("blue", "red"), pch = c(16, 17))

# Other visualizations follow same translated pattern...

## missing data - none, data was complete

## outliers
# outliers are displayed per variable for each year

### variable transformation - unitarization
normalizacja <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

dane08_norm <- apply(dane08, MARGIN = 2, FUN = normalizacja)
dane22_norm <- apply(dane22, MARGIN = 2, FUN = normalizacja)

print(dane08_norm)
print('-------------------------------------------------------')
print(dane22_norm)

fviz_nbclust(dane08_norm, kmeans, method = "wss") +
  ggtitle("Elbow method")

fviz_nbclust(dane08_norm, kmeans, method = "silhouette") +
  ggtitle("Silhouette coefficient method")

### cluster analysis
## Ward's method
hc08=hclust(dist(dane08_norm),method="ward")
print(skupienia08<-cutree(hc08,k = 3))
plot(dane08_norm,col=skupienia08,main="Ward's method 2008", pch = 16)
plot(hc08, main = "Ward's method 2008")
rect.hclust(hc08, k = 3, border = 2:4)

hc22=hclust(dist(dane22_norm),method="ward")
print(skupienia22<-cutree(hc22, k = 3))
plot(dane22_norm,col=skupienia22,main="Ward's method 2022", pch = 16)
plot(hc22, main = "Ward's method 2022")
rect.hclust(hc22, k = 3, border = 2:4)

cbind(skupienia08, skupienia22)

## k-means method
set.seed(123)
cl08=kmeans(dist(dane08_norm),3, 20)
plot(dane08_norm,col=cl08$cluster, pch = 16, main="k-means method 2008")
fviz_cluster(cl08, data = dane08_norm, repel = T) + theme_minimal() + ggtitle("k-means method - 2008")

cl22=kmeans(dist(dane22_norm),3, 20)
plot(dane22_norm,col=cl22$cluster, pch = 16, main="k-means method 2022")
fviz_cluster(cl22, data = dane22_norm, repel = T) + theme_minimal() + ggtitle("k-means method - 2022")

cbind(cl08$cluster, cl22$cluster)

## comparison between methods, BUT NOTE: k-means is non-hierarchical
cbind(skupienia08, cl08$cluster) # 2008
cbind(skupienia22, cl22$cluster) # 2022

## PCA analysis for 2008 data
pca08 <- prcomp(dane08_norm, scale = TRUE)
summary(pca08)

fviz_pca_ind(pca08, repel = TRUE, col.ind = cl08$cluster) + 
  theme_minimal() + 
  ggtitle("PCA - 2008")

biplot(pca08,pc.biplot=T,cex=0.6, main = "PCA - 2008")

## PCA analysis for 2022 data
pca22 <- prcomp(dane22_norm, scale = TRUE)
summary(pca22)

fviz_pca_ind(pca22, repel = TRUE, col.ind = cl22$cluster) + 
  theme_minimal() + 
  ggtitle("PCA - 2022")

biplot(pca22,pc.biplot=T,cex=0.6,main = "PCA - 2022")

# Plot - explained variance means how much of the total variance 
# in the data is explained by each principal component

fviz_eig(pca08) + ggtitle("explained variance - PCA 2008")
fviz_eig(pca22) + ggtitle("explained variance - PCA 2022")

# The explained variance plots show what percentage of variability is explained 
# by successive components, which helps assess how many components are worth keeping 
# in the analysis to capture as much relevant information as possible from the data.

### cluster quality assessment

## 1. Calculating the Silhouette width coefficient
# Calculating silhouette width for Ward's method
ward_sil08 <- silhouette(skupienia08, dist(dane08_norm))
rownames(ward_sil08) <- rnames
ward_sil22 <- silhouette(skupienia22, dist(dane22_norm))
rownames(ward_sil22) <- rnames
# Calculating silhouette width for k-means
kmeans_sil08 <- silhouette(cl08$cluster, dist(dane08_norm))
rownames(kmeans_sil08) <- rnames
kmeans_sil22 <- silhouette(cl22$cluster, dist(dane22_norm))
rownames(kmeans_sil22) <- rnames

# Silhouette plots
plot(ward_sil08, main = "Silhouette coefficient plot for Ward's method - 2008")
plot(ward_sil22, main = "Silhouette coefficient plot for Ward's method - 2022")
plot(kmeans_sil08, main = "Silhouette coefficient plot for k-means method - 2008")
plot(kmeans_sil22, main = "Silhouette coefficient plot for k-means method - 2022")
