
library(dplyr)

df <- read.csv("/home/sm3sh4r1c/bsu/3 курс срити/R_Labs/data/Customers.csv", sep = ";")
head(df)
str(df)

df$gender <- factor(df$gender)

df_clean <- df %>% 
  mutate(gender = as.numeric(gender)) %>% 
  select(-customer_id)

str(df_clean)

tot.withinss <- vector(mode="character", length=10)
for(i in 1:10){
     CCluster <- kmeans(df_clean, center=i, nstart=20)
     tot.withinss[i] <- CCluster$tot.withinss
}

png("L_3_elbow_method.png", width = 800, height = 600)
plot(1:10, tot.withinss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Количество кластеров K",
     ylab = "Сумма квадратов внутри кластеров (WSS)",
     main = "Метод локтя")
dev.off()

set.seed(42)

CCluster <- kmeans(df_clean, centers = 3, nstart = 20)
CCluster

png("L_3_kmeans_clusters.png", width = 800, height = 600)
plot(df_clean$age, df_clean$spending_score, 
     col = CCluster$cluster, 
     pch = CCluster$cluster,
     xlab = "Age", 
     ylab = "Spending Score",
     main = "Кластеризация K-means")
dev.off()

D <- dist(df_clean)
hclusters <- hclust(D, method = "average")

png("L_3_dendrogram.png", width = 800, height = 600)
plot(
     x = hclusters, 
     main = "Иерархическая кластеризация (Дендрограмма)")
rect.hclust(hclusters, k = 3, border = 2:4)
dev.off()

cuts <- cutree(tree = hclusters, k = 3)
print(cuts)

png("L_3_hierarchical_clusters.png", width = 800, height = 600)
plot(df_clean$age, df_clean$spending_score, 
     col = cuts, 
     pch = cuts,
     xlab = "Age", 
     ylab = "Spending Score",
     main = "Иерархическая кластеризация")
dev.off()
