#=== 3 ===
library(dplyr)
#=== 1 ===
df <- read.csv("/home/sm3sh4r1c/bsu/3 курс срити/R_Labs/data/Customers.csv", sep = ";")
#=== 2 ===

print(head(df))
str(df)
#=== 4 ===
df$gender <- factor(df$gender)
#=== 5-6 ===
df_clean <- df %>% 
  mutate(gender = as.numeric(gender)) %>%    
  select(-customer_id)

print(head(df_clean))
#=== 7 ===
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
#=== 8 ===
set.seed(42)
#=== 9 ===
CCluster <- kmeans(df_clean, centers = 3, nstart = 20)
CCluster
#=== 10-12 ===
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
print(hclusters)
#=== 13 ===
png("L_3_dendrogram.png", width = 800, height = 600)
plot(
     x = hclusters, 
     main = "Иерархическая кластеризация (Дендрограмма)")
rect.hclust(hclusters, k = 3, border = 2:4)
dev.off()
#=== 14 ===
cuts <- cutree(tree = hclusters, k = 3)
print('Cuts : ')
print(cuts)
#=== 15 ===
png("L_3_hierarchical_clusters.png", width = 800, height = 600)
plot(df_clean$age, df_clean$spending_score, 
     col = cuts, 
     pch = cuts,
     xlab = "Age", 
     ylab = "Spending Score",
     main = "Иерархическая кластеризация")
dev.off()
