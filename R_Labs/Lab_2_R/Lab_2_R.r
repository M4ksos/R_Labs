
#1. Загрузить файл с данными Risk.csv;
risk <- read.csv("/home/sm3sh4r1c/bsu/3 курс срити/R_Labs/data/Risk.csv")

#2. Просмотреть структуру данных с помощью функций head() или View(). 
# Результат с описанием структуры данных включить в отчёт;
print(head(risk))
View(risk)

#3 3. Загрузить библиотеку RColorBrew и создать цветовую палитру 
# (палитра Set2 с 3 цветами);
library(RColorBrewer)
palette <- brewer.pal(3, "Set2")

#4 Преобразовать категориальные признаки в фактор;
risk$Gender <- factor(risk$Gender)
risk$State <- factor(risk$State)
risk$Risk <- factor(risk$Risk)

#5 Визуализировать данные в виде цветной матрицы рассеивания, 
#  применив созданную палитру. 
#  Полученную матрицу и ее описание включить в отчет;
png("L_2_pairs_risk.png", width = 800, height = 600)
pairs(risk[, c("State.Rate", "BMI", "Age")],
      col = palette[as.numeric(risk$Risk)],
      pch = 19,
      main = "Matrix Risk Data")
dev.off()

#6 Создать диаграмму рассеивания возраста от BMI, 
#  используя созданную палитру.
#  Диаграмму с описанием включить в отчёт;
png("L_2_bmi_age.png", width = 800, height = 600)
plot(x = risk$BMI,
     y = risk$Age,
     col = palette[as.numeric(risk$Risk)],
     pch = 19,
     xlab = "BMI",
     ylab = "Age",
     main = "diagramm")
legend("topright",
       legend = levels(risk$Risk),
       col = palette,
       pch = 19)
dev.off()

#7 Для функции set.seed() установить аргумент, равный 42.
set.seed(42)

#11 Загрузить пакет caret;
library(caret)

#8 - 9 Создать индексы для обучающей и тестовой выборок 
#  с помощью функции createDataPartition(), 
#  с параметрами p=0.8 и list=FALSE;
#
#  Используя созданные индексы создать обучающую и тестовую выборки;
indexes <- createDataPartition(risk$Risk, p = 0.8, list = FALSE)
train <- risk[indexes, ]
test <- risk[-indexes, ]

#10. Определить количество строк в обучающей 
#   и тестовой выборках с помощью функции nrow();
nrow(train)
nrow(test)

#12 - 14. Загрузить пакет caret;
knnModel <- knn3(Risk ~ Age + BMI + Gender + State.Rate, data = train, k = 3)
knnPred <- predict(knnModel, test, type = "class")
knnMatrix <- confusionMatrix(knnPred, test$Risk)
print(knnMatrix)

#15 - 18.
library(tree)

treeModel <- tree(Risk ~ Age + BMI + Gender + State.Rate, data = train)

png("L_2_tree_plot.png", width = 800, height = 600)
plot(treeModel)
text(treeModel, pretty = 0)
dev.off()

treePred <- predict(treeModel, test, type = "class")
treeMatrix <- confusionMatrix(treePred, test$Risk)
print(treeMatrix)

#22 - 24
library(nnet)

neuralModel <- nnet(Risk ~ Age + BMI + Gender + State.Rate,
                    data = train,
                    size = 10,
                    decay = 0.00001,
                    maxit = 500)

print(neuralModel)

#25 - 29
library(NeuralNetTools)

png("L_2_neuralnet_plot.png", width = 800, height = 600)
plotnet(neuralModel)
dev.off()

neuralPred <- predict(neuralModel, test, type = "class")
neuralMatrix <- confusionMatrix(factor(neuralPred), test$Risk)
print(neuralMatrix)

#30
cat("Accuracy kNN:", knnMatrix$overall['Accuracy'], "\n")
cat("Accuracy Tree:", treeMatrix$overall['Accuracy'], "\n")
cat("Accuracy Neural:", neuralMatrix$overall['Accuracy'], "\n")