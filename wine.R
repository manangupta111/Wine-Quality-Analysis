checkInstallLoad <- function(libName)
  { 
  if(!require(libName, character.only=TRUE))
  { 
    install.packages(libName) 
    require(libName, character.only=TRUE)
  }
  }
checkInstallLoad("RWeka") 
checkInstallLoad("partykit") 
checkInstallLoad ("rpart") 
checkInstallLoad ("plotly") 
checkInstallLoad ("rpart.plot") 
checkInstallLoad ("RColorBrewer") 
checkInstallLoad ("rattle") 
checkInstallLoad("data.table") 
checkInstallLoad("Matrix") 
checkInstallLoad("YaleToolkit") 
checkInstallLoad("Metrics") 
checkInstallLoad("plyr") 
checkInstallLoad("dplyr") 
checkInstallLoad("scales")
checkInstallLoad("lubridate")
checkInstallLoad("ggplot2")
checkInstallLoad("pander") 
checkInstallLoad("memisc")
checkInstallLoad("caret") 
checkInstallLoad("caretEnsemble") 
checkInstallLoad("corrplot") 
checkInstallLoad("randomForest")
checkInstallLoad("C50")
checkInstallLoad("adabag")
checkInstallLoad("plotly")
checkInstallLoad("GGally")
checkInstallLoad("Simpsons")
checkInstallLoad ('gridExtra')
checkInstallLoad("knitr")
checkInstallLoad("Rmisc")
checkInstallLoad("e1071")
checkInstallLoad("class")
checkInstallLoad("broom")
checkInstallLoad("rJava")
checkInstallLoad("kernlab")
checkInstallLoad("kknn")

print("============== Completed loading libraries ===================")

##reading datasets
redwine <- read.csv(file = "D:\\Project\\Wine Quality Analysis\\winequality-red.csv") 
whitewine <- read.csv(file = "D:\\Project\\Wine Quality Analysis\\winequality-white.csv") 
knitr::kable(head(redwine)) 
knitr::kable(head(whitewine))
(str(redwine))
(str(whitewine))

#visualising alcohol content in red wine
wine2 <- redwine 
wine2$qualitychar <- ifelse(wine2$quality == 3, "a_Three", ifelse(wine2$quality == 4, "b_Four", ifelse(wine2$quality == 5, "c_Five", ifelse(wine2$quality == 6, "d_Six", ifelse(wine2$quality == 7, "e_Seven", ifelse(wine2$quality == 8, "f_Eight", "g_Nine"))) )))
plot_ly(data = wine2, x = ~qualitychar, y = ~alcohol, color = ~qualitychar, type = "box", colors = "Dark2")
#visualising alcohol content in white wine
wine2 <- whitewine
wine2$qualitychar <- ifelse(wine2$quality == 3, "a_Three", ifelse(wine2$quality == 4, "b_Four", ifelse(wine2$quality == 5, "c_Five", ifelse(wine2$quality == 6, "d_Six", ifelse(wine2$quality == 7, "e_Seven", ifelse(wine2$quality == 8, "f_Eight", "g_Nine"))) ))) 
plot_ly(data = wine2, x = ~qualitychar, y = ~alcohol, color = ~qualitychar, type = "box")

#visualising density of red wine
wine2 <- redwine 
wine2$qualitychar <- ifelse(wine2$quality == 3, "a_Three", ifelse(wine2$quality == 4, "b_Four", ifelse(wine2$quality == 5, "c_Five", ifelse(wine2$quality == 6, "d_Six", ifelse(wine2$quality == 7, "e_Seven", ifelse(wine2$quality == 8, "f_Eight", "g_Nine"))) ))) 
plot_ly(data = wine2, x = ~qualitychar, y = ~density, color = ~qualitychar, type = "box", colors = "Dark2")
#visualising density of white wine
wine2 <- whitewine 
wine2$qualitychar <- ifelse(wine2$quality == 3, "a_Three", ifelse(wine2$quality == 4, "b_Four", ifelse(wine2$quality == 5, "c_Five", ifelse(wine2$quality == 6, "d_Six", ifelse(wine2$quality == 7, "e_Seven", ifelse(wine2$quality == 8, "f_Eight", "g_Nine"))) ))) 
plot_ly(data = wine2, x = ~qualitychar, y = ~density, color = ~qualitychar, type = "box")


#visualising chlorides of red wine
grid.arrange(ggplot(redwine, aes( x = 1, y = chlorides )) +
               geom_jitter(alpha = 0.1 ) + 
               geom_boxplot(alpha = 0.2, color = 'red' ), ggplot(data = redwine, aes(x = chlorides)) + 
               geom_histogram(binwidth = 0.08, color = 'black',fill = I('tomato')) + 
               scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)
#visualising chlorides of white wine
grid.arrange(ggplot(whitewine, aes( x = 1, y = chlorides)) + 
               geom_jitter(alpha = 0.1 ) + 
               geom_boxplot(alpha = 0.2, color = 'red' ), ggplot(data = whitewine, aes(x = chlorides)) + 
               geom_histogram(binwidth = 0.08, color = 'black',fill = I('white')) + 
               scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)

#visulising PH of red wine
grid.arrange(ggplot(redwine, aes( x = 1, y = pH)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ), ggplot(data = redwine, aes(x = pH)) + 
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('tomato')), ncol = 2)
#visulising PH of white wine
grid.arrange(ggplot(whitewine, aes( x = 1, y = pH)) + 
               geom_jitter(alpha = 0.1 ) + 
               geom_boxplot(alpha = 0.2, color = 'red' ), ggplot(data = whitewine, aes(x = pH)) + 
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('white')), ncol = 2)

#creating a factored variable "rating"
##for white wine
whitewine$quality <- factor(whitewine$quality, ordered = T)
whitewine$rating <- ifelse(whitewine$quality < 5, 'bad', ifelse( whitewine$quality < 7, 'average', 'good')) 
whitewine$rating <- ordered(whitewine$rating, levels = c('bad', 'average', 'good'))
##for red wine
redwine$quality <- factor(redwine$quality, ordered = T) 
redwine$rating <- ifelse(redwine$quality < 5, 'bad', ifelse( redwine$quality < 7, 'average', 'good')) 
redwine$rating <- ordered(redwine$rating, levels = c('bad', 'average', 'good'))

#visualising quality of both the wines
p1<-ggplot(data = whitewine, aes(x = quality)) + 
  geom_bar(width = 1, color = 'black',fill = I('white')) 
p1<-p1+ggtitle("White wine quality")

p2<-ggplot(data = redwine, aes(x = quality)) + 
  geom_bar(width = 1, color = 'black',fill = I('tomato')) 
p2<-p2+ggtitle("Red wine quality")
multiplot(p1, p2, cols=2)

#visualising ratings of both the wines
p1<-ggplot(data = whitewine, aes(x = rating)) + 
  geom_bar(width = 1, color = 'black',fill = I('white')) 
p1<-p1+ggtitle("White wine") 

p2<-ggplot(data = redwine, aes(x = rating)) + 
  geom_bar(width = 1, color = 'black',fill = I('tomato')) 
p2<-p2+ggtitle("Red wine") 
multiplot(p1, p2, cols=2)

#splitting the datasets
sz <- round(.75 * dim(redwine)[1]) 
red_training_set <- redwine[1:sz,] 
red_testing_set <- redwine[-(1:sz),] 

sz <- round(.75 * dim(whitewine)[1])
white_training_set <- whitewine[1:sz,] 
white_testing_set <- whitewine[-(1:sz),]

#cart decision tree using rpart inbuilt library
##for red wine
tree1 <- rpart(quality ~ ., data = red_training_set) 
tree1
tree1.pred <- predict(tree1, newdata = red_testing_set, type = 'class') 
confusionMatrix(red_testing_set$quality, tree1.pred) 
fancyRpartPlot(tree1)
##for white wine
tree2 <- rpart(quality ~ ., data = white_training_set) 
tree2
tree2.pred <- predict(tree2, newdata = white_testing_set, type = 'class') 
confusionMatrix(white_testing_set$quality, tree2.pred)
fancyRpartPlot(tree2)

#J48 Decision tree
##for white wine
j48tree1<-J48(quality~.,data=white_training_set,control=Weka_control(),options=NULL) 
j48tree1 
tree.predict481<- predict(j48tree1, white_testing_set, type = "class") 
confusionMatrix(white_testing_set$quality, tree.predict481)
##for red wine
j48tree2<-J48(quality~.,data=red_training_set,control=Weka_control(),options=NULL) 
j48tree2 
tree.predict482<- predict(j48tree2, red_testing_set, type = "class") 
confusionMatrix(red_testing_set$quality, tree.predict482)

##Random Forest
##redwine
rf.model2<- randomForest(quality~., data = red_training_set,
                         importance=TRUE,keep.forest=TRUE) 
rf.model2 
rf.predict2 <- predict(rf.model2, red_testing_set) 
confusionMatrix(red_testing_set$quality, rf.predict2)
##white wine
rf.model1<- randomForest(quality~., data = white_training_set,
                         importance=TRUE,keep.forest=TRUE) 
rf.model1 
rf.predict1 <- predict(rf.model1, white_testing_set) 
confusionMatrix(white_testing_set$quality, rf.predict1)


#K-nearest neighbours
t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5) 
kknn.grid <- expand.grid(kmax = c(3, 5, 7 ,9, 11), distance = c(1, 2),kernel = c("rectangular", "gaussian", "cos"))
#for red wine
kknn.train2 <- train(quality ~ ., data = red_training_set,method = "kknn", trControl = t.ctrl, tuneGrid = kknn.grid, preProcess = c("center", "scale")) 
plot(kknn.train2) 
kknn.predict2 <- predict(kknn.train2, red_testing_set) 
confusionMatrix(kknn.predict2, red_testing_set $quality)
#for white wine
kknn.train1 <- train(quality ~ ., data = white_training_set,
                     method = "kknn", trControl = t.ctrl, 
                     tuneGrid = kknn.grid, preProcess = c("center", "scale")) 
plot(kknn.train1) 
kknn.predict1 <- predict(kknn.train1, white_testing_set)
confusionMatrix(kknn.predict1, white_testing_set $quality)

#SVM
#for white wine
svm.grid <- expand.grid(C = 2^(1:3), sigma = seq(0.25, 2, length = 8)) 

svm.train1 <- train(quality ~ ., data = white_training_set,
                    method = "svmRadial", trControl = t.ctrl, 
                    tuneGrid = svm.grid, preProcess = c("center", "scale")) 
plot(svm.train1) 
svm.predict1 <- predict(svm.train1, white_testing_set) 
confusionMatrix(svm.predict1, white_testing_set $quality)

#for red wine
svm.train2 <- train(quality ~ ., data = red_training_set, method = "svmRadial", trControl = t.ctrl, tuneGrid = svm.grid, preProcess = c("center", "scale")) 
plot(svm.train2) 
rf.predict2 <- predict(rf.train2, red_testing_set) 
confusionMatrix(rf.predict2, red_testing_set $quality)

#linear regression
#for redwine
quality1<-as.numeric(red_training_set$quality)
quality2<-as.numeric(red_testing_set$quality)
linearMod<-lm(sqrt(quality1)~.,data=red_training_set)
summary(linearMod)
update(linearMod,~.-rating-quality)              
pred1<-predict(linearMod, newdata=red_testing_set)
rmse <- sqrt(sum((log(pred1) - quality2)^2)/length(quality2))
c(RMSE = rmse, R2=summary(linearMod)$r.squared)
par(mfrow=c(1,1))
table(pred1)
table(red_testing_set$quality)
#for white wine
quality3<-as.numeric(white_training_set$quality)
quality4<-as.numeric(white_testing_set$quality)
linearMod2<-lm(log(quality3)~.,data=white_training_set)
summary(linearMod2)
update(linearMod2,~.-rating-quality)              
pred2<-predict(linearMod2, newdata=white_testing_set)
rmse2 <- sqrt(sum((log(pred2) - quality3)^2)/length(quality3))
c(RMSE = rmse2, R2=summary(linearMod2)$r.squared)
par(mfrow=c(1,1))
table(pred2)
table(white_testing_set$quality)
