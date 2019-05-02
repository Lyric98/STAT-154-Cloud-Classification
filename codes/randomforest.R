Img1 <- read.table("C:/Users/junho/Desktop/berkeley/Statistics/154/project2/project2 (1)/image1.txt", quote="\"", comment.char="")
Img2 <- read.table("C:/Users/junho/Desktop/berkeley/Statistics/154/project2/project2 (1)/image2.txt", quote="\"", comment.char="")
Img3 <- read.table("C:/Users/junho/Desktop/berkeley/Statistics/154/project2/project2 (1)/image3.txt", quote="\"", comment.char="")

#

imgName <- rep(1, nrow(Img1))
img1 <- cbind(Img1, imgName)
imgName <- rep(2, nrow(Img2))
img2 <- cbind(Img2, imgName)
imgName <- rep(3, nrow(Img3))
img3 <- cbind(Img3, imgName)
img <- rbind(img1, img2, img3)
img <- cbind(1: nrow(img), img)
Colnames <- c("No","y", "x", "label", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN", "imgName")
colnames(img) <- Colnames
img$x <- img$x - min(img$x) + 1 #centered
img$y <- img$y - min(img$y) + 1 #centered
x.max <- max(img$x)
y.max <- max(img$y)

#
library(ggplot2)
library(gridExtra)
library(dplyr)
library(MASS)
library(caret)
library(GGally)
library(class)
library(randomForest)

#
test.valid.rate <- 0.2
x.test <- round(seq(from=1, to=x.max-1, length.out = round(x.max * sqrt(test.valid.rate))))
y.test <- round(seq(from=1, to=y.max-1, length.out = round(y.max * sqrt(test.valid.rate))))
x.valid <- x.test + 1
y.valid <- y.test + 1
test <- img %>% filter(x %in% x.test, y %in% y.test)
valid <- img %>% filter(x %in% x.valid, y %in% y.valid)
train <- img %>% filter(!No %in% test$No, !No %in% valid$No)


train_angle 

#
trainCV <- rbind(train, valid)
trainCV <- trainCV %>% filter(label != 0)
# NDAI, SD, CORR, DF, CF, BF, AF, AN
trainCV.x <- trainCV %>% dplyr::select(NDAI, SD, CORR)
trainCV.y <- trainCV$label
trainCV.y <- as.factor(trainCV.y)
#
validCV <- valid %>% filter(label !=0)
validCV.x <- validCV %>% dplyr::select(NDAI, SD, CORR)
validCV.y <- validCV$label
validCV.y <- as.factor(validCV.y)
validCV1 <- cbind(validCV.x,label=validCV.y)
validCV1
#
testCV <- test %>% filter(label != 0)
testCV.x <- test %>% 
  filter(label != 0) %>%
  dplyr::select(NDAI, SD, CORR)
testCV.y <- test %>% filter(label != 0)
testCV.y <- testCV.y$label
testCV.y <- as.factor(testCV.y)
testCV1 <- testCV %>% filter(label!=0) %>% dplyr::select(label, NDAI, SD, CORR)
testCV1$label <- as.factor(testCV1$label)
# trainCV.y[trainCV.y == 0] = 1
# for test
train.x <- trainCV.x
train.y <- trainCV.y
valid.x <- validCV.x
valid.y <- validCV.y

test <- test %>% filter(label !=0)
test$label <- as.factor(test$label)

trainCV1 <- trainCV %>% dplyr::select(label,NDAI,SD,CORR)
trainCV1$label <- as.factor(trainCV1$label)


#
model_rf <- CVgeneric(trainCV.x, trainCV.y, classifier = rFClassifier, k=10)
1-model_rf$lossList
model_rf$fit
mean(1-model_rf$lossList) #91.758 % accuracy for CV_loss
randomForest::varImpPlot(model_rf$fit, sort=T, n.var=3)

rf_fr <- randomForest(label~., data=trainCV1, ntree=300, importance = T)
plot(rf_fr, main= NULL)
randomForest::varImpPlot(rf_fr, sort=T, n.var=3)

test_pred_rf <- predict(model_rf$fit,test) #prediction on test
confusionMatrix(data=test_pred_rf, reference = test$label,positive="1")
cm_rf <- confusionMatrix(data=test_pred_rf, reference = test$label,positive="1")
cm_rf$byClass
1-err_rate(test_pred_rf,test$label) #test accuracy

library(ROCR)
library(ROSE)
a1 <- predict(model_rf$fit,test,type="prob")[,2]
pred <- ROCR::prediction(a1,test$label)
rocs <- performance(pred,"tpr","fpr")
plot(rocs)
roc.curve(test$label,a1)

#
test_mis <- testCV[testCV$label != test_pred_rf,]
test_mis$label[test_mis$label=="-1"] <- "No-Cloud"

ggplot(test_mis) + geom_point(aes(x=x,y=y,color=factor(label))) + scale_color_hue(name="label",direction = -1,h.start=150) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis) + geom_point(aes(x=x,y=y)) + scale_color_hue()+theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis) + geom_point(aes(x=x,y=y,color=NDAI)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis) + geom_point(aes(x=x,y=y,color=CORR)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis) + geom_point(aes(x=x,y=y,color=SD)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()


library(reshape)

img_default <- img[img$label!=0,]
ggplot(data = img_default) +
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.4)

ggplot(data = test_mis) +
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.4)





######
train_angle.x <- trainCV[,c(5:12)]
train_angle.y <- trainCV$label
train_angle.y <- as.factor(train_angle.y)
test_angle <- testCV[,c(4:12)]
test_angle$label <- as.factor(test_angle$label)
train_angle_all <- trainCV[,c(4:12)]
train_angle_all$label <- as.factor(train_angle_all$label)


model_rf_angle <- CVgeneric(train_angle.x, train_angle.y, classifier = rFClassifier, k=10)
1-model_rf_angle$lossList
model_rf_angle$fit
mean(1-model_rf_angle$lossList) #91.758 % accuracy for CV_loss
randomForest::varImpPlot(model_rf_angle$fit, sort=T, n.var=8)

rf_angle_fr <- randomForest(label~., data=train_angle_all, ntree=300, importance = T)
plot(rf_angle_fr, main= NULL)
randomForest::varImpPlot(rf_angle_fr, sort=T, n.var=8)

test_pred_angle_rf <- predict(model_rf_angle$fit,test_angle) #prediction on test
confusionMatrix(data=test_pred_angle_rf, reference = test_angle$label,positive="1")
1-err_rate(test_pred_angle_rf,test_angle$label) #test accuracy

library(ROCR)
library(ROSE)
a11 <- predict(model_rf_angle$fit,test_angle,type="prob")[,2]
pred_angle <- ROCR::prediction(a11,test_angle$label)
rocs_angle <- performance(pred_angle,"tpr","fpr")
plot(rocs_angle)
roc.curve(test_angle$label,a11)

#
test_mis_angle <- testCV[testCV$label != test_pred_angle_rf,]

ggplot(test_mis_angle) + geom_point(aes(x=x,y=y,color=factor(label))) + scale_color_hue(name="label",direction = -1,h.start=150) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_angle) + geom_point(aes(x=x,y=y)) + scale_color_hue()+theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_angle) + geom_point(aes(x=x,y=y,color=NDAI)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_angle) + geom_point(aes(x=x,y=y,color=CORR)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_angle) + geom_point(aes(x=x,y=y,color=SD)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()


library(reshape)

img_default <- img[img$label!=0,]
ggplot(data = img_default) +
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.4)

ggplot(data = test_mis_angle) +
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.4)


# prediction based on rf - 3 feature

img_l <- img[img$label !=0,]
img_l$label <- as.factor(img_l$label)


img_ul <- img[img$label ==0,] 
img_ul$label <- predict(model_rf$fit,img_ul)
img_ul$label 

img_new <- rbind(img_l,img_ul)
dim(img_new)
img_new$label

ggplot(img_new) + 
  geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_discrete(name = "Expert label", labels = c("No cloud", "Cloud")) + 
  theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()

#prediction based on rf - 9 feature

img_ull <- img[img$label==0,]
img_ull$label <- predict(model_rf_angle$fit,img_ull)
img_new1 <- rbind(img_l, img_ull)

ggplot(img_new1) + 
  geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_discrete(name = "Expert label", labels = c("No cloud", "Cloud")) + 
  theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()



#################### 

set.seed(123)
ind <- sample(seq_len(nrow(img)),size=floor(0.80*nrow(img)))
ran_train <- img[ind,-c(1:3)]
ran_trainCV <- ran_train[,c(1:4)]
ran_test <- img[-ind,-c(1:3)]
ran_testCV <- ran_test[,c(1:4)]

ind2 <- sample(seq_len(nrow(ran_train)),size=floor(0.75*nrow(ran_train)))
ran_trainCV1 <- ran_trainCV[ind2,]
ran_valid1 <- ran_trainCV[-ind2,]

r_train <- ran_trainCV1
r_test <- ran_testCV
r_valid <- ran_valid1

r_trainCV <- rbind(r_train,r_valid)
r_trainCV <- r_trainCV %>% filter(label !=0)
r_trainCV$label <- as.factor(r_trainCV$label)
r_trainCV.x <- r_trainCV %>% dplyr::select(NDAI, SD, CORR)
r_trainCV.y <- r_trainCV$label
r_trainCV.y

r_testCV <- r_test %>% filter(label !=0)
r_testCV$label <- as.factor(r_testCV$label)
r_testCV.x <- r_testCV %>% dplyr::select(NDAI, SD, CORR)
r_testCV.y <- r_testCV$label
r_testCV.y

#
r_model_rf <- CVgeneric(r_trainCV.x, r_trainCV.y, classifier = rFClassifier, k=10)
1-r_model_rf$lossList
r_model_rf$fit
mean(1-r_model_rf$lossList) #91.758 % accuracy for CV_loss


rf_fr_r <- randomForest(label~., data=r_trainCV, ntree=300, importance = T)
plot(rf_fr_r, main= NULL)
randomForest::varImpPlot(rf_fr_r, sort=T, n.var=3)

test_pred_rf_r <- predict(r_model_rf$fit,r_testCV) #prediction on test
confusionMatrix(data=test_pred_rf_r, reference = r_testCV$label,positive="1")
 



1-err_rate(test_pred_rf_r,r_testCV$label) #test accuracy

library(ROCR)
library(ROSE)
a3 <- predict(r_model_rf$fit,r_testCV,type="prob")[,2]
pred_r <- ROCR::prediction(a3,r_testCV$label)
rocs_r <- performance(pred_r,"tpr","fpr")
plot(rocs_r)
roc.curve(r_testCV$label,a3)

#
img11 <- img[-ind,]

ran_test1 <- img11 %>% filter(label !=0)
test_mis_r <- ran_test1[ran_test1$label != test_pred_rf_r,]

ggplot(test_mis_r) + geom_point(aes(x=x,y=y,color=factor(label))) + scale_color_hue(name="label",direction = -1,h.start=150) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_r) + geom_point(aes(x=x,y=y)) + scale_color_hue()+theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_r) + geom_point(aes(x=x,y=y,color=NDAI)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_r) + geom_point(aes(x=x,y=y,color=CORR)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()
ggplot(test_mis_r) + geom_point(aes(x=x,y=y,color=SD)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~imgName) + coord_fixed()


library(reshape)

img_default <- img[img$label!=0,]
ggplot(data = img_default) +
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.4)

ggplot(data = test_mis_r) +
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.4)




#####################
#
tr_ctrl <- trainControl(method="repeatedcv", number=10 , repeats=10)
dt_fit <- train(label ~ ., data = training, method = "rpart",
                parms = list(split = "information"),
                trControl=tr_ctrl,
                tuneLength = 10)

test_pr_dt <- predict(dt_fit, newdata = test)
test_pr_dt

err_rate(test,test_pr_dt)

prp(dt_fit$finalModel, box.palette = "Blue", extra = "auto")
confusionMatrix(data=test_pr_dt,reference = test$label,positive="1")




