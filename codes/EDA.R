library(tidyverse)
library(gridExtra)
library(knitr)
library(pander)
library(corrplot)
library(e1071)
library(caret)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(reshape2)
library(leaps)
library(ggvis)
library(party)
library(class)
library(neuralnet)
library(ggplot2)
library(knitr)

#plug your own image 

image1 <- read.table("C:/Users/junho/Desktop/berkeley/Statistics/154/project2/project2 (1)/image1.txt", quote="\"", comment.char="")
image2 <- read.table("C:/Users/junho/Desktop/berkeley/Statistics/154/project2/project2 (1)/image2.txt", quote="\"", comment.char="")
image3 <- read.table("C:/Users/junho/Desktop/berkeley/Statistics/154/project2/project2 (1)/image3.txt", quote="\"", comment.char="")

image1 <- setNames(image1, c("y","x","label","NDAI","SD","CORR","DF","CF","BF","AF","AN"))
image2 <- setNames(image2, c("y","x","label","NDAI","SD","CORR","DF","CF","BF","AF","AN"))
image3 <- setNames(image3, c("y","x","label","NDAI","SD","CORR","DF","CF","BF","AF","AN"))

image <- rbind(image1,image2,image3)

dim(image1)
dim(image2)
dim(image3)

summary(image1)
summary(image2)
summary(image3)


####

a <- factor(image1$label) 
c1 <- sum(a==1) #cloud in image1
nl1 <- sum(a==0) #no label in image1
ncl1 <- sum(a==-1) #no cloud in image2
t1<- nrow(image1)

c1/t1 #17.76% 
nl1/t1 #38.45%
ncl1/t1 #43.77%

#therefore for image1,
#17.76% of pixel represents cloud,
#43.77% represents no-cloud
#38.45% are not labeled(i.e, whether cloud or no-cloud)




####################


b <- factor(image2$label) 
c2 <- sum(b==1) #cloud in image1
nl2 <- sum(b==0) #no label in image1
ncl2 <- sum(b==-1) #no cloud in image2
t2<- nrow(image2)

c2/t #34.11% 
nl2/t #28.63%
ncl2/t #37.25%

#therefore for image2, 
#34.11% of pixel represents cloud,
#28.63% represents no-cloud
#37.25% are not labeled(i.e, whether cloud or no-cloud)

####################


c <- factor(image3$label) 
c3 <- sum(c==1) #cloud in image1
nl3 <- sum(c==0) #no label in image1
ncl3 <- sum(c==-1) #no cloud in image2
t3<- nrow(image3)

c3/t #18.45% 
nl3/t #52.31%
ncl3/t #29.32%

#therefore for image3, 
#18.45% of pixel represents cloud,
#52.31% represents no-cloud
#29.32% are not labeled(i.e, whether cloud or no-cloud)

#########################

t <- factor(image$expert) 
tc <- sum(t==1) #cloud in image1
nl <- sum(t==0) #no label in image1
ncl <- sum(t==-1) #no cloud in image2
t4<- nrow(image)

tc/t4 #23.43% 
nl/t4 #39.78%
ncl/t4 #36.77%

#######################################

#adding an image number on a new column
image1$image <- "Image 1"
image2$image <- "Image 2"
image3$image <- "Image 3"

image <- rbind(image1,image2,image3)


######################################################

#remove all unlabels and change no-cloud from -1 to 0
image_c <- image[image$label !=0,]
image_c$label[image_c$label==-1] <- 0
image_c$label <- factor(image_c$label)


image_lb <- image[image$label !=0,]
image_lb$label[image_lb$label==1] <- "cloud"
image_lb$label[image_lb$label==-1] <- "no-cloud"
image_lb$label <- factor(image_lb$label)



#1(b)

# image with labels (figure1)
ggplot(image_lb) + geom_point(aes(x=x,y=y,color=factor(label))) + scale_color_discrete(name="label") + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~image) + coord_fixed()


#1(c)
# image with NDAI (figure2)
ggplot(image_lb) + geom_point(aes(x=x,y=y,color=NDAI)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~image) + coord_fixed()

# we can see that clouded region most likeley has positive NDAI whereas no-cloud region most likely has negative NDAI

# image with CORR (figure3)
ggplot(image_lb) + geom_point(aes(x=x,y=y,color=CORR)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~image) + coord_fixed()

# we can't find the relationship between cloud and no-cloud by looking at CORR

# image with SD (figure4)
ggplot(image_lb) + geom_point(aes(x=x,y=y,color=SD)) + scale_color_gradientn(colors=topo.colors(10)) + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(.~image) + coord_fixed()

# again, we can't find the relationship between cloud and no-cloud by looking at SD

library(corrplot)
image_num <- image_c
image_num$label <- as.numeric(image_c$label)
factor(image_num$label)

corr <- cor(image_num[,c(3:11)])
corrplot(corr,method=c("circle"),addCoef.col = "black",tl.srt=45,sig.level = 0.03, tl.cex=0.6)

image1$assignment <- 1 + (image1$x < mean(image1$x) & 
                            image1$y > mean(image1$y)) + 
  3*(image1$y < mean(image1$y) & image1$x > mean(image1$x)) + 
  2*((image1$x > mean(image1$x)) & (image1$y > mean(image1$y)))

###############################################################################################

library(reshape)
dsty <- melt(image_lb[, c(3:11)], id.vars = 'label')

ggplot(data = dsty) +
  geom_density(aes(x = value, group = factor(label), fill = factor(label)), 
               alpha = 0.4) +
  scale_fill_discrete(name = "label") + facet_wrap(variable~., scales = "free") 

#9 density plots with different features
#this shows that only NDAI gives a good classifying threshold around 1

ggplot(data=image_lb) +
  geom_density(aes(x=NDAI, group=factor(label),fill = factor(label)), alpha=0.4) + ggtitle("NDAI density plot")

#setting the threshold 1 for classification with NDAI 
#we will see how good is this threshold

ndai1 <- image_lb[image_lb$NDAI >= 0.25,] #given in paper
ndai2 <- image_lb[image_lb$NDAI <0.25, ]

dim(ndai1)
dim(ndai2)
summary(ndai1$label)
summary(ndai2$label)

#we set NDAI = 1 as our threshold, and if we split data with NDAI less than 1 and more than or equal to 1,
#there are 89107 data points with NDAI more than or equal to 1 and 74453 of them are clouds; 83% accuracy
#there are 118954 data with NDAI less than 1 and 112426 of them are no-clouds; 94.5 # accuracy
74453/89107
112426/118954


dim(image_lb)



#####################################################
# splitting data 
# (1) splitting by x-y coordinates with quantile
summary(image_lb)

train_image <- image_lb$y

quad_1 <- (image_lb$x > mean(image_lb$x) & image_lb$y > mean(image_lb$y)) #train
quad_2 <- (image_lb$x < mean(image_lb$x) & image_lb$y > mean(image_lb$y)) #train
quad_3 <- (image_lb$x < mean(image_lb$x) & image_lb$y < mean(image_lb$y)) #validation
quad_4 <- (image_lb$x < mean(image_lb$x) & image_lb$y < mean(image_lb$y)) #test

a <- image_lb[quad_1,]
b <- image_lb[quad_2,]
c <- image_lb[quad_3,]
d <- image_lb[quad_4,]

tr_image <- rbind(a,b)
val_image <- c
test_image <- d


library(reshape)
dsty1 <- melt(test_image[, c(3:11)], id.vars = 'label')

ggplot(data = dsty) +
  geom_density(aes(x = value, group = factor(label), fill = factor(label)), 
               alpha = 0.4) +
  scale_fill_discrete(name = "label") + facet_wrap(variable~., scales = "free") 



#############################

cl <- image_lb$label=="cloud"
cl_image_lb <- image_lb[cl,]

ncl <- image_lb$label=="no-cloud"
ncl_image_lb <- image_lb[ncl,]


attach(image_lb)
par(mfrow=c(1,2))
boxplot(y~label, xlab = 'label', ylab = 'y', main ="y-axis")
boxplot(x~label, xlab = 'label', ylab = 'x', main ="x-axis")
par(mfrow=c(1,3))
boxplot(NDAI~label, xlab = 'label', ylab = 'NDAI', main ="NDAI") #NDAI
boxplot(SD~label, xlab = 'label', ylab = 'SD', main="SD") #SD
boxplot(CORR~label, xlab = 'label', ylab = 'CORR', main ="CORR") #CORR
par(mfrow=c(1,5))
boxplot(DF~label, xlab = 'label', ylab = 'DF', main='DF')
boxplot(CF~label, xlab = 'label', ylab = 'CF', main='CF')
boxplot(BF~label, xlab = 'label', ylab = 'BF', main='BF')
boxplot(AF~label, xlab = 'label', ylab = 'AF', main='AF')
boxplot(AN~label, xlab = 'label', ylab = 'AN', main='AN')


require(reshape2)
ggplot(data = melt(image_lb[ ,c(1:11)]), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + theme_bw()

########################################################################################



