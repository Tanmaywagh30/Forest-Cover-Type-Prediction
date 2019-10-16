library(gbm)
setwd("~/Forest")
train <- read.csv("train.csv")
# Prepare data for analysis
# set non-numeric classes
train$Cover_Type <- as.factor(train$Cover_Type)
# check if there is more than 1 wilderness area
wilderness.check <- rowSums(train[,12:15])
summary(wilderness.check) 
mat <- as.matrix(train[,12:15])
train$wilderness <- factor(mat%*%(1:4), labels = colnames(mat))
write.table(train,"trainwild.csv",sep=",",row.names=FALSE,quote=FALSE)
# check if there is more than 1 soil type
soil.check <- rowSums(train[,16:55])
summary(soil.check)
soil.check <- colSums(train[,16:55])
sel <- which(soil.check == 0)
mat <- as.matrix(train[,16:55])
mat <- mat[,-sel]
train$Soil_Type <- factor(mat%*%(1:38), labels = colnames(mat))
levels(train$Soil_Type) <- c(levels(train$Soil_Type),names(sel))
# remove variables
train <- train[,-c(12:55)]
write.table(train,"trainclean.csv",sep=",",row.names=FALSE,quote=FALSE)


library(gbm)
gbm1 <- gbm(Cover_Type~Elevation+Slope+Horizontal_Distance_To_Hydrology+Vertical_Distance_To_Hydrology+
              Horizontal_Distance_To_Roadways+Hillshade_9am+Hillshade_Noon+Hillshade_3pm+
              Horizontal_Distance_To_Fire_Points+wilderness+Soil_Type+Aspect,
            # formula
            data=train,                   # dataset
            distribution="multinomial",     
            n.trees=1000,                # number of trees
            shrinkage=0.05,              # shrinkage or learning rate,
            # 0.001 to 0.1 usually work
            interaction.depth=3,         
            bag.fraction = 0.5,          
            train.fraction = 0.5,       
            n.minobsinnode = 10,         # minimum total weight needed in each node
            cv.folds = 10,                # do 10-fold cross-validation
            keep.data=TRUE,              # keep a copy of the dataset with the object
            verbose=TRUE,               # print out progress
            n.cores=1)

gbm3 <- gbm(Cover_Type ~ ., data = train, distribution="multinomial",
            n.trees = 1000, shrinkage=0.05, interaction.depth=3,
            bag.fraction = 0.5, verbose = TRUE, train.fraction = 0.7,
            cv.folds = 10)
test <- read.csv("test.csv")
wilderness.check <- rowSums(test[,12:15])
summary(wilderness.check) 
mat <- as.matrix(test[,12:15])
test$wilderness <- factor(mat%*%(1:4), labels = colnames(mat))
soil.check <- rowSums(test[,16:55])
summary(soil.check)
soil.check <- colSums(test[,16:55])
sel <- which(soil.check == 0)
mat <- as.matrix(test[,16:55])  
test$Soil_Type <- factor(mat%*%(1:40), labels = colnames(mat))
test <- test[,-c(12:55)]
write.table(test,"testclean.csv",sep=",",row.names=FALSE,quote=FALSE)


best.iter <- gbm.perf(gbm3,method="OOB")
print(best.iter)
best.iter <- gbm.perf(gbm3,method="test")
print(best.iter)
best.iter <- gbm.perf(gbm3,method="cv")
print(best.iter)



pred1 <- predict(gbm3, test, best.iter,type="response")
mat <-pred1[,,1]
k <- ncol(mat)
Cover_Type <- apply(mat,1,function(x) which(x[1:k] == max(x[1:k])))
submit <- data.frame(Id=test$Id,Cover_Type)
write.table(submit,"submitgbm.csv",sep=",",row.names=FALSE,quote=FALSE)

