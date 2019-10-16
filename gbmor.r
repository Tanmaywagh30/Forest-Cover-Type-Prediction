library(gbm)
setwd("~/Forest")
train <- read.csv("train.csv")
#----------------------------------------------
#----------------------------------------------
# Prepare data for analysis
#----------------------------------------------
# set non-numeric classes
train$Cover_Type <- as.factor(train$Cover_Type)
# check if there is more than 1 wilderness area per sample (dummy variable)
wilderness.check <- rowSums(train[,12:15])
summary(wilderness.check) #ok, only one
mat <- as.matrix(train[,12:15])
train$wilderness <- factor(mat%*%(1:4), labels = colnames(mat))
write.table(train,"trainwild.csv",sep=",",row.names=FALSE,quote=FALSE)
# check if there is more than 1 soil type per sample (dummy variable)
soil.check <- rowSums(train[,16:55])
summary(soil.check) #ok, only
soil.check <- colSums(train[,16:55])
sel <- which(soil.check == 0)
mat <- as.matrix(train[,16:55])
mat <- mat[,-sel]
train$Soil_Type <- factor(mat%*%(1:38), labels = colnames(mat))
levels(train$Soil_Type) <- c(levels(train$Soil_Type),names(sel))
# remove dummy variables
train <- train[,-c(12:55)]
# transform Aspect in radians and then cosinus transformation
train$Aspect_cos <- cos(train$Aspect*pi/180) #northness
train$Aspect_sin <- sin(train$Aspect*pi/180) #eastness
train <- train[,-c(3)]
library(gbm)
gbm1 <- gbm(Cover_Type~Elevation+Slope+Horizontal_Distance_To_Hydrology+Vertical_Distance_To_Hydrology+
              Horizontal_Distance_To_Roadways+Hillshade_9am+Hillshade_Noon+Hillshade_3pm+
              Horizontal_Distance_To_Fire_Points+wilderness+Soil_Type+Aspect_cos+Aspect_sin,
            # formula
            data=train,                   # dataset
            #var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
            # +1: monotone increase,
            #  0: no monotone restrictions
            distribution="multinomial",     # see the help for other choices
            n.trees=1000,                # number of trees
            shrinkage=0.05,              # shrinkage or learning rate,
            # 0.001 to 0.1 usually work
            interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
            bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
            train.fraction = 0.5,        # fraction of data for training,
            # first train.fraction*N used for training
            n.minobsinnode = 10,         # minimum total weight needed in each node
            cv.folds = 10,                # do 10-fold cross-validation
            keep.data=TRUE,              # keep a copy of the dataset with the object
            verbose=TRUE,               # print out progress
            n.cores=1)
gbm2 <- gbm.fit(x=train[,c(2:10,12:15)],
                y=train[,11],
                distribution="multinomial",     # see the help for other choices
                n.trees=1000,                # number of trees
                shrinkage=0.05,              # shrinkage or learning rate,
                interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                nTrain = 15000,        # fraction of data for training,
                n.minobsinnode = 10,         # minimum total weight needed in each node
                keep.data=TRUE,             # keep a copy of the dataset with the object
                #cv.folds = 10,
                verbose=TRUE)
train <- train[,-c(1)]
gbm3 <- gbm(Cover_Type ~ ., data = train, distribution="multinomial",
            n.trees = 1000, shrinkage=0.05, interaction.depth=3,
            bag.fraction = 0.5, verbose = TRUE, train.fraction = 0.7,
            cv.folds = 10)
test <- read.csv("test.csv")
wilderness.check <- rowSums(test[,12:15])
summary(wilderness.check) #ok, only one
mat <- as.matrix(test[,12:15])
test$wilderness <- factor(mat%*%(1:4), labels = colnames(mat))
# check if there is more than 1 soil type per sample (dummy variable)
soil.check <- rowSums(test[,16:55])
summary(soil.check) #ok, only one
# check if any soil type is not represented
soil.check <- colSums(test[,16:55])
sel <- which(soil.check == 0)
mat <- as.matrix(test[,16:55])
#mat <- mat[,-sel]
test$Soil_Type <- factor(mat%*%(1:40), labels = colnames(mat))
#levels(test$Soil_Type) <- c(levels(test$Soil_Type),names(sel))
test <- test[,-c(12:55)]
# transform Aspect in radians and then cosinus transformation
test$Aspect_cos <- cos(test$Aspect*pi/180) #northness (create eastness as well)
test$Aspect_sin <- sin(test$Aspect*pi/180) #northness (create eastness as well)
test <- test[,-c(3)]
pred1 <- predict(gbm3, test, best.iter,type="response")
mat <-pred1[,,1]
k <- ncol(mat)
Cover_Type <- apply(mat,1,function(x) which(x[1:k] == max(x[1:k])))
pred1 <- predict(gbm3, test, best.iter,type="response")
best.iter <- gbm.perf(gbm3,method="OOB")
print(best.iter)
best.iter <- gbm.perf(gbm3,method="test")
print(best.iter)
best.iter <- gbm.perf(gbm3,method="cv")
pred1 <- predict(gbm3, test, best.iter,type="response")
mat <-pred1[,,1]
k <- ncol(mat)
Cover_Type <- apply(mat,1,function(x) which(x[1:k] == max(x[1:k])))
submit <- data.frame(Id=test$Id,Cover_Type)
write.table(submit,"submitgbm.csv",sep=",",row.names=FALSE,quote=FALSE)
