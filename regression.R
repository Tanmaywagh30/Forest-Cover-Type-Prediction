setwd("~/Forest")
train <- read.csv("trainclean.csv")
test<- read.csv("testclean.csv")
fit<-lm(Cover_Type~Elevation+Slope+Horizontal_Distance_To_Hydrology+Vertical_Distance_To_Hydrology+
          Horizontal_Distance_To_Roadways+Hillshade_9am+Hillshade_Noon+Hillshade_3pm+
          Horizontal_Distance_To_Fire_Points+wilderness+Soil_Type+Aspect,data=train)
summary(fit)