#import dataset
df <- read.csv("train.csv")

# Preprocessing
for (x in (1 : nrow(df))){
  df[x , ncol(df)] <- df[x, ncol(df)] + 1
}


# checking for null values : 

price_range <- df[,ncol(df)]
df <- df[,-ncol(df)]

# model
km<-kmeans(df,4)

# prediction
df$price_range<-as.factor(km$cluster)
confusionMatrix(table(price_range,df$price_range))


# visualization
library(factoextra)


fviz_cluster(km, data = (read.csv("train.csv")),
             palette=c("red","blue","green","black"),
             ellipse.type = "euclid",
             star.plot = T,
             repe1 = T,
             ggtheme=theme())