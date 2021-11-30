library(rpart)                        # Popular decision tree algorithm
library(rattle)                    # Fancy tree plot
library(rpart.plot)                # Enhanced tree plots
library(RColorBrewer)                # Color selection for fancy tree plot
library(party)                    # Alternative decision tree algorithm
library(partykit)                # Convert rpart object to BinaryTree
library(caret)

df <- read.csv("train.csv")

for (x in (1:nrow(df))) {
  df[x,ncol(df)] <- toString(df[x,ncol(df)])
}


train_idx <- sample(1 : nrow(df), size = floor(0.8 * nrow(df)), replace = FALSE)
train <- df[train_idx, ]
test <- df[-train_idx, ]

#model
tree<-rpart(price_range~.,data=train)

prp(tree, cex = 1)

p<-predict(object = tree,test,type="class")

confusionMatrix(table(p,test$price_range))

# naive bayesian

library(e1071)
model<-naiveBayes(price_range~., train)
summary(model)

p<-predict(model,test)
confusionMatrix(table(p,test$price_range))
