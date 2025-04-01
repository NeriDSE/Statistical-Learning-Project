# Other Models House
set.seed(42)
house <- house %>% sample_n(4000)

tree_house=tree(median_house_value~.,data=house)
summary(tree_house) 
# High residual mean deviance and no misclassification error rate it seems

plot(tree_house)
text(tree_house,pretty=0) # I mean awesome.
# Of course, median house cost is mostly dependent on median income, but ocean proximity and median age play their role once income is ruled out of their way.

# I can try on the larger model. This is a bit of a reduced model

tree_house
set.seed(42)

train_tree=sample(1:nrow(house),2640)

tree_house_train <- tree(median_house_value~., house, subset = train_tree)

plot(tree_house_train)
text(tree_house_train,pretty=0)

yhat<- predict(tree_house_train, newdata=house[-train_tree,])
house_test = house$median_house_value[-train_tree]

plot(yhat, house_test)
abline(0,1) # what is this showing me? a bit quadratic I feel.
# the values that the tree predicts vs the value that the train predicted

cor(house[-train_tree,]$median_house_value, tree_test) # Check correlation
MSE_tree_house <- mean((yhat - house_test)^2) # MSE 
RMSE_tree_house <- sqrt(MSE_tree_house) # RMSE
RMSE_tree_house
# RMSE is 79377 which is actually less than the linear regression model believe it or not. I haven't trained/tested that one tho. So. I may have to hold on cheering for my work on the lm

tree_house_train2<-ctree(median_house_value~.,house,subset=train_tree)
plot(tree_house_train2) 




# Cross Validation and Pruning

cv_tree_house=cv.tree(tree_house_train)
plot(cv_tree_house$size,cv_tree_house$dev,type='b') # 3 - 5 is the big jump, but the lowest is 8, indisputably

prune_house=prune.tree(tree_house_train, best=8)
plot(prune_house)
text(prune_house,pretty=0) # wonderful

yhat2<- predict(prune_house, newdata=house[-train_tree,])
MSE_prune_house <- mean((yhat2 - house_test)^2) # MSE 
RMSE_prune_house <- sqrt(MSE_prune_house) # RMSE
RMSE_prune_house
# it's higher or it's the same. no way around it seems. that's the lowest and I will accept it.

tree_house_rpart  <-rpart(median_house_value~.,house,subset=train_tree)
printcp(tree_house_rpart )
rpart.plot(tree_house_rpart)

prune_house_2<-prune(tree_house_rpart,cp=0.0157) # get the cp higher i'd say.
rpart.plot(prune_house_2) # pruned tree plot in nice graphics. I will say though, it's not cross validated or RMSE checked. so, let me do that.

# Random Forest

rf_house=randomForest(median_house_value~.,data=house,subset=train_tree)
rf_house # 2 variables split
sqrt(3872315297) # a low RMSE, comparable to the lm one

oob.err=double(8)
test.err=double(8)
for(mtry in 1:8){
  fit=randomForest(median_house_value~.,data=house,subset=train,mtry=mtry,ntree=500)
  oob.err[mtry]=fit$mse[500]
  pred=predict(fit,house[-train_tree,])
  test.err[mtry]=with(house[-train_tree,],mean((median_house_value-pred)^2))
  cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))

# Final model using the best mtry (mtry = 4)
rf_house_oob <- randomForest(median_house_value ~ ., data = house, subset = train_tree, mtry = 4, ntree = 500)

# Test the final model on the test set
final_predictions <- predict(rf_house_oob, house[-train_tree,])
final_rmse <- sqrt(mean((final_predictions - house$median_house_value[-train_tree])^2))
final_rmse 
# that is not a lower rmse than the first random forest with 2 splits. not sure why. who knows anything about anything. should I do boosting too while im at it? shit, maybe.


# then with caret and roc curves and everything and then with different datasets then rf and the rest. 


