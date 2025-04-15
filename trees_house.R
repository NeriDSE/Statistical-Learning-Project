# Other Models House:

house_data <- read_csv('1553768847-housing.csv')
house_data <- house_data %>% dplyr::select(-c(longitude, latitude))

house$log_income <- log(house$median_income)
house$log_bedrooms <- log(house$total_bedrooms)
house$log_value <- log(house$median_house_value)

house <- house_data



house <- house %>% sample_n(4000)

tree_house=tree(median_house_value~. -log_value -log_income -log_bedrooms ,data=house)
summary(tree_house) # High residual mean deviance and no misclassification error rate it seems
plot(tree_house)
text(tree_house,pretty=0, cex=.8) 
tree_house
# Of course, median house cost is mostly dependent on median income, but ocean proximity also plays a small role.





set.seed(42)
train_tree=sample(1:nrow(house),2640)
tree_house_train <- tree(median_house_value~.  -log_value -log_income -log_bedrooms, house, subset = train_tree)
summary(tree_house_train)
plot(tree_house_train)
text(tree_house_train,pretty=0, cex=.7)
# We have age again, I can't believe it. On the training dataset

yhat<- predict(tree_house_train, newdata=house[-train_tree,])
house_test <- house[-train_tree, 'median_house_value']

house_test <- house_test[[1]]
plot(yhat, house_test)
abline(0,1)

mean((yhat-house$median_house_value)^2)
MSE_tree_house <- mean((yhat - house_test)^2) # MSE 
RMSE_tree_house <- sqrt(MSE_tree_house) # RMSE
RMSE_tree_house

cv_tree=cv.tree(tree_house_train)
plot(cv_tree$size,cv_tree$dev,type='b')

# Log-Tree

tree_house_log <- tree(log_value~. -median_house_value -median_income -total_bedrooms, data=house, subset = train_tree)
summary(tree_house_log)
plot(tree_house_log)
text(tree_house_log,pretty=0, cex=.8) 

yhat_log<- predict(tree_house_log, newdata=house[-train_tree,])
house_test_log <- house[-train_tree, 'log_value']

house_test_log <- house_test_log[[1]]
plot(yhat_log, house_test_log)
abline(0,1)
MSE_tree_log <- mean((yhat_log - house$log_value)^2)
c <- exp(yhat_log + MSE_tree_log/2)
rmse_log <- sqrt(mean((house$median_house_value - c)^2))
rmse_log
RMSE_tree_house <- sqrt(MSE_tree_log) # RMSE
RMSE_tree_house

cv_tree_log=cv.tree(tree_house_log)
plot(cv_tree_log$size,cv_tree_log$dev,type='b')



tree_house_train2<-ctree(median_house_value~.,house,subset=train_tree)
plot(tree_house_train2) 




# (Nested) Cross Validation and Pruning - Linear Model

prune_house=prune.tree(tree_house_log, best=6)
plot(prune_house)
text(prune_house,pretty=0, cex =.8) # wonderful

yhat2<- predict(prune_house, newdata=house[-train_tree,])
MSE_prune_house <- mean((yhat2 - house$log_value)^2) # MSE 
RMSE_prune_house <- sqrt(MSE_prune_house) # RMSE
RMSE_prune_house

# Make it pretty:

tree_house_rpart  <-rpart(log_value~.  -median_house_value -median_income -total_bedrooms ,house,subset=train_tree)
printcp(tree_house_rpart )
rpart.plot(tree_house_rpart)

prune_house_2<-prune(tree_house_rpart,cp=0.0157) # get the cp higher i'd say.
rpart.plot(prune_house_2) # pruned tree plot in nice graphics. I will say though, it's not cross validated or RMSE checked. so, let me do that.



# Random Forest

rf_house=randomForest(median_house_value~. -log_value -log_income -log_bedrooms,data=house, subset=train_tree)
#p/3 default number of variables randomly sampled as candidates at each split.
rf_house # 2 variables split
rmse_rf <- sqrt(4212821589) # A low RMSE, comparable to the lm one
rmse_rf

rf_log=randomForest(log_value~. -median_house_value -median_income -total_bedrooms ,data=house, subset=train_tree, mtry = 3)
#p/3 default number of variables randomly sampled as candidates at each split.
rf_log # 2 variables split
rmse_log <- sqrt(4143648754)
rmse_log

oob.err=double(8)
test.err=double(8)

for(mtry in 1:8){
  fit=randomForest(median_house_value~. -log_value -log_income -log_bedrooms,data=house,subset=train_tree,mtry=mtry,ntree=500)
  oob.err[mtry]=fit$mse[500]
  pred=predict(fit,house[-train_tree,])
  test.err[mtry]=with(house[-train_tree,],mean((median_house_value-pred)^2))
  cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))

# Final model using the best mtry (mtry = 4)
set.seed(42)
rf_house2 <- randomForest(median_house_value ~. -log_value -log_income -log_bedrooms, data = house, subset = train_tree, mtry = 3, ntree = 500)
rf_house2
varImp(rf_house2)

# Test the final model on the test set
final_predictions <- predict(rf_house_oob, house[-train_tree,])
final_rmse <- sqrt(mean((final_predictions - house$median_house_value[-train_tree])^2))
final_rmse

# do VarImp.

# that is not a lower RMSE than the first random forest with 2 splits. Not sure why. Who knows anything about anything. Should I do boosting too while im at it? shit, maybe.


# then with caret and roc curves and everything and then with different datasets then rf and the rest.



