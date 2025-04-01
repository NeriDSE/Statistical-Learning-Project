# Resampling methods

# I have to cross validate the full dataset first with all the available variables first instead of taking them out like that on a whim.

# Model Selection with a single validation set (5 variables as best):

set.seed(42)
dim(house)
train=sample(seq(4000),2000,replace=FALSE)
train
regfit.fwd=regsubsets(median_house_value~., data=house[train,],method="forward", nvmax=10) # you stepforward select the training data in order to have a decent model. some good variables

val.errors=rep(NA,10)
x.test=model.matrix(house$median_house_value[-train]~.,data=house[-train,])
for(i in 1:10){
  coefi=coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean((house$median_house_value[-train]-pred)^2)
}

plot(sqrt(val.errors),ylab="Root MSE",ylim=range(c(sqrt(val.errors), sqrt(regfit.fwd$rss[-1] / 2000))),pch=19,type="b")

points(sqrt(regfit.fwd$rss[-1]/2000),col="blue",pch=19,type="b")

legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19)

# root MSE of 71054.51 for the validation, 5 variables

# root MSE of 67967.53 for the training, 10 variables


predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}
# Cross Validation for forward selection (model selection)

set.seed(42)
folds=sample(rep(1:10,length=nrow(house)))
folds
table(folds)
cv.errors=matrix(NA,10,8)

for(k in 1:10){
  best.fit=
    regsubsets(median_house_value~.,
               data=house[folds!=k,], 
               method="forward")
  
  for(i in 1:8){
    pred=predict(best.fit,
                 house[folds==k,],
                 id=i)
    
    cv.errors[k,i]=mean((house$median_house_value[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")

# all of this works, the thing is with 5 variables the error  drops considerably.



# I may not try ridge. What am I even comparing the methods on here on?

