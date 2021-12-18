library(class)
library(mass)
library(leaps)
library(glmnet) 
library(pls)
library(rda)
library(pcr)
library(lda)
library(ISLR)
library(gam)
library(pROC)
library(ROCR)


#Reading in data#
jmtrain = read.table(file.choose())
jmtest = read.table(file.choose())
length(names(jm))
dim(jm)

#definfing  x and y train
xtrain = model.matrix(nature~., data=jmtrain)[,-1]
ytrain = jmtrain$nature

xtest = model.matrix(nature~., data=jmtest)[,-1]
ytest = jmtest$nature

#multiple regression#

#QUESTION 1#

#Model in full#
lm.fit1 = lm(nature~.,data=jmtrain)
summary(lm.fit1)
anova(lm.fit1)


#Model in reduced form#
lm.fit2 = lm(nature ~ x8+x12+x14+x15+x18+x19+x44+x56+x63+x64+x66+x69+x78+x201+x202+x203+x205+x206+x207+x208+x209+x210+x211+x212+x213+x214+x218+x219+x220,data = jmtrain)
summary(lm.fit2)
anova(lm.fit1,lm.fit2)
predictions = predict(lm.fit2)


#coverting predictions to 0's and 1's#
pred = rep("1",nrow(jmtest))
pred[probabilities<0.5] = "0"
tail(pred)

#confusion matrix#
table(pred,ytest)
probabilities = predict(lm.fit2, newdata = jmtest)
pred = rep("0", nrow(jmtest))
pred[probabilities > 0.5] = "1"
table(pred, jmtest$nature)

#Question 2#

#forward stepwise selection# used leaps
fit.fwd = regsubsets(nature~.,data=jmtrain,nvmax=122,method="forward")
fwd.summaryfit = summary(fit.fwd)


plot(fwd.summaryfit$cp, type = "l",main = "Forward Cp")
points(x= which.min(fwd.summaryfit$cp),y=min(fwd.summaryfit$cp),col="cyan")
which.min(fwd.summaryfit$cp)

plot(fwd.summaryfit$cp, type = "l",main = "Forward BIC")
points(x= which.min(fwd.summaryfit$bic),y=min(fwd.summaryfit$bic),col="cyan")
which.min(fwd.summaryfit$bic)

plot(fwd.summaryfit$cp, type = "l",main = "Forward ADJR2")
points(x= which.min(fwd.summaryfit$adjr2),y=min(fwd.summaryfit$adjr2),col="cyan")
which.min(fwd.summaryfit$adjr2)

#backward stepwise selection#
fit.bwk = regsubsets(nature~.,data=jmtrain,nvmax=122,method="backward")
bwk.summaryfit=summary(fit.bwk)

plot(bwk.summaryfit$cp, type = "l",main = "Backward Cp")
points(x= which.min(bwk.summaryfit$cp),y=min(bwk.summaryfit$cp),col="purple")
which.min(bwk.summaryfit$cp)

plot(bwk.summaryfit$cp, type = "l",main = "Backward BIC")
points(x= which.min(bwk.summaryfit$bic),y=min(bwk.summaryfit$bic),col="purple")
which.min(bwk.summaryfit$bic)

plot(bwk.summaryfit$cp, type = "l",main = "BACKWARD ADJR2")
points(x= which.min(bwk.summaryfit$adjr2),y=min(bwk.summaryfit$adjr2),col="purple")
which.min(bwk.summaryfit$adjr2)

coef(fit.fwd,21)
lm.fit3 = lm(nature~x8+x44+x47+x56+x63+x69+x78+x203+x205+x206+x207+x208+x209
                  +x210+x211+x212+x213+x214+x218+x219+x220, data=jmtrain)
summary(lm.fit3)

#confusion matrix for 21 variables#
probabilities2 = predict(lm.fit3, newdata = jmtest)
pred2 = rep("0", nrow(jmtest))
pred2[probabilities2 > 0.5] = "1"
table(pred2, jmtest$nature)


coef(fit.fwd,21)
lm.fit4 = lm(nature~x8+x44+x47+x56+x63+x69+x78+x203+x205+x206+x207+x208+x209
             +x210+x211+x212+x213+x214+x218+x219+x220, data=jmtrain)
summary(lm.fit4)

#confusion matrix for 21 variables#
probabilities3 = predict(lm.fit4, newdata = jmtest)
pred3 = rep("0", nrow(jmtest))
pred3[probabilities3 > 0.5] = "1"
table(pred3, jmtest$nature)


#Question 4#

#Ridge Regression#
ridge.fit = glmnet(xtrain,ytrain, alpha=0)
plot(ridge.fit)

set.seed(8)
cv.out=cv.glmnet(xtrain, ytrain, alpha=0)
plot(cv.out)
best.lambda=cv.out$lambda.min

#Prediction#
pred.ridge= predict(ridge.fit, s=best.lambda, newx=xtest) 
mean((ridge.pred-ytest)^2)

#Ride confusion matrix#
matrix.ridge = rep("0", nrow(jmtest))
matrix.ridge[pred.ridge > 0.5] = "1"
table(pred.ridge, jmtest$nature)

#Lasso  Regression#
lasso.fit = glmnet(xtrain,ytrain, alpha=1)
plot(lasso.fit)

set.seed(8)
cv.out2=cv.glmnet(xtrain, ytrain, alpha=1)
plot(cv.out2)
best.lambda2=cv.out$lambda.min

##Prediction#
pred.lasso= predict(lasso.fit, s=best.lambda2, newx=xtest) 
mean((pred.lasso-ytest)^2)

#Lasso Drop#
drop.lasso = glmnet(xtrain, ytrain, alpha = 1)
coeff.lasso = predict(drop.lasso, type="coefficients", s=best.lambda2)
coeff.lasso

#Lasso confusion matrix#
matrix.lasso = rep("0", nrow(jmtest))
matrix.lasso[pred.lasso > 0.5] = "1"
table(pred.lasso, jmtest$nature)


#QUESTION 4#

#pls#
set.seed(8)
pls.fit=plsr(nature~., data = jmtrain, scale =TRUE, validation ="CV")
validationplot(pls.fit, val.type="MSEP")
summary(pls.fit)
fit.pls.pred=predict(pls.fit,xtest,ncom=3) 

mean((fit.pls.pred-ytest)^2)


#pcr#
set.seed(8)
pcr.fit=pcr(nature~.,data=jmtrain, scale=TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)

pred.pcr = predict(pcr.fit, xtest, ncomp=3)
mean((pred.pcr-ytest)^2)


#QUESTION 5#
#lda Model#
lda.fit = lda(nature~., data=jmtrain)
lda.fit
pred.fit.lda = predict(lda.fit, jmtest)

mean(ytest!=pred.fit.lda$class)

table(pred.fit.lda$class, ytest)

#qda Model#
set.seed(8)
qda.fit = qda(nature~., data=jmtrain)
qda.fit
pred.fit.qda = predict(qda.fit, jmtest)

mean(ytest!=pred.fit.qda$class)

table(pred.fit.qda$class, ytest)

#rda Model#
set.seed(8)
fit.rda=rda(nature~.,jmtrain)
pred.fit.rda = predict(fit.rda, jmtest)

mean(ytest!=pred.fit.rda$class)

table(pred.fit.rda$class, ytest)


#QUESTION 8#

#Logistic Model#
logistic.model=glm(nature~., data=jmtrain, family=binomial)
summary(logistic.model)

log.prob = predict(logistic.model, newdata = jmtest)
log.matrix = rep("0", nrow(jmtest))
log.matrix[log.prob > 0.5] = "1"
table(log.matrix, jmtest$nature)

yes

#QUESTION 9#

#KNN Model#
set.seed(1)
pred.knn = knn(xtrain, xtest, ytrain, k=5)
mean(ytest!=pred.knn)

table(pred.knn,ytest)

#AUC#
lin.model= glm(nature~., family = "binomial", data=jmtest)
probs = predict(lin.model,type = c("response"))
plot(roc(jmtest$nature,probs),print.auc=TRUE)
