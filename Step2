library(data.table)
library(tibble)
library(survival)
library(randomForestSRC)
library(survivalsvm)
library(gbm)
library(caret)
library(abess)
library(glmnet)
library(e1071)
library(kknn)
library(qs)
library(genalg)
library(MASS)
library(pROC)
#Read the stored risk scores of individual base learners
bas_learn <- readRDS("base_learners.rds")
bas_learn <- do.call(cbind,bas_learn)
bas_learn <- as.data.frame(bas_learn)
bas_learn[,-1] <-  apply(bas_learn[,-1], 2, as.numeric)
rownames(bas_learn) <- bas_learn$id
bas_learn <- bas_learn[,-1]
##Genetic algorithm identifies the best combination of base learners.

fitnessFunction <- function (bitstring) {
  if (sum(bitstring ) == 0){return(1)}  else {
    selectedColumns <- which(bitstring == 1)+1
    datset <-  dat[,c(1, selectedColumns)]
    fin_fit<-glm(res ~ ., data = datset, family = "binomial")
    fin_fit<-stepAIC(fin_fit, direction = "both")
    datset$riskscore <- predict( fin_fit ,datset)
    roc_value <- roc(datset$res,datset$riskscore)
    roc_value <- 1 - as.numeric(roc_value$auc)
    return(roc_value)
  }
}
monitor <- function(obj) {
  minEval = min(obj$evaluations);
  plot(obj, type="hist");
}
dat <- bas_learn
result <- rbga.bin(size=ncol(dat)-1, mutationChance=0.05, zeroToOneRatio=10,
                   evalFunc=fitnessFunction , verbose=F, monitorFunc=monitor,popSize=50, iters=20)
####Determine the optimal combination of base learners
bestMember <- result$population[which.min(result$evaluations), ]
bestMember <- which(bestMember == 1)+1
rt<- dat[,c(1,bestMember )]
fin_fit<- glm(res ~ ., data = rt, family = "binomial")
fin_fit<-stepAIC(fin_fit, direction = "both")
dat$riskscore <- predict(fin_fit,dat)
###Calculate the AUC
roc(dat$res,dat$riskscore)
 
