
#Load libraries
library(h2o)
library(pROC)
library(preprocessCore)
library(ggplot2)
library(caret)
localH2O = h2o.init()


#Load the dataset
setwd("C:/Users/serim/Documents/academic/MRM")
dataset_df <- read.csv(file="CA19_9_fin_no_background.csv")
dataset_df$group <- as.factor(ifelse(dataset_df$type=="PC",1,0))
dataset_df <- dataset_df[,-(1:3)]


#Quantile normalization
normdataset=normalize.quantiles(t(as.matrix(dataset_df[,-ncol(dataset_df)])))
dataset_df[1:(ncol(dataset_df)-1)]=t(normdataset)
response <- 'group'
names(dataset_df)
predictors <- setdiff(names(dataset_df), response)
performance_training=matrix( rep( 0, len=21), nrow = 3)
performance_testing=matrix( rep( 0, len=56), nrow = 8)

performance=matrix(rep( 0, len=56), nrow = 8)
performance_testing_list=list()
performance_training_list=list()

#Random sampling
rand <- sample(nrow(dataset_df))
dataset_df=dataset_df[rand, ]

#Randomly split the training dataset
trainIndex <- createDataPartition(dataset_df$group, p = .8,list = FALSE,times = 1)
TrainSet <- dataset_df[ trainIndex,]
TestSet <- dataset_df[-trainIndex,]
TrainSet$group=as.factor(paste0("X",TrainSet$group))
TestSet$group=as.factor(paste0("X",TestSet$group))

#10-fold crossvalidation
control <- trainControl(method="cv", number=10,classProbs = TRUE,summaryFunction =twoClassSummary)

#DL algorithm
dataset.hex<-as.h2o(TrainSet, destination_frame="train.hex")
valid <- as.h2o(TestSet, destination_frame="test.hex")
dataset.hex$group <- as.factor(dataset.hex$group)
valid$group <- as.factor(valid$group)

#hyper_params <- list(
#  activation=c("Rectifier","Tanh","Maxout"),
#  hidden=list(c(20),c(40),c(60),c(20,20),c(40,40),c(60,60),c(20,20,20),c(40,40,40),c(60,60,60)),
#  input_dropout_ratio=c(0,0.05,0.1,0.2,0.3,0.4,0.5),
#  hidden_dropout_ratios=c(0.5,0.6),
#  l1=seq(0,1e-4,1e-5),
#  l2=seq(0,1e-4,1e-5),
#  train_samples_per_iteration =c(0,-1,-2),
#  epochs = c(1,5,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000),
#  momentum_start=c(0,0.5),
#  rho=c(0.9,0.95,0.99,0.995,0.999),
#  quantile_alpha=c(0,1),
#  huber_alpha=seq(0,1) )

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout"),
  input_dropout_ratio=c(0,0.05,0.1,0.2,0.3),
  epochs = c(20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800),
  epsilon=c(1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10),
  #rho=c(0.9,0.95,0.99,0.995,0.999),
  train_samples_per_iteration =c(0,-1,-2)
  #hidden=list(c(20),c(40),c(60),c(20,20),c(40,40),c(20,20,20),c(40,40,40))
  )


#m_loaded <- h2o.loadModel(path)
#search_criteria = list(strategy = "RandomDiscrete", max_models = 100, stopping_rounds=5,
#                       stopping_tolerance=1e-2)

CV_results<-list()
results_df<-data.frame(activation=c(),input_dropout_ratio=c(),epochs=c(),train_samples_per_iteration=c(),epsilon=c(),AUC=c())


for (activation in hyper_params$activation){
  for (id_ratio in hyper_params$input_dropout_ratio){
    for (epochs in hyper_params$epochs){
      for (ts_per_iter in hyper_params$train_samples_per_iteration){
        for (epsilon in hyper_params$epsilon){
            
            aml=h2o.deeplearning(
              training_frame=dataset.hex,
              validation_frame=valid,
              nfolds=10,
              x=predictors,
              y="group",
              activation=activation,
              hidden=c(20,20),
              epochs=epochs,
              train_samples_per_iteration = ts_per_iter,
              seed=7,
              rho=0.99,
              epsilon=epsilon,
              input_dropout_ratio = id_ratio,
              variable_importances=TRUE,
              export_weights_and_biases=T,
              standardize=T,
              stopping_metric="misclassification",
              stopping_tolerance=1e-2,
              stopping_rounds=2,
              score_validation_samples=10000,
              score_duty_cycle=0.025,
              max_w2=10
            ) 
            
            results_df=rbind(results_df,data.frame(activation=activation,input_dropout_ratio=id_ratio,epochs=epochs,train_samples_per_iteration=ts_per_iter,epsilon=epsilon,AUC=as.numeric(aml@model$cross_validation_metrics_summary$mean[2])))
            print(paste0("Done for activation: ",activation," id_ratio: ",id_ratio, " epochs: ",epochs," ts_per_iter: ",ts_per_iter," epsilon: ",epsilon))
        }
      }
    }
  }
}


for (epochs in c(20,30,40,50,60,70,80,90,100)){
  results_df_new<-data.frame(activation=c(),input_dropout_ratio=c(),epochs=c(),train_samples_per_iteration=c(),epsilon=c(),AUC=c(),accuracy=c(),precision=c(),recall=c())
  for (ts_per_iter in hyper_params$train_samples_per_iteration){
    for (epsilon in hyper_params$epsilon){
      
      aml=h2o.deeplearning(
        training_frame=dataset.hex,
        validation_frame=valid,
        nfolds=10,
        x=predictors,
        y="group",
        activation="Tanh",
        hidden=c(20,20),
        epochs=epochs,
        train_samples_per_iteration = ts_per_iter,
        seed=7,
        rho=0.99,
        epsilon=epsilon,
        input_dropout_ratio = 0,
        variable_importances=TRUE,
        export_weights_and_biases=T,
        standardize=T,
        stopping_metric="misclassification",
        stopping_tolerance=1e-2,
        stopping_rounds=2,
        score_validation_samples=10000,
        score_duty_cycle=0.025,
        max_w2=10
      ) 
      
      results_df_new=rbind(results_df_new,data.frame(activation="Rectifier",input_dropout_ratio=0,epochs=epochs,train_samples_per_iteration=ts_per_iter,epsilon=epsilon,AUC=as.numeric(aml@model$cross_validation_metrics_summary$mean[2]),accuracy=as.numeric(aml@model$cross_validation_metrics_summary$mean[1]),precision=as.numeric(aml@model$cross_validation_metrics_summary$mean[17]),recall=as.numeric(aml@model$cross_validation_metrics_summary$mean[19])))
      print(paste0("Done for activation: ","Rectifier"," id_ratio: ",0, " epochs: ",epochs," ts_per_iter: ",ts_per_iter," epsilon: ",epsilon))
      
    }
  }
  write.csv(results_df_new,paste0("DL_dataset/Rectifier_0_",epochs,".csv"))
}

h2o.shutdown(prompt = TRUE)

data_1<-read.csv("DL_dataset/Rectifier_0_600.csv")
data_2<-read.csv("DL_dataset/Rectifier_0_800.csv")[,1:7]
fulldata<-rbind(data_1,data_2[,1:7])
dim(fulldata)

for (id_ratio in c(0.05,0.1,0.2)){
  for (epochs in hyper_params$epochs){
    fulldata<-rbind(fulldata,read.csv(paste0("DL_dataset/Rectifier_",id_ratio,"_",epochs,".csv"))[,1:7])
  }
}

for (epochs in hyper_params$epochs){
  fulldata<-rbind(fulldata,read.csv(paste0("DL_dataset/Tanh_0_",epochs,".csv"))[,1:7])
}

fulltable<-xtable(fulldata[1248:1278,])

display(fulltable)[7] <- "f"
digits(fulltable) <- 5
fulltable

besttable<-xtable(fulldata[c(1268,379,432,1261,665),])

display(besttable)[7] <- "f"
digits(besttable) <- 5
besttable

valAUC=c()

fulldata[c(1268,379,432,1261,665),]

read.csv("DL_dataset/Rectifier_0.2_500.csv")[c(1,8),]
read.csv("DL_dataset/Rectifier_0.05_40.csv")[1,]
read.csv("DL_dataset/Rectifier_0.05_60.csv")[1,]
read.csv("DL_dataset/Rectifier_0.05_800.csv")[14,]

for (i in c(1268,379,432,1261,665)){
  p<-fulldata[i,3:6]
  
  best=h2o.deeplearning(
    training_frame=dataset.hex,
    validation_frame=valid,
    x=predictors,
    y="group",
    activation="Rectifier",
    hidden=c(20,20),
    epochs=p[2][1,1],
    train_samples_per_iteration = p[3][1,1],
    seed=7,
    rho=0.99,
    epsilon=p[4][1,1],
    input_dropout_ratio = p[1][1,1],
    variable_importances=TRUE,
    export_weights_and_biases=T,
    standardize=T,
    stopping_metric="misclassification",
    stopping_tolerance=1e-2,
    stopping_rounds=2,
    score_validation_samples=10000,
    score_duty_cycle=0.025,
    max_w2=10
  ) 
  valAUC<-c(valAUC,h2o.auc(best))
  confusion<-xtable(h2o.confusionMatrix(best))
  confusion
}

val_df<-cbind(fulldata[c(1268,379,432,1261,665),-c(1,5)],data.frame(val_AUC=valAUC))
val<-xtable(cbind(fulldata[c(1268,379,432,1261,665),-c(1,5)],data.frame(val_AUC=valAUC)))
display(val)[7:8] <- "f"
digits(val) <- 5
val


a<-which(fulldata$input_dropout_ratio==0.2)
b<-which(fulldata$epochs==500)
c<-which(fulldata$train_samples_per_iteration==-1)
d<-which(fulldata$epsilon==1e-04)

par(mfrow=c(1,1))

plot(fulldata[intersect(intersect(a,b),d),c(5,7)],type="b",xlab="train sample per iteration",ylab="AUC")

plot(1:5,val_df[,5],type="b",col="red",ylim=c(0.88,0.99),lwd=1.5,xlab="",ylab="AUC")
lines(1:5,val_df[,6],col="blue",lwd=1.5,type="b")
legend("topleft", c("Validation", "Test"), col = c("red", "blue"),pch=c(1,1))
       
       
for (i in c(665)){
  p<-fulldata[i,3:6]
  
  best=h2o.deeplearning(
    training_frame=dataset.hex,
    validation_frame=valid,
    x=predictors,
    y="group",
    activation="Rectifier",
    hidden=c(20,20),
    epochs=p[2][1,1],
    train_samples_per_iteration = p[3][1,1],
    seed=7,
    rho=0.99,
    epsilon=p[4][1,1],
    input_dropout_ratio = p[1][1,1],
    variable_importances=TRUE,
    export_weights_and_biases=T,
    standardize=T,
    stopping_metric="misclassification",
    stopping_tolerance=1e-2,
    stopping_rounds=2,
    score_validation_samples=10000,
    score_duty_cycle=0.025,
    max_w2=10
  ) 
}

var_df<-best@model$variable_importances
best@model$variable_importances
plot(1:33,var_df[,3],type="b",,xlab="",ylab="",main="variable scaled importance")

var<-xtable(best@model$variable_importances)

val<-xtable(cbind(fulldata[c(1268,379,432,1261,665),-c(1,5)],data.frame(val_AUC=valAUC)))
display(val)[7:8] <- "f"
digits(val) <- 5
val

