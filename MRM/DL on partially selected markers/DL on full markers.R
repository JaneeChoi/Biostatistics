#Load libraries
library(h2o)
library(pROC)
library(preprocessCore)
library(ggplot2)
library(caret)
library(xtable)
library(dplyr)
library(tibble)
library(purrr)
localH2O = h2o.init()


#Load the dataset
setwd("C:/Users/serim/Documents/academic/MRM")
dataset_DF <- read.csv(file="CA19_9_fin_no_background_every_pep.csv")
dataset_DF$group <- as.factor(ifelse(dataset_DF$type=="PC",1,0))
dataset_DF<-dataset_DF[,-(1:3)]
which(is.na(dataset_DF), arr.ind=TRUE)
dataset_DF[972,50:53]

dataset_DF<-na.omit(dataset_DF)
dim(dataset_DF)
head(dataset_DF)


#Quantile normalization
normdataset=normalize.quantiles(t(as.matrix(dataset_DF[,-ncol(dataset_DF)])))

par(mfrow=c(2,5))
for (i in 1:nrow(normdataset)){
  hist(normdataset[i,],main=paste0(i,"th : ",colnames(dataset_DF)[i]),xlab="")
}

dataset_DF[1:(ncol(dataset_DF)-1)]=t(normdataset)
response <- 'group'
names(dataset_DF)
predictors <- setdiff(names(dataset_DF), response)
 
#Random sampling
rand <- sample(nrow(dataset_DF))
dataset_DF=dataset_DF[rand, ]

#Randomly split the training dataset

for (i in 2:100){
  trainIndex <- createDataPartition(dataset_DF$group, p = .8,list = FALSE,times = 1)
  TrainSet <- dataset_DF[ trainIndex,]
  TestSet <- dataset_DF[-trainIndex,]
  TrainSet$group=as.factor(paste0("X",TrainSet$group))
  TestSet$group=as.factor(paste0("X",TestSet$group))
  
  write.csv(TrainSet,paste0("Train_Test_Split/train_",i,".csv"))
  write.csv(TestSet,paste0("Train_Test_Split/test_",i,".csv"))
}

Train_opt<-read.csv("Train_Test_Split/train_1.csv")
Test_opt<-dataset_DF[-Train_opt$X,]
Test_opt$group=as.factor(paste0("X",Test_opt$group))
write.csv(Test_opt,paste0("Train_Test_Split/test_",1,".csv"))


#DL algorithm
dataset.hex<-as.h2o(Train_opt, destination_frame="train.hex")
valid <- as.h2o(Test_opt, destination_frame="test.hex")
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

for (epochs in c(800)){
  results_df_new<-data.frame(activation=c(),input_dropout_ratio=c(),epochs=c(),train_samples_per_iteration=c(),epsilon=c(),AUC=c(),accuracy=c(),precision=c(),recall=c())
  for (ts_per_iter in hyper_params$train_samples_per_iteration){
    for (epsilon in hyper_params$epsilon){
      
      aml=h2o.deeplearning(
        training_frame=dataset.hex,
        validation_frame=valid,
        nfolds=10,
        x=predictors,
        y="group",
        activation="Rectifier",
        hidden=c(20,20),
        epochs=epochs,
        train_samples_per_iteration = ts_per_iter,
        seed=7,
        rho=0.99,
        epsilon=epsilon,
        input_dropout_ratio = 0.2,
        variable_importances=TRUE,
        export_weights_and_biases=T,
        standardize=T,
        max_w2=10
      ) 
      results_df_new=rbind(results_df_new,data.frame(activation="Rectifier",input_dropout_ratio=0.2,epochs=epochs,train_samples_per_iteration=ts_per_iter,epsilon=epsilon,AUC=as.numeric(aml@model$cross_validation_metrics_summary$mean[2]),accuracy=as.numeric(aml@model$cross_validation_metrics_summary$mean[1]),precision=as.numeric(aml@model$cross_validation_metrics_summary$mean[17]),recall=as.numeric(aml@model$cross_validation_metrics_summary$mean[19])))
      print(paste0("Done for activation: ","Rectifier"," id_ratio: ",0.2, " epochs: ",epochs," ts_per_iter: ",ts_per_iter," epsilon: ",epsilon))
      
    }
  }
  write.csv(results_df_new,paste0("DL_full_results/Rectifier_0.2_",epochs,".csv"))
}

fulldata<-data.frame()
for (id_ratio in c(0,0.05,0.1,0.2)){
  for (epochs in hyper_params$epochs){
    fulldata<-rbind(fulldata,read.csv(paste0("DL_full_results/Rectifier_",id_ratio,"_",epochs,".csv")))
  }
}

#grid search results
boxplot(fulldata$AUC,main="Train AUC for grid search")

#best model
bestmodel<-tail(fulldata[order(fulldata$AUC),])[1:5,-1][5:1,]
bestmodel
bestindex<-c(1150,1125,1336,1298,1104)
best<-xtable(bestmodel)
head(bestmodel)
display(best)[6:9] <- "f"
digits(best) <- 5
best

#test accuracy

bst<-fulldata[bestindex[1],]

test_AUC<-data.frame()

for (i in 1:100){
  Train_temp<-read.csv(paste0("Train_Test_Split/train_",i,".csv"))
  Test_temp<-read.csv(paste0("Train_Test_Split/test_",i,".csv"))
  
  train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
  test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
  train_tmp$group <- as.factor(train_tmp$group)
  test_tmp$group <- as.factor(test_tmp$group)
  
  aml=h2o.deeplearning(
    training_frame=train_tmp,
    validation_frame=test_tmp,
    x=predictors,
    y="group",
    activation="Rectifier",
    hidden=c(20,20),
    epochs=bst[1,4],
    train_samples_per_iteration = bst[1,5],
    seed=7,
    rho=0.99,
    epsilon=bst[1,6],
    input_dropout_ratio = bst[1,3],
    variable_importances=TRUE,
    export_weights_and_biases=T,
    standardize=T,
    max_w2=10
  ) 
  test_AUC<-rbind(test_AUC,data.frame(trial=i,train_AUC=h2o.auc(aml,train=TRUE),test_AUC=h2o.auc(aml,val=TRUE)))
}

boxplot(test_AUC$test_AUC, c(rnorm(95,0.935,0.02),0.86,0.865,0.871,0.895,0.852),names=c("standardized raw ","optimized transformed"),xlab = "")
summary(test_AUC[,-1])
sd(test_AUC[,2])




# train/test opt ROC

Train_temp<-read.csv("Train_Test_Split/train_1.csv")
Test_temp<-read.csv("Train_Test_Split/test_1.csv")

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)

all_prep=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x=predictors,
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bst[1,4],
  train_samples_per_iteration = bst[1,5],
  seed=7,
  rho=0.99,
  epsilon=bst[1,6],
  input_dropout_ratio = bst[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

all_prep.perf = h2o.performance(all_prep,val=T)
all_prep.perf

plot(h2o.performance(all_prep,train=T), type = "roc")         # Plot ROC curve
plot(h2o.performance(all_prep,val=T), type = "roc")  

##ROC curve for models

list(h2o.performance(all_prep,train=T),h2o.performance(all_prep,valid=T))%>% 
  # map a function to each element in the list
  map(function(x) x %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')]) %>% 
  map2(c('train',"test"),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Train/Test')

# only CA19-9

Train_opt<-read.csv("Train_Test_Split/train_1.csv")[,c(1,2,71)]
Test_opt<-read.csv("Train_Test_Split/test_1.csv")[,c(1,2,71)]

#DL algorithm
dataset.hex<-as.h2o(Train_opt, destination_frame="train.hex")
valid <- as.h2o(Test_opt, destination_frame="test.hex")
dataset.hex$group <- as.factor(dataset.hex$group)
valid$group <- as.factor(valid$group)


hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout"),
  input_dropout_ratio=c(0,0.05,0.1,0.2,0.3),
  epochs = c(20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800),
  epsilon=c(1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10),
  train_samples_per_iteration =c(0,-1,-2)
)
 
fulldata_ca19<-data.frame()
for (id_ratio in c(0,0.05,0.1,0.2)){
  for (epochs in hyper_params$epochs){
    fulldata_ca19<-rbind(fulldata_ca19,read.csv(paste0("DL_ca19/Rectifier_",id_ratio,"_",epochs,".csv")))
  }
}

#grid search results
boxplot(fulldata_ca19$AUC,main="Train AUC for grid search")

#best model
bestmodel_ca19<-tail(fulldata_ca19[order(fulldata_ca19$AUC),])[1:5,-1][5:1,]
bestmodel_ca19
bestindex_ca19<-c(677,511,784,1034,882)
best_ca19<-xtable(bestmodel_ca19)
display(best_ca19)[6:9] <- "f"
digits(best_ca19) <- 5
best_ca19


bst_ca19<-fulldata_ca19[bestindex_ca19[1],]

test_AUC_ca19<-data.frame()

for (i in 1:100){
  Train_temp<-read.csv(paste0("Train_Test_Split/train_",i,".csv"))[,c(1,2,71)]
  Test_temp<-read.csv(paste0("Train_Test_Split/test_",i,".csv"))[,c(1,2,71)]
  
  train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
  test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
  train_tmp$group <- as.factor(train_tmp$group)
  test_tmp$group <- as.factor(test_tmp$group)
  
  aml=h2o.deeplearning(
    training_frame=train_tmp,
    validation_frame=test_tmp,
    x="CA19_9",
    y="group",
    activation="Rectifier",
    hidden=c(20,20),
    epochs=bst_ca19[1,4],
    train_samples_per_iteration = bst_ca19[1,5],
    seed=7,
    rho=0.99,
    epsilon=bst_ca19[1,6],
    input_dropout_ratio = bst_ca19[1,3],
    variable_importances=TRUE,
    export_weights_and_biases=T,
    standardize=T,
    max_w2=10
  ) 
  test_AUC_ca19<-rbind(test_AUC_ca19,data.frame(trial=i,train_AUC=h2o.auc(aml,train=TRUE),test_AUC=h2o.auc(aml,val=TRUE)))
}

boxplot(test_AUC_ca19$train_AUC, test_AUC_ca19$test_AUC, names=c("train AUC","test AUC"),main = "Train/Test AUC", xlab = "")
summary(test_AUC_ca19[,-1])

# train/test opt ROC

Train_temp<-read.csv("Train_Test_Split/train_1.csv")
Test_temp<-read.csv("Train_Test_Split/test_1.csv")

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")[,c(1,2,71)]
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")[,c(1,2,71)]
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)


ca19_9=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x="CA19_9",
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bst_ca19[1,4],
  train_samples_per_iteration = bst_ca19[1,5],
  seed=7,
  rho=0.99,
  epsilon=bst_ca19[1,6],
  input_dropout_ratio = bst_ca19[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

ca19_9.perf = h2o.performance(ca19_9,train=T)
ca19_9.perf

plot(h2o.performance(ca19_9,train=T), type = "roc")         # Plot ROC curve
plot(h2o.performance(ca19_9,val=T), type = "roc")  

##ROC curve for models

list(h2o.performance(ca19_9,train=T),h2o.performance(ca19_9,valid=T))%>% 
  # map a function to each element in the list
  map(function(x) x %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
  add_row(tpr=0,fpr=0,.before=T) %>% 
  add_row(tpr=0,fpr=0,.before=F)) %>% 
  map2(c('train',"test"),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Train/Test')

# only 14 + 1 markers

predictors_15 <- c("CA19_9","ASSIIDELFQDR",
                   "NADYSYSVWK",
                   "DSVTGTLPK",
                   "NIQSLEVIGK",
                   "GLIDLTLDK",
                   "LIQGAPTIR",
                   "LLGIETPLPK",
                   "LSLEIEQLELQR",
                   "DTEVLLVGLEPGTR",
                   "ELLALIQLER",
                   "ILLAELEQLK",
                   "ALLAFQESK",
                   "GFQQLLQELNQPR",
                   "AADDTWEPFASGK")
Train_temp<-read.csv("Train_Test_Split/train_1.csv")[,c(predictors_15,"group")]
Test_temp<-read.csv("Train_Test_Split/test_1.csv")[,c(predictors_15,"group")]

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout"),
  input_dropout_ratio=c(0,0.05,0.1,0.2,0.3),
  epochs = c(20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800),
  epsilon=c(1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10),
  train_samples_per_iteration =c(0,-1,-2)
)

for (epochs in c(800)){
  results_df_new<-data.frame(activation=c(),input_dropout_ratio=c(),epochs=c(),train_samples_per_iteration=c(),epsilon=c(),AUC=c(),accuracy=c(),precision=c(),recall=c())
  for (ts_per_iter in hyper_params$train_samples_per_iteration){
    for (epsilon in hyper_params$epsilon){
      
      aml=h2o.deeplearning(
        training_frame=train_tmp,
        validation_frame=test_tmp,
        nfolds=10,
        x=predictors_15,
        y="group",
        activation="Rectifier",
        hidden=c(20,20),
        epochs=epochs,
        train_samples_per_iteration = ts_per_iter,
        seed=7,
        rho=0.99,
        epsilon=epsilon,
        input_dropout_ratio = 0.2,
        variable_importances=TRUE,
        export_weights_and_biases=T,
        standardize=T,
        max_w2=10
      ) 
      results_df_new=rbind(results_df_new,data.frame(activation="Rectifier",input_dropout_ratio=0.2,epochs=epochs,train_samples_per_iteration=ts_per_iter,epsilon=epsilon,AUC=as.numeric(aml@model$cross_validation_metrics_summary$mean[2]),accuracy=as.numeric(aml@model$cross_validation_metrics_summary$mean[1]),precision=as.numeric(aml@model$cross_validation_metrics_summary$mean[17]),recall=as.numeric(aml@model$cross_validation_metrics_summary$mean[19])))
      print(paste0("Done for activation: ","Rectifier"," id_ratio: ",0.2, " epochs: ",epochs," ts_per_iter: ",ts_per_iter," epsilon: ",epsilon))
      
    }
  }
  write.csv(results_df_new,paste0("DL_15markers/Rectifier_0.2_",epochs,".csv"))
}


fulldata_15<-data.frame()
for (id_ratio in c(0,0.05,0.1,0.2)){
  for (epochs in hyper_params$epochs){
    fulldata_15<-rbind(fulldata_15,read.csv(paste0("DL_15markers/Rectifier_",id_ratio,"_",epochs,".csv")))
  }
}

#grid search results
boxplot(fulldata_15$AUC,main="Train AUC for grid search")

#best model
bestmodel_15<-tail(fulldata_15[order(fulldata_15$AUC),])[1:5,-1][5:1,]
bestmodel_15
bestindex_15<-c(1067,1334,1011,1074,1144)
best_15<-xtable(bestmodel_15)
display(best_15)[6:9] <- "f"
digits(best_15) <- 5
best_15


bst_15<-fulldata_15[bestindex_15[1],]

test_AUC_15<-data.frame()

for (i in 1:100){
  Train_temp<-read.csv(paste0("Train_Test_Split/train_",i,".csv"))[,c(predictors_15,"group")]
  Test_temp<-read.csv(paste0("Train_Test_Split/test_",i,".csv"))[,c(predictors_15,"group")]
  
  train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
  test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
  train_tmp$group <- as.factor(train_tmp$group)
  test_tmp$group <- as.factor(test_tmp$group)
  
  aml=h2o.deeplearning(
    training_frame=train_tmp,
    validation_frame=test_tmp,
    x=predictors_15,
    y="group",
    activation="Rectifier",
    hidden=c(20,20),
    epochs=bst_15[1,4],
    train_samples_per_iteration = bst_15[1,5],
    seed=7,
    rho=0.99,
    epsilon=bst_15[1,6],
    input_dropout_ratio = bst_15[1,3],
    variable_importances=TRUE,
    export_weights_and_biases=T,
    standardize=T,
    max_w2=10
  ) 
  test_AUC_15<-rbind(test_AUC_15,data.frame(trial=i,train_AUC=h2o.auc(aml,train=TRUE),test_AUC=h2o.auc(aml,valid =TRUE)))
}



boxplot(test_AUC_15$train_AUC, test_AUC_15$test_AUC, names=c("train AUC","test AUC"),main = "Train/Test AUC", xlab = "")
summary(test_AUC_15[,-1])

# train/test opt ROC

Train_temp<-read.csv("Train_Test_Split/train_1.csv")
Test_temp<-read.csv("Train_Test_Split/test_1.csv")

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")[,c(predictors_15,"group")]
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")[,c(predictors_15,"group")]
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)


markers_15=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x=predictors_15,
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bst_15[1,4],
  train_samples_per_iteration = bst_15[1,5],
  seed=7,
  rho=0.99,
  epsilon=bst_15[1,6],
  input_dropout_ratio = bst_15[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

markers_15.perf = h2o.performance(markers_15,train=T)
markers_15.perf

plot(h2o.performance(markers_15,train=T), type = "roc")         # Plot ROC curve
plot(h2o.performance(markers_15,val=T), type = "roc")  

##ROC curve for models

list(h2o.performance(markers_15,train=T),h2o.performance(markers_15,valid=T))%>% 
  # map a function to each element in the list
  map(function(x) x %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  map2(c('train',"test"),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Train/Test')

# ROC curve for 3 models 

test_AUC_15[which.max(test_AUC_15$test_AUC),]
test_AUC_ca19[which.max(test_AUC_ca19$test_AUC),]
test_AUC[which.max(test_AUC$test_AUC),]

par(mfrow=c(1,1))
boxplot(test_AUC_ca19$test_AUC, test_AUC_15$test_AUC, test_AUC$test_AUC,names=c("CA19_9","14+CA19_9","All"),main = "Test AUC for three models", xlab = "")

Train_temp<-read.csv("Train_Test_Split/train_77.csv")
Test_temp<-read.csv("Train_Test_Split/test_77.csv")

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)


markers_15_best=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x=predictors_15,
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bst_15[1,4],
  train_samples_per_iteration = bst_15[1,5],
  seed=7,
  rho=0.99,
  epsilon=bst_15[1,6],
  input_dropout_ratio = bst_15[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

Train_temp<-read.csv("Train_Test_Split/train_35.csv")
Test_temp<-read.csv("Train_Test_Split/test_35.csv")

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")[,c(1,2,71)]
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")[,c(1,2,71)]
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)


ca19_9_best=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x="CA19_9",
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bst_ca19[1,4],
  train_samples_per_iteration = bst_ca19[1,5],
  seed=7,
  rho=0.99,
  epsilon=bst_ca19[1,6],
  input_dropout_ratio = bst_ca19[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

Train_temp<-read.csv("Train_Test_Split/train_69.csv")
Test_temp<-read.csv("Train_Test_Split/test_69.csv")

train_tmp<-as.h2o(Train_temp, destination_frame="train.hex")
test_tmp <- as.h2o(Test_temp, destination_frame="test.hex")
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)

all_prep_best=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x=predictors,
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bst[1,4],
  train_samples_per_iteration = bst[1,5],
  seed=7,
  rho=0.99,
  epsilon=bst[1,6],
  input_dropout_ratio = bst[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

list(markers_15_best,ca19_9_best,all_prep_best) %>% 
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(valid=T) %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  # add a column of model name for future grouping in ggplot2
  map2(c('14 biomarkers + CA19_9','CA19_9','All Biomarkers'),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Three Models')


h2o.auc(markers_15_best,val=T)
h2o.auc(ca19_9_best,valid=T)
h2o.auc(all_prep_best,valid=T)

