# DL

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

train=AACR_transformed_tr_stdtogether_tr
validation=AACR_transformed_tr_stdtogether_validation
AMC=AACR_transformed_te_stdtogether
trainval=AACR_transformed_tr_stdtogether

train$group <- as.factor(ifelse(train$type=="PC",1,0))
validation$group <- as.factor(ifelse(validation$type=="PC",1,0))
AMC$group <- as.factor(ifelse(AMC$type=="PC",1,0))
trainval$group <- as.factor(ifelse(trainval$type=="PC",1,0))

dataset.hex<-as.h2o(train, destination_frame="train.hex")
valid <- as.h2o(validation, destination_frame="test.hex")
dataset.hex$group <- as.factor(dataset.hex$group)
valid$group <- as.factor(valid$group)

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout"),
  input_dropout_ratio=c(0,0.05,0.1,0.2,0.3),
  epochs = c(20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800),
  epsilon=c(1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10),
  train_samples_per_iteration =c(0,-1,-2)
)

preds<-list(pval_28,aic_28,auc_28,lasso_28,none_28)
preds_str<-c("pval_28","aic_28","auc_28","lasso_28","none_28")

pval58<-c("CA19_9","LIQGAPTIR","TIVTTLQDSIR","NADYSYSVWK","ASSIIDELFQDR","DSVTGTLPK")
aic58<-c("CA19_9","ICLDPDAPR","GLPGEVLGAQPGPR","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","DSVTGTLPK","GFQQLLQELNQPR","LLGIETPLPK","NIQSLEVIGK", "GLTSVINQK","DTEVLLVGLEPGTR","ELLALIQLER","LSLEIEQLELQR","ALLAFQESK","DYFIATCK","TNFDNDIALVR","IEDGFSLK","GGSASTWLTAFALR")
auc58<-c("CA19_9","LIQGAPTIR","GLPGEVLGAQPGPR" ,"TIVTTLQDSIR", "ALAQCAPPPAVCAELVR","NADYSYSVWK","ASSIIDELFQDR","VSTLPAITLK","DSVTGTLPK","LLGIETPLPK","LSLEIEQLELQR" , "NIQSLEVIGK" , "ELLALIQLER" , "ALLAFQESK" , "DYFIATCK" , "TNFDNDIALVR" , "IEDGFSLK" , "GGSASTWLTAFALR" , "DPTFIPAPIQAK" , "DTEVLLVGLEPGTR" ,"GLTSVINQK")
lasso58<-c("CA19_9","AADDTWEPFASGK","ALAQCAPPPAVCAELVR","ASSIIDELFQDR","CDENILWLDYK","CINQLLCK","DSVTGTLPK","DTEVLLVGLEPGTR","DYFIATCK","ELLALIQLER","GDIGETGVPGAEGPR","GFQQLLQELNQPR","GGSASTWLTAFALR","GLIDLTLDK", "GLPGEVLGAQPGPR" ,"GLTSVINQK","ICLDPDAPR","IEDGFSLK","ILLAELEQLK","LIQGAPTIR","LLGIETPLPK", "LSLEIEQLELQR","NADYSYSVWK" ,"NIQSLEVIGK" , "TIVTTLQDSIR", "TLLSNLEEAK" ,"VLFYVDSEK")
none58<-c("CA19_9","AADDTWEPFASGK","AGFSWIEVTFK","ALAQCAPPPAVCAELVR","ALLAFQESK","ASCLYGQLPK","ASSIIDELFQDR","CDENILWLDYK","CDLISIPK","CINQLLCK","DGYLFQLLR","DLLLPQPDLR","DPTFIPAPIQAK","DSVTGTLPK","DTEVLLVGLEPGTR","DYFIATCK","EFGNTLEDK","ELLALIQLER","EWFSETFQK","FAVLQENVAWGNGR","FQASVATPR","GDIGETGVPGAEGPR","GEWVALNPLR","GFLLLASLR","GFQQLLQELNQPR","GGSASTWLTAFALR","GLIDLTLDK","GLPGEVLGAQPGPR","GLTSVINQK","ICLDPDAPR","IEDGFSLK","ILLAELEQLK","ILSGDPYCEK","IQPSGGTNINEALLR","IVVVTAGVR","LALDNGGLAR","LIQGAPTIR","LLGIETPLPK","LSLEIEQLELQR","LSNNALSGLPQGVFGK","LSSGLVTAALYGR","NADYSYSVWK","NIQSLEVIGK","NNLELSTPLK","TGESVEFVCK","TIVTTLQDSIR","TLLSNLEEAK","TLNICEVGTIR","TNFDNDIALVR","TWYPEVPK","VAAGAFQGLR","VAEGTQVLELPFK","VCPFAGILENGAVR","VFSLQWGEVK","VLFYVDSEK","VSTLPAITLK","VTGVVLFR","YGQPLPGYTTK")

pval23<-c("CA19_9","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","DSVTGTLPK","GLIDLTLDK")
aic23<-c("CA19_9","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","DSVTGTLPK","LSLEIEQLELQR","LLGIETPLPK","ALLAFQESK","DYFIATCK","ELLALIQLER","DTEVLLVGLEPGTR")
auc23<-c("CA19_9","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","DSVTGTLPK","LSLEIEQLELQR","DYFIATCK")
lasso23<-c("CA19_9","AADDTWEPFASGK","DSVTGTLPK","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","LLGIETPLPK","LSLEIEQLELQR","GLIDLTLDK","ELLALIQLER","ILLAELEQLK","DTEVLLVGLEPGTR","CINQLLCK")
none23<-c("CA19_9","AADDTWEPFASGK","GFQQLLQELNQPR","DSVTGTLPK","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","LLGIETPLPK","ALLAFQESK","LSLEIEQLELQR","GLIDLTLDK","ELLALIQLER","ILLAELEQLK","DTEVLLVGLEPGTR","CINQLLCK","NNLELSTPLK","DLLLPQPDLR","AGFSWIEVTFK","VFSLQWGEVK","DYFIATCK","ILSGDPYCEK","LSSGLVTAALYGR")

pval_28<-c("CA19_9","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","TIVTTLQDSIR","DSVTGTLPK","LLGIETPLPK")
aic_28<-c("CA19_9","TIVTTLQDSIR","LIQGAPTIR","CINQLLCK","NADYSYSVWK","ASSIIDELFQDR","DSVTGTLPK","DYFIATCK")
auc_28<-c("CA19_9","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","TIVTTLQDSIR","DSVTGTLPK","LLGIETPLPK")
lasso_28<-c("CA19_9","AADDTWEPFASGK","DSVTGTLPK","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","LLGIETPLPK","LSLEIEQLELQR","GLIDLTLDK","ELLALIQLER","ILLAELEQLK","DTEVLLVGLEPGTR","CINQLLCK","GFLLLASLR","TIVTTLQDSIR")
none_28<-c("CA19_9","AADDTWEPFASGK","GFQQLLQELNQPR","DSVTGTLPK","LIQGAPTIR","ASSIIDELFQDR","NADYSYSVWK","NIQSLEVIGK","LLGIETPLPK","ALLAFQESK","LSLEIEQLELQR","GLIDLTLDK","ELLALIQLER","ILLAELEQLK","DTEVLLVGLEPGTR","CINQLLCK","NNLELSTPLK","DLLLPQPDLR","AGFSWIEVTFK","VFSLQWGEVK","DYFIATCK","ILSGDPYCEK","LSSGLVTAALYGR","VSTLPAITLK","GFLLLASLR","TIVTTLQDSIR","ASCLYGQLPK","VAAGAFQGLR")



for (i in 1:5){
  for (id_ratio in hyper_params$input_dropout_ratio){
    for (epochs in hyper_params$epochs){
    results_df_new<-data.frame(activation=c(),input_dropout_ratio=c(),epochs=c(),train_samples_per_iteration=c(),epsilon=c(),train_AUC=c(),val_AUC=c())
      for (ts_per_iter in hyper_params$train_samples_per_iteration){
        for (epsilon in hyper_params$epsilon){
      
          aml=h2o.deeplearning(
            training_frame=dataset.hex,
            validation_frame=valid,
            x=preds[[i]],
            y="group",
            activation="Rectifier",
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
            max_w2=10
          ) 
          results_df_new=rbind(results_df_new,data.frame(activation="Rectifier",input_dropout_ratio=id_ratio,epochs=epochs,train_samples_per_iteration=ts_per_iter,epsilon=epsilon,train_AUC=h2o.auc(aml,train = T),val_AUC=h2o.auc(aml,val = T)))
          print(paste0("Done for activation: ","Rectifier"," id_ratio: ",id_ratio," epochs: ",epochs," ts_per_iter: ",ts_per_iter," epsilon: ",epsilon))
        }
      }
  write.csv(results_df_new,paste0("grid_",preds_str[i],"/Rectifier_",id_ratio,"_",epochs,".csv"))
  }
  }
}

resultdf<-data.frame()

for (i in 1:5){
  for (id_ratio in hyper_params$input_dropout_ratio){
    for (epochs in hyper_params$epochs){
      resultdf<-rbind(resultdf,read.csv(paste0("grid_",preds_str[i],"/Rectifier_",id_ratio,"_",epochs,".csv")))
    }
  }
}


#best model

bestmodel<-tail(resultdf[order(resultdf$val_AUC),])[6,]
bestmodel

all_prep=h2o.deeplearning(
  training_frame=dataset.hex,
  validation_frame=valid,
  x=none_28,
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bestmodel[1,4],
  train_samples_per_iteration = bestmodel[1,5],
  seed=7,
  rho=0.99,
  epsilon=bestmodel[1,6],
  input_dropout_ratio = bestmodel[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

all_prep@model$training_metrics
all_prep@model$validation_metrics

h2o.performance(all_prep,test_tmp)

trainval_tmp<-as.h2o(as.h2o(trainval, destination_frame="trainval.hex"))
test_tmp <- as.h2o(AMC, destination_frame="AMC.hex")
trainval_tmp$group <- as.factor(trainval_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)

all_prep_1=h2o.deeplearning(
  training_frame=trainval_tmp,
  validation_frame=test_tmp,
  x=none_28,
  y="group",
  activation="Rectifier",
  hidden=c(20,20),
  epochs=bestmodel[1,4],
  train_samples_per_iteration = bestmodel[1,5],
  seed=7,
  rho=0.99,
  epsilon=bestmodel[1,6],
  input_dropout_ratio = bestmodel[1,3],
  variable_importances=TRUE,
  export_weights_and_biases=T,
  standardize=T,
  max_w2=10
) 

h2o.auc(all_prep_1,val = T)
all_prep_1@model$validation_metrics

