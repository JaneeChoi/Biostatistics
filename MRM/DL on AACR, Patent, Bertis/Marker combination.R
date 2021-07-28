setwd("C:/Users/serim/Documents/academic/MRM/New")

rm(list = ls())

if (!requireNamespace("readr", quietly = TRUE)){
  install.packages("readr")
}
if (!requireNamespace("dplyr", quietly = TRUE)){
  install.packages("dplyr")
}

library(readr)
library(dplyr)
library(stringr)

rawdata = readr::read_csv("rawdata.csv", col_types = cols(
  X = col_character(),
  batch = col_factor(),
  type = col_character(),
  CA19_9 = col_double(),
  AADDTWEPFASGK = col_double(),
  AGFSWIEVTFK = col_double(),
  ALAPEYAK = col_double(),
  ALAQCAPPPAVCAELVR = col_double(),
  ALLAFQESK = col_double(),
  ASCLYGQLPK = col_double(),
  ASSIIDELFQDR = col_double(),
  CDENILWLDYK = col_double(),
  CDLISIPK = col_double(),
  CINQLLCK = col_double(),
  CMPTFQFFK = col_double(),
  DGYLFQLLR = col_double(),
  DLLLPQPDLR = col_double(),
  DPTFIPAPIQAK = col_double(),
  DSVTGTLPK = col_double(),
  DTEVLLVGLEPGTR = col_double(),
  DYFIATCK = col_double(),
  EFGNTLEDK = col_double(),
  ELLALIQLER = col_double(),
  EPAVLELEGK = col_double(),
  EWFSETFQK = col_double(),
  FAVLQENVAWGNGR = col_double(),
  FLEQECNVLPLK = col_double(),
  FQASVATPR = col_double(),
  GDIGETGVPGAEGPR = col_double(),
  GDSVSDSGSDALR = col_double(),
  GEWVALNPLR = col_double(),
  GFLLLASLR = col_double(),
  GFQQLLQELNQPR = col_double(),
  GGSASTWLTAFALR = col_double(),
  GLIDLTLDK = col_double(),
  GLPGEVLGAQPGPR = col_double(),
  GLTSVINQK = col_double(),
  ICLDPDAPR = col_double(),
  IEDGFSLK = col_double(),
  ILLAELEQLK = col_double(),
  ILSGDPYCEK = col_double(),
  IQPSGGTNINEALLR = col_double(),
  IVVVTAGVR = col_double(),
  LALDNGGLAR = col_double(),
  LIQGAPTIR = col_double(),
  LLGIETPLPK = col_double(),
  LSFQEFLK = col_double(),
  LSLEIEQLELQR = col_double(),
  LSNNALSGLPQGVFGK = col_double(),
  LSSGLVTAALYGR = col_double(),
  NADYSYSVWK = col_double(),
  NIQSLEVIGK = col_double(),
  NNLELSTPLK = col_double(),
  NVLVTLYER = col_double(),
  QGIQFYTQLK = col_double(),
  SPAYTLVWTR = col_double(),
  TGESVEFVCK = col_double(),
  TIVTTLQDSIR = col_double(),
  TLLSNLEEAK = col_double(),
  TLNICEVGTIR = col_double(),
  TNFDNDIALVR = col_double(),
  TWYPEVPK = col_double(),
  VAAGAFQGLR = col_double(),
  VAEGTQVLELPFK = col_double(),
  VCPFAGILENGAVR = col_double(),
  VFSLQWGEVK = col_double(),
  VLDVNDNAPK = col_double(),
  VLFYVDSEK = col_double(),
  VSTLPAITLK = col_double(),
  VTGVVLFR = col_double(),
  VTLNGVPAQPLGPR = col_double(),
  YGQPLPGYTTK = col_double())
)

validation_index = c(
  'SNU-NL145',
  'NCC-PC049',
  'SNU-NL133',
  'SNU-PC058',
  'SNU-PC536',
  'SNU-PC067',
  'NCC-PC041',
  'SNU-NL224',
  'SNU-NL063',
  'NCC-PC047',
  'SMC-PC072',
  'NCC-PC039',
  'SNU-NL351',
  'SNU-PB002',
  'SNU-PC071',
  'NCC-PC126',
  'SNU-NL416',
  'SNU-NL367',
  'SMC-PC090',
  'SNU-NL118',
  'SNU-NL267',
  'NCC-PC077',
  'SMC-PC069',
  'SNU-NL105',
  'YMC-PB020',
  'NCC-PC117',
  'SNU-NL176',
  'YMC-PC012',
  'SNU-NL124',
  'SMC-PC081',
  'SNU-NL412',
  'NCC-PC076',
  'NCC-PC055',
  'SNU-NL396',
  'SNU-NL041',
  'SMC-PC079',
  'SNU-NL304',
  'SMC-PC049',
  'YMC-PC001',
  'SNU-NL152',
  'SMC-PB016',
  'SMC-PC033',
  'SMC-PB030',
  'SNU-NL062',
  'SNU-NL148',
  'SMC-PC046',
  'SNU-NL090',
  'SNU-NL075',
  'SNU-NL222',
  'SNU-NL070',
  'NCC-PC026',
  'YMC-PC050',
  'SMC-PC016',
  'YMC-PC025',
  'SNU-NL192',
  'NCC-PC138',
  'SMC-PC032',
  'SMC-PB011',
  'SNU-PC521',
  'SMC-PC024',
  'NCC-PC079',
  'SNU-PC507',
  'SNU-NL038',
  'NCC-PC085',
  'SNU-NL406',
  'NCC-PC087',
  'SNU-PC506',
  'SNU-NL052',
  'SNU-PC063',
  'YMC-PB026',
  'NCC-PC131',
  'SNU-NL081',
  'SNU-NL243',
  'SNU-NL112',
  'SMC-PC042',
  'YMC-PB030',
  'SNU-NL119',
  'SMC-PB001',
  'SNU-NL324',
  'SMC-PC041',
  'SNU-NL227',
  'SNU-PB020',
  'SMC-PB018',
  'SNU-NL082',
  'YMC-PC038',
  'SNU-NL297',
  'NCC-PC119',
  'YMC-PC045',
  'NCC-PC021',
  'YMC-PC004',
  'SNU-NL221',
  'SNU-NL432',
  'SNU-NL299',
  'SNU-PC530',
  'SNU-NL048',
  'SNU-PC505',
  'SMC-PC099',
  'SNU-NL120',
  'SNU-NL352',
  'SMC-PC055',
  'NCC-PC037',
  'SNU-PC537',
  'SNU-NL044',
  'SNU-NL308',
  'NCC-PC023',
  'NCC-PC027',
  'SNU-NL355',
  'SNU-NL295',
  'SMC-PB007',
  'SMC-PC018',
  'SNU-NL341',
  'SNU-NL384',
  'SNU-NL051',
  'SMC-PC026',
  'YMC-PB006',
  'NCC-PC094',
  'NCC-PC097',
  'SNU-NL346',
  'SNU-NL237',
  'SNU-NL064',
  'SMC-PC011',
  'SNU-NL080',
  'SMC-PC014',
  'SNU-NL001',
  'SNU-NL144',
  'NCC-PC088',
  'SMC-PC028',
  'SMC-PC038',
  'SNU-NL086',
  'SNU-NL335',
  'SMC-PC054',
  'NCC-PC024',
  'SNU-NL068',
  'YMC-PB011',
  'SMC-PC043',
  'SNU-NL363',
  'SNU-NL380'
)
rawdata %>% .[!str_detect(.[['X']], '^AMC'),] %>% .[!str_detect(.[['type']], 'OC|OB'),] -> rawdata_train
rawdata %>% .[str_detect(.[['X']], '^AMC'),] -> AMC

predictor_patent = c('AADDTWEPFASGK',
                     'GFQQLLQELNQPR',
                     'DSVTGTLPK',
                     'LIQGAPTIR',
                     'ASSIIDELFQDR',
                     'NADYSYSVWK',
                     'CINQLLCK',
                     'NNLELSTPLK',
                     'DLLLPQPDLR',
                     'AGFSWIEVTFK',
                     'VFSLQWGEVK',
                     'DYFIATCK',
                     'ILSGDPYCEK',
                     'LSSGLVTAALYGR')
predictor_aacr = c('AADDTWEPFASGK',
                   'GFQQLLQELNQPR',
                   'DSVTGTLPK',
                   'LIQGAPTIR',
                   'ASSIIDELFQDR',
                   'NADYSYSVWK',
                   'NIQSLEVIGK',
                   'LLGIETPLPK',
                   'ALLAFQESK',
                   'LSLEIEQLELQR',
                   'GLIDLTLDK',
                   'ELLALIQLER',
                   'ILLAELEQLK',
                   'DTEVLLVGLEPGTR')
predictor_union = c('AADDTWEPFASGK',
                    'GFQQLLQELNQPR',
                    'DSVTGTLPK',
                    'LIQGAPTIR',
                    'ASSIIDELFQDR',
                    'NADYSYSVWK',
                    'NIQSLEVIGK',
                    'LLGIETPLPK',
                    'ALLAFQESK',
                    'LSLEIEQLELQR',
                    'GLIDLTLDK',
                    'ELLALIQLER',
                    'ILLAELEQLK',
                    'DTEVLLVGLEPGTR',
                    'CINQLLCK',
                    'NNLELSTPLK',
                    'DLLLPQPDLR',
                    'AGFSWIEVTFK',
                    'VFSLQWGEVK',
                    'DYFIATCK',
                    'ILSGDPYCEK',
                    'LSSGLVTAALYGR')
predictor_union_bertis = c('AADDTWEPFASGK',
                           'GFQQLLQELNQPR',
                           'DSVTGTLPK',
                           'LIQGAPTIR',
                           'ASSIIDELFQDR',
                           'NADYSYSVWK',
                           'NIQSLEVIGK',
                           'LLGIETPLPK',
                           'ALLAFQESK',
                           'LSLEIEQLELQR',
                           'GLIDLTLDK',
                           'ELLALIQLER',
                           'ILLAELEQLK',
                           'DTEVLLVGLEPGTR',
                           'CINQLLCK',
                           'NNLELSTPLK',
                           'DLLLPQPDLR',
                           'AGFSWIEVTFK',
                           'VFSLQWGEVK',
                           'DYFIATCK',
                           'ILSGDPYCEK',
                           'LSSGLVTAALYGR',
                           'VLFYVDSEK',
                           'VSTLPAITLK',
                           'GFLLLASLR',
                           'TIVTTLQDSIR',
                           'ASCLYGQLPK',
                           'ICLDPDAPR',
                           'VAAGAFQGLR')


temp = rawdata_train[['X']] %in% validation_index
rm(validation_index)

rawdata_train %>% .[temp,] -> validation
rawdata_train %>% .[!temp,] -> train
rm(temp)

write.csv(validation,"validation.csv")
write.csv(train,"train.csv")
write.csv(AMC,"AMC.csv")

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

train$group <- as.factor(ifelse(train$type=="PC",1,0))
validation$group <- as.factor(ifelse(validation$type=="PC",1,0))
AMC$group <- as.factor(ifelse(AMC$type=="PC",1,0))

predictors<-colnames(train)[c(-1,-2,-3,-73)]

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

for (epochs in c(20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800)){
  results_df_new<-data.frame(activation=c(),input_dropout_ratio=c(),epochs=c(),train_samples_per_iteration=c(),epsilon=c(),train_AUC=c(),val_AUC=c())
  for (ts_per_iter in hyper_params$train_samples_per_iteration){
    for (epsilon in hyper_params$epsilon){
      
      aml=h2o.deeplearning(
        training_frame=dataset.hex,
        validation_frame=valid,
        x=c("CA19_9",predictor_union_bertis),
        y="group",
        activation="Rectifier",
        hidden=c(20,20),
        epochs=epochs,
        train_samples_per_iteration = ts_per_iter,
        seed=7,
        rho=0.99,
        epsilon=epsilon,
        input_dropout_ratio = 0.3,
        variable_importances=TRUE,
        export_weights_and_biases=T,
        standardize=T,
        max_w2=10
      ) 
      results_df_new=rbind(results_df_new,data.frame(activation="Rectifier",input_dropout_ratio=0.3,epochs=epochs,train_samples_per_iteration=ts_per_iter,epsilon=epsilon,train_AUC=h2o.auc(aml,train = T),val_AUC=h2o.auc(aml,val = T)))
      print(paste0("Done for activation: ","Rectifier"," id_ratio: ",0.3, " epochs: ",epochs," ts_per_iter: ",ts_per_iter," epsilon: ",epsilon))
    }
  }
  write.csv(results_df_new,paste0("union_bertis/Rectifier_0.3_",epochs,".csv"))
}


bertisunion<-data.frame()
for (id_ratio in c(0,0.05,0.1,0.2,0.3)){
  for (epochs in hyper_params$epochs){
    bertisunion<-rbind(bertisunion,read.csv(paste0("union_bertis/Rectifier_",id_ratio,"_",epochs,".csv")))
  }
}

boxplot(aacr2021$train_AUC,patent$train_AUC,aacrpatent$train_AUC,bertisunion$train_AUC,names=c("AACR(15)","patent(15)","Union(23)","Bertis(30)"),xlab = "",main="Train AUC")

#best model
bestmodel<-tail(bertisunion[order(bertisunion$val_AUC),])[2:6,-1][5:1,]
bestmodel
bestindex<-c(1340,1019,1182,1253,856)
best<-xtable(bestmodel)
head(bestmodel)
display(best)[6:7] <- "f"
digits(best) <- 5
best

bst<-bertisunion[564,]
bst

train_tmp<-as.h2o(rbind(train,validation), destination_frame="train.hex")
test_tmp <- as.h2o(AMC, destination_frame="test.hex")
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)

dataset.hex<-as.h2o(train, destination_frame="train.hex")
valid <- as.h2o(validation, destination_frame="test.hex")
dataset.hex$group <- as.factor(dataset.hex$group)
valid$group <- as.factor(valid$group)

all_prep=h2o.deeplearning(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x=c("CA19_9",predictor_union_bertis),
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

all_prep@model$validation_metrics

h2o.sensitivity(all_prep.perf)

##ROC curve for models

list(h2o.performance(all_prep,train=T),h2o.performance(all_prep,valid=T))%>% 
  # map a function to each element in the list
  map(function(x) x %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')]) %>% 
  map2(c('Train+Validation',"AMC"),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Train+Val/AMC')

#GBM

gbm_params2 <- list(learn_rate = seq(0.01, 0.1, 0.01),
                    max_depth = seq(2, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))

dataset.hex<-as.h2o(train, destination_frame="train.hex")
valid <- as.h2o(validation, destination_frame="test.hex")
dataset.hex$group <- as.factor(dataset.hex$group)
valid$group <- as.factor(valid$group)

results_df_gbm<-data.frame(learn_rate=c(),max_depth=c(),sample_rate=c(),col_sample_rate=c(),train_AUC=c(),val_AUC=c())

for (lr in gbm_params2$learn_rate){
  for (md in gbm_params2$max_depth){
    for (sr in gbm_params2$sample_rate){
      for (csr in gbm_params2$col_sample_rate){
        aml=h2o.gbm(
          training_frame=dataset.hex,
          validation_frame=valid,
          x=predictors,
          y="group",
          learn_rate = lr,
          max_depth = md,
          sample_rate = sr,
          col_sample_rate = csr,
          seed=7
        ) 
        results_df_gbm=rbind(results_df_gbm,data.frame(learn_rate=lr,max_depth=md,sample_rate=sr,col_sample_rate=csr,train_AUC=h2o.auc(aml,train = T),val_AUC=h2o.auc(aml,val = T)))
        print(paste0("Done for learn rate: ",lr," max depth: ",md, " sample_rate: ",sr," col_sample_rate: ",csr))
        
      }
    }
  }
}

boxplot(results_df_gbm$train_AUC, results_df_gbm$val_AUC,names=c("Train AUC","Validation AUC"),xlab = "")

#best model
bestgbm<-tail(results_df_gbm[order(results_df_gbm$val_AUC),])[1:5,][5:1,]
bestgbm
bestind<-c(881,1001,941,761,1051)
best<-xtable(bestgbm)
display(best)[2:5] <- "f"
digits(best) <- 5
best

bst<-results_df_gbm[bestind[1],]

bst

train_tmp<-as.h2o(rbind(train,validation), destination_frame="train.hex")
test_tmp <- as.h2o(AMC, destination_frame="test.hex")
train_tmp$group <- as.factor(train_tmp$group)
test_tmp$group <- as.factor(test_tmp$group)

test=h2o.gbm(
  training_frame=train_tmp,
  validation_frame=test_tmp,
  x=predictors,
  y="group",
  learn_rate = 0.02,
  max_depth = 7,
  sample_rate = 0.9,
  col_sample_rate = 0.1,
  seed=7
) 

write.csv(results_df_gbm,"gbm_results.csv")
test@model$validation_metrics
test@model$training_metrics

