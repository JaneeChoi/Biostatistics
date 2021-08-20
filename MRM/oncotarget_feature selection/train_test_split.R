rawdata<-read.csv("rawdata.csv")
set.seed(210820)
library(dplyr)

PCAMC_ind=sample(1:75,60)
PCNCC_ind=sample(1:128,102)
PCSMC_ind=sample(1:101,81)
PCSNU_ind=sample(1:50,40)
PCYMC_ind=sample(1:47,38)
PBAMC_ind=sample(1:47,38)
PBSMC_ind=sample(1:30,24)
PBSNU_ind=sample(1:5,4)
PBYMC_ind=sample(1:27,22)
NLSNU_ind=sample(1:349,279)

training=data.frame()
test1=data.frame()
test2=test1
test3=test1
test4=data.frame()

training=rbind(training,rawdata[rawdata$type=="NL" | rawdata$type=="OB",][NLSNU_ind,])
test1=rbind(test1,rawdata[ rawdata$type=="NL" | rawdata$type=="OB",][-NLSNU_ind,])

write.csv(training,"train.csv")
write.csv(test1,"test1.csv")
write.csv(test2,"test2.csv")
write.csv(test3,"test3.csv")
write.csv(test4,"test4.csv")

