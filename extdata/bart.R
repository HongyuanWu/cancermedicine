#½¡¿µ  vs  ¶ñĞÔÁö  age+gender+5snps
setwd("C:/Users/liyistat/Desktop/bart/data and output")
liyi<-read.table("liyi.txt",header=TRUE)
str(liyi)
y<-liyi[,4]
sample<-liyi[,1]
gender<-liyi[,2]
age<-liyi[,3]
length(which(liyi[,5]=="CC"))
length(which(liyi[,5]=="CT"))
length(which(liyi[,5]=="TT"))
newcol5<-numeric(length(liyi[,5]))
newcol5[which(liyi[,5]=="CC")]<-0
newcol5[which(liyi[,5]=="CT")]<-1
newcol5[which(liyi[,5]=="TT")]<-2
liyi[,5]<-newcol5

length(which(liyi[,6]=="AA"))
length(which(liyi[,6]=="GA"))
length(which(liyi[,6]=="GG"))
newcol6<-numeric(length(liyi[,6]))
newcol6[which(liyi[,6]=="GG")]<-0
newcol6[which(liyi[,6]=="GA")]<-1
newcol6[which(liyi[,6]=="AA")]<-2
liyi[,6]<-newcol6

length(which(liyi[,7]=="CC"))
length(which(liyi[,7]=="CT"))
length(which(liyi[,7]=="TT"))
newcol7<-numeric(length(liyi[,7]))
newcol7[which(liyi[,7]=="CC")]<-0
newcol7[which(liyi[,7]=="CT")]<-1
newcol7[which(liyi[,7]=="TT")]<-2
liyi[,7]<-newcol7

length(which(liyi[,8]=="CC"))
length(which(liyi[,8]=="GC"))
length(which(liyi[,8]=="GG"))
newcol8<-numeric(length(liyi[,8]))
newcol8[which(liyi[,8]=="CC")]<-0
newcol8[which(liyi[,8]=="GC")]<-1
newcol8[which(liyi[,8]=="GG")]<-2
liyi[,8]<-newcol8

length(which(liyi[,9]=="TT"))
length(which(liyi[,9]=="TG"))
length(which(liyi[,9]=="GG"))
newcol9<-numeric(length(liyi[,9]))
newcol9[which(liyi[,9]=="TT")]<-0
newcol9[which(liyi[,9]=="TG")]<-1
newcol9[which(liyi[,9]=="GG")]<-2
liyi[,9]<-newcol9
str(liyi)
newliyi<-liyi[,c(2:9)]
str(newliyi)


#prediction  of no_innocent_tumor
no_innocent_tumor<-newliyi[which(newliyi[,3]!=1),]
str(no_innocent_tumor)
set.seed(10000)
n<-sample(1850)
x<-no_innocent_tumor[n[1:1665],c(1:2,4:8)]
y<-no_innocent_tumor[n[1:1665],3]
y[which(y==2)]<-1
xtest1<-no_innocent_tumor[n[1666:1850],c(1:2,4:8)]
ytest1<-no_innocent_tumor[n[1666:1850],3]
ytest1[which(ytest1==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest1)
length(xtest1)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict1 = apply(pdr,2,mean)
#AUC
library(verification)
auc1<-roc.area(ytest1,predict1)
#sensitivity,specificity
predicty<-predict1
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict1[i])
}
table1<-table(predicty,ytest1)
library(caret)
predicty1<-factor(predicty)
ref1<-factor(ytest1,levels = c(1, 0))
sensitivity1<-sensitivity(predicty1, ref1)
specificity1<-specificity(predicty1, ref1)


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
xtest2<-no_innocent_tumor[n[1481:1665],c(1:2,4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest2)
length(xtest2)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict2 = apply(pdr,2,mean)
#AUC
auc2<-roc.area(ytest2,predict2)
#sensitivity,specificity
predicty<-predict2
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict2[i])
}
table2<-table(predicty,ytest2)
library(caret)
predicty2<-factor(predicty)
ref2<-factor(ytest2,levels = c(1, 0))
sensitivity2<-sensitivity(predicty2, ref2)
specificity2<-specificity(predicty2, ref2)


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
xtest3<-no_innocent_tumor[n[1296:1480],c(1:2,4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest3)
length(xtest3)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict3 = apply(pdr,2,mean)
#AUC
auc3<-roc.area(ytest3,predict3)
#sensitivity,specificity
predicty<-predict3
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict3[i])
}
table3<-table(predicty,ytest3)
library(caret)
predicty3<-factor(predicty)
ref3<-factor(ytest3,levels = c(1, 0))
sensitivity3<-sensitivity(predicty3, ref3)
specificity3<-specificity(predicty3, ref3)


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
xtest4<-no_innocent_tumor[n[1111:1295],c(1:2,4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest4)
length(xtest4)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict4 = apply(pdr,2,mean)
#AUC
auc4<-roc.area(ytest4,predict4)
#sensitivity,specificity
predicty<-predict4
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict4[i])
}
table4<-table(predicty,ytest4)
library(caret)
predicty4<-factor(predicty)
ref4<-factor(ytest4,levels = c(1, 0))
sensitivity4<-sensitivity(predicty4, ref4)
specificity4<-specificity(predicty4, ref4)


x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
xtest5<-no_innocent_tumor[n[926:1110],c(1:2,4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest5)
length(xtest5)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict5 = apply(pdr,2,mean)
#AUC
auc5<-roc.area(ytest5,predict5)
#sensitivity,specificity
predicty<-predict5
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict5[i])
}
table5<-table(predicty,ytest5)
library(caret)
predicty5<-factor(predicty)
ref5<-factor(ytest5,levels = c(1, 0))
sensitivity5<-sensitivity(predicty5, ref5)
specificity5<-specificity(predicty5, ref5)


x<-no_innocent_tumor[n[c(1:740,926:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
xtest6<-no_innocent_tumor[n[741:925],c(1:2,4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest6)
length(xtest6)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict6 = apply(pdr,2,mean)
#AUC
auc6<-roc.area(ytest6,predict6)
#sensitivity,specificity
predicty<-predict6
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict6[i])
}
table6<-table(predicty,ytest6)
library(caret)
predicty6<-factor(predicty)
ref6<-factor(ytest6,levels = c(1, 0))
sensitivity6<-sensitivity(predicty6, ref6)
specificity6<-specificity(predicty6, ref6)



x<-no_innocent_tumor[n[c(1:555,741:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
xtest7<-no_innocent_tumor[n[556:740],c(1:2,4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest7)
length(xtest7)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict7 = apply(pdr,2,mean)
#AUC
auc7<-roc.area(ytest7,predict7)
#sensitivity,specificity
predicty<-predict7
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict7[i])
}
table7<-table(predicty,ytest7)
library(caret)
predicty7<-factor(predicty)
ref7<-factor(ytest7,levels = c(1, 0))
sensitivity7<-sensitivity(predicty7, ref7)
specificity7<-specificity(predicty7, ref7)


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
xtest8<-no_innocent_tumor[n[371:555],c(1:2,4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest8)
length(xtest8)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict8 = apply(pdr,2,mean)
#AUC
auc8<-roc.area(ytest8,predict8)
#sensitivity,specificity
predicty<-predict8
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict8[i])
}
table8<-table(predicty,ytest8)
library(caret)
predicty8<-factor(predicty)
ref8<-factor(ytest8,levels = c(1, 0))
sensitivity8<-sensitivity(predicty8, ref8)
specificity8<-specificity(predicty8, ref8)


x<-no_innocent_tumor[n[c(1:185,371:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
xtest9<-no_innocent_tumor[n[186:370],c(1:2,4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest9)
length(xtest9)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict9 = apply(pdr,2,mean)
#AUC
auc9<-roc.area(ytest9,predict9)
#sensitivity,specificity
predicty<-predict9
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict9[i])
}
table9<-table(predicty,ytest9)
library(caret)
predicty9<-factor(predicty)
ref9<-factor(ytest9,levels = c(1, 0))
sensitivity9<-sensitivity(predicty9, ref9)
specificity9<-specificity(predicty9, ref9)



x<-no_innocent_tumor[n[c(186:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
xtest10<-no_innocent_tumor[n[1:185],c(1:2,4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest10)
length(xtest10)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict10 = apply(pdr,2,mean)
#AUC
auc10<-roc.area(ytest10,predict10)
#sensitivity,specificity
predicty<-predict10
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict10[i])
}
table10<-table(predicty,ytest10)
library(caret)
predicty10<-factor(predicty)
ref10<-factor(ytest10,levels = c(1, 0))
sensitivity10<-sensitivity(predicty10, ref10)
specificity10<-specificity(predicty10, ref10)


#ave ROC
library(ROCR)
pdf("no_innocent_tumor's ave_ROC by BART.pdf")
predictions<-list(predict1,predict2,predict3,predict4,predict5,predict6,predict7,predict8,predict9,predict10)
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.bart1 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.bart1 ,col="red",lty=3,main="The ROC curve of BART's 10-fold  cross validation",cex.main=1)
plot(no_innocent_tumor_ave_perf.bart1 ,col="red",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)

#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)

#ci by  pROC
library(pROC)
#ci of auc(se,spµÈ)
ciauc1_1<-roc(ytest1,predict1, ci=TRUE, of="auc")
ciauc1_2<-roc(ytest2,predict2, ci=TRUE, of="auc")
ciauc1_3<-roc(ytest3,predict3, ci=TRUE, of="auc")
ciauc1_4<-roc(ytest4,predict4, ci=TRUE, of="auc")
ciauc1_5<-roc(ytest5,predict5, ci=TRUE, of="auc")
ciauc1_6<-roc(ytest6,predict6, ci=TRUE, of="auc")
ciauc1_7<-roc(ytest7,predict7, ci=TRUE, of="auc")
ciauc1_8<-roc(ytest8,predict8, ci=TRUE, of="auc")
ciauc1_9<-roc(ytest9,predict9, ci=TRUE, of="auc")
ciauc1_10<-roc(ytest10,predict10, ci=TRUE, of="auc")

#ACC
accuracy1<-(table1[1,1]+table1[2,2])/(table1[1,1]+table1[1,2]+table1[2,1]+table1[2,2])
accuracy2<-(table2[1,1]+table2[2,2])/(table2[1,1]+table2[1,2]+table2[2,1]+table2[2,2])
accuracy3<-(table3[1,1]+table3[2,2])/(table3[1,1]+table3[1,2]+table3[2,1]+table3[2,2])
accuracy4<-(table4[1,1]+table4[2,2])/(table4[1,1]+table4[1,2]+table4[2,1]+table4[2,2])
accuracy5<-(table5[1,1]+table5[2,2])/(table5[1,1]+table5[1,2]+table5[2,1]+table5[2,2])
accuracy6<-(table6[1,1]+table6[2,2])/(table6[1,1]+table6[1,2]+table6[2,1]+table6[2,2])
accuracy7<-(table7[1,1]+table7[2,2])/(table7[1,1]+table7[1,2]+table7[2,1]+table7[2,2])
accuracy8<-(table8[1,1]+table8[2,2])/(table8[1,1]+table8[1,2]+table8[2,1]+table8[2,2])
accuracy9<-(table9[1,1]+table9[2,2])/(table9[1,1]+table9[1,2]+table9[2,1]+table9[2,2])
accuracy10<-(table10[1,1]+table10[2,2])/(table10[1,1]+table10[1,2]+table10[2,1]+table10[2,2])

no_innocent_tumor_mean_auc1<-mean(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_sd_auc<-sd(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_mean_sensitivity<-mean(c(sensitivity1,sensitivity2,sensitivity3,sensitivity4,sensitivity5,sensitivity6,sensitivity7,sensitivity8,sensitivity9,sensitivity10))
no_innocent_tumor_sd_sensitivity<-sd(c(sensitivity1,sensitivity2,sensitivity3,sensitivity4,sensitivity5,sensitivity6,sensitivity7,sensitivity8,sensitivity9,sensitivity10))
no_innocent_tumor_mean_specificity<-mean(c(specificity1,specificity2,specificity3,specificity4,specificity5,specificity6,specificity7,specificity8,specificity9,specificity10))
no_innocent_tumor_sd_specificity<-sd(c(specificity1,specificity2,specificity3,specificity4,specificity5,specificity6,specificity7,specificity8,specificity9,specificity10))
no_innocent_tumor_mean_acc<-mean(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,accuracy9,accuracy10))
no_innocent_tumor_sd_acc<-sd(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,accuracy9,accuracy10))


#½¡¿µ  vs  ¶ñĞÔÁö(age+gender+5snps)
#no_innocent_tumor_mean_auc,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,
#no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc
x<-c(no_innocent_tumor_mean_auc1,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc)
y<-c("mean_auc","sd_auc","mean_sensitivity","sd_sensitivity","mean_specificity","sd_specificity","mean_acc","sd_acc")
no_innocent_tumor_result1<-data.frame(name=y,value=x)







#½¡¿µvs¶ñĞÔÁö  gender+5snps
#prediction  of no_innocent_tumor
no_innocent_tumor<-newliyi[which(newliyi[,3]!=1),]
str(no_innocent_tumor)
set.seed(10000)
n<-sample(1850)
x<-no_innocent_tumor[n[1:1665],c(1,4:8)]
y<-no_innocent_tumor[n[1:1665],3]
y[which(y==2)]<-1
xtest1<-no_innocent_tumor[n[1666:1850],c(1,4:8)]
ytest1<-no_innocent_tumor[n[1666:1850],3]
ytest1[which(ytest1==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest1)
length(xtest1)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict1 = apply(pdr,2,mean)
#AUC
library(verification)
auc1<-roc.area(ytest1,predict1)
#sensitivity,specificity
predicty<-predict1
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict1[i])
}
table1<-table(predicty,ytest1)
library(caret)
predicty1<-factor(predicty)
ref1<-factor(ytest1,levels = c(1, 0))
sensitivity1<-sensitivity(predicty1, ref1)
specificity1<-specificity(predicty1, ref1)


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
xtest2<-no_innocent_tumor[n[1481:1665],c(1,4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest2)
length(xtest2)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict2 = apply(pdr,2,mean)
#AUC
auc2<-roc.area(ytest2,predict2)
#sensitivity,specificity
predicty<-predict2
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict2[i])
}
table2<-table(predicty,ytest2)
library(caret)
predicty2<-factor(predicty)
ref2<-factor(ytest2,levels = c(1, 0))
sensitivity2<-sensitivity(predicty2, ref2)
specificity2<-specificity(predicty2, ref2)


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
xtest3<-no_innocent_tumor[n[1296:1480],c(1,4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest3)
length(xtest3)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict3 = apply(pdr,2,mean)
#AUC
auc3<-roc.area(ytest3,predict3)
#sensitivity,specificity
predicty<-predict3
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict3[i])
}
table3<-table(predicty,ytest3)
library(caret)
predicty3<-factor(predicty)
ref3<-factor(ytest3,levels = c(1, 0))
sensitivity3<-sensitivity(predicty3, ref3)
specificity3<-specificity(predicty3, ref3)


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
xtest4<-no_innocent_tumor[n[1111:1295],c(1,4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest4)
length(xtest4)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict4 = apply(pdr,2,mean)
#AUC
auc4<-roc.area(ytest4,predict4)
#sensitivity,specificity
predicty<-predict4
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict4[i])
}
table4<-table(predicty,ytest4)
library(caret)
predicty4<-factor(predicty)
ref4<-factor(ytest4,levels = c(1, 0))
sensitivity4<-sensitivity(predicty4, ref4)
specificity4<-specificity(predicty4, ref4)


x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
xtest5<-no_innocent_tumor[n[926:1110],c(1,4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest5)
length(xtest5)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict5 = apply(pdr,2,mean)
#AUC
auc5<-roc.area(ytest5,predict5)
#sensitivity,specificity
predicty<-predict5
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict5[i])
}
table5<-table(predicty,ytest5)
library(caret)
predicty5<-factor(predicty)
ref5<-factor(ytest5,levels = c(1, 0))
sensitivity5<-sensitivity(predicty5, ref5)
specificity5<-specificity(predicty5, ref5)


x<-no_innocent_tumor[n[c(1:740,926:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
xtest6<-no_innocent_tumor[n[741:925],c(1,4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest6)
length(xtest6)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict6 = apply(pdr,2,mean)
#AUC
auc6<-roc.area(ytest6,predict6)
#sensitivity,specificity
predicty<-predict6
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict6[i])
}
table6<-table(predicty,ytest6)
library(caret)
predicty6<-factor(predicty)
ref6<-factor(ytest6,levels = c(1, 0))
sensitivity6<-sensitivity(predicty6, ref6)
specificity6<-specificity(predicty6, ref6)



x<-no_innocent_tumor[n[c(1:555,741:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
xtest7<-no_innocent_tumor[n[556:740],c(1,4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest7)
length(xtest7)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict7 = apply(pdr,2,mean)
#AUC
auc7<-roc.area(ytest7,predict7)
#sensitivity,specificity
predicty<-predict7
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict7[i])
}
table7<-table(predicty,ytest7)
library(caret)
predicty7<-factor(predicty)
ref7<-factor(ytest7,levels = c(1, 0))
sensitivity7<-sensitivity(predicty7, ref7)
specificity7<-specificity(predicty7, ref7)


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
xtest8<-no_innocent_tumor[n[371:555],c(1,4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest8)
length(xtest8)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict8 = apply(pdr,2,mean)
#AUC
auc8<-roc.area(ytest8,predict8)
#sensitivity,specificity
predicty<-predict8
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict8[i])
}
table8<-table(predicty,ytest8)
library(caret)
predicty8<-factor(predicty)
ref8<-factor(ytest8,levels = c(1, 0))
sensitivity8<-sensitivity(predicty8, ref8)
specificity8<-specificity(predicty8, ref8)


x<-no_innocent_tumor[n[c(1:185,371:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
xtest9<-no_innocent_tumor[n[186:370],c(1,4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest9)
length(xtest9)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict9 = apply(pdr,2,mean)
#AUC
auc9<-roc.area(ytest9,predict9)
#sensitivity,specificity
predicty<-predict9
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict9[i])
}
table9<-table(predicty,ytest9)
library(caret)
predicty9<-factor(predicty)
ref9<-factor(ytest9,levels = c(1, 0))
sensitivity9<-sensitivity(predicty9, ref9)
specificity9<-specificity(predicty9, ref9)



x<-no_innocent_tumor[n[c(186:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
xtest10<-no_innocent_tumor[n[1:185],c(1,4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest10)
length(xtest10)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict10 = apply(pdr,2,mean)
#AUC
auc10<-roc.area(ytest10,predict10)
#sensitivity,specificity
predicty<-predict10
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict10[i])
}
table10<-table(predicty,ytest10)
library(caret)
predicty10<-factor(predicty)
ref10<-factor(ytest10,levels = c(1, 0))
sensitivity10<-sensitivity(predicty10, ref10)
specificity10<-specificity(predicty10, ref10)


#ave ROC
library(ROCR)
predictions<-list(predict1,predict2,predict3,predict4,predict5,predict6,predict7,predict8,predict9,predict10)
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.bart2 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.bart2 ,col="green",lty=3,add=TRUE)
plot(no_innocent_tumor_ave_perf.bart2 ,col="green",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)


#ci by  pROC
library(pROC)
#ci of auc(se,spµÈ)
ciauc2_1<-roc(ytest1,predict1, ci=TRUE, of="auc")
ciauc2_2<-roc(ytest2,predict2, ci=TRUE, of="auc")
ciauc2_3<-roc(ytest3,predict3, ci=TRUE, of="auc")
ciauc2_4<-roc(ytest4,predict4, ci=TRUE, of="auc")
ciauc2_5<-roc(ytest5,predict5, ci=TRUE, of="auc")
ciauc2_6<-roc(ytest6,predict6, ci=TRUE, of="auc")
ciauc2_7<-roc(ytest7,predict7, ci=TRUE, of="auc")
ciauc2_8<-roc(ytest8,predict8, ci=TRUE, of="auc")
ciauc2_9<-roc(ytest9,predict9, ci=TRUE, of="auc")
ciauc2_10<-roc(ytest10,predict10, ci=TRUE, of="auc")

#ACC
accuracy1<-(table1[1,1]+table1[2,2])/(table1[1,1]+table1[1,2]+table1[2,1]+table1[2,2])
accuracy2<-(table2[1,1]+table2[2,2])/(table2[1,1]+table2[1,2]+table2[2,1]+table2[2,2])
accuracy3<-(table3[1,1]+table3[2,2])/(table3[1,1]+table3[1,2]+table3[2,1]+table3[2,2])
accuracy4<-(table4[1,1]+table4[2,2])/(table4[1,1]+table4[1,2]+table4[2,1]+table4[2,2])
accuracy5<-(table5[1,1]+table5[2,2])/(table5[1,1]+table5[1,2]+table5[2,1]+table5[2,2])
accuracy6<-(table6[1,1]+table6[2,2])/(table6[1,1]+table6[1,2]+table6[2,1]+table6[2,2])
accuracy7<-(table7[1,1]+table7[2,2])/(table7[1,1]+table7[1,2]+table7[2,1]+table7[2,2])
accuracy8<-(table8[1,1]+table8[2,2])/(table8[1,1]+table8[1,2]+table8[2,1]+table8[2,2])
accuracy9<-(table9[1,1]+table9[2,2])/(table9[1,1]+table9[1,2]+table9[2,1]+table9[2,2])
accuracy10<-(table10[1,1]+table10[2,2])/(table10[1,1]+table10[1,2]+table10[2,1]+table10[2,2])

no_innocent_tumor_mean_auc2<-mean(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_sd_auc<-sd(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_mean_sensitivity<-mean(c(sensitivity1,sensitivity2,sensitivity3,sensitivity4,sensitivity5,sensitivity6,sensitivity7,sensitivity8,sensitivity9,sensitivity10))
no_innocent_tumor_sd_sensitivity<-sd(c(sensitivity1,sensitivity2,sensitivity3,sensitivity4,sensitivity5,sensitivity6,sensitivity7,sensitivity8,sensitivity9,sensitivity10))
no_innocent_tumor_mean_specificity<-mean(c(specificity1,specificity2,specificity3,specificity4,specificity5,specificity6,specificity7,specificity8,specificity9,specificity10))
no_innocent_tumor_sd_specificity<-sd(c(specificity1,specificity2,specificity3,specificity4,specificity5,specificity6,specificity7,specificity8,specificity9,specificity10))
no_innocent_tumor_mean_acc<-mean(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,accuracy9,accuracy10))
no_innocent_tumor_sd_acc<-sd(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,accuracy9,accuracy10))

#½¡¿µ  vs  ¶ñĞÔÁö(gender+5snps)
#no_innocent_tumor_mean_auc,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,
#no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc
x<-c(no_innocent_tumor_mean_auc2,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc)
y<-c("mean_auc","sd_auc","mean_sensitivity","sd_sensitivity","mean_specificity","sd_specificity","mean_acc","sd_acc")
no_innocent_tumor_result2<-data.frame(name=y,value=x)








#½¡¿µ  vs  ¶ñĞÔÁö(5snps)
#prediction  of no_innocent_tumor
no_innocent_tumor<-newliyi[which(newliyi[,3]!=1),]
str(no_innocent_tumor)
set.seed(10000)
n<-sample(1850)
x<-no_innocent_tumor[n[1:1665],c(4:8)]
y<-no_innocent_tumor[n[1:1665],3]
y[which(y==2)]<-1
xtest1<-no_innocent_tumor[n[1666:1850],c(4:8)]
ytest1<-no_innocent_tumor[n[1666:1850],3]
ytest1[which(ytest1==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest1)
length(xtest1)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict1 = apply(pdr,2,mean)
#AUC
library(verification)
auc1<-roc.area(ytest1,predict1)
#sensitivity,specificity
predicty<-predict1
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict1[i])
}
table1<-table(predicty,ytest1)
library(caret)
predicty1<-factor(predicty)
ref1<-factor(ytest1,levels = c(1, 0))
sensitivity1<-sensitivity(predicty1, ref1)
specificity1<-specificity(predicty1, ref1)


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
xtest2<-no_innocent_tumor[n[1481:1665],c(4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest2)
length(xtest2)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict2 = apply(pdr,2,mean)
#AUC
auc2<-roc.area(ytest2,predict2)
#sensitivity,specificity
predicty<-predict2
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict2[i])
}
table2<-table(predicty,ytest2)
library(caret)
predicty2<-factor(predicty)
ref2<-factor(ytest2,levels = c(1, 0))
sensitivity2<-sensitivity(predicty2, ref2)
specificity2<-specificity(predicty2, ref2)


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
xtest3<-no_innocent_tumor[n[1296:1480],c(4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest3)
length(xtest3)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict3 = apply(pdr,2,mean)
#AUC
auc3<-roc.area(ytest3,predict3)
#sensitivity,specificity
predicty<-predict3
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict3[i])
}
table3<-table(predicty,ytest3)
library(caret)
predicty3<-factor(predicty)
ref3<-factor(ytest3,levels = c(1, 0))
sensitivity3<-sensitivity(predicty3, ref3)
specificity3<-specificity(predicty3, ref3)


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
xtest4<-no_innocent_tumor[n[1111:1295],c(4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest4)
length(xtest4)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict4 = apply(pdr,2,mean)
#AUC
auc4<-roc.area(ytest4,predict4)
#sensitivity,specificity
predicty<-predict4
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict4[i])
}
table4<-table(predicty,ytest4)
library(caret)
predicty4<-factor(predicty)
ref4<-factor(ytest4,levels = c(1, 0))
sensitivity4<-sensitivity(predicty4, ref4)
specificity4<-specificity(predicty4, ref4)


x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
xtest5<-no_innocent_tumor[n[926:1110],c(4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest5)
length(xtest5)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict5 = apply(pdr,2,mean)
#AUC
auc5<-roc.area(ytest5,predict5)
#sensitivity,specificity
predicty<-predict5
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict5[i])
}
table5<-table(predicty,ytest5)
library(caret)
predicty5<-factor(predicty)
ref5<-factor(ytest5,levels = c(1, 0))
sensitivity5<-sensitivity(predicty5, ref5)
specificity5<-specificity(predicty5, ref5)


x<-no_innocent_tumor[n[c(1:740,926:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
xtest6<-no_innocent_tumor[n[741:925],c(4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest6)
length(xtest6)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict6 = apply(pdr,2,mean)
#AUC
auc6<-roc.area(ytest6,predict6)
#sensitivity,specificity
predicty<-predict6
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict6[i])
}
table6<-table(predicty,ytest6)
library(caret)
predicty6<-factor(predicty)
ref6<-factor(ytest6,levels = c(1, 0))
sensitivity6<-sensitivity(predicty6, ref6)
specificity6<-specificity(predicty6, ref6)



x<-no_innocent_tumor[n[c(1:555,741:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
xtest7<-no_innocent_tumor[n[556:740],c(4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest7)
length(xtest7)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict7 = apply(pdr,2,mean)
#AUC
auc7<-roc.area(ytest7,predict7)
#sensitivity,specificity
predicty<-predict7
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict7[i])
}
table7<-table(predicty,ytest7)
library(caret)
predicty7<-factor(predicty)
ref7<-factor(ytest7,levels = c(1, 0))
sensitivity7<-sensitivity(predicty7, ref7)
specificity7<-specificity(predicty7, ref7)


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
xtest8<-no_innocent_tumor[n[371:555],c(4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest8)
length(xtest8)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict8 = apply(pdr,2,mean)
#AUC
auc8<-roc.area(ytest8,predict8)
#sensitivity,specificity
predicty<-predict8
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict8[i])
}
table8<-table(predicty,ytest8)
library(caret)
predicty8<-factor(predicty)
ref8<-factor(ytest8,levels = c(1, 0))
sensitivity8<-sensitivity(predicty8, ref8)
specificity8<-specificity(predicty8, ref8)


x<-no_innocent_tumor[n[c(1:185,371:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
xtest9<-no_innocent_tumor[n[186:370],c(4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest9)
length(xtest9)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict9 = apply(pdr,2,mean)
#AUC
auc9<-roc.area(ytest9,predict9)
#sensitivity,specificity
predicty<-predict9
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict9[i])
}
table9<-table(predicty,ytest9)
library(caret)
predicty9<-factor(predicty)
ref9<-factor(ytest9,levels = c(1, 0))
sensitivity9<-sensitivity(predicty9, ref9)
specificity9<-specificity(predicty9, ref9)



x<-no_innocent_tumor[n[c(186:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
xtest10<-no_innocent_tumor[n[1:185],c(4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
library(BayesTree)
rb = bart(x,y,xtest10)
length(xtest10)
dim(rb$yhat.test) 
pdr = pnorm(rb$yhat.test)
predict10 = apply(pdr,2,mean)
#AUC
auc10<-roc.area(ytest10,predict10)
#sensitivity,specificity
predicty<-predict10
for(i in 1:length(predicty))
{
predicty[i]<-rbinom(1,1,predict10[i])
}
table10<-table(predicty,ytest10)
library(caret)
predicty10<-factor(predicty)
ref10<-factor(ytest10,levels = c(1, 0))
sensitivity10<-sensitivity(predicty10, ref10)
specificity10<-specificity(predicty10, ref10)

#auc1-3
no_innocent_tumor_mean_auc3<-mean(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_mean_auc1
no_innocent_tumor_mean_auc2
no_innocent_tumor_mean_auc3

#ave ROC
library(ROCR)
predictions<-list(predict1,predict2,predict3,predict4,predict5,predict6,predict7,predict8,predict9,predict10)
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.bart3 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.bart3 ,col="blue",lty=3,add=TRUE)
plot(no_innocent_tumor_ave_perf.bart3 ,col="blue",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
legend("topleft", c("10-fold CV by BART(age+gender+SNPs)", "Mean of 10-fold CV by BART(age+gender+SNPs)","10-fold CV by BART(gender+SNPs)","Mean of 10-fold CV by BART(gender+SNPs)","10-fold cv by BART(SNPs)","Mean of 10-fold CV by BART(SNPs)"),col=c("red","red","green","green","blue","blue"),cex=0.70,lty=c(3,1,3,1,3,1),lwd=1)
legend("bottomright",c("AUC*(age+gender+SNPs)=0.6490","AUC*(gender+SNPs)=0.5944","AUC*(SNPs)=0.5906"),cex=0.70)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)

#ci by  pROC
library(pROC)
#ci of auc(se,spµÈ)
ciauc3_1<-roc(ytest1,predict1, ci=TRUE, of="auc")
ciauc3_2<-roc(ytest2,predict2, ci=TRUE, of="auc")
ciauc3_3<-roc(ytest3,predict3, ci=TRUE, of="auc")
ciauc3_4<-roc(ytest4,predict4, ci=TRUE, of="auc")
ciauc3_5<-roc(ytest5,predict5, ci=TRUE, of="auc")
ciauc3_6<-roc(ytest6,predict6, ci=TRUE, of="auc")
ciauc3_7<-roc(ytest7,predict7, ci=TRUE, of="auc")
ciauc3_8<-roc(ytest8,predict8, ci=TRUE, of="auc")
ciauc3_9<-roc(ytest9,predict9, ci=TRUE, of="auc")
ciauc3_10<-roc(ytest10,predict10, ci=TRUE, of="auc")

#ACC
accuracy1<-(table1[1,1]+table1[2,2])/(table1[1,1]+table1[1,2]+table1[2,1]+table1[2,2])
accuracy2<-(table2[1,1]+table2[2,2])/(table2[1,1]+table2[1,2]+table2[2,1]+table2[2,2])
accuracy3<-(table3[1,1]+table3[2,2])/(table3[1,1]+table3[1,2]+table3[2,1]+table3[2,2])
accuracy4<-(table4[1,1]+table4[2,2])/(table4[1,1]+table4[1,2]+table4[2,1]+table4[2,2])
accuracy5<-(table5[1,1]+table5[2,2])/(table5[1,1]+table5[1,2]+table5[2,1]+table5[2,2])
accuracy6<-(table6[1,1]+table6[2,2])/(table6[1,1]+table6[1,2]+table6[2,1]+table6[2,2])
accuracy7<-(table7[1,1]+table7[2,2])/(table7[1,1]+table7[1,2]+table7[2,1]+table7[2,2])
accuracy8<-(table8[1,1]+table8[2,2])/(table8[1,1]+table8[1,2]+table8[2,1]+table8[2,2])
accuracy9<-(table9[1,1]+table9[2,2])/(table9[1,1]+table9[1,2]+table9[2,1]+table9[2,2])
accuracy10<-(table10[1,1]+table10[2,2])/(table10[1,1]+table10[1,2]+table10[2,1]+table10[2,2])

no_innocent_tumor_mean_auc3<-mean(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_sd_auc<-sd(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_mean_sensitivity<-mean(c(sensitivity1,sensitivity2,sensitivity3,sensitivity4,sensitivity5,sensitivity6,sensitivity7,sensitivity8,sensitivity9,sensitivity10))
no_innocent_tumor_sd_sensitivity<-sd(c(sensitivity1,sensitivity2,sensitivity3,sensitivity4,sensitivity5,sensitivity6,sensitivity7,sensitivity8,sensitivity9,sensitivity10))
no_innocent_tumor_mean_specificity<-mean(c(specificity1,specificity2,specificity3,specificity4,specificity5,specificity6,specificity7,specificity8,specificity9,specificity10))
no_innocent_tumor_sd_specificity<-sd(c(specificity1,specificity2,specificity3,specificity4,specificity5,specificity6,specificity7,specificity8,specificity9,specificity10))
no_innocent_tumor_mean_acc<-mean(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,accuracy9,accuracy10))
no_innocent_tumor_sd_acc<-sd(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6,accuracy7,accuracy8,accuracy9,accuracy10))
dev.off()

#½¡¿µ  vs  ¶ñĞÔÁö(5snps)
#no_innocent_tumor_mean_auc,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,
#no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc
x<-c(no_innocent_tumor_mean_auc3,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc)
y<-c("mean_auc","sd_auc","mean_sensitivity","sd_sensitivity","mean_specificity","sd_specificity","mean_acc","sd_acc")
no_innocent_tumor_result3<-data.frame(name=y,value=x)


#½¡¿µ  vs  ¶ñĞÔÁö(age+gender+5snps)
no_innocent_tumor_result1
#½¡¿µ  vs  ¶ñĞÔÁö(gender+5snps)
no_innocent_tumor_result2
#½¡¿µ  vs  ¶ñĞÔÁö(5snps)
no_innocent_tumor_result3


#½¡¿µ  vs  ¶ñĞÔÁö(age+gender+5snps)
#ci of auc
ciauc1_1$ci
ciauc1_2$ci
ciauc1_3$ci
ciauc1_4$ci
ciauc1_5$ci
ciauc1_6$ci
ciauc1_7$ci
ciauc1_8$ci
ciauc1_9$ci
ciauc1_10$ci
#½¡¿µ  vs  ¶ñĞÔÁö(gender+5snps)
#ci of auc
ciauc2_1$ci
ciauc2_2$ci
ciauc2_3$ci
ciauc2_4$ci
ciauc2_5$ci
ciauc2_6$ci
ciauc2_7$ci
ciauc2_8$ci
ciauc2_9$ci
ciauc2_10$ci
#½¡¿µ  vs  ¶ñĞÔÁö(5snps)
#ci of auc
ciauc3_1$ci
ciauc3_2$ci
ciauc3_3$ci
ciauc3_4$ci
ciauc3_5$ci
ciauc3_6$ci
ciauc3_7$ci
ciauc3_8$ci
ciauc3_9$ci
ciauc3_10$ci

