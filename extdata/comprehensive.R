#健康vs良性瘤  健康vs恶性瘤    age+gender+5snps
setwd("C:/Users/liyistat/Desktop/svm")
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
y<-as.factor(y)
xtest1<-no_innocent_tumor[n[1666:1850],c(1:2,4:8)]
ytest1<-no_innocent_tumor[n[1666:1850],3]
ytest1[which(ytest1==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest1,decision.values = TRUE, probability = TRUE)
predict1<-as.numeric(levels(pred)[pred])
p1<-attr(pred, "probabilities")
#y=1的概率 p1[,2]
table1<-table(pred,ytest1)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity1<-sensitivity(table1)
sensitivity1<-specificity(table1)
#AUC
library(verification)
auc1<-roc.area(ytest1,p1[,2])


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest2<-no_innocent_tumor[n[1481:1665],c(1:2,4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest2,decision.values = TRUE, probability = TRUE)
predict2<-as.numeric(levels(pred)[pred])
p2<-attr(pred, "probabilities")
#y=1的概率 p2[,2]
table2<-table(pred,ytest2)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity2<-sensitivity(table2)
sensitivity2<-specificity(table2)
#AUC
library(verification)
auc2<-roc.area(ytest2,p2[,2])


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest3<-no_innocent_tumor[n[1296:1480],c(1:2,4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest3,decision.values = TRUE, probability = TRUE)
predict3<-as.numeric(levels(pred)[pred])
p3<-attr(pred, "probabilities")
#y=1的概率 p3[,2]
table3<-table(pred,ytest3)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity3<-sensitivity(table3)
sensitivity3<-specificity(table3)
#AUC
library(verification)
auc3<-roc.area(ytest3,p3[,2])


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest4<-no_innocent_tumor[n[1111:1295],c(1:2,4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest4,decision.values = TRUE, probability = TRUE)
predict4<-as.numeric(levels(pred)[pred])
p4<-attr(pred, "probabilities")
#y=1的概率 p4[,2]
table4<-table(pred,ytest4)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity4<-sensitivity(table4)
sensitivity4<-specificity(table4)
#AUC
library(verification)
auc4<-roc.area(ytest4,p4[,2])


x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest5<-no_innocent_tumor[n[926:1110],c(1:2,4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest5,decision.values = TRUE, probability = TRUE)
predict5<-as.numeric(levels(pred)[pred])
p5<-attr(pred, "probabilities")
#y=1的概率 p5[,2]
table5<-table(pred,ytest5)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity5<-sensitivity(table5)
sensitivity5<-specificity(table5)
#AUC
library(verification)
auc5<-roc.area(ytest5,p5[,2])

x<-no_innocent_tumor[n[c(1:740,926:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest6<-no_innocent_tumor[n[741:925],c(1:2,4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest6,decision.values = TRUE, probability = TRUE)
predict6<-as.numeric(levels(pred)[pred])
p6<-attr(pred, "probabilities")
#y=1的概率 p6[,2]
table6<-table(pred,ytest6)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity6<-sensitivity(table6)
sensitivity6<-specificity(table6)
#AUC
library(verification)
auc6<-roc.area(ytest6,p6[,2])


x<-no_innocent_tumor[n[c(1:555,741:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest7<-no_innocent_tumor[n[556:740],c(1:2,4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest7,decision.values = TRUE, probability = TRUE)
predict7<-as.numeric(levels(pred)[pred])
p7<-attr(pred, "probabilities")
#y=1的概率 p7[,2]
table7<-table(pred,ytest7)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity7<-sensitivity(table7)
sensitivity7<-specificity(table7)
#AUC
library(verification)
auc7<-roc.area(ytest7,p7[,2])


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest8<-no_innocent_tumor[n[371:555],c(1:2,4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest8,decision.values = TRUE, probability = TRUE)
predict8<-as.numeric(levels(pred)[pred])
p8<-attr(pred, "probabilities")
#y=1的概率 p8[,2]
table8<-table(pred,ytest8)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity8<-sensitivity(table8)
sensitivity8<-specificity(table8)
#AUC
library(verification)
auc8<-roc.area(ytest8,p8[,2])

x<-no_innocent_tumor[n[c(1:185,371:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest9<-no_innocent_tumor[n[186:370],c(1:2,4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest9,decision.values = TRUE, probability = TRUE)
predict9<-as.numeric(levels(pred)[pred])
p9<-attr(pred, "probabilities")
#y=1的概率 p9[,2]
table9<-table(pred,ytest9)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity9<-sensitivity(table9)
sensitivity9<-specificity(table9)
#AUC
library(verification)
auc9<-roc.area(ytest9,p9[,2])


x<-no_innocent_tumor[n[c(186:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest10<-no_innocent_tumor[n[1:185],c(1:2,4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest10,decision.values = TRUE, probability = TRUE)
predict10<-as.numeric(levels(pred)[pred])
p10<-attr(pred, "probabilities")
#y=1的概率 p10[,2]
table10<-table(pred,ytest10)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity10<-sensitivity(table10)
sensitivity10<-specificity(table10)
#AUC
library(verification)
auc10<-roc.area(ytest10,p10[,2])

#ave ROC
library(ROCR)
pdf("no_innocent_tumor's ave_ROC by svm.pdf")
predictions<-list(p1[,2],p2[,2],p3[,2],p4[,2],p5[,2],p6[,2],p7[,2],p8[,2],p9[,2],p10[,2])
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.svm1 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.svm1 ,col="red",lty=3,main="The ROC curve of SVM's 10-fold  cross validation",cex.main=1)
plot(no_innocent_tumor_ave_perf.svm1 ,col="red",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)


#ci by  pROC
library(pROC)
#ci of auc(se,sp等)
ciauc1_1<-roc(ytest1,p1[,2], ci=TRUE, of="auc")
ciauc1_2<-roc(ytest2,p2[,2], ci=TRUE, of="auc")
ciauc1_3<-roc(ytest3,p3[,2], ci=TRUE, of="auc")
ciauc1_4<-roc(ytest4,p4[,2], ci=TRUE, of="auc")
ciauc1_5<-roc(ytest5,p5[,2], ci=TRUE, of="auc")
ciauc1_6<-roc(ytest6,p6[,2], ci=TRUE, of="auc")
ciauc1_7<-roc(ytest7,p7[,2], ci=TRUE, of="auc")
ciauc1_8<-roc(ytest8,p8[,2], ci=TRUE, of="auc")
ciauc1_9<-roc(ytest9,p9[,2], ci=TRUE, of="auc")
ciauc1_10<-roc(ytest10,p10[,2], ci=TRUE, of="auc")



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


#健康  vs  恶性瘤(age+gender+5snps)
#no_innocent_tumor_mean_auc,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,
#no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc
x<-c(no_innocent_tumor_mean_auc1,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc)
y<-c("mean_auc","sd_auc","mean_sen","sd_sen","mean_spe","sd_spe","mean_acc","sd_acc")
no_innocent_tumor_result1<-data.frame(name=y,value=x)





#健康vs恶性瘤  gender+5snps
#prediction  of no_innocent_tumor
no_innocent_tumor<-newliyi[which(newliyi[,3]!=1),]
str(no_innocent_tumor)
set.seed(10000)
n<-sample(1850)
x<-no_innocent_tumor[n[1:1665],c(1,4:8)]
y<-no_innocent_tumor[n[1:1665],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest1<-no_innocent_tumor[n[1666:1850],c(1,4:8)]
ytest1<-no_innocent_tumor[n[1666:1850],3]
ytest1[which(ytest1==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest1,decision.values = TRUE, probability = TRUE)
predict1<-as.numeric(levels(pred)[pred])
p1<-attr(pred, "probabilities")
#y=1的概率 p1[,2]
table1<-table(pred,ytest1)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity1<-sensitivity(table1)
sensitivity1<-specificity(table1)
#AUC
library(verification)
auc1<-roc.area(ytest1,p1[,2])


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest2<-no_innocent_tumor[n[1481:1665],c(1,4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest2,decision.values = TRUE, probability = TRUE)
predict2<-as.numeric(levels(pred)[pred])
p2<-attr(pred, "probabilities")
#y=1的概率 p2[,2]
table2<-table(pred,ytest2)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity2<-sensitivity(table2)
sensitivity2<-specificity(table2)
#AUC
library(verification)
auc2<-roc.area(ytest2,p2[,2])


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest3<-no_innocent_tumor[n[1296:1480],c(1,4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest3,decision.values = TRUE, probability = TRUE)
predict3<-as.numeric(levels(pred)[pred])
p3<-attr(pred, "probabilities")
#y=1的概率 p3[,2]
table3<-table(pred,ytest3)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity3<-sensitivity(table3)
sensitivity3<-specificity(table3)
#AUC
library(verification)
auc3<-roc.area(ytest3,p3[,2])


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest4<-no_innocent_tumor[n[1111:1295],c(1,4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest4,decision.values = TRUE, probability = TRUE)
predict4<-as.numeric(levels(pred)[pred])
p4<-attr(pred, "probabilities")
#y=1的概率 p4[,2]
table4<-table(pred,ytest4)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity4<-sensitivity(table4)
sensitivity4<-specificity(table4)
#AUC
library(verification)
auc4<-roc.area(ytest4,p4[,2])


x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest5<-no_innocent_tumor[n[926:1110],c(1,4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest5,decision.values = TRUE, probability = TRUE)
predict5<-as.numeric(levels(pred)[pred])
p5<-attr(pred, "probabilities")
#y=1的概率 p5[,2]
table5<-table(pred,ytest5)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity5<-sensitivity(table5)
sensitivity5<-specificity(table5)
#AUC
library(verification)
auc5<-roc.area(ytest5,p5[,2])

x<-no_innocent_tumor[n[c(1:740,926:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest6<-no_innocent_tumor[n[741:925],c(1,4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest6,decision.values = TRUE, probability = TRUE)
predict6<-as.numeric(levels(pred)[pred])
p6<-attr(pred, "probabilities")
#y=1的概率 p6[,2]
table6<-table(pred,ytest6)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity6<-sensitivity(table6)
sensitivity6<-specificity(table6)
#AUC
library(verification)
auc6<-roc.area(ytest6,p6[,2])


x<-no_innocent_tumor[n[c(1:555,741:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest7<-no_innocent_tumor[n[556:740],c(1,4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest7,decision.values = TRUE, probability = TRUE)
predict7<-as.numeric(levels(pred)[pred])
p7<-attr(pred, "probabilities")
#y=1的概率 p7[,2]
table7<-table(pred,ytest7)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity7<-sensitivity(table7)
sensitivity7<-specificity(table7)
#AUC
library(verification)
auc7<-roc.area(ytest7,p7[,2])


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest8<-no_innocent_tumor[n[371:555],c(1,4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest8,decision.values = TRUE, probability = TRUE)
predict8<-as.numeric(levels(pred)[pred])
p8<-attr(pred, "probabilities")
#y=1的概率 p8[,2]
table8<-table(pred,ytest8)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity8<-sensitivity(table8)
sensitivity8<-specificity(table8)
#AUC
library(verification)
auc8<-roc.area(ytest8,p8[,2])

x<-no_innocent_tumor[n[c(1:185,371:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest9<-no_innocent_tumor[n[186:370],c(1,4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest9,decision.values = TRUE, probability = TRUE)
predict9<-as.numeric(levels(pred)[pred])
p9<-attr(pred, "probabilities")
#y=1的概率 p9[,2]
table9<-table(pred,ytest9)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity9<-sensitivity(table9)
sensitivity9<-specificity(table9)
#AUC
library(verification)
auc9<-roc.area(ytest9,p9[,2])


x<-no_innocent_tumor[n[c(186:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest10<-no_innocent_tumor[n[1:185],c(1,4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest10,decision.values = TRUE, probability = TRUE)
predict10<-as.numeric(levels(pred)[pred])
p10<-attr(pred, "probabilities")
#y=1的概率 p10[,2]
table10<-table(pred,ytest10)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity10<-sensitivity(table10)
sensitivity10<-specificity(table10)
#AUC
library(verification)
auc10<-roc.area(ytest10,p10[,2])

#ave ROC
library(ROCR)
predictions<-list(p1[,2],p2[,2],p3[,2],p4[,2],p5[,2],p6[,2],p7[,2],p8[,2],p9[,2],p10[,2])
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.svm2 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.svm2 ,col="green",lty=3,add=TRUE)
plot(no_innocent_tumor_ave_perf.svm2 ,col="green",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)


#ci by  pROC
library(pROC)
#ci of auc(se,sp等)
ciauc2_1<-roc(ytest1,p1[,2], ci=TRUE, of="auc")
ciauc2_2<-roc(ytest2,p2[,2], ci=TRUE, of="auc")
ciauc2_3<-roc(ytest3,p3[,2], ci=TRUE, of="auc")
ciauc2_4<-roc(ytest4,p4[,2], ci=TRUE, of="auc")
ciauc2_5<-roc(ytest5,p5[,2], ci=TRUE, of="auc")
ciauc2_6<-roc(ytest6,p6[,2], ci=TRUE, of="auc")
ciauc2_7<-roc(ytest7,p7[,2], ci=TRUE, of="auc")
ciauc2_8<-roc(ytest8,p8[,2], ci=TRUE, of="auc")
ciauc2_9<-roc(ytest9,p9[,2], ci=TRUE, of="auc")
ciauc2_10<-roc(ytest10,p10[,2], ci=TRUE, of="auc")



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

#健康  vs  恶性瘤(gender+5snps)
#no_innocent_tumor_mean_auc,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,
#no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc
x<-c(no_innocent_tumor_mean_auc2,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc)
y<-c("mean_auc","sd_auc","mean_sen","sd_sen","mean_spe","sd_spe","mean_acc","sd_acc")
no_innocent_tumor_result2<-data.frame(name=y,value=x)






#健康  vs  恶性瘤(5snps)
#prediction  of no_innocent_tumor
no_innocent_tumor<-newliyi[which(newliyi[,3]!=1),]
str(no_innocent_tumor)
set.seed(10000)
n<-sample(1850)
x<-no_innocent_tumor[n[1:1665],c(4:8)]
y<-no_innocent_tumor[n[1:1665],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest1<-no_innocent_tumor[n[1666:1850],c(4:8)]
ytest1<-no_innocent_tumor[n[1666:1850],3]
ytest1[which(ytest1==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest1,decision.values = TRUE, probability = TRUE)
predict1<-as.numeric(levels(pred)[pred])
p1<-attr(pred, "probabilities")
#y=1的概率 p1[,2]
table1<-table(pred,ytest1)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity1<-sensitivity(table1)
sensitivity1<-specificity(table1)
#AUC
library(verification)
auc1<-roc.area(ytest1,p1[,2])

x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest2<-no_innocent_tumor[n[1481:1665],c(4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest2,decision.values = TRUE, probability = TRUE)
predict2<-as.numeric(levels(pred)[pred])
p2<-attr(pred, "probabilities")
#y=1的概率 p2[,2]
table2<-table(pred,ytest2)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity2<-sensitivity(table2)
sensitivity2<-specificity(table2)
#AUC
library(verification)
auc2<-roc.area(ytest2,p2[,2])

x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest3<-no_innocent_tumor[n[1296:1480],c(4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest3,decision.values = TRUE, probability = TRUE)
predict3<-as.numeric(levels(pred)[pred])
p3<-attr(pred, "probabilities")
#y=1的概率 p3[,2]
table3<-table(pred,ytest3)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity3<-sensitivity(table3)
sensitivity3<-specificity(table3)
#AUC
library(verification)
auc3<-roc.area(ytest3,p3[,2])

x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest4<-no_innocent_tumor[n[1111:1295],c(4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest4,decision.values = TRUE, probability = TRUE)
predict4<-as.numeric(levels(pred)[pred])
p4<-attr(pred, "probabilities")
#y=1的概率 p4[,2]
table4<-table(pred,ytest4)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity4<-sensitivity(table4)
sensitivity4<-specificity(table4)
#AUC
library(verification)
auc4<-roc.area(ytest4,p4[,2])

x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest5<-no_innocent_tumor[n[926:1110],c(4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest5,decision.values = TRUE, probability = TRUE)
predict5<-as.numeric(levels(pred)[pred])
p5<-attr(pred, "probabilities")
#y=1的概率 p5[,2]
table5<-table(pred,ytest5)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity5<-sensitivity(table5)
sensitivity5<-specificity(table5)
#AUC
library(verification)
auc5<-roc.area(ytest5,p5[,2])

x<-no_innocent_tumor[n[c(1:740,926:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest6<-no_innocent_tumor[n[741:925],c(4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest6,decision.values = TRUE, probability = TRUE)
predict6<-as.numeric(levels(pred)[pred])
p6<-attr(pred, "probabilities")
#y=1的概率 p6[,2]
table6<-table(pred,ytest6)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity6<-sensitivity(table6)
sensitivity6<-specificity(table6)
#AUC
library(verification)
auc6<-roc.area(ytest6,p6[,2])

x<-no_innocent_tumor[n[c(1:555,741:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest7<-no_innocent_tumor[n[556:740],c(4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest7,decision.values = TRUE, probability = TRUE)
predict7<-as.numeric(levels(pred)[pred])
p7<-attr(pred, "probabilities")
#y=1的概率 p7[,2]
table7<-table(pred,ytest7)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity7<-sensitivity(table7)
sensitivity7<-specificity(table7)
#AUC
library(verification)
auc7<-roc.area(ytest7,p7[,2])

x<-no_innocent_tumor[n[c(1:370,556:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest8<-no_innocent_tumor[n[371:555],c(4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest8,decision.values = TRUE, probability = TRUE)
predict8<-as.numeric(levels(pred)[pred])
p8<-attr(pred, "probabilities")
#y=1的概率 p8[,2]
table8<-table(pred,ytest8)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity8<-sensitivity(table8)
sensitivity8<-specificity(table8)
#AUC
library(verification)
auc8<-roc.area(ytest8,p8[,2])

x<-no_innocent_tumor[n[c(1:185,371:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest9<-no_innocent_tumor[n[186:370],c(4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest9,decision.values = TRUE, probability = TRUE)
predict9<-as.numeric(levels(pred)[pred])
p9<-attr(pred, "probabilities")
#y=1的概率 p9[,2]
table9<-table(pred,ytest9)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity9<-sensitivity(table9)
sensitivity9<-specificity(table9)
#AUC
library(verification)
auc9<-roc.area(ytest9,p9[,2])

x<-no_innocent_tumor[n[c(186:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest10<-no_innocent_tumor[n[1:185],c(4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
#十分之九为train,十分之一为test
library(e1071)
model<-svm(x, y, probability = TRUE)
pred <-predict(model, xtest10,decision.values = TRUE, probability = TRUE)
predict10<-as.numeric(levels(pred)[pred])
p10<-attr(pred, "probabilities")
#y=1的概率 p10[,2]
table10<-table(pred,ytest10)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity10<-sensitivity(table10)
sensitivity10<-specificity(table10)
#AUC
library(verification)
auc10<-roc.area(ytest10,p10[,2])
no_innocent_tumor_mean_auc3<-mean(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_mean_auc1
no_innocent_tumor_mean_auc2
no_innocent_tumor_mean_auc3



#ave ROC
library(ROCR)
predictions<-list(p1[,2],p2[,2],p3[,2],p4[,2],p5[,2],p6[,2],p7[,2],p8[,2],p9[,2],p10[,2])
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.svm3 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.svm3 ,col="blue",lty=3,add=TRUE)
plot(no_innocent_tumor_ave_perf.svm3 ,col="blue",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
legend("topleft", c("10-fold CV by SVM(age+gender+SNPs)", "Mean of 10-fold CV by SVM(age+gender+SNPs)","10-fold CV by SVM(age+SNPs)","Mean of 10-fold CV by SVM(age+SNPs)","10-fold cv by SVM(SNPs)","Mean of 10-fold CV by SVM(SNPs)"),col=c("red","red","green","green","blue","blue"),cex=0.70,lty=c(3,1,3,1,3,1),lwd=1)
legend("bottomright",c("AUC*(age+gender+SNPs)=0.5756","AUC*(age+SNPs)=0.5699","AUC*(SNPs)=0.5494"),cex=0.70)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)


#ci by  pROC
library(pROC)
#ci of auc(se,sp等)
ciauc3_1<-roc(ytest1,p1[,2], ci=TRUE, of="auc")
ciauc3_2<-roc(ytest2,p2[,2], ci=TRUE, of="auc")
ciauc3_3<-roc(ytest3,p3[,2], ci=TRUE, of="auc")
ciauc3_4<-roc(ytest4,p4[,2], ci=TRUE, of="auc")
ciauc3_5<-roc(ytest5,p5[,2], ci=TRUE, of="auc")
ciauc3_6<-roc(ytest6,p6[,2], ci=TRUE, of="auc")
ciauc3_7<-roc(ytest7,p7[,2], ci=TRUE, of="auc")
ciauc3_8<-roc(ytest8,p8[,2], ci=TRUE, of="auc")
ciauc3_9<-roc(ytest9,p9[,2], ci=TRUE, of="auc")
ciauc3_10<-roc(ytest10,p10[,2], ci=TRUE, of="auc")

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

#健康  vs  恶性瘤(5snps)
#no_innocent_tumor_mean_auc,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,
#no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc
x<-c(no_innocent_tumor_mean_auc3,no_innocent_tumor_sd_auc,no_innocent_tumor_mean_sensitivity,no_innocent_tumor_sd_sensitivity,no_innocent_tumor_mean_specificity,no_innocent_tumor_sd_specificity,no_innocent_tumor_mean_acc,no_innocent_tumor_sd_acc)
y<-c("mean_auc","sd_auc","mean_sen","sd_sen","mean_spe","sd_spe","mean_acc","sd_acc")
no_innocent_tumor_result3<-data.frame(name=y,value=x)


#健康  vs  恶性瘤(age+gender+5snps)
no_innocent_tumor_result1
#健康  vs  恶性瘤(gender+5snps)
no_innocent_tumor_result2
#健康  vs  恶性瘤(5snps)
no_innocent_tumor_result3


#健康  vs  恶性瘤(age+gender+5snps)
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
#健康  vs  恶性瘤(gender+5snps)
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
#健康  vs  恶性瘤(5snps)
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

