#健康vs良性瘤  健康vs恶性瘤    age+gender+5snps
setwd("C:/Users/liyistat/Desktop/knn")
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
library(class)
model<-knn(x,xtest1,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty1<-factor(model)
#predicty1<-levels(predicty1)[predicty1]
#predicty1<-as.numeric(predicty1)
table1<-table(predicty1, ytest1)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity1<-sensitivity(table1)
sensitivity1<-specificity(table1)
#AUC
library(verification)
#y=1的概率
p1<-p
for(i in 1:length(predicty1))
{
if(predicty1[i]==0)
p1[i]=1-p[i]
else
p1[i]=p[i]
}
auc1<-roc.area(ytest1,p1)


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest2<-no_innocent_tumor[n[1481:1665],c(1:2,4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest2,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty2<-factor(model)
table2<-table(predicty2, ytest2)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity2<-sensitivity(table2)
sensitivity2<-specificity(table2)
#AUC
library(verification)
#y=1的概率
p2<-p
for(i in 1:length(predicty2))
{
if(predicty2[i]==0)
p2[i]=1-p[i]
else
p2[i]=p[i]
}
auc2<-roc.area(ytest2,p2)


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest3<-no_innocent_tumor[n[1296:1480],c(1:2,4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest3,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty3<-factor(model)
table3<-table(predicty3, ytest3)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity3<-sensitivity(table3)
sensitivity3<-specificity(table3)
#AUC
library(verification)
#y=1的概率
p3<-p
for(i in 1:length(predicty3))
{
if(predicty3[i]==0)
p3[i]=1-p[i]
else
p3[i]=p[i]
}
auc3<-roc.area(ytest3,p3)


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest4<-no_innocent_tumor[n[1111:1295],c(1:2,4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest4,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty4<-factor(model)
table4<-table(predicty4, ytest4)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity4<-sensitivity(table4)
sensitivity4<-specificity(table4)
#AUC
library(verification)
#y=1的概率
p4<-p
for(i in 1:length(predicty4))
{
if(predicty4[i]==0)
p4[i]=1-p[i]
else
p4[i]=p[i]
}
auc4<-roc.area(ytest4,p4)

x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest5<-no_innocent_tumor[n[926:1110],c(1:2,4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest5,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty5<-factor(model)
table5<-table(predicty5, ytest5)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity5<-sensitivity(table5)
sensitivity5<-specificity(table5)
#AUC
library(verification)
#y=1的概率
p5<-p
for(i in 1:length(predicty5))
{
if(predicty5[i]==0)
p5[i]=1-p[i]
else
p5[i]=p[i]
}
auc5<-roc.area(ytest5,p5)


x<-no_innocent_tumor[n[c(1:740,926:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest6<-no_innocent_tumor[n[741:925],c(1:2,4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest6,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty6<-factor(model)
table6<-table(predicty6, ytest6)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity6<-sensitivity(table6)
sensitivity6<-specificity(table6)
#AUC
library(verification)
#y=1的概率
p6<-p
for(i in 1:length(predicty6))
{
if(predicty6[i]==0)
p6[i]=1-p[i]
else
p6[i]=p[i]
}
auc6<-roc.area(ytest6,p6)


x<-no_innocent_tumor[n[c(1:555,741:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest7<-no_innocent_tumor[n[556:740],c(1:2,4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest7,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty7<-factor(model)
table7<-table(predicty7, ytest7)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity7<-sensitivity(table7)
sensitivity7<-specificity(table7)
#AUC
library(verification)
#y=1的概率
p7<-p
for(i in 1:length(predicty7))
{
if(predicty7[i]==0)
p7[i]=1-p[i]
else
p7[i]=p[i]
}
auc7<-roc.area(ytest7,p7)


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest8<-no_innocent_tumor[n[371:555],c(1:2,4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest8,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty8<-factor(model)
table8<-table(predicty8, ytest8)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity8<-sensitivity(table8)
sensitivity8<-specificity(table8)
#AUC
library(verification)
#y=1的概率
p8<-p
for(i in 1:length(predicty8))
{
if(predicty8[i]==0)
p8[i]=1-p[i]
else
p8[i]=p[i]
}
auc8<-roc.area(ytest8,p8)

x<-no_innocent_tumor[n[c(1:185,371:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest9<-no_innocent_tumor[n[186:370],c(1:2,4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest9,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty9<-factor(model)
table9<-table(predicty9, ytest9)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity9<-sensitivity(table9)
sensitivity9<-specificity(table9)
#AUC
library(verification)
#y=1的概率
p9<-p
for(i in 1:length(predicty9))
{
if(predicty9[i]==0)
p9[i]=1-p[i]
else
p9[i]=p[i]
}
auc9<-roc.area(ytest9,p9)


x<-no_innocent_tumor[n[c(186:1850)],c(1:2,4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest10<-no_innocent_tumor[n[1:185],c(1:2,4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest10,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty10<-factor(model)
table10<-table(predicty10, ytest10)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity10<-sensitivity(table10)
sensitivity10<-specificity(table10)
#AUC
library(verification)
#y=1的概率
p10<-p
for(i in 1:length(predicty10))
{
if(predicty10[i]==0)
p10[i]=1-p[i]
else
p10[i]=p[i]
}
auc10<-roc.area(ytest10,p10)

#ave ROC
library(ROCR)
library(RGraphics)
pdf("no_innocent_tumor's ave_ROC by knn.pdf")
predictions<-list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.knn1 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.knn1 ,col="red",lty=3,main="The ROC curve of KNN's 10-fold  cross validation",cex.main=1)
plot(no_innocent_tumor_ave_perf.knn1 ,col="red",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)

#ci by  pROC
library(pROC)
#ci of auc(se,sp等)
ciauc1_1<-roc(ytest1,p1, ci=TRUE, of="auc")
ciauc1_2<-roc(ytest2,p2, ci=TRUE, of="auc")
ciauc1_3<-roc(ytest3,p3, ci=TRUE, of="auc")
ciauc1_4<-roc(ytest4,p4, ci=TRUE, of="auc")
ciauc1_5<-roc(ytest5,p5, ci=TRUE, of="auc")
ciauc1_6<-roc(ytest6,p6, ci=TRUE, of="auc")
ciauc1_7<-roc(ytest7,p7, ci=TRUE, of="auc")
ciauc1_8<-roc(ytest8,p8, ci=TRUE, of="auc")
ciauc1_9<-roc(ytest9,p9, ci=TRUE, of="auc")
ciauc1_10<-roc(ytest10,p10, ci=TRUE, of="auc")


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
y<-c("mean_auc","sd_auc","mean_sensitivity","sd_sensitivity","mean_specificity","sd_specificity","mean_acc","sd_acc")
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
library(class)
model<-knn(x,xtest1,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty1<-factor(model)
#predicty1<-levels(predicty1)[predicty1]
#predicty1<-as.numeric(predicty1)
table1<-table(predicty1, ytest1)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity1<-sensitivity(table1)
sensitivity1<-specificity(table1)
#AUC
library(verification)
#y=1的概率
p1<-p
for(i in 1:length(predicty1))
{
if(predicty1[i]==0)
p1[i]=1-p[i]
else
p1[i]=p[i]
}
auc1<-roc.area(ytest1,p1)


x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest2<-no_innocent_tumor[n[1481:1665],c(1,4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest2,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty2<-factor(model)
table2<-table(predicty2, ytest2)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity2<-sensitivity(table2)
sensitivity2<-specificity(table2)
#AUC
library(verification)
#y=1的概率
p2<-p
for(i in 1:length(predicty2))
{
if(predicty2[i]==0)
p2[i]=1-p[i]
else
p2[i]=p[i]
}
auc2<-roc.area(ytest2,p2)


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest3<-no_innocent_tumor[n[1296:1480],c(1,4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest3,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty3<-factor(model)
table3<-table(predicty3, ytest3)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity3<-sensitivity(table3)
sensitivity3<-specificity(table3)
#AUC
library(verification)
#y=1的概率
p3<-p
for(i in 1:length(predicty3))
{
if(predicty3[i]==0)
p3[i]=1-p[i]
else
p3[i]=p[i]
}
auc3<-roc.area(ytest3,p3)


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest4<-no_innocent_tumor[n[1111:1295],c(1,4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest4,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty4<-factor(model)
table4<-table(predicty4, ytest4)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity4<-sensitivity(table4)
sensitivity4<-specificity(table4)
#AUC
library(verification)
#y=1的概率
p4<-p
for(i in 1:length(predicty4))
{
if(predicty4[i]==0)
p4[i]=1-p[i]
else
p4[i]=p[i]
}
auc4<-roc.area(ytest4,p4)

x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest5<-no_innocent_tumor[n[926:1110],c(1,4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest5,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty5<-factor(model)
table5<-table(predicty5, ytest5)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity5<-sensitivity(table5)
sensitivity5<-specificity(table5)
#AUC
library(verification)
#y=1的概率
p5<-p
for(i in 1:length(predicty5))
{
if(predicty5[i]==0)
p5[i]=1-p[i]
else
p5[i]=p[i]
}
auc5<-roc.area(ytest5,p5)


x<-no_innocent_tumor[n[c(1:740,926:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest6<-no_innocent_tumor[n[741:925],c(1,4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest6,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty6<-factor(model)
table6<-table(predicty6, ytest6)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity6<-sensitivity(table6)
sensitivity6<-specificity(table6)
#AUC
library(verification)
#y=1的概率
p6<-p
for(i in 1:length(predicty6))
{
if(predicty6[i]==0)
p6[i]=1-p[i]
else
p6[i]=p[i]
}
auc6<-roc.area(ytest6,p6)


x<-no_innocent_tumor[n[c(1:555,741:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest7<-no_innocent_tumor[n[556:740],c(1,4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest7,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty7<-factor(model)
table7<-table(predicty7, ytest7)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity7<-sensitivity(table7)
sensitivity7<-specificity(table7)
#AUC
library(verification)
#y=1的概率
p7<-p
for(i in 1:length(predicty7))
{
if(predicty7[i]==0)
p7[i]=1-p[i]
else
p7[i]=p[i]
}
auc7<-roc.area(ytest7,p7)


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest8<-no_innocent_tumor[n[371:555],c(1,4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest8,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty8<-factor(model)
table8<-table(predicty8, ytest8)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity8<-sensitivity(table8)
sensitivity8<-specificity(table8)
#AUC
library(verification)
#y=1的概率
p8<-p
for(i in 1:length(predicty8))
{
if(predicty8[i]==0)
p8[i]=1-p[i]
else
p8[i]=p[i]
}
auc8<-roc.area(ytest8,p8)

x<-no_innocent_tumor[n[c(1:185,371:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest9<-no_innocent_tumor[n[186:370],c(1,4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest9,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty9<-factor(model)
table9<-table(predicty9, ytest9)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity9<-sensitivity(table9)
sensitivity9<-specificity(table9)
#AUC
library(verification)
#y=1的概率
p9<-p
for(i in 1:length(predicty9))
{
if(predicty9[i]==0)
p9[i]=1-p[i]
else
p9[i]=p[i]
}
auc9<-roc.area(ytest9,p9)


x<-no_innocent_tumor[n[c(186:1850)],c(1,4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest10<-no_innocent_tumor[n[1:185],c(1,4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest10,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty10<-factor(model)
table10<-table(predicty10, ytest10)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity10<-sensitivity(table10)
sensitivity10<-specificity(table10)
#AUC
library(verification)
#y=1的概率
p10<-p
for(i in 1:length(predicty10))
{
if(predicty10[i]==0)
p10[i]=1-p[i]
else
p10[i]=p[i]
}
auc10<-roc.area(ytest10,p10)


#ave ROC
library(ROCR)
predictions<-list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.knn2 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.knn2 ,col="green",lty=3,add=TRUE)
plot(no_innocent_tumor_ave_perf.knn2 ,col="green",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)
#also can calculate acc


#ci by  pROC
library(pROC)
#ci of auc(se,sp等)
ciauc2_1<-roc(ytest1,p1, ci=TRUE, of="auc")
ciauc2_2<-roc(ytest2,p2, ci=TRUE, of="auc")
ciauc2_3<-roc(ytest3,p3, ci=TRUE, of="auc")
ciauc2_4<-roc(ytest4,p4, ci=TRUE, of="auc")
ciauc2_5<-roc(ytest5,p5, ci=TRUE, of="auc")
ciauc2_6<-roc(ytest6,p6, ci=TRUE, of="auc")
ciauc2_7<-roc(ytest7,p7, ci=TRUE, of="auc")
ciauc2_8<-roc(ytest8,p8, ci=TRUE, of="auc")
ciauc2_9<-roc(ytest9,p9, ci=TRUE, of="auc")
ciauc2_10<-roc(ytest10,p10, ci=TRUE, of="auc")



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
y<-c("mean_auc","sd_auc","mean_sensitivity","sd_sensitivity","mean_specificity","sd_specificity","mean_acc","sd_acc")
no_innocent_tumor_result2<-data.frame(name=y,value=x)






#健康vs良性瘤  健康vs恶性瘤    5snps
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
library(class)
model<-knn(x,xtest1,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty1<-factor(model)
#predicty1<-levels(predicty1)[predicty1]
#predicty1<-as.numeric(predicty1)
table1<-table(predicty1, ytest1)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity1<-sensitivity(table1)
sensitivity1<-specificity(table1)
#AUC
library(verification)
#y=1的概率
p1<-p
for(i in 1:length(predicty1))
{
if(predicty1[i]==0)
p1[i]=1-p[i]
else
p1[i]=p[i]
}
auc1<-roc.area(ytest1,p1)

x<-no_innocent_tumor[n[c(1:1480,1666:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1480,1666:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest2<-no_innocent_tumor[n[1481:1665],c(4:8)]
ytest2<-no_innocent_tumor[n[1481:1665],3]
ytest2[which(ytest2==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest2,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty2<-factor(model)
table2<-table(predicty2, ytest2)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity2<-sensitivity(table2)
sensitivity2<-specificity(table2)
#AUC
library(verification)
#y=1的概率
p2<-p
for(i in 1:length(predicty2))
{
if(predicty2[i]==0)
p2[i]=1-p[i]
else
p2[i]=p[i]
}
auc2<-roc.area(ytest2,p2)


x<-no_innocent_tumor[n[c(1:1295,1481:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1295,1481:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest3<-no_innocent_tumor[n[1296:1480],c(4:8)]
ytest3<-no_innocent_tumor[n[1296:1480],3]
ytest3[which(ytest3==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest3,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty3<-factor(model)
table3<-table(predicty3, ytest3)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity3<-sensitivity(table3)
sensitivity3<-specificity(table3)
#AUC
library(verification)
#y=1的概率
p3<-p
for(i in 1:length(predicty3))
{
if(predicty3[i]==0)
p3[i]=1-p[i]
else
p3[i]=p[i]
}
auc3<-roc.area(ytest3,p3)


x<-no_innocent_tumor[n[c(1:1110,1296:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:1110,1296:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest4<-no_innocent_tumor[n[1111:1295],c(4:8)]
ytest4<-no_innocent_tumor[n[1111:1295],3]
ytest4[which(ytest4==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest4,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty4<-factor(model)
table4<-table(predicty4, ytest4)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity4<-sensitivity(table4)
sensitivity4<-specificity(table4)
#AUC
library(verification)
#y=1的概率
p4<-p
for(i in 1:length(predicty4))
{
if(predicty4[i]==0)
p4[i]=1-p[i]
else
p4[i]=p[i]
}
auc4<-roc.area(ytest4,p4)

x<-no_innocent_tumor[n[c(1:925,1111:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:925,1111:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest5<-no_innocent_tumor[n[926:1110],c(4:8)]
ytest5<-no_innocent_tumor[n[926:1110],3]
ytest5[which(ytest5==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest5,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty5<-factor(model)
table5<-table(predicty5, ytest5)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity5<-sensitivity(table5)
sensitivity5<-specificity(table5)
#AUC
library(verification)
#y=1的概率
p5<-p
for(i in 1:length(predicty5))
{
if(predicty5[i]==0)
p5[i]=1-p[i]
else
p5[i]=p[i]
}
auc5<-roc.area(ytest5,p5)


x<-no_innocent_tumor[n[c(1:740,926:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:740,926:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest6<-no_innocent_tumor[n[741:925],c(4:8)]
ytest6<-no_innocent_tumor[n[741:925],3]
ytest6[which(ytest6==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest6,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty6<-factor(model)
table6<-table(predicty6, ytest6)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity6<-sensitivity(table6)
sensitivity6<-specificity(table6)
#AUC
library(verification)
#y=1的概率
p6<-p
for(i in 1:length(predicty6))
{
if(predicty6[i]==0)
p6[i]=1-p[i]
else
p6[i]=p[i]
}
auc6<-roc.area(ytest6,p6)


x<-no_innocent_tumor[n[c(1:555,741:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:555,741:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest7<-no_innocent_tumor[n[556:740],c(4:8)]
ytest7<-no_innocent_tumor[n[556:740],3]
ytest7[which(ytest7==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest7,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty7<-factor(model)
table7<-table(predicty7, ytest7)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity7<-sensitivity(table7)
sensitivity7<-specificity(table7)
#AUC
library(verification)
#y=1的概率
p7<-p
for(i in 1:length(predicty7))
{
if(predicty7[i]==0)
p7[i]=1-p[i]
else
p7[i]=p[i]
}
auc7<-roc.area(ytest7,p7)


x<-no_innocent_tumor[n[c(1:370,556:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:370,556:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest8<-no_innocent_tumor[n[371:555],c(4:8)]
ytest8<-no_innocent_tumor[n[371:555],3]
ytest8[which(ytest8==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest8,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty8<-factor(model)
table8<-table(predicty8, ytest8)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity8<-sensitivity(table8)
sensitivity8<-specificity(table8)
#AUC
library(verification)
#y=1的概率
p8<-p
for(i in 1:length(predicty8))
{
if(predicty8[i]==0)
p8[i]=1-p[i]
else
p8[i]=p[i]
}
auc8<-roc.area(ytest8,p8)

x<-no_innocent_tumor[n[c(1:185,371:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(1:185,371:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest9<-no_innocent_tumor[n[186:370],c(4:8)]
ytest9<-no_innocent_tumor[n[186:370],3]
ytest9[which(ytest9==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest9,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty9<-factor(model)
table9<-table(predicty9, ytest9)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity9<-sensitivity(table9)
sensitivity9<-specificity(table9)
#AUC
library(verification)
#y=1的概率
p9<-p
for(i in 1:length(predicty9))
{
if(predicty9[i]==0)
p9[i]=1-p[i]
else
p9[i]=p[i]
}
auc9<-roc.area(ytest9,p9)


x<-no_innocent_tumor[n[c(186:1850)],c(4:8)]
y<-no_innocent_tumor[n[c(186:1850)],3]
y[which(y==2)]<-1
y<-as.factor(y)
xtest10<-no_innocent_tumor[n[1:185],c(4:8)]
ytest10<-no_innocent_tumor[n[1:185],3]
ytest10[which(ytest10==2)]<-1
#十分之九为train,十分之一为test
library(class)
model<-knn(x,xtest10,y,k=2,prob=TRUE)
p<-attributes(model)$prob
predicty10<-factor(model)
table10<-table(predicty10, ytest10)
#sensitivity,specificity
library(caret)
#table 默认y=0为阳性结果
specificity10<-sensitivity(table10)
sensitivity10<-specificity(table10)
#AUC
library(verification)
#y=1的概率
p10<-p
for(i in 1:length(predicty10))
{
if(predicty10[i]==0)
p10[i]=1-p[i]
else
p10[i]=p[i]
}
auc10<-roc.area(ytest10,p10)
no_innocent_tumor_mean_auc3<-mean(c(auc1$A,auc2$A,auc3$A,auc4$A,auc5$A,auc6$A,auc7$A,auc8$A,auc9$A,auc10$A))
no_innocent_tumor_mean_auc1
no_innocent_tumor_mean_auc2
no_innocent_tumor_mean_auc3

#ave ROC
library(ROCR)
predictions<-list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
ytests<-list(ytest1,ytest2,ytest3,ytest4,ytest5,ytest6,ytest7,ytest8,ytest9,ytest10)
pred <- prediction(predictions, ytests)
no_innocent_tumor_ave_perf.knn3 <- performance(pred,"tpr","fpr")
plot(no_innocent_tumor_ave_perf.knn3 ,col="blue",lty=3,add=TRUE)
plot(no_innocent_tumor_ave_perf.knn3 ,col="blue",lwd=1,avg="vertical",spread.estimate="stddev",plotCI.lwd=2,add=TRUE)
legend("topleft", c("10-fold CV by knn(age+gender+SNPs)", "Mean of 10-fold CV by knn(age+gender+SNPs)","10-fold CV by knn(gender+SNPs)","Mean of 10-fold CV by knn(gender+SNPs)","10-fold cv by knn(SNPs)","Mean of 10-fold CV by knn(SNPs)"),col=c("red","red","green","green","blue","blue"),cex=0.70,lty=c(3,1,3,1,3,1),lwd=1)
legend("bottomright",c("AUC*(age+gender+SNPs)=0.6463","AUC*(gender+SNPs)=0.5572","AUC*(SNPs)=0.5589"),cex=0.70)
#calculate by ROCR
#auc.tmp <-performance(pred,"auc")
#auc<-as.numeric(auc.tmp@y.values)


#ci by  pROC
library(pROC)
#ci of auc(se,sp等)
ciauc3_1<-roc(ytest1,p1, ci=TRUE, of="auc")
ciauc3_2<-roc(ytest2,p2, ci=TRUE, of="auc")
ciauc3_3<-roc(ytest3,p3, ci=TRUE, of="auc")
ciauc3_4<-roc(ytest4,p4, ci=TRUE, of="auc")
ciauc3_5<-roc(ytest5,p5, ci=TRUE, of="auc")
ciauc3_6<-roc(ytest6,p6, ci=TRUE, of="auc")
ciauc3_7<-roc(ytest7,p7, ci=TRUE, of="auc")
ciauc3_8<-roc(ytest8,p8, ci=TRUE, of="auc")
ciauc3_9<-roc(ytest9,p9, ci=TRUE, of="auc")
ciauc3_10<-roc(ytest10,p10, ci=TRUE, of="auc")


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
y<-c("mean_auc","sd_auc","mean_sensitivity","sd_sensitivity","mean_specificity","sd_specificity","mean_acc","sd_acc")
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



