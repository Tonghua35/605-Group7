elec1=read.csv("election_1.csv")
elec2=read.csv("election_2.csv")
elec=cbind(elec1,elec2)
elec=elec[which(elec$VCF0004==2016),]

#Remove useless columns and rows
del=c()
for(i in 1:120){
  if(sum(is.na(elec[,i]))>=4000){
    del=c(del,i)
  }
}
elec=elec[,-del]
del=c()
for(i in 1:4270){
  if(is.na(elec$VCF0702[i])){
    del=c(del,i)
  }
}
elec=elec[-del,]

#Regression method for prediction
k=c(6,8,9,11,14,18,19,21,22,23,24:32,33,34,35,37:40,48:51,57,75:78)
elec_reg=elec[which(is.na(elec$VCF0704)==F),k]
for(i in 1:35){
  m=mean(elec_reg[,i],na.rm=T)
  for(j in 1:2609){
    if(is.na(elec_reg[j,i])){
      elec_reg[j,i]=m
    }
  }
}
train=elec_reg[1:2000,]
test=elec_reg[2001:2609,]

lm1=lm(VCF0704~.,data=train)
summary(lm1)
lm2=step(lm(VCF0704~1,data=train),scope=formula(lm1),direction='both',trace=F)
summary(lm2)
anova(lm2)
plot(lm2$residuals,ylab='residuals')
plot(lm2$fitted.values,lm2$residuals,xlab='fitted values',ylab='residuals')

fit=predict(lm2,test[,-31])
fit=fit<1.5
a=sum(test$VCF0704==1 & fit==T)
c=sum(test$VCF0704==2 & fit==T)
b=sum(test$VCF0704==1 & fit==F)
d=sum(test$VCF0704==2 & fit==F)

mat=matrix(c(a,b,c,d),ncol=2)
colnames(mat)=c('Democrat','Republican')
rownames(mat)=c('pred_Democrat','pred_Republican')
mat
accuracy=(a+d)/609
accuracy

#Tree method

train$VCF0704=factor(train$VCF0704)
library(rpart)
tc<-rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=5,cp=0.005)
rpart.mod=rpart(VCF0704~.,data=train,method="class",
                parms = list(prior = c(0.5,0.5), split = "gini"),
                control=tc)
summary(rpart.mod)
rpart.mod$variable.importance
rpart.mod$cp
plotcp(rpart.mod)
library(rpart.plot)
rpart.plot(rpart.mod,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Tree")
rpart.mod.pru<-prune(rpart.mod, cp= rpart.mod$cptable[which.min(rpart.mod$cptable[,"xerror"]),"CP"]) 
rpart.mod.pru$cp
rpart.plot(rpart.mod.pru,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Tree_1")
rpart.pred<-predict(rpart.mod.pru,test[,-31])
pred<-ifelse(rpart.pred[,2]>0.5,2,1)

a=sum(test$VCF0704==1 & pred==1)
c=sum(test$VCF0704==2 & pred==1)
b=sum(test$VCF0704==1 & pred==2)
d=sum(test$VCF0704==2 & pred==2)

mat=matrix(c(a,b,c,d),ncol=2)
colnames(mat)=c('Democrat','Republican')
rownames(mat)=c('pred_Democrat','pred_Republican')
mat
accuracy=(a+d)/609
accuracy

#ANOVA to search for factors
elec_1=elec[which(is.na(elec$VCF0704)==F),]

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0017))) #interview mode

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0050a))) #level of political info
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0050a)),mean)

summary(lm(elec_1$VCF0704~elec_1$VCF0101)) #Age

t.test(elec_1$VCF0704[which(elec_1$VCF0104==1)],elec_1$VCF0704[which(elec_1$VCF0104==2)]) #Gender

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0110))) #education
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0110)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0112))) #region
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0112)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0113))) #south
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0113)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0114))) #income
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0114)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0118))) #work
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0118)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0127))) #labor union
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0127)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0128))) #religion
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0128)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0143))) #Native born
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0143)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0146))) #home owner
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0146)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0147))) #marital
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0147)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0148))) #social class
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0148)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF0156))) #work laid off
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF0156)),mean)

anova(lm(elec_1$VCF0704~factor(elec_1$VCF9027))) #previous elections
aggregate(elec_1$VCF0704,list(factor(elec_1$VCF9027)),mean)

#Prediction by basic information
elec3=read.csv('election.csv')
elec3=elec3[,c(3,25,27,29,33,35,38,39,45:50,53,65,68,72,92,95:97,109,110,503,505)]
elec4=elec3[which(elec3$VCF0004==2016),]

#Remove useless columns and rows
del=c()
for(i in 1:26){
  if(sum(is.na(elec4[,i]))>=4000){
    del=c(del,i)
  }
}
elec4=elec4[,-del]
del=c()
for(i in 1:4270){
  if(is.na(elec4$VCF0702[i])){
    del=c(del,i)
  }
}
elec4=elec4[-del,]

#Regression method for prediction
k=c(2,3,5,6,8,10,11,13,15,16,17,19,20,21,23)
elec_reg=elec4[which(is.na(elec4$VCF0704)==F),k]
for(i in 1:15){
  m=mean(elec_reg[,i],na.rm=T)
  for(j in 1:2609){
    if(is.na(elec_reg[j,i])){
      elec_reg[j,i]=m
    }
  }
}
train=elec_reg[1:2000,]
test=elec_reg[2001:2609,]

lm1=lm(VCF0704~.,data=train)
summary(lm1)
lm2=step(lm(VCF0704~1,data=train),scope=formula(lm1),direction='both',trace=F)
summary(lm2)
anova(lm2)
plot(lm2$residuals,ylab='residuals')
plot(lm2$fitted.values,lm2$residuals,xlab='fitted values',ylab='residuals')

fit=predict(lm2,test[,-15])
fit=fit<1.5
a=sum(test$VCF0704==1 & fit==T)
c=sum(test$VCF0704==2 & fit==T)
b=sum(test$VCF0704==1 & fit==F)
d=sum(test$VCF0704==2 & fit==F)

mat=matrix(c(a,b,c,d),ncol=2)
colnames(mat)=c('Democrat','Republican')
rownames(mat)=c('pred_Democrat','pred_Republican')
mat
accuracy=(a+d)/609
accuracy

#Tree method
k=c(2:21,23)
elec_tree=elec4[which(is.na(elec4$VCF0704)==F),k]
for(i in c(3,6,8,11,13,17)){elec_tree[,i]=factor(elec_tree[,i])}

train=elec_tree[1:2000,]
test=elec_tree[2001:2609,]

train$VCF0704=factor(train$VCF0704)
library(rpart)
tc<-rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=5,cp=0.005)
rpart.mod=rpart(VCF0704~.,data=train,method="class",
                parms = list(prior = c(0.5,0.5), split = "gini"),
                control=tc)
summary(rpart.mod)
rpart.mod$variable.importance
rpart.mod$cp
plotcp(rpart.mod)
library(rpart.plot)
rpart.plot(rpart.mod,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Tree")
rpart.mod.pru<-prune(rpart.mod, cp= rpart.mod$cptable[which.min(rpart.mod$cptable[,"xerror"]),"CP"]) 
rpart.mod.pru$cp
rpart.plot(rpart.mod.pru,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Tree")
rpart.pred<-predict(rpart.mod.pru,test[,-21])
pred<-ifelse(rpart.pred[,2]>0.5,2,1)

a=sum(test$VCF0704==1 & pred==1)
c=sum(test$VCF0704==2 & pred==1)
b=sum(test$VCF0704==1 & pred==2)
d=sum(test$VCF0704==2 & pred==2)

mat=matrix(c(a,b,c,d),ncol=2)
colnames(mat)=c('Democrat','Republican')
rownames(mat)=c('pred_Democrat','pred_Republican')
mat
accuracy=(a+d)/609
accuracy
