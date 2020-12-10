library(data.table)
#library(foreign)
#library(factoextra)
#library(FactoMineR)
#library(corrplot)
library(missForest)
#library(e1071)
#library(pROC)
library(VIM)
library(mice)

data_target=fread("selected_data.csv")
data_mice=fread("mice_imputed.csv")

index=which(names(data_target)%in%c("VCF0017","VCF0018b","VCF0050b","VCF0070a","VCF0072a","VCF0110","VCF0700","VCF0702","VCF0705","VCF0707","VCF0708","VCF0712","VCF0713","VCF0714","VCF0717","VCF0718","VCF0720","VCF0731","VCF0740","VCF0742","VCF1013","VCF1014"))
#9,11,13,14,16,21,94,95,98,99,100,103,104,105,106,107,109,113,117,119,161,162

data_target=data_target[,-c(9,11,13,14,16,21,94,95,98,99,100,103,104,105,106,107,109,113,117,119,161,162)]
data_mice=data_mice[,-c(9,11,13,14,16,21,94,95,98,99,100,103,104,105,106,107,109,113,117,119,161,162)]

data_target=data.frame(data_target)

for(i in 1:ncol(data_target)){
  print(paste(names(data_target)[i],sum(is.na(data_target[,i]))))
}
size=nrow(data_target)/2
name_list=names(data_target)
remove_list=c()
for(i in 1:ncol(data_target)){
  name=name_list[i]
  if(sum(is.na(data_target[,which(names(data_target)==name)]))>=1824){
    remove_list=c(remove_list,i)
    print(paste(names(data_target)[i],(sum(is.na(data_target[,which(names(data_target)==name)])))))
  }
}
#91,133,137,138,140,150,215,246,247,249 should be removed
data_target=data_target[,-c(91,133,137,138,140,150,215,246,247,249)]
data_mice=data_mice[,-c(91,133,137,138,140,150,215,246,247,249)]

data_target=data_target[,-130]
data_mice=data_mice[,-130]

label=which(names(data_target)%in%c("VCF0704","VCF0709","VCF0710","VCF0734","VCF0736"))

names(data_target)[label]

data_target=data_target[which(is.na(data_target$VCF0704)==F&is.na(data_target$VCF0734)==F),]
data_target=data_target[,-c(90,97)]

z=missForest(data_target)

data_1_test=round(z$ximp)

write.table(data_1_test,"rf_process.csv",col.names=T,row.names=F,quote=F,sep="\t")

imp=mice(data_target,m=5)

result=complete(imp,action=5)

write.table(result,"mice_process.csv",col.names=T,row.names=F,quote=F,sep="\t")