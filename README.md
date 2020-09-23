# Data-challenge
Prédiction sous R
#Training data
input=read.table("traininginputs.csv",header = T,sep = ",")
output=read.table("trainingoutput.csv",header = T,sep = ",")
data=merge(output,input,by.x = "PROC_TRACEINFO")
#Test data
test=read.table("testinputs.csv",header = T,sep = ",")
colname = test[,1]

#Remplacer les données manquées
rdata= data.frame(sapply(data[,-1],function(x) ifelse(is.na(x),round(mean(x, na.rm = TRUE),2),x)))
test= data.frame(sapply(test[,-1],function(x) ifelse(is.na(x),mean(x, na.rm = TRUE),x)))

#rename column's name & factorize la variable expliquée
colnames(rdata) = c("Resultat","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13")
colnames(test) = c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13")

#Transform variable expliquée aux factors
rdata[["Resultat"]] <- factor(rdata[["Resultat"]])

data =rdata
#Analyse descriptative
summary(data)
by(data,list(Resultat = data$Resultat),summary)
table(data$Resultat)/nrow(data)

library(lattice)
histogram(~x1 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x2 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x3 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x4 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x5 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x6 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x7 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x8 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x9 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x10 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x11 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x12 | Resultat , data = data, type="percent", col="grey",breaks=10)
histogram(~x13 | Resultat , data = data, type="percent", col="grey",breaks=10)

#Liaison des variables explicatives continues
datacors=data[,-1]
cors=round(cor(datacors),2)
print(cors)
cor.idx <- which(abs(cors) >= 0.5 & cors != 1, arr.ind = TRUE)
cor.names <- paste0(colnames(datacors)[cor.idx[,1]], "+", 
                    colnames(datacors)[cor.idx[,2]], ": ", round(cors[cor.idx], 2))
print(cor.names)

########
#1. Modèle logit avec les variables explicatives continues
#### A. La methode backward

modback <- function(db){
    logit<- glm(Resultat~., db, family=binomial(link = "logit"))
    selection <- step(logit, direction="backward", trace=TRUE, k = 2)
    selection
   }

#### B. La methode forward
modfor <- function(db){
    predicteurs <- -grep('Resultat', names(db))
    formule <- as.formula(paste("y ~ ",paste(names(db[,predicteurs]),collapse="+")))
    logit<- glm(Resultat~1,db,family=binomial(link = "logit"))
    selection <- step(logit, direction="forward", trace=TRUE, k = 2, scope=list(upper=formule))
    selection
    }
    
####Enlever variables corrélées et choisir entres les modèles, un modèle qui donne l'AIC minimum
#Modele1 choisi dans les données sans x9 et 10
modele1a <- modback(data[,-c(10,11)])
summary(modele1a)
modele1b <- modfor(data[,-c(10,11)])
summary(modele1b)
#Modele2 choisi dans les données sans x9, x12 et x13
modele2a <- modback(data[,-c(10,13,14)])
summary(modele2a)
modele2b <- modfor(data[,-c(10,13,14)])
summary(modele2b)
#Modele3 choisi dans les données sans x10 et x13
modele3a <- modback(data[,-c(11,14)])
summary(modele3a)
modele3b <- modfor(data[,-c(11,14)])
summary(modele3b)


####Vérifier AUROC sur le modèle choisi 1a
#install.packages("sampling")
db1 = data[,-c(10,11)]
library(sampling)
set.seed(123)
id <- strata(db1, stratanames="Resultat", size=c(sum(db1$Resultat==0)*2/3,sum(db1$Resultat==1)*2/3), method="srswor", description=T)$ID_unit
train  <- db1[id,]
valid  <- db1[-id,]
table(train$Resultat)/nrow(train)
table(valid$Resultat)/nrow(valid)

check1 = glm(Resultat ~ x1 + x7 + x11 + x12 + x13, family = binomial(link = "logit"), data = data)

pred.logit1 <- predict(check1, newdata=valid, type="response")
library(pROC)
auc(valid$Resultat,pred.logit1, quiet=TRUE) 

predict.test <- predict(check1, newdata=test, type="response")
predict.test = data.frame(colname,predict.test)
colnames(predict.test) = c("PROC_TRACEINFO","Binar OP130_Resultat_Global_v")
write.csv(predict.test,"testoutput1.csv",row.names = FALSE)


########
#2. Modèle logit avec les variables explicatives discrètes
data2 =rdata
#Définir les seuils pour la discrétisation 
ti = function(x, pas=0.25) {
    q <- unique(quantile(x, seq(0, 1, by=pas)))
    q[1] <- q[1]-1 
    qx <- cut(x,q)
    tab <- table(qx,data2$Resultat)
    print(prop.table(tab,1))
    barplot(prop.table(tab,1)[,2],las=3,main=deparse(substitute(x)),ylab="Taux défaut",density=0,horiz=F)
    abline(h=prop.table(table(data2$Resultat))[2],lty=2)
    }
ti(data2$x1)  
ti(data2$x2)  
ti(data2$x3) #quantile(x3)
ti(data2$x4)
ti(data2$x5)
ti(data2$x6) 
ti(data2$x7) 
ti(data2$x8) #seuil 11.97,11.98, 11.99
ti(data2$x9) 
ti(data2$x10)
ti(data2$x11) 
ti(data2$x12)
ti(data2$x13)


data2$x1 <- cut(data2$x1,c(-Inf,149,159,169,Inf),right=TRUE)
data2$x2 <- cut(data2$x2,c(-Inf,149,156,164,Inf),right=TRUE)
data2$x3 <- cut(data2$x3,c(-Inf,149.4,158.7,168.9,Inf),right=TRUE)
data2$x4 <- cut(data2$x4,c(-Inf,113,Inf),right=TRUE)
data2$x5 <- cut(data2$x5,c(-Inf,11.8,12,12.1,Inf),right=TRUE)
data2$x6 <- cut(data2$x6,c(-Inf,12.21,Inf),right=TRUE) 
data2$x7 <- cut(data2$x7,c(-Inf,0.39,Inf),right=TRUE) 
data2[["x8"]] <- factor(data2[["x8"]])
data2$x9 <- cut(data2$x9,c(-Inf,6.41,Inf),right=TRUE)
data2$x10 <- cut(data2$x10,c(-Inf,22.3,23.9,25.3,Inf),right=TRUE)
data2$x11 <- cut(data2$x11,c(-Inf,13.5,16.4,20.2,Inf),right=TRUE)
data2$x12 <- cut(data2$x12,c(-Inf,94.3,Inf),right=TRUE)
data2$x13 <- cut(data2$x13,c(-Inf,6.42,Inf),right=TRUE)

#Liaison des variables explicatives avec la variable à expliquer
cramer <- matrix(NA,ncol(data2),3)
effectif <- dim(data2)[1]
for (i in (1:ncol(data2))) {
    cramer[i,1] <- names(data2[i])
    cramer[i,2] <- sqrt(chisq.test(table(data2[,i],data2$Resultat))$statistic/effectif) # on verra plus que le package questionr permet de calculer le V de Cramer
    cramer[i,3] <- chisq.test(table(data2[,i],data2$Resultat))$p.value
    }
colnames(cramer) <- c("variable", "V de Cramer", "p-value chi2")
vcramer <- cramer [order(cramer[,2], decreasing=T),]
vcramer

#Liaisons entre les variables explicatives
#install.packages("questionr")
library(questionr)
cramer <- matrix(NA,ncol(data2),ncol(data2))
for (i in (1:ncol(data2))) { 
    for (j in (1:ncol(data2))) {cramer[i,j] <- cramer.v(table(data2[,i],data2[,j]))
    }
    }
colnames(cramer) <- colnames(data2)
rownames(cramer) <- colnames(data2)
#install.packages("corrplot")
library(corrplot)
corrplot(cramer, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
old <- par(no.readonly = TRUE)
par(omi=c(0.4,0.4,0.4,0.4))
corrplot(cramer, type="upper", tl.srt=45, tl.col="black", tl.cex=1, diag=F,
           addCoef.col="black", addCoefasPercent=T)
par(old)

####Enlever variables corrélées et choisir entres les modèles, un modèle qui donne l'AIC minimum

#Modele1 choisi dans les données sans x9 et 13
modele4a <- modback(data2[,-c(10,14)])
summary(modele4a)
modele4b <- modfor(data2[,-c(10,14)])
summary(modele4b)

#Modele2 choisi dans les données sans x5, x7 et x9
modele5a <- modback(data2[,-c(6,8,10)])
summary(modele5a)
modele5b <- modfor(data2[,-c(6,8,10)])
summary(modele5b)

#Modele3 choisi dans les données sans x5, x7 et x13
modele6a <- modback(data2[,-c(6,8,14)])
summary(modele6a)

modele6b <- modfor(data2[,-c(6,8,14)])
summary(modele6b)


####Vérifier AUROC sur le modèle choisi
#install.packages("sampling")
db2 =data2[,-c(6,8,10)]
library(sampling)
set.seed(123)
id <- strata(db2, stratanames="Resultat", size=c(sum(db2$Resultat==0)*2/3,sum(db2$Resultat==1)*2/3), method="srswor", description=T)$ID_unit
train  <- db2[id,]
valid  <- db2[-id,]
table(train$Resultat)/nrow(train)
table(valid$Resultat)/nrow(valid)

check2 = glm(Resultat ~ x4 + x10 + x12 + x13, family = binomial(link = "logit"), data = data2)
summary(check2)
pred.logit2 <- predict(check2, newdata=valid, type="response")
library(pROC)
auc(valid$Resultat,pred.logit2, quiet=TRUE) 

#######Decoupe variables explicatives du test
test$x1 <- cut(test$x1,c(-Inf,149,159,169,Inf),right=TRUE)
test$x2 <- cut(test$x2,c(-Inf,149,156,164,Inf),right=TRUE)
test$x3 <- cut(test$x3,c(-Inf,149.4,158.7,168.9,Inf),right=TRUE)
test$x4 <- cut(test$x4,c(-Inf,113,Inf),right=TRUE)
test$x5 <- cut(test$x5,c(-Inf,11.8,12,12.1,Inf),right=TRUE)
test$x6 <- cut(test$x6,c(-Inf,12.21,Inf),right=TRUE) 
test$x7 <- cut(test$x7,c(-Inf,0.39,Inf),right=TRUE) 
test[["x8"]] <- factor(test[["x8"]])
test$x9 <- cut(test$x9,c(-Inf,6.41,Inf),right=TRUE)
test$x10 <- cut(test$x10,c(-Inf,22.3,23.9,25.3,Inf),right=TRUE)
test$x11 <- cut(test$x11,c(-Inf,13.5,16.4,20.2,Inf),right=TRUE)
test$x12 <- cut(test$x12,c(-Inf,94.3,Inf),right=TRUE)
test$x13 <- cut(test$x13,c(-Inf,6.42,Inf),right=TRUE)

predict.test <- predict(check2, newdata=test, type="response")
predict.test = data.frame(colname,predict.test)
colnames(predict.test) = c("PROC_TRACEINFO","Binar OP130_Resultat_Global_v")
write.csv(predict.test,"testoutput2.csv",row.names = FALSE)

#######################
#### Reclassification de variable expliquée déséquilibrée
table(data2$Resultat) 

#La méthode ROSE pour reclassifier la variable expliquée
db3=data2
library(sampling)
set.seed(123)
id <- strata(db3, stratanames="Resultat", size=c(sum(db4$Resultat==0)*2/3,sum(db3$Resultat==1)*2/3), method="srswor", description=T)$ID_unit
train  <- db3[id,]
valid  <- db3[-id,]
table(train$Resultat)/nrow(train)
table(valid$Resultat)/nrow(valid)

install.packages("ROSE")
library(ROSE)
data.rose2 <- ROSE(Resultat ~., data = train, seed = 1)$data 
table(train$Resultat) 
table(data.rose2$Resultat) 
str(data.rose2)

#3. Modèle logit avec les variables explicatives discrètes

logit.rose1 = modback(data.rose2[,-c(10,14)]) #AIC 30420
summary(logit.rose1)
logit.rose2 = modback(data.rose2[,-c(6,8,10)]) #AIC 30470
summary(logit.rose2)
logit.rose3 = modback(data.rose2[,-c(6,8,14)]) #AIC 30750
summary(logit.rose3)

check3 = glm(Resultat ~x1 + x2 + x3 + x4 + x6 + x10 + x11 + x12 + x13, family = binomial(link = "logit"), data = data.rose2)
summary(check3)
pred.logit3 <- predict(check3, newdata=valid, type="response")
library(pROC)
auc(valid$Resultat,pred.logit3, quiet=TRUE) 


predict.test <- predict(check3, newdata=test, type="response")
predict.test = data.frame(colname,predict.test)
colnames(predict.test) = c("PROC_TRACEINFO","Binar OP130_Resultat_Global_v")
write.csv(predict.test,"testoutput3.csv",row.names = FALSE)

#################
#4. Modèle l'arbre de décision
db4=data
#install.packages("sampling")
library(sampling)
set.seed(123)
id <- strata(db4, stratanames="Resultat", size=c(sum(db4$Resultat==0)*2/3,sum(db4$Resultat==1)*2/3), method="srswor", description=T)$ID_unit
train  <- db4[id,]
valid  <- db4[-id,]
table(train$Resultat)/nrow(train)
table(valid$Resultat)/nrow(valid)

#install.packages("ROSE")
library(ROSE)
data.rose2 <- ROSE(Resultat ~., data = train, seed = 1)$data 
table(train$Resultat) 
table(data.rose2$Resultat) 
str(data.rose2)

#install.packages("rpart")
library(rpart)
treeimb <- rpart(Resultat ~ ., data = data.rose2, cp=0)
printcp(treeimb)
sink("a.txt")
sink()
#Application de la règle « 1 SE » élaguer l'arbre au niveau du plus petit arbre
xerr <- treeimb$cptable[,"xerror"]
minxerr <- which.min(xerr)
seuilerr <- treeimb$cptable[minxerr, "xerror"] + treeimb$cptable[minxerr, "xstd"]
xerr [xerr < seuilerr]
#SE1
mincp1 <- treeimb$cptable[names(xerr [xerr < seuilerr][1]),"CP"]
mincp1
prunedtreeimb1 <- prune(treeimb,cp=mincp1)

#SE0
mincp0=treeimb$cptable[which.min(treeimb$cptable[,"xerror"]),"CP"]
mincp0
prunedtreeimb0 <- prune(treeimb,cp=mincp0)

valid$treeimb1 <- predict(prunedtreeimb1, type="prob", valid)
valid$treeimb0 <- predict(prunedtreeimb0, type="prob", valid)

library(pROC)
auc(valid$Resultat, valid$treeimb1[,2]) #0.6257
auc(valid$Resultat, valid$treeimb0[,2]) #0.6196

#Calcul des erreurs et des aires sous la courbe ROC
set.seed(235)
auc <- matrix(NA,nrow(treeimb$cptable)-1,4)
for(i in 2:nrow(treeimb$cptable)){
    cartp <- prune(treeimb, cp=treeimb$cptable[i,"CP"])
    predc <- predict(cartp, type="prob", valid)[,2]
    auc[i-1,1] <- treeimb$cptable[i,"CP"]
    auc[i-1,2] <- treeimb$cptable[i,"nsplit"]+1
    auc[i-1,3] <- treeimb$cptable[i,"xerror"]
    auc[i-1,4] <- auc(valid$Resultat, predc)
    } 
colnames(auc) <- c("CP","nfeuilles","erreur","AUC")
auc

check4 <- prune(treeimb,cp=mincp1)

predict.test <- predict(check4, type="prob", newdata=test)
predict.test <- predict.test[,2]
predict.test = data.frame(colname,predict.test)
colnames(predict.test) = c("PROC_TRACEINFO","Binar OP130_Resultat_Global_v")
write.csv(predict.test,"testoutput4.csv",row.names = FALSE)





