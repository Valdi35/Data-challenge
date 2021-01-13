#############################################################################################
#############" projet de datamining- challenge QRT###########################################
#############################################################################################

#prediction de la performanance des actions cotées en bourse
#packages utilisés
install.packages("Amelia")
install.packages("tidyverse")
install.packages('funModeling')
install.packages("devtools")

# IMPORTATION DES DONNEES
y_train = read.table("y_train_JQU4vbI.csv",header = T,sep = ",")
x_train = read.table("x_train_Lafd4AH.csv",header = T,sep = ",")
x_test = read.table("x_test_c7ETL4q.csv",header = T,sep = ",")

#fusion du fichier x_train et y_train
data = merge(x_train,y_train)

# pour connaitre la dimension, le nom des variables et leur types
dim(data)
str(data)


#PREPARATION DE LA BASE DE DONNEES DE TRAVAIL


#pour connaitre le nombre de valeurs differentes dans chaque colonnes
sapply(y_train, function(x) length(unique(x)))
sapply(x_train, function(x) length(unique(x)))
sapply(data, function(x) length(unique(x)))


# compter le nombre de valeurs manquantes dans chaque colonnes de notre base
sapply(data,function(x) sum(is.na(x)))


#afficher un graphe representant les valeurs manquantes et les valeurs observées
library(Amelia)
missmap(data, main = "Missing values vs observed")
# on a 7% de valeurs manquante dans l'échantillon

# imputation des valeurs manquantes par la mediane dans toutes les variables avec données manquantes sauf la variable expliquée
for (i in (1:ncol(data[,-48])))
{
  data[,i][which(is.na(data[,i]))] <-median(data[,i],na.rm=T)
}

#verification que nos valeurs manquantes ont bien été remplacées par la médiane
sapply(data,function(x) sum(is.na(x)))

# LA VARIABLE A EXPLIQUER

data$RET = as.character(data$RET)
data$RET[data$RET == "False"] <- 0 # RET false
data$RET[data$RET == "True"] <- 1 # RET true
data$RET <- factor(data$RET)
table(data$RET)

# mise en facteur des variables SECTOR, INDUSTRY, INDUSTRY_GROUP et SUB_INDUSTRY
data$SECTOR <- factor(data$SECTOR)
data$INDUSTRY_GROUP <-factor(data$INDUSTRY_GROUP)
data$INDUSTRY <- factor(data$INDUSTRY)
data$SUB_INDUSTRY <- factor(data$SUB_INDUSTRY)

#verifions que la variable RET est bien un facteur avec les modalités 0 et 1 dans la base data1
str(data)
dim(data)

#1ere base de travail
data1 = data


#STATISTQUES DESCRIPTIVES
# statistiques de base

# qualité des données
data_integrity(data1)
status(data1)
#pour voir les differents vecteurs de noms des variables, savoir il ya combien de variables character, numerique, facteur, combien de valeurs manquantes etc

summary(data1)
library(Hmisc)
hist.data.frame(data1[,1:48])
library(stargazer)
stargazer(data1, type = "text")

# pour les variables quantitatives

#histogramme
par(mfrow = c(1,1))
plot(hist(data1$STOCK), main = 'RET_1' , col ='blue')
plot(hist(data1$RET_1), main = 'RET_1' , col ='blue')
plot(hist(data1$VOLUME_1), main = 'volume_1' , col ='blue')
plot(hist(data1$RET_2), main = 'RET_2' , col ='blue')
plot(hist(data1$VOLUME_2), main = 'volume_2' , col ='blue')
plot(hist(data1$RET_3), main = 'RET_3' , col ='blue')
plot(hist(data1$VOLUME_3), main = 'volume_3' , col ='blue')
plot(hist(data1$RET_4), main = 'RET_4' , col ='blue')
plot(hist(data1$VOLUME_4), main = 'volume_4' , col ='blue')
plot(hist(data1$RET_5), main = 'RET_5' , col ='blue')
plot(hist(data1$VOLUME_5), main = 'volume_5' , col ='blue')
plot(hist(data1$RET_6), main = 'RET_6' , col ='blue')
plot(hist(data1$VOLUME_6), main = 'volume_6' , col ='blue')
plot(hist(data1$RET_7), main = 'RET_7' , col ='blue')
plot(hist(data1$VOLUME_7), main = 'volume_7' , col ='blue')
plot(hist(data1$RET_8), main = 'RET_8' , col ='blue')
plot(hist(data1$VOLUME_8), main = 'volume_8' , col ='blue')
plot(hist(data1$RET_9), main = 'RET_9' , col ='blue')
plot(hist(data1$VOLUME_9), main = 'volume_9' , col ='blue')
plot(hist(data1$RET_10), main = 'RET_10' , col ='blue')
plot(hist(data1$VOLUME_10), main = 'volume_10' , col ='blue')
plot(hist(data1$RET_11), main = 'RET_11' , col ='blue')
plot(hist(data1$VOLUME_11), main = 'volume_11' , col ='blue')
plot(hist(data1$RET_12), main = 'RET_12' , col ='blue')
plot(hist(data1$VOLUME_12), main = 'volume_12' , col ='blue')
plot(hist(data1$RET_13), main = 'RET_13' , col ='blue')
plot(hist(data1$VOLUME_13), main = 'volume_13' , col ='blue')
plot(hist(data1$RET_14), main = 'RET_14' , col ='blue')
plot(hist(data1$VOLUME_14), main = 'volume_14' , col ='blue')
plot(hist(data1$RET_15), main = 'RET_15' , col ='blue')
plot(hist(data1$VOLUME_15), main = 'volume_15' , col ='blue')
plot(hist(data1$RET_16), main = 'RET_16' , col ='blue')
plot(hist(data1$VOLUME_16), main = 'volume_16' , col ='blue')
plot(hist(data1$RET_17), main = 'RET_17' , col ='blue')
plot(hist(data1$VOLUME_17), main = 'volume_17' , col ='blue')
plot(hist(data1$RET_18), main = 'RET_18' , col ='blue')
plot(hist(data1$VOLUME_18), main = 'volume_18' , col ='blue')
plot(hist(data1$RET_19), main = 'RET_19' , col ='blue')
plot(hist(data1$VOLUME_19), main = 'volume_19' , col ='blue')
plot(hist(data1$RET_20), main = 'RET_20' , col ='blue')
plot(hist(data1$VOLUME_20), main = 'volume_20' , col ='blue')


#Boxplot

boxplot(data1[,8:10])
boxplot(data1[,11:14])
boxplot(data1[,15:18])
boxplot(data1[,19:22])
boxplot(data1[,23:26])
boxplot(data1[,27:30])
boxplot(data1[,31:33])
boxplot(data1[,34:37])
boxplot(data1[,38:41])
boxplot(data1[,42:45])
boxplot(data1[,45:47])

#histogramme avec fonction de densité (remplacer volume_1 par le nom des variables quantitatives qu'ont veut voir)

hist(data1$VOLUME_1, nclass=25, prob=TRUE, col="cornflowerblue", border="white",
     xlim=c(10,120), main="", xlab="VOLUME_1", ylab="Densité")
lines(density(data1$VOLUME_1, na.rm=TRUE), lwd=2, col="orange")
text(20, 0.015, paste("N =", sum(complete.cases(data1$VOLUME_1))), cex=0.8)

#pour les variables qualitatives

library(funModeling)
# fréquence des variables qualitatives
freq(data1)
freq(data1$SUB_INDUSTRY)
categ_analysis(data = data1, target = "RET")
# la frequence de la variable sub_industry ne s'affiche pas car elle a plus de 100 modalités

#statistiques descriptives par groupe des variables quantitatives

by(data1[,c("RET_1" ,"VOLUME_1" ,"RET_2" , "VOLUME_2","RET_3","VOLUME_3","RET_4","VOLUME_4","RET_5","VOLUME_5" )], list(RET=data1$RET), summary)
by(data1[,8:47], list(RET=data1$RET), summary)

# distribution des variables quantitatives
?plot_num
plot_num(data1[,8:47], bins=20)

#correlation entre les données numeriques de notre base
library(corrplot)
vcor = cor(data1[,8:47 ])
corrplot(vcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


#la majorité des variables ont les mêmes histogrammes et distribution. ainsi au vue de tout ce qui précède,
#notre choix des variables s'est porté sur les 10 variables les plus recentes. analysons donc de plus près ces variables. 


#ANALYSE BIVARIEE AVEC LES RECENTES VALEURS SELECTIONNEES DE LA BASE DE DONNEES

#histogramme des variables quantitatives avec la variable expliquée

# histogramme
library(lattice)
histogram(~RET_1 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~RET_2 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~RET_3 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~RET_4 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~RET_5 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~VOLUME_1 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~VOLUME_2 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~VOLUME_3 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~VOLUME_4 | RET , data = data1, type="percent", col="grey", breaks=10)
histogram(~VOLUME_5 | RET , data = data1, type="percent", col="grey", breaks=10)


#matrice de nuage de poinst avec les variables retenues
pairs(data1[, c("RET_1","VOLUME_1","RET_2","VOLUME_2","RET_3","VOLUME_3","RET_4","VOLUME_4","RET_5","VOLUME_5")])
#la matrice nous montre qu'il n y a pas vraiment de rélation entre nos variables explicatives. 


#on va verifier la correlation entre ces variables
library(corrplot)
vcor = cor(data1[, c("RET_1","VOLUME_1","RET_2","VOLUME_2","RET_3","VOLUME_3","RET_4","VOLUME_4","RET_5","VOLUME_5")])
corrplot(vcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(vcor, abbr.colnames=FALSE)
#ces resulats montre qu'il y a une corrélation positive situé entre 0,3 et 0,6 entre les variables volume_1 et volume_2, volume_3 et volume_2
# et Volume_5 et volume_4.
# on remarque donc que globalement, il n'ya pas de corrélation significatives entre les variables quantitatives.

#RELATION ENTRE VARIABLES CONTINUES ET VARIABLE EXPLIQUEE
#test de Kruskal wallis
kruskal.test(data1$RET_1~data$RET)$statistic
kruskal.test(data1$RET_2~data$RET)$statistic
kruskal.test(data1$RET_3~data$RET)$statistic
kruskal.test(data1$RET_4~data$RET)$statistic
kruskal.test(data1$RET_5~data$RET)$statistic
kruskal.test(data1$VOLUME_1~data$RET)$statistic
kruskal.test(data1$VOLUME_2~data$RET)$statistic
kruskal.test(data1$VOLUME_3~data$RET)$statistic
kruskal.test(data1$VOLUME_4~data$RET)$statistic
kruskal.test(data1$VOLUME_5~data$RET)$statistic

#on remarque que les variables les plus corrélées à la variables expliquée sont : VOLUME_1, RET_1,VOLUME_5,VOLUME_3,VOLUME_4,RET_2

# fonction d'affichage du RET par quantile
ti = function(x, y, pas=0.1)
{
  (newOmi <- par()$omi)    
  newOmi[1] <- 1
  old <- par(no.readonly = TRUE)   
  par(omi=newOmi)
  q <- unique(quantile(x, seq(0, 1, by=pas), na.rm=T))
  q[1] <- q[1]-1 
  qx <- cut(x,q)
  tab <- table(qx, y, exclude=NULL)
  print(prop.table(tab,1)) 
  barplot(prop.table(tab,1)[,2], las=3, main=deparse(substitute(x)), ylab="RET", density=0, horiz=F)
  abline(h=prop.table(table(y))[2], lty=2)
  par(old)
}

ti(data1$RET_1, data1$RET)
ti(data1$RET_2, data1$RET)
ti(data1$RET_3, data1$RET)
ti(data1$RET_4, data1$RET)
ti(data1$RET_5, data1$RET)
ti(data1$VOLUME_1, data1$RET)
ti(data1$VOLUME_2, data1$RET)
ti(data1$VOLUME_3, data1$RET)
ti(data1$VOLUME_4, data1$RET)
ti(data1$VOLUME_5, data1$RET)

#les differents graphique du croisement des variables continues et de la variable expliquée montrent qu'on ne peut pas vraiment discrétiser celles-ci


#LIAISON ENTRE LES VARIABLES QUALITATIVES

library(ggplot2)
library(tidyverse)
library(dplyr)

qual = data1[,c("INDUSTRY" , "INDUSTRY_GROUP" ,"SECTOR" , "SUB_INDUSTRY", "RET")]
str(qual)
bar <- ggplot (qual, aes(x = SECTOR, fill = INDUSTRY_GROUP)) 
bar + geom_bar (position = "stack")
bar <- ggplot (qual, aes(x = SECTOR, fill = INDUSTRY)) 
bar + geom_bar (position = "stack")
bar <- ggplot (qual, aes(x = INDUSTRY, fill = SUB_INDUSTRY)) 
bar + geom_bar (position = "stack")
bar <- ggplot (qual, aes(x = INDUSTRY_GROUP, fill = SUB_INDUSTRY)) 
bar + geom_bar (position = "stack")
#ces graphiques nous montrent le lien qu'il y a entre ces variables qualitatives. 


#RELATION ENTRE VARIABLES QUALITATIVES ET VARIABLE EXPLIQUEE

#test de KHI 2 entre les variables qualitatives et la variable expliquée
library(questionr)
chisq.test(data1$RET, data1$SECTOR)
chisq.test(data1$RET, data1$INDUSTRY)
chisq.test(data1$RET, data1$INDUSTRY_GROUP)
chisq.test(data1$RET, data1$SUB_INDUSTRY)
# tout les p_value sont < 0,05.ON peut donc conclure qu'il ya un lien entre la variable expliquée et les variables qualitatives

# le V de cramer entre les variables qualitatives et la variable expliquée

cramer  <- matrix(NA,ncol(qual),3)
effectif <- dim(qual)[1]
for (i in (1:ncol(qual)))
{   cramer[i,1] <- names(qual[i])
cramer[i,2] <- sqrt(chisq.test(table(qual[,i],qual$RET))$statistic/effectif)
cramer[i,3] <- chisq.test(table(qual[,i],qual$RET))$p.value
}
colnames(cramer) <- c("variable","V de Cramer","p-value chi2")

# affichage des variables par V de Cramer décroissants
vcramer <- cramer [order(cramer[,2], decreasing=T),]
vcramer
# graphique
old <- par(no.readonly = TRUE)
par(mar = c(8, 4, 4, 0))
barplot(as.numeric(vcramer[-1,2]),col=gray(0:nrow(vcramer)/nrow(vcramer)),
        names.arg=vcramer[-1,1], ylab='V de Cramer', ylim=c(0,0.35),cex.names = 0.8, las=3)
par(old)
 
#le graphique de cramer montre que les variables les plus corrélées à la variable expliquée sont dans l'ordre suivant:  "SUB_INDUSTRY" "INDUSTRY" "INDUSTRY_GROUP" et "SECTOR"    


# fonction de sélection de variables sur la base de la significativité des coefficients des modèles sur des échantillons bootstrap
#creation d'une base avec nos 10 variables recentes et nos variables qualitatives
base = data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,48)]

library(boot)
fonction <- function(data, i){
  d <- data[i,]
  logit <- glm(RET ~ ., data=d, family=binomial(link = "logit"))
  return((summary(logit)$coefficients[,4] < 0.05))
}
fonction(base,1:1000)
set.seed(123)
(resultat <- boot(data=base, statistic=fonction, R=100))
resultat$t0
resultat$t
apply(resultat$t,2,sum)
as.matrix(cbind(resultat$t0, apply(resultat$t,2,sum)))
# malhereusement notre algorithme ne converge pas avec un nombre de ligne =1000. avec un plus grand on n'y arrive pas car le logiciel nous envoie un message d'erreur "impossible d'allouer un vecteur de taille 19.1 Go "


#avec le critère de cramer et celui de Kruskal wallis, créons de nouvelle variable

#CREATION DE NOUVELLE VARIABLE


#comme dans le benchmarK, créons une variable qui combine le secteur la date et ret_1
df1 <- setNames(aggregate(RET_1 ~ SECTOR+DATE, data = base, FUN = mean),c("SECTOR","DATE","RET_1.par.sec.et.date"))
base <- merge(base,df1,by=c("SECTOR","DATE"))

#créons une variable entre le volume_1 et le sub_industry les 2 variables qui explique le plus notre variable expliquée
df2 <- setNames(aggregate(VOLUME_1 ~ SUB_INDUSTRY, data = base, FUN = mean),c("SUB_INDUSTRY","vol1.sub_ind"))
base <- merge(base,df2,by=c("SUB_INDUSTRY"))

#créons une autre variable entre ret_1 et group_industry
df3 <- setNames(aggregate(RET_1 ~ INDUSTRY_GROUP, data = base, FUN = mean),c("INDUSTRY_GROUP","RET_1.indus.group"))
base <- merge(base,df3,by=c("INDUSTRY_GROUP"))

#entre volume1 et secteur (si le volume echangés des actions par secteur a un impact sur le rendement de l'action)
df4 <- setNames(aggregate(VOLUME_1 ~ SECTOR, data = base, FUN = mean),c("SECTOR","vol1.sec"))
base <- merge(base,df4,by=c("SECTOR"))

#creons une variable entre le RET_1 et le sub_industry
df5 <- setNames(aggregate(RET_1 ~ SUB_INDUSTRY, data = base, FUN = mean),c("SUB_INDUSTRY","RET1.sub_ind"))
base <- merge(base,df5,by=c("SUB_INDUSTRY"))


#test de kruskal wallis pour voir la correlation entre ret et les nouvelles variables crées
kruskal.test(RET_1.par.sec.et.date~RET, data=base)
kruskal.test(vol1.sub_ind~RET, data=base)
kruskal.test(RET_1.indus.group~RET, data=base)
kruskal.test(vol1.sec~RET, data=base)
kruskal.test(RET1.sub_ind~RET, data=base)


##on va verifier la correlation entre ces variables
library(corrplot)
mcor = cor(base[,-c(1,2,3,4,5,6,7,18)])
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)
#on remarque que RET_1.indus.group et RET1.sub_ind sont très corrélés d'ou on choisira un entre les 2 celui qui offre le plus grand kruskal donc volume RET1.sub_ind

#NOTRE BASE DE DONNEE DEFINITIVE

#ordonner la base selon ID
base<-base[order(base[,5],decreasing=F),]

#base definitive de travail
base1=base[,-c(1,2,3,4,6,7,21)]
colnames(base1)
nom=base1[,1]
base1=base1[,-1]
rownames(base1)=nom


#ECHANTILLONNAGE DE D'APPRENTISSAGE ET DE VALIDATION

library(sampling)
set.seed(123)
id <- strata(base1, stratanames="RET", size=c(sum(base1$RET==0)*2/3,sum(base1$RET==1)*2/3), method="srswor", description=T)$ID_unit
train  <- base1[id,]
valid  <- base1[-id,]
table(train$RET)/nrow(train)
table(valid$RET)/nrow(valid)

######################################################################################################################################################"

#REGRESSSION LOGISTIQUE

# modèle logit
logit <- logit <- glm(RET~., data = train, family=binomial(link = "logit"))
summary(logit)
summary(logit)$coefficients
#on remarque que les variablex RET_1,VOLUME_1, RET_2 sont toutes significatives au seuil de 0,001. quand au nouvelle variables créé, toutes son significatives au seuil de 5%
#excepté la variable la variable vol1.sec

#prediction
pred.logit <- predict(logit, newdata=valid, type="response")
head(pred.logit)


# aire sous la courbe ROC
library(pROC)
auc(valid$RET, pred.logit, quiet=TRUE)
plot.roc(valid$RET,pred.logit)
roc <- plot.roc(valid$RET, pred.logit, main="", percent=TRUE, ci=TRUE)
roc.se <- ci.se(roc, specificities=seq(0, 100, 5))
plot(roc.se, type="shape", col="grey")
#l'aire sous la courbe roc est de 0,5185

# aire sous la courbe précision-rappel
library(PRROC)
(pr <- pr.curve(scores.class0 = pred.logit[which(valid$RET==1)], scores.class1 = pred.logit[which(valid$RET==0)], curve=T))
plot(pr)
(roc <- roc.curve(scores.class0 = pred.logit[which(valid$RET==1)], scores.class1 = pred.logit[which(valid$RET==0)], curve=T))
plot(roc)

# matrice de confusion et error for data base
pred <- predict(logit, newdata=base1, type="response")
print(pred)
pred <- factor(ifelse(pred > 0.5, "1","0"))
a=table(base1$RET,pred)
print(a)
class(a)
#le taux de bonne prediction
bon = ((a[1,1]+a[2,2])/sum(a))
bon
#le taux d'erreur
err = ((a[2,1]+a[1,2])/sum(a))
err

#nous avons un taux de bon classement qui est de 51,16% et un taux de mauvais classement estimé à 48,84
library(caret)
confusionMatrix(reference = base1$RET,data = pred)

#ce qui prouve que nos modèle n'est pas performant pour expliquer la variable RET
######################################################################################################################################################"


#ARBRE DE DECISION

library(rpart)

# arbre optimisé
cart <- rpart(RET~., data = train, method="class", parms=list(split="gini"))
cart
summary(cart, digits=3)
# affichage graphique de l'arbre
plot(cart,branch=.2, uniform=T, compress=T, margin=.1) # tracé de l'arbre
text(cart, fancy=T, use.n=T, pretty=0, all=T, cex=.6) # ajout des légendes des noeuds

#prediction 
library(pROC)
pred.cart  <- predict(cart, newdata=valid,type="prob")
auc(valid$RET, pred.cart[,2], quiet=TRUE)

# élaguage automatique au minimum d'erreur
prunedcart <- prune(cart, cp=cart$cptable[which.min(cart$cptable[,"xerror"]),"CP"])
prunedcart
summary(prunedcart, digits=3)
pred.cart  <- predict(prunedcart, type="prob", valid)
auc(valid$RET, pred.cart[,2], quiet=TRUE)
# affichage amélioré avec package rattle
library(rattle)
fancyRpartPlot(prunedcart, sub=" ")

# stumps
stump <- rpart(RET~., data = train, method="class", parms=list(split="gini"), control=list(maxdepth=1,cp=-1))

fancyRpartPlot(stump, sub=" ")
pred.stump <- predict(stump, valid, type="prob")
auc(valid$RET, pred.stump[,2], quiet=TRUE)

#l'aire sous la courbe roc de la prediction est de 0,5091 avec l'arbre de decision.
#l'aire sous la courbe roc de la prediction est de 0,5091 avec stump.

#matrice de confusion et erreur 
pred2 <- predict(cart, newdata=base1, type="prob")
print(pred2)
pred2 <- factor(ifelse(pred2[,2] > 0.5, "1","0"))
b=table(base1$RET,pred2)
print(b)
class(b)
#le taux de bonne prediction
bon = ((b[1,1]+b[2,2])/sum(b))
bon
#le taux d'erreur
err = ((b[2,1]+b[1,2])/sum(b))
err

#le taux de bonne prediction est de 50,83% et le taux de mauvaises prediction est de 49,16%
# cette prediction est moins bonne que la regression logistique
######################################################################################################################################################"

# L'ALGORITHME XGBOOST

#gradient boosting avec XGBOOST et avec tous les variables de la base

library(xgboost)
library(Matrix)
library(pROC)
# transformation data frames en matrices sparses

train.mx <- model.matrix(RET ~ ., train)
valid.mx <- model.matrix(RET ~ ., valid)


# transformation matrices sparses en matrices Xgb

dtrain   <- xgb.DMatrix(train.mx, label=as.numeric(train$RET)-1)
dvalid   <- xgb.DMatrix(valid.mx, label=as.numeric(valid$RET)-1)
# modélisation
set.seed(234)
gdbt <- xgb.train(params=list(objective="binary:logistic", eval_metric="auc", eta=0.1, max_depth=10, colsample_bylevel=0.1, nthread=4),
                              verbose = 1, data=dtrain, nrounds=1000, early_stopping_rounds = 10, watchlist=list(eval=dvalid))
pred.gbm <- predict(gdbt, newdata=dvalid)
auc(valid$RET, pred.gbm, quiet=TRUE)


install.packages("DiagrammeR")
library(DiagrammeR)
# affichage en HTML d'arbres (1 à k) avec une numérotation commençant à 0 (et non 1)
xgb.plot.tree(model = gdbt, trees = 0, show_node_id = FALSE)

#importance des variables
print(head (xgb.importance(colnames(dtrain), model=gdbt),15))
print(head (xgb.importance(colnames(dvalid), model=gdbt),15))

#l'aire sous la courbe roc est de 0,5562



######################################################################################################################################################"



# forêts aléatoires avec xgboost
#avec une profondeur de 10 et du nombre d'arbre = 1000
set.seed(235)
foret <- xgb.train(params=list(booster = "gbtree", objective="binary:logistic", eval_metric="auc",eta=0.10, max_depth=10, subsample=1, colsample_bylevel=0.1), data=dtrain,
                              nrounds=1, num_parallel_tree = 1000, nthread = 4, verbose=1)

#prediction
pred.foret <- predict(foret, newdata=dvalid)
auc(valid$RET, pred.foret, quiet=TRUE) 
#l'aire sous la courbe roc est estimé à 0,5502

#avec une profondeur de 10 et du nombre d'arbre = 500
set.seed(235)
foret2 <- xgb.train(params=list(booster = "gbtree", objective="binary:logistic", eval_metric="auc",eta=0.10, max_depth=10, subsample=1, colsample_bylevel=0.1), data=dtrain,
                   nrounds=1, num_parallel_tree = 500, nthread = 4, verbose=1)
pred.foret2 <- predict(foret2, newdata=dvalid)
auc(valid$RET, pred.foret2, quiet=TRUE) 
#l'aire sous la courbe roc est estimé à 0,5497
######################################################################################################################################################"
#au vue de tout ce qui precède, on voit que le meilleur modèle est le gradient boosting



#ARRANGEMENT DE LA BASE DE TEST.

str(x_test)

#pour connaitre le nombre de valeurs differentes dans chaque colonnes
sapply(x_test, function(x) length(unique(x)))

# compter le nombre de valeurs manquantes dans chaque dans chaque colonnes
sapply(x_test,function(x) sum(is.na(x)))

# imputation des valeurs manquantes par la medianne
for (i in (1:ncol(x_test)))
{
  x_test[,i][which(is.na(x_test[,i]))] <-median(x_test[,i],na.rm=T)
}

#verification que nos valeurs manquantes ont bien été remplacées par la médiane
sapply(x_test,function(x) sum(is.na(x)))


summary(x_test)

# on va mettre des variables en facteur
x_test$SECTOR <- factor(x_test$SECTOR)
x_test$INDUSTRY_GROUP <-factor(x_test$INDUSTRY)
x_test$INDUSTRY <- factor(x_test$INDUSTRY)
x_test$SUB_INDUSTRY <- factor(x_test$SUB_INDUSTRY)


#creation d'une base avec nos 10 variables recentes et nos variables qualitatives
x_test = x_test[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
colnames(x_test)


#CREATION DE NOUVELLE VARIABLE
#comme dans le benmarch, créons une variable qui combine le secteur la date et ret 1
df1 <- setNames(aggregate(RET_1 ~ SECTOR+DATE, data = x_test, FUN = mean),c("SECTOR","DATE","RET_1.par.sec.et.date"))
x_test <- merge(x_test,df1,by=c("SECTOR","DATE"))

#creons une variable entre le volume_1 et le sub_industry les 2 variables qui explique le plus notre variable expliquée
df2 <- setNames(aggregate(VOLUME_1 ~ SUB_INDUSTRY, data = x_test, FUN = mean),c("SUB_INDUSTRY","vol1.sub_ind"))
x_test <- merge(x_test,df2,by=c("SUB_INDUSTRY"))


#entre volume1 et secteur
df4 <- setNames(aggregate(VOLUME_1 ~ SECTOR, data = x_test, FUN = mean),c("SECTOR","vol1.sec"))
x_test <- merge(x_test,df4,by=c("SECTOR"))

#creons une variable entre le RET_1 et le sub_industry
df5 <- setNames(aggregate(RET_1 ~ SUB_INDUSTRY, data = x_test, FUN = mean),c("SUB_INDUSTRY","RET1.sub_ind"))
x_test <- merge(x_test,df5,by=c("SUB_INDUSTRY"))

#NOTRE BASE DE DONNEE DEFINITIVE
x_test<-x_test[order(x_test[,4],decreasing=F),]

test=x_test[,-c(1,2,3,5,6,7)]
colnames(test)
name=test[,1]
test=test[,-1]
rownames(test)=name

#PREDICTION DU MODELE MEILLEUR SUR LA BASE DE TEST

#prediction sur la donnée test avec le modèle du gradient boosting
test.mx  <- model.matrix(~., test)
dtest = data.matrix(test.mx)
colnames(dtest)
pred_test <- predict(gdbt, newdata=dtest)
print(head(pred_test))
pred_test <- factor(ifelse(pred_test > 0.5, "True","False"))
print(head(pred_test))

# Fichier de soumission
final = data.frame(name,pred_test)
colnames(final) = c("ID","RET")


#telechargement du fichier
write.csv(final,"soumission.csv", row.names = FALSE)

