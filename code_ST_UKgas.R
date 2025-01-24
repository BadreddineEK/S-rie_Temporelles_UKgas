################################################################################
#                         TP Final Série temporelle - UKgas
#                 EL KHALFIOU Nadir - EL KHAMLICHI Badreddine
#                                     MAM 4A
################################################################################

#### Description et premier étude la série ####
library(forecast)

# Chargement des données
data("UKgas")

UKgas_log <- log(UKgas)

par(mfrow = c(2, 1))
plot(UKgas)
plot(UKgas_log)
par(mfrow = c(1, 1))

# Decompose
decomp_UKgas <- decompose(UKgas)
par(mfrow=c(3,1))
plot(decomp_UKgas$seasonal, main = "Composante saisonnière de UKgas", xlab = "Temps (année)", ylab = "Consommation de gaz (thm)")
plot(decomp_UKgas$trend, main = "Tendance de UKgas", xlab = "Temps (année)", ylab = "Consommation de gaz (thm)")
plot(decomp_UKgas$random, main = "Résidus de UKgas", xlab = "Temps (année)", ylab = "Consommation de gaz (thm)")
par(mfrow = c(1, 1))

# Division des données en partie à modéliser et partie de test
UKgas_train <- window(UKgas, end = c(1985, 4))
UKgas_test <- window(UKgas, start = c(1986, 1))

UKgas_trainLog <- log(UKgas_train)
UKgas_testLog <- log(UKgas_test)

plot(UKgas_train)

#### Lissage exponentiel #############################################################################
#Question 3
fitLES = ets(UKgas_train, "ANN")

pred.LES = forecast(fitLES, h=4)
plot(pred.LES)

points(UKgas_test,type='l',col='darkgreen',lwd=2)

#Question 4
RMSE.predLES = sqrt(mean((pred.LES$mean - UKgas_test)^2))
MAPE.predLES = mean(abs(pred.LES$mean - UKgas_test)/abs(UKgas_test)) 

#Question 5
#LED
fitLED = ets(UKgas_train, "AAN")
summary(fitLED)
pred.LED = forecast(fitLED, h=4)
plot(pred.LED)

points(UKgas_test,type='l',col='darkgreen',lwd=2)
RMSE.predLED = sqrt(mean((pred.LED$mean - UKgas_test)^2))
MAPE.predLED = mean(abs(pred.LED$mean - UKgas_test)/abs(UKgas_test)) 

#Holt-Winters
fitHW = ets(UKgas_train, "AAA")
summary(fitHW)
pred.HW = forecast(fitHW, h=4)
plot(pred.HW , main = "Prévision à l'horizon 4 avec la méthode Holt-Winters" , xlab = "Temps (année)", ylab = "Consommation de gaz (thm)" )

points(UKgas_test,type='l',col='darkgreen',lwd=2)
RMSE.predHW = sqrt(mean((pred.HW$mean - UKgas_test)^2))
MAPE.predHW = mean(abs(pred.HW$mean - UKgas_test)/abs(UKgas_test)) 

#Question 7
#Holt-Winters
fitHWM = ets(UKgas_train, "MMM", damped = FALSE)
summary(fitHWM)
pred.HWM = forecast(fitHWM, h=4)
plot(pred.HWM)

points(UKgas_test,type='l',col='darkgreen',lwd=2)
RMSE.predHWM = sqrt(mean((pred.HWM$mean - UKgas_test)^2))
MAPE.predHWM = mean(abs(pred.HWM$mean - UKgas_test)/abs(UKgas_test)) 


#Question 8
fitAuto = ets(UKgas_train)
summary(fitAuto)
pred.Auto = forecast(fitAuto, h=4)
plot(pred.Auto)

points(UKgas_test,type='l',col='darkgreen',lwd=2)
RMSE.predAuto = sqrt(mean((pred.Auto$mean - UKgas_test)^2))
MAPE.predAuto = mean(abs(pred.Auto$mean - UKgas_test)/abs(UKgas_test)) 


#Question 9
annees = 10
horizon = 4*annees
fitFinal = ets(UKgas, model ="AAA", damped = FALSE)
summary(fitFinal)
pred.Final = forecast(fitFinal, h=horizon)
plot(pred.Final, main = "Prédiction de la consommation de gaz sur 10 ans avec la méthode de Holt-Winters additif
" , xlab = "Temps (année)", ylab = "Consommation de gaz (thm)" )


#### Régression Linéaire ##########################################################################

#Expliquons la tendance et la saisonnalite de notre serie temporelle

#Estimation de la tendance
Trend <-cbind(seq(1,length(UKgas_trainLog),1))

# j/4
f <- t(as.matrix(1:2))/4
f

#Estimation de la saisonalite
Seasonal <- cbind(cos(2*pi*Trend%*%f), sin(2*pi*Trend%*%f))[,-4] 
Seasonal

#Matrice de regresseur
Regresseur <- as.data.frame(cbind(Trend, Seasonal))
Regresseur

#modelisationa vec la fonction lm()
fit_lm <- lm(UKgas_trainLog~., data=Regresseur)
summary(fit_lm)


#Commentaire
#Trés bon R^2 ca veut dire que nos fonction trigonométrique explique très bien notre série

#Critère AIC: 
AIC(fit_lm)

#Vérifions que les résidus sont des bruits blancs: 
#4 façcons de le faire qui son: car si on a pas de normalité 
#on ne peut pas prédire donc un intervalle de confiance de prévision
#SI les résidus ne sont pas des bruits blancs faut dire c'est quoi

# -> le lagplot
lag.plot(fit_lm$residuals, 12, layout=c(4,3), do.lines=FALSE, diag.col = "red", main="lagplot des résidus à différents pas (1 à 12)")
#interprétation: 
#plot des résidus pour chaque h de epsilon_t en fonction (epsilon_t-h) Cas corrélation 
#et cas non corrélation voir notre du cours
#mais là, corrélation dans les résidus notre hypothèse de départ n'est pas vérifier 
#car pour epsilon_t,epsilon_t_-_4 il y a corrélation
#premier pas h=4 il y a une corrélation AR(4)

par(mfrow = c(2, 1))
# -> acf: Auto... simple
acf(fit_lm$residuals, main = "Autocorrélogramme simple des résidus de lm(UKgas_trainLog~., data=Regresseur)") 
#Pour un bruit blanc normalement on devrait avoir des 0 pour h=1, h=2,... et non nulle à h=0
#Barre bleu significativement différent de 0 lorsque c'est à l'extérieur (lorsque lees barres dépassent la ligne en bleu)
#et à l'intérieur pas significativement différent de zéro

#Décroissance exponentielle

# -> pacf: Auto...partiel
pacf(fit_lm$residuals, main = "Autocorrélogramme partiel des résidus de lm(UKgas_trainLog~., data=Regresseur)")
#Vaut 0 si h est strictement supérieur à 4 => de plus AR(4)
#AR(p) <=> condition necessaire et suffisante pour que cela soit un AR(p)
#Condition necessaire et suffisante pour que les résidus soit un AR(4) (Voir cours)

# -> qqnorm
qqnorm(fit_lm$residuals) #Résidus normaux

# -> les test de ports de manteaux (Boxtex)
Box.test(fit_lm$residuals, lag = 1, type = "Ljung")
Box.test(fit_lm$residuals, lag = 2, type = "Ljung")
Box.test(fit_lm$residuals, lag = 3, type = "Ljung")
Box.test(fit_lm$residuals, lag = 4, type = "Ljung")
# -> Si la p-valeur est inférieure à un seuil prédéfini, généralement 0,05 ou 0,01, on rejette 
#l'hypothèse nulle d'absence d'autocorrélation. Cela indique qu'il y a des preuves 
#statistiquement significatives d'autocorrélation dans la série temporelle.
# -> En revanche, si la p-valeur est supérieure au seuil choisi, on ne peut pas rejeter 
#l'hypothèse nulle. Cela suggère qu'il n'y a pas suffisamment de preuves pour conclure à 
#l'existence d'autocorrélation dans la série temporelle.

#Conclusion: Modèle non valide car epsilon_t pas bruit blanc donc remodéliser en prenant en compte l'AR(4)

#Previson

#Test avec les données UKgas_testLog pour vérifier que ce n'est pas bon

Trend_test <-cbind(seq(length(UKgas_trainLog)+1, length(UKgas_trainLog)+length(UKgas_testLog),1))
Trend_test

Seasonal_test <- cbind(cos(2*pi*Trend_test%*%f), sin(2*pi*Trend_test%*%f))[,-4]
Seasonal_test

Regresseur_test <- as.data.frame(cbind(Trend_test, Seasonal_test))
Regresseur_test

#Prévision à l'odre 12 les résultats avec les IC 
#Pour faire la prévision on utilise forecast pas lm 
#forecast <=> X*Beta(chapeau)
Prevision <- forecast(fit_lm, newdata = Regresseur_test)
Prevision

#Erreur quadratique moyenne
#PAS OUBLIER DE PASSER A l'EXPONENTIEL
RMSE.prevision=sqrt(mean((exp(Prevision$mean)-UKgas_test)^2))
RMSE.prevision
#Erreur relative moyenne
MAPE.prevision=mean(abs(exp(Prevision$mean)-UKgas_test)/abs(UKgas_test))
MAPE.prevision

#Tracé graphique
plot(UKgas, lwd = 5, col = "gray", main = "Prévision de UKgas grâce à la modélisation lm")

#pas oublier de mettre le ts et exp
lines(ts(exp(fit_lm$fitted.values), start= 1960, end = c(1985,4), frequency=4), col="orange", lwd = 2)
lines(ts(exp(Prevision$mean), start = c(1986,1), frequency=4), col="red",lty= 2, lwd= 2)
#Bleu série ajusté X beta chapeur
#En rouge la prévision
legend(x="topleft", legend=c("Ukgas","Série ajusté", "Prévision"), col=c("gray","orange","red"), pch=c(15,15,15))


#Modélisation avec l'AR(4)
#Paramètre à estimer c'est phi voir photo
#Utilisation d'Arima
fit_lm4 <- Arima(UKgas_trainLog, order= c(4,0,0),xreg=as.matrix(Regresseur))
AIC(fit_lm4)

#Commentaire AIC: 
#but minimiser AIC le modèle qu'on obtient a déja un AIC meilleur que le premier
# -> le lagplot
lag.plot(fit_lm4$residuals, 12, layout=c(4,3), do.lines=FALSE, diag.col = "red", main="lagplot des résidus à différents pas (1 à 12)")
#interprétation: 

par(mfrow = c(2, 1))
# -> acf: Auto... simple
acf(fit_lm4$residuals, main ="Autocorrélogramme simple des résidus du Arima(UKgas_trainLog, order= c(4,0,0),xreg=as.matrix(Regresseur)") 
#Pour un bruit blanc normalement on devrait avoir des 0 pour h=1, h=2,... et non nulle à h=0
#On ne peut pas dire que ce n'est pas un bruit blanc

# -> pacf: Auto...partiel
pacf(fit_lm4$residuals, main ="Autocorrélogramme partiel des résidus du Arima(UKgas_trainLog, order= c(4,0,0),xreg=as.matrix(Regresseur)")
#Vau
par(mfrow = c(1, 1))
# -> qqnorm
qqnorm(fit_lm4$residuals) #Résidus normaux

# -> les test de ports de manteaux (Boxtex)
Box.test(fit_lm4$residuals, lag = 1, type = "Ljung")
Box.test(fit_lm4$residuals, lag = 2, type = "Ljung")
Box.test(fit_lm4$residuals, lag = 3, type = "Ljung")
Box.test(fit_lm4$residuals, lag = 4, type = "Ljung")

#Faut calculer maintenant le pouvoir prédictive de la série
Pred.AP.REG4 <- forecast(fit_lm4, xreg=as.matrix(Regresseur_test))
plot(Pred.AP.REG4, main = "Prédiction de la regression avec un AR(4)")

#Erreur quadratique moyenne
#PAS OUBLIER DE PASSER A l'EXPONENTIEL
RMSE.prevision=sqrt(mean((exp(Pred.AP.REG4$mean)-UKgas_test)^2))
RMSE.prevision
#Erreur relative moyenne
MAPE.prevision=mean(abs(exp(Pred.AP.REG4$mean)-UKgas_test)/abs(UKgas_test))
MAPE.prevision


#RMSE ET MAP meilleure 

plot(UKgas, lwd = 5, col = "gray", main = "Prévision de UKgas grâce à la modélisation AR(4)")
lines(ts(exp(fit_lm4$fitted), start= 1960, end = c(1985,4), frequency=4), col="orange",lwd = 2) #just fitted car on est parti d'un AR
lines(ts(exp(Pred.AP.REG4$mean), start = c(1986,1), frequency=4), col="red",lty= 2, lwd = 2)
legend(x="topleft", legend=c("Ukgas","Série ajusté", "Prévision"), col=c("gray","orange","red"), pch=c(15,15,15))






#Conclusion: A partir de plusieurs caractéristiques on choisit le dernier modèle
annee_pred<-5

Trend <-cbind(seq(1,length(UKgas_log),1))
# j/4
f <- t(as.matrix(1:2))/4
f
#Estimation de la saisonalite
Seasonal <- cbind(cos(2*pi*Trend%*%f), sin(2*pi*Trend%*%f))[,-4] 
Seasonal
#Matrice de regresseur
Regresseur <- as.data.frame(cbind(Trend, Seasonal))
Regresseur
fit_final <- Arima(UKgas_log, order= c(4,0,0),xreg=as.matrix(Regresseur))
#modelisationa vec la fonction lm()
#fit_lm <- lm(UKgas_trainLog~., data=Regresseur)

#Modèle choisi
Trend_final <-cbind(seq(length(UKgas)+1, length(UKgas)+4*annee_pred,1))
Seasonal_final <- cbind(cos(2*pi*Trend_final%*%f), sin(2*pi*Trend_final%*%f))[,-4]
Regresseur_final <- as.data.frame(cbind(Trend_final, Seasonal_final))
Prevision_final <- forecast(fit_final, xreg=as.matrix(Regresseur_final))

plot(UKgas,xlim=c(1960,1987+annee_pred),ylim=c(0,2000), main = "Prévisions avec modèle de Régression Linéaire AR(4) sur 5 ans", ylab = "Consommation du gaz en thm", xlab = "Temps(années)")
lines(ts(exp(Prevision_final$mean), start = c(1987,1), frequency=4), col="red", lwd = 2)
#Intervalle de confiance
lines(ts(exp(Prevision_final$upper[,2]), start = c(1987,1), frequency=4), col="orange")#IC supérieur
lines(ts(exp(Prevision_final$lower[,2]), start = c(1987,1), frequency=4), col="purple")#IC inférieur
legend(x="topleft", legend=c("Prévisions","Prévisions borne sup", "Prévisions borne inf"), col=c("red","orange","purple"), pch=c(15,15,15))

