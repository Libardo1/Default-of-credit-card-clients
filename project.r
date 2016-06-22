# 1. korak
# NEDOSTAJUćE VRIJEDNOSTI

#
# Podjela podataka na skup za treniranje i skup za testiranje
#

# Pretvaram prazne stringove u nedostajeće vrijednosti na skupu za treniranje
# i to odmah prilikom učitavanja podataka
train=read.csv("trening.csv", na.strings = c(""))
test=read.csv("test.csv", na.strings = c(""))

# Prikaz podataka data.framea "train"
fix(train)

# Prikaz podataka data.framea "test"
fix(test)

# Struktura data.framea "train" otkriva da imamo tipove podataka Factor i numeric
str(train)

#
# Konverzija svih stupaca data.frame-a "train" iz factora u numeric tip
#

# Najprije tražim indekse svih elemenata koji su tipa faktor
indx <- sapply(train, is.factor)
# Zatim koristim indekse za konverziju tipova u svim stupcima data.framea
train[indx] <- lapply(train[indx], function(x) as.numeric(as.character(x)))

# Struktura data.framea "train" sad pokazuje da su svi atributi tipa numeric
str(train)

#
# Tražim elemente koji sadrže "?"
#

# Kreiram matricu sa logičkim vrijednostima TRUE (gdje ima upitnika) i FALSE (gdje nema upitnika)
# Varijabla index sadrži isti broj stupaca i redaka kao i dani data.frame
index <- train == "?"
# Dohvaćene indekse koristim da im pridružim specijalnu vrijednost NA
is.na(train) <- index

# Prikaz podataka potvrđuje da su "?" zamijenjeni sa specijalnom vrijednošću NA
fix(train)

# # Mijenjam svaki podatak sa specijalnom vrijednošću NA u aritmetičku sredinu za taj stupac
# for(i in 1:ncol(train)){
#   train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
# }

###
### ILI
###

# Mijenjam svaki podatak sa specijalnom vrijednošću NA u nulu
train[is.na(train)] <- 0

# Prikaz podataka data.framea train otkriva da su sve specijalne vrijednosti NA
# zamijenjene sa vrijednošću aritmetičke sredine svakog pojedinog stupca
fix(train)

# Retci s nedostajućim vrijednostima (nema ih više)
train[!complete.cases(train),]

# Broj redaka bez nedostajuće vrijednosti (7000 => 100% uspješno)
nrow(train[complete.cases(train),])

# Prikaz strukture podataka skupa za treniranje
str(train)

# Nema više nedostajućih vrijednosti niti "?" u data.frameu "train"
summary(train) #provjera dali imamo nedostajucih vrijednosti, odnosno "?"
fix(train)


#
# Sad isto radim i sa skupom podataka za treniranje
#

# Prikaz podataka data.framea "train"
fix(test)

#
# Konverzija više stupaca data.frame-a iz factora u numeric tip
#

# Najprije tražim indekse svih elemenata koji su tipa faktor
indx <- sapply(test, is.factor)
# Zatim koristim indekse za konverziju tipova u svim stupcima data.framea
test[indx] <- lapply(test[indx], function(x) as.numeric(as.character(x)))


#
# Tražim elemente koji sadrže "?"
#

# Kreiram matricu sa logičkim vrijednostima TRUE (gdje ima upitnika) i FALSE (gdje nema upitnika)
# Varijabla index sadrži isti broj stupaca i redaka kao i dani data.frame
index <- test == "?"
# Dohvaćene indekse koristim da im pridružim specijalnu vrijednost NA
is.na(test) <- index

# Prikaz podataka potvrđuje da su "?" zamijenjeni sa specijalnom vrijednošću NA
fix(test)

# # Mijenjam svaki podatak sa specijalnom vrijednošću NA u aritmetičku sredinu za taj stupac
# for(i in 1:ncol(test)){
#   test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
# }

###
### ILI
###

# Mijenjam svaki podatak sa specijalnom vrijednošću NA u nulu
test[is.na(test)] <- 0

# Retci s nedostajućim vrijednostima (nema ih više)
test[!complete.cases(test),]

# Broj redaka bez nedostajuće vrijednosti (3000 => 100% uspješno)
nrow(test[complete.cases(test),])

#
# Grafički prikaz nedostajućih vrijednosti iz paketa Amelia
#

#TODO: upotrijebiti fju za prikaz prije (ima NA vrijednosti) i poslije (nema NA vrijednosti)
library(Amelia)
missmap(test)


# Izbacujem vrijednosti sa NA vrijednošću iz skupa podataka za testiranje
#test = na.omit(test)


# Nema više nedostajućih vrijednosti niti "?" U data.frameu "test"
summary(test)
fix(test)

# Broj redaka u skupu za treniranje je 7000
nrow(train)
# Broj redaka u skupu za treniranje je 3000
nrow(test)



#################
#               #
# Random Forest #
#               #
#################



# Koristimo Random Forest algoritam za odabir značajki
library(randomForest)

# Postavljamo sjeme
set.seed(1)

# Kreiranje modela upotrebom metode randomForest
rfm <- randomForest(as.factor(Y) ~ X1 + X6 , train)
rfm

# Predviđanje na skupu podataka za testiranje
p <- predict(rfm, test)
p

# Provjeravamo strukturu dobivenog predviđanja
str(p)
fix(p)

table(test[,5], p)

mean(test[,5] == p)
importance(rfm)

Id = c(1:3000)
Prediction = p

# Konverzija iz faktora u numeric radi unosa u .csv datoteku
Prediction = as.numeric(levels(Prediction))[Prediction]

# Provjera strukture prije unosa u .csv format
str(Id)
str(Prediction)

# Kreiranje data.framea kojeg unosimo u .csv datoteku
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.83933 točnosti
write.csv(rjesenje, "RandomForest.csv", row.names=FALSE)



######################
#                    #
# Linearna regresija #
#                    #
######################



lmModel <- lm(Y ~. , data = train)
# Pogreška: 0.3879
summary(lmModel)

# Radim drugi model koji se bazira na značajnim vrijednostima prvog modela
# Značajne vrijednosti dobivamo naredbom summary(lmModel)
lmModel2 <- lm(Y ~ X6 + X2 + X11 + X5 , data = train)
# Pogreška: 0.3908
summary(lmModel2)

# Predviđanje prvog modela na skupu za testiranje
rezultat = predict(lmModel, test)
rezultat

# Konverzija dobivenih rezultata u 1 ili 0
rezultat = ifelse(rezultat > 0.5, 1, 0)
rezultat

# Provjera broja podataka (3000)
length(rezultat)

# Stvaram numerički vektor od Named num-a
rezultat = as.vector(rezultat)

# Rezultat je sada numeric
str(rezultat)

# Grafički prikaz 1. modela dobivenog Linearnom regresijom
# 1) Residuals vs Fitted
# 2) Normal Q-Q
# 3) Scale-Location
# 4) Residuals vs Leverege
scp = plot(lmModel)

# Kreiranje .csv datoteke za predaju na Kaggle
Id = c(1:3000)
Prediction = rezultat
str(Id)
str(Prediction)
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.82200 preciznosti
write.csv(rjesenje, "LinReg1.csv", row.names=FALSE)


# ILI (drugi model)

rezultat = predict(lmModel2, test)
rezultat
rezultat = ifelse(rezultat > 0.5, 1, 0)
rezultat
length(rezultat)

# Stvaram numerički vektor od Named num-a
rezultat = as.vector(rezultat)
str(rezultat)

# Grafički prikaz 2. modela dobivenog Linearnom regresijom
# 1) Residuals vs Fitted
# 2) Normal Q-Q
# 3) Scale-Location
# 4) Residuals vs Leverege
scp = plot(lmModel)

# Kreiranje .csv datoteke za predaju na Kaggle
Id = c(1:3000)
Prediction = rezultat
str(Id)
str(Prediction)
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.82200 preciznosti
write.csv(rjesenje, "LinReg2.csv", row.names=FALSE)




########################
#                      #
# Logistička regresija #
#                      #
########################



# Logistička regresija
model = glm(as.factor(Y) ~ ., family = binomial, data = train)

# Odabir najboljeg modela prema AIC kriteriju
step(model)

# Predviđanje
rezultat = predict(model, newdata = test, type = 'response')

# Postavljanje praga (threshold)
rezultat = ifelse(rezultat > 0.5, 1, 0)

# Stvaram numerički vektor od Named num-a
rezultat = as.vector(rezultat)

str(rezultat)


Id = c(1:3000)
Prediction = rezultat
str(Id)
str(Prediction)
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.82867 točnosti
write.csv(rjesenje, "LogReg1.csv", row.names=FALSE)



# Primjer Konfuzijske matrice (1)
library(caret)
# Odnos točnih rezultata i rezultata dobivenih u modelu
cfm = confusionMatrix(data=rezultat, reference=test$Y)
cfm$overall[[1]]
table(test$Y)

# Primjer konfuzijske matrice (2)
library(gmodels)
CrossTable(x = rezultat, y = test[,1], prop.chisq=FALSE)


########################
#                      #
# K Nearest Neighbours #
#                      #
########################

#install.packages("class")
library(class)
#broj najbližih susjeda k=1
rezultat <- knn(train = train, test = test, cl = train$Y, k=1)

rezultat
str(rezultat)

#install.packages("gmodels")
library(gmodels)
CrossTable(x=rezultat,y=test$Y, prop.chisq=FALSE)


Id = c(1:3000)
Prediction = rezultat
Prediction = as.numeric(levels(Prediction))[Prediction]
str(Id)
str(Prediction)
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.67733 preciznosti
write.csv(rjesenje, "knn.csv", row.names=FALSE)





rezultat2 <- knn(train = train, test = test, cl = train$Y, k = 142)
CrossTable(x = rezultat2, y = test$Y, prop.chisq = FALSE)

Id = c(1:3000)
Prediction = rezultat2
Prediction = as.numeric(levels(Prediction))[Prediction]
str(Id)
str(Prediction)
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.78933 preciznosti
write.csv(rjesenje, "knn2.csv", row.names=FALSE)





rezultat3 <- knn(train=train, test=test, cl=train$Y, k=sqrt(nrow(train)))
CrossTable(x = rezultat3, y = test$Y, prop.chisq = FALSE)

Id = c(1:3000)
Prediction = rezultat3
Prediction = as.numeric(levels(Prediction))[Prediction]
str(Id)
str(Prediction)
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje: 0.78533 preciznosti
write.csv(rjesenje, "knn3.csv", row.names=FALSE)




#############################
#                           #
# Artificial Neural Network #
#                           #
#############################



library(neuralnet)

m <- model.matrix( ~ ., data = train)
head(m)

annModel <- neuralnet(Y ~ X6 + X15, data=m, hidden=10, threshold=0.01, linear.output = FALSE, lifesign = "minimal")

plot(annModel, rep = "best")

temp_test <- subset(test, select = c("X6", "X15"))
annModel.results <- compute(annModel, temp_test)
head(temp_test)

results <- data.frame(actual = test$Y, prediction = annModel.results$net.result)
results[1:10,]

results$prediction <- round(results$prediction)
results[1:10,]

head(results)

length(results$prediction)
str(results$prediction)


Id = c(1:3000)
Prediction = results$prediction

# Provjera strukture prije unosa u .csv format
str(Id)
str(Prediction)

# Kreiranje data.framea kojeg unosimo u .csv datoteku
rjesenje = data.frame(Id, Prediction)
rjesenje

# Rješenje:  0.83800 preciznosti
write.csv(rjesenje, "ANN1.csv", row.names=FALSE)