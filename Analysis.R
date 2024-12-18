# Instalarea si activarea packetelor necesare
#install.packages("neuralnet")
library("neuralnet")
library(readr)

################################################################################
################# 2. Prezentarea bazei de date #################################
# Generarea nr aleatoare
set.seed(1234)
# Incarcarea si vizualizarea datelor
data <- read_csv("D:/OneDrive/Desktop/Master/An 2/Retele neurale/PROIECT/Online Survey Data on Education Bd.csv")
### Operatii preliminare #####
str(data)
# elimin coloanele care nu imi folosesc in analiza
data_del <- data[, c(3,7,9,12,13,14,16,17)]
data <-data[, -c(3,7,9,12,13,14,16,17)]
# redenumim variabilele 
names(data)<-c("lvl_study","age","result_a_onl", 
               "knowledge_a_onl","happy","internet_acces","total_h_study_b_onl",
               "total_h_study_a_onl","gender")
names(data_del)<-c("device_b_onl","area_institute","internet_type", 
               "class_perf","institute_type","area_study","issues_onl_class",
               "pref_device")
head(data)
# verificam ce tip de date sunt variabilele
sapply(data,class)
# transformam in factor 
data$lvl_study <- as.factor(data$lvl_study)
data$result_a_onl <- as.factor(data$result_a_onl)
data$knowledge_a_onl <- as.factor(data$knowledge_a_onl)
data$happy <- as.factor(data$happy)
data$internet_acces <- as.factor(data$internet_acces)
data$gender <- as.factor(data$gender)
# transformam in factor si var nefolosite
data_del$device_b_onl <- as.factor(data_del$device_b_onl)
data_del$area_institute <- as.factor(data_del$area_institute)
data_del$internet_type <- as.factor(data_del$internet_type)
data_del$class_perf <- as.factor(data_del$class_perf)
data_del$institute_type <- as.factor(data_del$institute_type)
data_del$issues_onl_class <- as.factor(data_del$issues_onl_class)
data_del$pref_device <- as.factor(data_del$pref_device)

# verificam daca au avut loc schimbarile
sapply(data,class)
# aranjam coloanele
data <- subset(data, select=c(3,1,2,4,5,6,7,8,9))

# verificam daca avem observatii lipsa
colSums(is.na(data))
# se vor omite datele lipsa
data <- na.omit(data)
dim(data)

################################################################################
################# 4. Analiza exploratorie a datelor ############################
summary(data)
summary(data_del)
## 4.1. Pentru variabile numerice 
hist(data$age,
     main="Varsta",
     xlab="Varsta", 
     col="darkmagenta")
hist(data$total_h_study_b_onl,
     main="Total ore de studiu inainte de educatia online",
     xlab="Total ore",
     breaks=seq(3.5,4.5,5.5),
     col="orange")
hist(data$total_h_study_a_onl,
     main="Total ore de studiu dupa educatia online",
     xlab="Total ore", 
     col="blue")

## 4.2. Pentru variabile nenumerice
barplot(table(data$lvl_study), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , main="Nivel de studiu", col="brown", legend.text=T)
barplot(table(data$result_a_onl), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) ,main="Rezultatul a crescut dupa educa??ia online", col="brown", legend.text=T)
barplot(table(data$knowledge_a_onl), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) ,main="Cuno??tintele au crescut dupa educa??ia online", col="brown", legend.text=T)
barplot(table(data$happy), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) ,main="Multumit de educatia online", col="brown", legend.text=T)
barplot(table(data$internet_acces), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) ,main="Aveti disponibilitate la internet", col="brown", legend.text=T)
barplot(table(data$gender), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) ,main="Gen", col="brown", legend.text=T)

## 4.3.Oulieri
# vizualizam boxplot
boxplot(data$age, main="Varsta",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE)
boxplot(data$total_h_study_b_onl, main="Total ore de studiu inainte de educatia online",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE)
boxplot(data$total_h_study_a_onl, main="Total ore de studiu dupa educatia online",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE)


################################################################################
## 7. Alegerea tipurilor de retele neurale si prezentarea structurii acestora ##
# transformam variabilele factor in numeric 
data$lvl_study <- as.numeric(as.factor(data$lvl_study))
data$result_a_onl <- as.numeric(as.factor(data$result_a_onl))
data$knowledge_a_onl <- as.numeric(as.factor(data$knowledge_a_onl))
data$happy <- as.numeric(as.factor(data$happy))
data$internet_acces <- as.numeric(as.factor(data$internet_acces))
data$gender <- as.numeric(as.factor(data$gender))

##Standardizarea
data <-scale(data)

#Form(-a)-ula re??elei neurale
formula <-result_a_onl ~.

#Asigurarea reproducerii rezultatelor
n=nrow(data)
train <- sample (1:n, (round(0.70*nrow(data))) , FALSE )

### Retea neurala cu 1 strat ascuns si 4 neuroni pe stratul ascuns
nn_1_4 <- neuralnet (formula,
                     data = data[train ,],
                     hidden =4,
                     algorithm = "rprop+",
                     err.fct = "sse",
                     act.fct = "logistic",
                     linear.output = FALSE )
# Vizualizarea retelei neurale
plot(nn_1_4 , intercept = T ,
     show.weights = T )

### Retea neurala cu 1 strat ascuns si 2 neuroni pe stratul ascuns
nn_1_2 <- neuralnet (formula,
                     data = data[train ,],
                     hidden =2,
                     algorithm = "rprop+",
                     err.fct = "sse",
                     act.fct = "logistic",
                     linear.output = FALSE )
#Vizualizarea retelei neurale
plot(nn_1_2 , intercept = T ,
     show.weights = T )

### Retea neurala cu 1 strat ascuns si 8 neuroni pe stratul ascuns
nn_1_8 <- neuralnet (formula,
                     data = data[train ,],
                     hidden =8,
                     algorithm = "rprop+",
                     err.fct = "sse",
                     act.fct = "logistic",
                     linear.output = FALSE )
#Vizualizarea retelei neurale
plot(nn_1_8 , intercept = T ,
     show.weights = T )

### Retea neurala cu 2 straturi ascunse
index <- sample(1 : nrow(data),
                round(0.70 * nrow(data)))
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data,
                              center = mins,
                              scale = maxs - mins))
train_ <- scaled[index, ]

nn_2_3 <- neuralnet(formula,
                    data = train_,
                    hidden = c(2, 3),
                    linear.output=T)

#Vizualizarea retelei neurale
plot(nn_2_2 , intercept = T ,
     show.weights = T )

######  Previziune pe baza retelei neurale MLP 
######
pred1 <- compute(nn_1_4 , data[-train ,2:9] )
r1 <- ifelse( pred1$net.result <= 0.5 , -1, 1)
table(sign(r1),sign(data[-train ,1]) ,
      dnn =c(" Predicted " , " Observed "))
error_rate = (1- sum( sign(r1) == sign(
  data[-train ,1]) )/2276 )
round( error_rate ,3)
######
pred2 <- compute(nn_1_2 , data[-train ,2:9] )
r2 <- ifelse( pred2$net.result <= 0.5 , -1, 1)
table(sign(r2),sign(data[-train ,1]) ,
      dnn =c(" Predicted " , " Observed "))
error_rate = (1- sum( sign(r2) == sign(
  data[-train ,1]) )/2276 )
round( error_rate ,3)
######
pred3 <- compute(nn_1_8 , data[-train ,2:9] )
r3 <- ifelse( pred3$net.result <= 0.5 , -1, 1)
table(sign(r3),sign(data[-train ,1]) ,
      dnn =c(" Predicted " , " Observed "))
error_rate = (1- sum( sign(r3) == sign(
  data[-train ,1]) )/2276 )
round( error_rate ,3)

