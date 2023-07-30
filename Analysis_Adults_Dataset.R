#Load of the Adult dataset
library(ggplot2)
adults <- read.table("C:/Users/alsol/Desktop/MINERIA_PARA_R/archive/adult.data",
                     sep = ",", #Separator
                     header = F, #No header in data file
                     col.names=c("age", 
                                 "workclass",
                                 "fnlwgt", 
                                 "education", 
                                 "education_num", 
                                 "marital_status",
                                 "occupation",
                                 "relationship",
                                 "race",
                                 "sex",
                                 "capital-gain",
                                 "capital_loss",
                                 "hours_per_week",
                                 "native_country",
                                 "income"),
                     fill=F, #Avoid the addition of blank fields
                     strip.white=T)
#Visualize the number of registers and the type of variables
dim(adults)
str(adults)

#Transformation of the chr variables to Factors
adults$workclass <- as.factor(adults$workclass)
adults$education <- as.factor(adults$education)
adults$marital_status <- as.factor(adults$marital_status)
adults$occupation <- as.factor(adults$occupation)
adults$relationship <- as.factor(adults$relationship)
adults$race <- as.factor(adults$race)
adults$sex  <- as.factor(adults$sex)
adults$native_country <- as.factor(adults$native_country)
adults$income <- as.factor(adults$income)

str(adults)

#Elimination of undesired columns (fnlwgt and education_num)
adults[['education_num']] = NULL
adults[['fnlwgt']] = NULL

#Quick analysis of some variables like age, occupation and marital status
summary(adults$age)
summary(adults$occupation)
summary(adults$marital_status)
summary(adults$hours_per_week)

#Visualization of the distribution of some variables
ggplot(adults,aes(x=age))+
  geom_bar(fill="blue",col="black")+
  ggtitle("Age Distribution")

boxplot(adults$age,main="Age Distribution")

ggplot(adults, aes(x=hours_per_week)) +
  geom_bar(fill="blue",col="black")+
  ggtitle("Hours poer week Distribution")

boxplot(adults$hours_per_week,main="Hours per week Distribution")

#Transformation of discrete variable to groups

hoursW <- adults$hours_per_week #Assign the column to a variable

#Definition of the intervals

hoursW[adults$hours_per_week <= 20] <- "1-20"
hoursW[adults$hours_per_week >= 20 & adults$hours_per_week < 40] <- "21-39"
hoursW[adults$hours_per_week >= 40 & adults$hours_per_week <= 45] <- "40-45"
hoursW[adults$hours_per_week > 45 & adults$hours_per_week <= 60] <- "46-60"
hoursW[adults$hours_per_week > 60 & adults$hours_per_week <= 80] <- "61-80"
hoursW[adults$hours_per_week >= 80] <- "80+"

hoursW <- as.factor(hoursW) #Transform the interval to a factor

totalHours <- summary(hoursW) #Analyze the new factors

percentages <- numeric(length(totalHours)) #Create an array of zeros with size equals the number of factor

for(i in 1: length(percentages)){ #For cycle to obtain the relative frequency of every interval 
  percentages[i] <- (totalHours[i]/nrow(adults))*100
}

data.frame(totalHours,percentages) #Creation of the dataframe with the interval and the relative frequency

#Analysis of capital gain
summary(adults$capital.gain)

sum(adults$capital.gain == 0)/nrow(adults) #Percentage of capital gain with value equal to zero

adults[['capital.gain']]=NULL #Elimination of column capital.gain

#Analysis of capital loss
summary(adults$capital_loss)
sum(adults$capital_loss == 0) / nrow(adults)

adults[['capital_loss']] = NULL #Elimination of column capital_loss
str(adults)

#Analysis with two variables
table(adults$sex,adults$age)

ggplot(adults,aes(x=age,fill=sex))+
  geom_histogram(binwidth=1,color='grey')+
  ggtitle("Distribución de sexo por edades")

#Finding correlation or relationships
boxplot(age ~ income, data=adults,
  main="Distribución de edades de acuerdo al ingreso",xlab="Ingresos",ylab="Edad")
  