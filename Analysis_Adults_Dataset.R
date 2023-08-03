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
adults <- subset(adults,!(workclass %in% c('Never-worked','Without-pay'))) #Elimination of rows with these workclass
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

#Remove unwanted rows like people whow never worked or without pay
str(adults)
summary(adults$workclass)

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

#Visualization of income based on type of 
summary(adults$workclass) #See a summary of the types and quantity of workers per class

## Creation of columns for two types of incomes per workclass
cont <- table(adults[adults$workclass == '?',]$income)["<=50K"] 
cont <- c(cont, table(adults[adults$workclass == '?',]$income)[">50K"]) 
#All Goverment workers assign to one value
cont <- c(cont, table(adults[adults$workclass == 'Federal-gov',]$income)["<=50K"]+
            table(adults[adults$workclass == 'Local-gov',]$income)["<=50K"]+
            table(adults[adults$workclass == 'State-gov',]$income)["<=50K"])

cont <- c(cont, table(adults[adults$workclass == 'Federal-gov',]$income)[">50K"]+
        table(adults[adults$workclass == 'Local-gov',]$income)[">50K"]+
        table(adults[adults$workclass == 'State-gov',]$income)[">50K"])

#Private workers
cont <- c(cont, table(adults[adults$workclass == 'Private',]$income)["<=50K"])

cont <- c(cont, table(adults[adults$workclass == 'Private',]$income)[">50K"])

#Independent people
cont <- c(cont, table(adults[adults$workclass == 'Self-emp-inc',]$income)["<=50K"]+
        table(adults[adults$workclass == 'Self-emp-not-inc',]$income)["<=50K"])

cont <- c(cont, table(adults[adults$workclass == 'Self-emp-inc',]$income)[">50K"]+
        table(adults[adults$workclass == 'Self-emp-not-inc',]$income)[">50K"])

cont <- as.numeric(cont) #Convertion to numeric data

typeWork <- rep(c('Unknowk','Goverment', 'Private', 'Independent'), each = 2) #Creation of table with names of types of workers 

ingresos <- rep(c('<=50K', '>50K'), 4) #Creation of table with the two types of incomes

typeWorks <- typeWork[1:8] #Creation of vectors with the type of workers and incomes

ingreso <- ingresos[1:8]

df <- data.frame(typeWorks,ingreso,cont) #Creation of dataframe with the values to analyze better

df$percentage <- (df$cont/nrow(adults))*100 #Calculation of percentages of every type of worker and income

df
#Stacked bar chart to see the distribution
ggplot(df,aes(fill=ingreso, y = cont, x = typeWorks))+
  geom_bar(position="stack",stat="identity")

#Stacked percentage chart to see the proportion of income per type of worker
ggplot(df,aes(fill=ingreso, y = cont, x = typeWorks))+
  geom_bar(position="fill",stat="identity")



#Analysis of the education variable
ggplot(adults,aes(x=education))+
  geom_bar(col="salmon")
dim(adults)
summary(adults$education)

#Creation of temporal dataframe to obtain better visualization
edu_level <- table(adults[adults$education == 'Some-college',]$education)['Some-college']
edu_level <- c(edu_level,
               table(adults[adults$education == '1st-4th',]$education)['1st-4th']+
                 table(adults[adults$education == '5th-6th',]$education)['5th-6th']+
                 table(adults[adults$education == '7th-8th',]$education)['7th-8th']+
                 table(adults[adults$education == '9th',]$education)['9th']+
                 table(adults[adults$education == '10th',]$education)['10th']+
                 table(adults[adults$education == '11th',]$education)['11th']+
                 table(adults[adults$education == '12th',]$education)['12th']
               )
edu_level <- c(edu_level, table(adults[adults$education == 'Prof-school',]$education)['Prof-school'])
edu_level <- c(edu_level, table(adults[adults$education == 'Masters',]$education)['Masters'])
edu_level <- c(edu_level, table(adults[adults$education == 'HS-grad',]$education)['HS-grad'])
edu_level <- c(edu_level, table(adults[adults$education == 'Doctorate',]$education)['Doctorate'])
edu_level <- c(edu_level, table(adults[adults$education == 'Bachelors',]$education)['Bachelors'])
edu_level <- c(edu_level, table(adults[adults$education == 'Assoc-acdm',]$education)['Assoc-acdm']+
                 table(adults[adults$education == 'Assoc-voc',]$education)['Assoc-voc'])
edu_level

count_edu_level <- as.numeric(edu_level)
type_Education <- c('Some-college','1st-12th','Prof-school','Masters','HS-grad','Doctorate','Bachelors','Assoc')
educa <- type_Education[1:8]
eduDF <- data.frame(type_Education,count_edu_level)
eduDF$percentage <- (eduDF$count_edu_level/nrow(adults))*100

ggplot(eduDF,aes(x=educa,y=count_edu_level))+
  geom_bar(col="blue",fill="salmon",stat="identity")

#Analysis of Two variables
boxplot(age ~ marital_status, data = adults,
        main = "Distribución de educación por sexo")
