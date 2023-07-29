#Load of the Adult dataset
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
summary(adults)

#Elimination of undesired columns (fnlwgt and education_num)
adults[['education_num']] = NULL
adults[['fnlwgt']] = NULL
str(adults)
