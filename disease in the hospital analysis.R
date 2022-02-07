#name: Yuchen Shen



library(readr);
library(tidyverse)
diabetic_data <- read_csv("diabetic_data.csv");
View(diabetic_data);
#b
#1. num_medications
summarise(diabetic_data,mean_medications=mean(num_medications))  #use summarise() to calculate the mean of num_medications,mean_medications=mean(num_medications)
summarise(diabetic_data,median_medications=median(num_medications)) #use summarise() to calculate the mean of num_medications, mean_medications=median(num_medications)
class(diabetic_data$num_medications)  #to see the calss of the variable
var(diabetic_data$num_medications)  #to see the variance
sd(diabetic_data$num_medications)  #to see the standard deviation
range(diabetic_data$num_medications) #to see the range of the variable
quantile(diabetic_data$num_medications)  #to see the quantiles
IQR(diabetic_data$num_medications)  #to see the inter-quantile range
#2. num_procedures
summarise(diabetic_data,mean_num_procedures=mean(num_procedures))
summarise(diabetic_data,median_num_procedures=median(num_procedures))
class(diabetic_data$num_procedures)
var(diabetic_data$num_procedures)
sd(diabetic_data$num_procedures)
range(diabetic_data$num_procedures)
quantile(diabetic_data$num_procedures)
IQR(diabetic_data$num_procedures)
#3.num_lab_procedures
summarise(diabetic_data,mean_num_lab_procedures=mean(num_lab_procedures))
summarise(diabetic_data,median_num_lab_procedures=median(num_lab_procedures))
class(diabetic_data$num_lab_procedures)
var(diabetic_data$num_lab_procedures)
sd(diabetic_data$num_lab_procedures)
range(diabetic_data$num_lab_procedures)
quantile(diabetic_data$num_lab_procedures)
IQR(diabetic_data$num_lab_procedures)
#4. time_in_hospital
summarise(diabetic_data,mean_time_in_hospital=mean(time_in_hospital))
summarise(diabetic_data,median_time_in_hospital=median(time_in_hospital))
class(diabetic_data$time_in_hospital)
var(diabetic_data$time_in_hospital)
sd(diabetic_data$time_in_hospital)
range(diabetic_data$time_in_hospital)
quantile(diabetic_data$time_in_hospital)
IQR(diabetic_data$time_in_hospital)
#5. number_diagnoses
summarise(diabetic_data,mean_number_diagnoses=mean(number_diagnoses))
summarise(diabetic_data,median_number_diagnoses=median(number_diagnoses))
class(diabetic_data$number_diagnoses)
var(diabetic_data$number_diagnoses)
sd(diabetic_data$number_diagnoses)
range(diabetic_data$number_diagnoses)
quantile(diabetic_data$number_diagnoses)
IQR(diabetic_data$number_diagnoses)
#6. number_outpatient
summarise(diabetic_data,mean_number_outpatient=mean(number_outpatient))
summarise(diabetic_data,median_number_outpatient=median(number_outpatient))
class(diabetic_data$number_outpatient)
var(diabetic_data$number_outpatient)
sd(diabetic_data$number_outpatient)
range(diabetic_data$number_outpatient)
quantile(diabetic_data$number_outpatient)
IQR(diabetic_data$number_outpatient)


#c
#use summarise_all() which can affect every variable to find the missing values
#in summarise_all(): the first parameter is the dataset, funs() means a function
#sum(is.na(.)) means that it can find the sum of missing values in each column.
summarise_all(diabetic_data,funs(sum(is.na(.))))

#d
#In sapply(): the first parameter is the dataset, function(x) means the function, 
#sum(is.na(x)) means the sum of values equal to NA in each column
sapply(diabetic_data,function(x) sum(is.na(x)))

#e
#a is the dataset choosen after filtering race=='AfricanAmerican', age=='[10-20)'
a<-diabetic_data %>% 
  filter(race=='AfricanAmerican', age=='[10-20)')
#in plot: x axis is num_medication and y axis is number_diagnoses, the color is gender
#the title of the plot is num_medications vs number_diagnoses, and the labs of x and y are given by labs()
ggplot(data=a) +
  geom_point(mapping = aes(x=num_medications,y=number_diagnoses,colour=gender),show.legend = TRUE)+
  ggtitle("num_medications vs number_diagnoses")+
  labs(x= "number of medications",y= "number of diagnoses")
 

#f
#number_outpatient before log transformation
ggplot(diabetic_data)+
  geom_histogram(mapping = aes(number_outpatient),binwidth = 1)
#number_inpatient before log transformation
ggplot(diabetic_data)+
  geom_histogram(mapping = aes(number_inpatient),binwidth = 1)
#number_outpatient after log transformation
ggplot(diabetic_data)+
  geom_histogram(mapping = aes(number_outpatient),binwidth = 1)+
  scale_y_log10()
##number_inpatient after log transformation
ggplot(diabetic_data)+
  geom_histogram(mapping = aes(number_inpatient),binwidth = 1)+
  scale_y_log10()

#g
#use mutate_if to change all the variables which type is character to numeric and we use factor to deal with the problem of missing values
#is.character£ºfind the variables whose types are character
#as.factor: change the variables whose types are character to factor
#is.factor: find the variables whose types are factor.
#as.numeric: change factor to numeric
g<-diabetic_data %>%
  mutate_if(is.character,as.factor) %>%
  mutate_if(is.factor, as.numeric)
#use ggplot()+geom_bar() to see each variable's bar chart.
ggplot(data=g)+geom_bar(mapping=aes(A1Cresult))
ggplot(data=g)+geom_bar(mapping=aes(metformin))
ggplot(data=g)+geom_bar(mapping=aes(glimepiride))
ggplot(data=g)+geom_bar(mapping=aes(glipizide))

#h
#select the variables want to use and change the type of variables using mutate_if
#if.numeric: find the variables whose type is numeric
#as.character: change the type find in if.numeric to character
diabetic_data %>%
  select(A1Cresult,readmitted) %>%
  mutate_if(is.numeric,as.character)
diabetic_data %>%
  select(encounter_id,max_glu_serum) %>%
  mutate_if(is.numeric,as.character)

#i
#group the dataset according to race, gender and age, and then calculate the mean of num_medications
i<-group_by(diabetic_data,race,gender,age) %>%
  summarise(medications=mean(num_medications))
view(i) #see the dataframe
nrow(i) #calcute the row to know the total number of records

#j
i %>%
  arrange(desc(age)) %>% #arrange i according to desc(age) 
  filter(age=="[0-10)")  #filter age="[0-10)"

#k
#the dataset is group by age and summarise the mean of num_medications
#In plot: x axis is age, y axis is medications, color is red, final draw a curve using geom_smooth()
ggplot(data=i,aes(x=age,y=medications)) + geom_smooth(mapping = aes(colour=gender,group=1))

#l
#the dataset is grouped by gender and age, and we use summarise() to calculate the mean of num_medications
#use ggplot() to set the x axis and y axis and the colour
#use geom_smooth() to see the smooth line
#use facet_wrap to make subplots according to gender
l<-group_by(diabetic_data,gender,age) %>%
  summarise(medications=mean(num_medications))
ggplot(data=l,aes(x=age,y=l$medications,col=gender)) + geom_smooth(mapping=aes(group=1)) + facet_wrap(~gender)

#n
n<-diabetic_data %>%
  group_by(age) 
  mutate_if(is.character,as.factor) %>%
  mutate_if(is.factor, as.numeric)
ggplot(data=n,aes(x=age,y=readmitted))+geom_line()+scale_y_log10()
