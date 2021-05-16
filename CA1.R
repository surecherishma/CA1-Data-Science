#COVID-19 Data analysis for DATA Science project
#Read the CSV file 
#Stored in a dataframe named ad Covid_data
#The data in the CSV files has missing values and blacks so Replacing the cells with NA and setting the character type into factor.

covid_data <- read.csv("covid.csv", na = "", header = T, stringsAsFactors = T)
covid_data

#describing of data in the data frame
describe(covid_data)

#Showing the Structure if data
str(covid_data)

#with the head command it displays the top most 5 rows 
head(covid_data)

#Displaying  the number of rows
nrow(covid_data)

#Displaying  the number of rows
ncol(covid_data)

#shows the summary of the data available in the data frame
summary(covid_data)


# CLEANING AND PREPROSSING OF DATA
# Checking the missing values in the data
# List rows with missing values
incomplete_data <- covid_data[!complete.cases(covid_data),]
incomplete_data

# Remove any rows that contain NA using listwise deletion
covid_data <- na.omit(covid_data)
covid_data

# The `date` field is in "YYYY-mm-dd" format as char type
# converted to a `date` variable to date from char type.
covid_data$date <- as.Date(covid_data$date)
str(covid_data$date)

#Installing the required packages
install.packages("dplyr") 
install.packages("VIM")
install.packages("ggplot2")   
install.packages("psych")     
install.packages("magrittr")
install.packages("dplyr")   
install.packages("mice") 
library(mice)
library(dplyr)
library(VIM)
library(magrittr)             
library(psych)
library(ggplot2)
library(mice)
#-------------------------------------------------------------------------------------------
#Question 1
#Does Male smokers are more likely to die from Covid_19?

#H0= Male Smokers are more likely to die from Covid_19
#H1= Male Smokers are not likely to die from Covid_19

#modifying the blank spaces/NA and replacing it with 0 for total death varaiable.
covid_data$total_deaths[is.na(covid_data$total_deaths)] <- 0
covid_data$total_deaths

#modifying the blank spaces/NA and replacing it with 0 for male smokers varaiable.
covid_data$male_smokers[is.na(covid_data$male_smokers)] <- 0
covid_data$male_smokers

#Creating the subsets for total deaths to male smokers
covid_data_subset <- subset(covid_data, select = c(total_deaths, male_smokers))
covid_data_subset

#aggreating the missing values
missing_values <- aggr(covid_data_subset, prop = FALSE, numbers = TRUE)
summary(missing_values)
str(covid_data_subset)

#Attaching the subset dataframe 
attach(covid_data_subset)

#Checking the linearity and correlation
ggplot(covid_data_subset, aes(x=total_deaths,y=male_smokers))+ geom_point(col="red", size=7)
#

#plotting the total deaths to male smokers using ggplot
plot(total_deaths, male_smokers, pch = 9, col= "red",
     main = "comparision of total_deaths with male_smokers",
     xlab = "total_deaths",
     ylab = "male_smokers")



#Summarize the medians of the data to confirm that the data is normally distributed or not
tapply(total_deaths, male_smokers, median)


#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 

#is total death normally disturbuted?
qqnorm(total_deaths)
# Add line that represents normal distribution
qqline(total_deaths, col = "blue")
# total_deaths appears not to be normally distributed

#Is diabetes_prevalence normally distributed?
qqnorm(male_smokers)
# Add line that represents normal distribution
qqline(male_smokers, col = "red")

#this Shows the scatter plot of the matrix formed by total deaths for male smokers.
pairs.panels(covid_data_subset,smooth = TRUE, scale = FALSE, density = TRUE,ellipses = TRUE,method = "spearman",pch = 21,lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2, 
hist.col = 4, stars = TRUE, ci = TRUE)    


# Calculating the P-values is to be determined.Need to decide a test for calculating p-value



correlation_1 <- cor.test(x=covid_data_subset$total_deaths, 
                  y=covid_data_subset$male_smokers, method = 'spearman')
correlation_1
#S=241899, p-value <0.00002
#It clearly shows that alternative hypothesis is true i.e. male smokers are not likely to die from covid_19
#----------------------------------------------------------------------------------------------------------------------------------

#Question 2: Is there any correlation between Icu patients and people vaccinated?
#H0:There is correlation between the ICU patients and people who are vaccinated
#H1There can be there is no correlation between the ICU patients and people who are vaccinated.

#creating a subset for the continent europe to with the lables icu patients and people vaccinated
Covid_Europe = subset(covid_data, iso_code == "GBR", select = c("icu_patients", "people_vaccinated"))
Covid_Europe

#drawing a pattern for Covid_europe
md.pattern(Covid_Europe)
summary(Covid_Europe)
#this Shows the scatter plot of the matrix formed by icu patients and people vaccinated.
pairs(Covid_Europe, labels = colnames(Covid_Europe), main = "Covid_19 plot")

#installing the library psych
library(psych)
pairs.panels(covid_data_subset,smooth = TRUE, scale = FALSE, density = TRUE,ellipses = TRUE,method = "spearman",pch = 21,lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2, 
             hist.col = 4, stars = TRUE, ci = TRUE) 

#removing the null values from the variable people vacconated
Covid_Europe$people_vaccinated[is.na(Covid_Europe$people_vaccinated)] <- 0
Covid_Europe

#ommitting the null values
na.omit(Covid_Europe)
Covid_Europe <- na.omit(Covid_Europe)
Covid_Europe

pairs(Covid_Europe, labels = colnames(Covid_Europe), main = "Covid_19 plot")

#installing the library psych
library(psych)
pairs.panels(covid_data_subset,smooth = TRUE, scale = FALSE, density = TRUE,ellipses = TRUE,method = "spearman",pch = 21,lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2, 
             hist.col = 4, stars = TRUE, ci = TRUE) 

md.pattern(Covid_Europe)


#uploading the library lattice and attaching the covid_Europe
library("lattice")
attach(Covid_Europe)

#Plotting a graph for icu patients and people vaccinated
plot(icu_patients, people_vaccinated, pch = 19, col = "red")

#plotting out histogram for icu patients and people vaccinated
histogram(~icu_patients | people_vaccinated, 
          data = Covid_Europe, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(icu_patients, people_vaccinated, median)


# check normality of data

qqnorm(icu_patients)
# this line represents normal distribution
qqline(icu_patients, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(Covid_Europe$icu_patients)
normality_test$p.value


hist(icu_patients)

# not normally distributed

cor.test(Covid_Europe$people_vaccinated, Covid_Europe$icu_patients,  method = "spearman")

# Spearman’s Correlation Coefficient
#since p value is smaller than cuto 0.05 thus we can reject the null hypthesis
# There is a strong negative corlation between icu pateints and people who are fully vaccinated

detach(Covid_Europe)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question : 3 Positive cases affect population

positive_dataset = subset(covid_data, !is.na(covid_data$population), select = c("total_deaths", "population"))
positive_dataset

na_records <- covid_data[!complete.cases(positive_dataset),]
nrow(na_records)
library(mice)
# 15 records have missing NI address
# 10 have missing type
md.pattern(positive_dataset)

library(VIM)

# Looks like the missing values are all in this 1 column
# for "Leave". We're not going to use it for now
# so we'll leave it as it is
missing_values <- aggr(positive_dataset, prop = FALSE, numbers = TRUE)
summary(positive_dataset)

#create a categorical column to determine country is with high desnity or low
positive_dataset$Density[positive_dataset$population >= 1101] <- "Extreme"
positive_dataset$Density[positive_dataset$population >= 701 & positive_dataset$population <= 1100] <- "High"
positive_dataset$Density[positive_dataset$population >= 101 & positive_dataset$population <= 700] <- "Average"
positive_dataset$Density[positive_dataset$population <= 100] <- "Low"
positive_dataset
summary(positive_dataset)


density <- factor(positive_dataset$Density, order = TRUE, levels = c("Low", "Average", "High", "Extreme"))
positive_dataset$Density <- density
str(positive_dataset)

positive_dataset <- na.omit(positive_dataset)
positive_dataset

#dataset is ready

#normality test

# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")
attach(positive_dataset)
plot(Density, total_deaths, pch = 19, col = "lightblue")

histogram(~total_deaths | Density, 
          data = positive_dataset, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(total_deaths, Density, median)


# check normality of data

qqnorm(total_deaths)
# this line represents normal distribution
qqline(total_deaths, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
library(dplyr)
sample_n(positive_dataset, 5)
temp <- sample_n(positive_dataset, 5000)
temp
normality_test <- shapiro.test(temp$total_deaths)
normality_test$p.value


hist(total_deaths)

# not normally distributed
# After consulting the chart, I am examining
# a dependent continuous variable (temp)
# with an independent categorical variable (activity)
# so I use the Mann-Whitney test
# this is also known as the "Kruskal-Wallis test"
# Format = wilcox.test(dependent~independent)
kruskal.test(total_deaths ~ Density, data = positive_dataset)

# P value is smaller than 0.05 thus we can reject the null hypothesis
---------------------------------------------------------------------------------------------------------------------------------------
#question 3 Does fully vacination affect the total number of cases?
#H0:Fully Vacination doesnt  affect the total number of cases
#H1 :Fully Vacination doesnt not affect the total number of cases

#Attaching of covid data in the plot
attach(covid_data)
names(covid_data)

#creating a subset for continent europe 
covid_subset <- subset(covid_data, continent %in% c("Europe"),
                       select = c(iso_code, location, date, people_fully_vaccinated, total_cases))
str(covid_subset)
head(covid_subset)
dim(covid_subset)
sum(is.na(covid_subset))

# Data cleaning for subset of data is now completed with no missing values. 
# A graph would be showing the total number of new cases per date, group by location. 
# We have to transform our dataset, so that it is grouped by date and location.

covid_new <- covid_subset %>% group_by(date, location) %>%
  summarize(people_fully_vaccinated = sum(people_fully_vaccinated), total_cases = sum(total_cases)) 
head(covid_new)

# Check for missing data
incomplete_data <- covid_subset[!complete.cases(covid_subset),]
nrow(incomplete_data)

#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)
covid_data

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# Check whether the variables used for the hypothesis test are normally distributed or not. 
# Doing this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)

plot(people_fully_vaccinated, total_cases, pch = 9, col= "red",
     main = "comparision of people_fully_vaccinated with total_cases",
     xlab = "people_fully_vaccinated",
     ylab = "total_cases")


options(scipen = 999)
ggplot(covid_subset, aes(x=people_fully_vaccinated,y=total_cases))+ geom_point(col="red", size=3)


# plotting histograms to view if the variables are normally Distributed 

#arrange the plots in 1 rows by 2 cols
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))
par = opar

hist(people_fully_vaccinated, col = "cyan", main = "dist of people_fully_vaccinated" , xlab = "people_fully_vaccinated in Europe")
hist(total_cases, col = "cyan", main = "dist of total_cases in Europe")

#Visual analysis seems to indicate the data normally distributed
#Summarize the
tapply(people_fully_vaccinated, total_cases, median)

# we can also examine the linear correlation between both variables using Quantile-quantile plot (Q-Q plot)
with (covid_subset, {qqplot (people_fully_vaccinated,total_cases,
                             main = "comparing people_fully_vaccinated and total_cases",
                             xlab = "people_fully_vaccinated",
                             ylab = "total_cases")})


# Using Quantile-quantile plot (Q-Q plot) allows us to check
# if the data is normally distributed or not 

#Is people_fully_vaccinated normally distributed?
with (covid_subset, {qqnorm (people_fully_vaccinated,
                             main = "Normal QQ-plot of people_fully_vaccinated",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})
# Add line that represents normal distribution
qqline(people_fully_vaccinated, col = "blue")
# people_fully_vaccinated appears not to be normally distributed


#Is total_cases normally distributed?
qqnorm(total_cases)

with (covid_subset, {qqnorm (total_cases,
                             main = "Normal QQ-plot of total_cases",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(total_cases, col = "blue")
# total_cases appears not to be normally distributed


# ------------------------------------------ shapiro-wilks test ---------------------------------------------------#
# we can run the formal test of normality provided through the widely used
# shapiro-wilks test

my_sample<-covid_subset[sample(1:nrow(covid_subset), 10000, replace = FALSE),]
my_sample

# normality test for people_fully_vaccinated
normality_test <- shapiro.test(my_sample$people_fully_vaccinated)
normality_test$p.value

#

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.000000000000000000000005501516
# 0.000000000000000000000005501516 > 0.05 (False)
# Therefore the var people_fully_vaccinated is not normally distributed

# normality test for total_cases
normality_test <- shapiro.test(my_sample$total_cases)
normality_test$p.value

# 


#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.00000000000000000000000000000000000000000000000000003300901
# 0.00000000000000000000000000000000000000000000000000003300901 > 0.05 (False)
# Therefore the var total_cases is not normally distributed


pairs.panels(covid_subset,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   

#0.62***
# Need to decide a test for calulating p-value
# Here both variables are continuous but the dependent variable is not normally distributed. 
# Hence going for a non parametric test i.e. 
# Spearman’s Correlation Coefficient (also use for ordinal data) 


corr1 <- cor.test(x=covid_subset$people_fully_vaccinated, 
                  y=covid_subset$total_cases, method = 'spearman')
corr1

#Spearman's rank correlation rho

#data:  covid_subset1$people_fully_vaccinated and covid_subset1$total_cases
#S = 1.1918e+10, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
# rho 
#0.6220994 
covid_data
#-----------------------------------------------------------------------------------------
#question :5

hand_wash <- subset(covid_data, !is.na(covid_data$handwashing_facilities),select = c("handwashing_facilities", "total_cases"))
hand_wash
md.pattern(hand_wash)
summary(hand_wash)

pairs(hand_wash, labels = colnames(hand_wash), main = "SARS Covid-19 dataset correlation plot")

library(psych)
pairs.panels(hand_wash,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



na.omit(hand_wash)
hand_wash <- na.omit(hand_wash)
hand_wash

pairs.panels(hand_wash,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     
md.pattern(hand_wash)


# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")
attach(hand_wash)
plot(total_cases, handwashing_facilities, pch = 19, col = "red")

histogram(~total_cases | handwashing_facilities, 
          data = hand_wash, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(total_cases, handwashing_facilities, median)


# check normality of data

qqnorm(total_cases)
# this line represents normal distribution
qqline(total_cases, col = "blue")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
temp <- sample_n(hand_wash, 5000)
normality_test <- shapiro.test(hand_wash$total_cases)
normality_test$p.value


hist(total_cases)

# not normally distributed

cor.test(handwashing_facilities, total_cases,  method = "spearman")

# Spearman’s Correlation Coefficient
#since p value is smaller than cuto 0.05 thus we can reject the null hypthesis
# There is a strong negative corlation between icu pateints and people who are fully vaccinated

detach(hand_wash)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------------------

#Question 1
#Does Female smokers are more likely to die from Covid_19?

#H0= Male Smokers are more likely to die from Covid_19
#H1= Male Smokers are not likely to die from Covid_19

#modifying the blank spaces/NA and replacing it with 0 for total death varaiable.
covid_data$total_deaths[is.na(covid_data$total_deaths)] <- 0
covid_data$total_deaths

#modifying the blank spaces/NA and replacing it with 0 for male smokers varaiable.
covid_data$male_smokers[is.na(covid_data$female_smokers)] <- 0
covid_data$male_smokers

#Creating the subsets for total deaths to male smokers
covid_data_subset <- subset(covid_data, select = c(total_deaths, female_smokers))
covid_data_subset

#aggreating the missing values
missing_values <- aggr(covid_data_subset, prop = FALSE, numbers = TRUE)
summary(missing_values)
str(covid_data_subset)

#Attaching the subset dataframe 
attach(covid_data_subset)

#Checking the linearity and correlation
ggplot(covid_data_subset, aes(x=total_deaths,y=female_smokers))+ geom_point(col="red", size=7)
#

#plotting the total deaths to male smokers using ggplot
plot(total_deaths, female_smokers, pch = 9, col= "red",
     main = "comparision of total_deaths with female_smokers",
     xlab = "total_deaths",
     ylab = "female_smokers")



#Summarize the medians of the data to confirm that the data is normally distributed or not
tapply(total_deaths, female_smokers, median)


#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 

#is total death normally disturbuted?
qqnorm(total_deaths)
# Add line that represents normal distribution
qqline(total_deaths, col = "blue")
# total_deaths appears not to be normally distributed

#Is diabetes_prevalence normally distributed?
qqnorm(female_smokers)
# Add line that represents normal distribution
qqline(female_smokers, col = "red")

#this Shows the scatter plot of the matrix formed by total deaths for male smokers.
pairs.panels(covid_data_subset,smooth = TRUE, scale = FALSE, density = TRUE,ellipses = TRUE,method = "spearman",pch = 21,lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2, 
             hist.col = 4, stars = TRUE, ci = TRUE)    


# Calculating the P-values is to be determined.Need to decide a test for calculating p-value



correlation_1 <- cor.test(x=covid_data_subset$total_deaths, 
                          y=covid_data_subset$female_smokers, method = 'spearman')
correlation_1
#S=241899, p-value <0.00002
#It clearly shows that alternative hypothesis is true i.e. male smokers are not likely to die from covid_19