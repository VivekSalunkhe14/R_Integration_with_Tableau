#Installing libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)

#Reading the dataset
CQuality <- read.csv("cityquality.csv")

#Overview of dataset
typeof(CQuality)
str(CQuality)
summary(CQuality)

#Identifying column names with NA values
colnames(CQuality)[apply(CQuality,2,anyNA)]

#Creating a City Quality column which is sum of all Individual Parameters
df <- CQuality %>% mutate(CityQuality = Housing+Cost.of.Living+Startups+Venture.Capital+Travel.Connectivity
                          +Commute+Business.Freedom+Safety+Healthcare+Education+Environmental.Quality
                          +Economy+Taxation+Internet.Access+Leisure...Culture+Tolerance+Outdoors)

df1 <- as_tibble(df)


#Linear Regression model for prediction of City Quality based on Significant Parameters
model<- lm(CityQuality ~ Cost.of.Living+Startups+Travel.Connectivity
           +Business.Freedom+Safety+Healthcare+Education+Environmental.Quality
           +Economy+Leisure...Culture+Outdoors,data = df)

summary(model)

#Gives the best quality parameter for each city
attach(CQuality)
df2 <- data.frame(Housing,Cost.of.Living,Startups,Venture.Capital,Travel.Connectivity,Commute,
                  Business.Freedom,Safety,Healthcare,Education,Environmental.Quality,Economy,Taxation,
                  Internet.Access,Leisure...Culture,Tolerance,Outdoors)
df2
Best_aspect <- colnames(df2)[apply(df2,1,which.max)]

a <- cbind(CQuality,Best_aspect)
fix(a)

#Dplyr functions:

#Gathering information about Top Cities based on their Quality Score
TopCities <- CQuality %>%
  arrange(desc(CityQuality))
fix(TopCities)

#Cities with above average housing quality
CQuality %>% filter(Housing > mean(Housing, na.rm = TRUE)) 
print(CQuality)

#Grouping cities by continents and Calculating statistical parameters 
CQuality %>%
  group_by(UA_Continent) %>%
  summarise(cost = mean(Cost.of.Living, na.rm = TRUE), Safety = sd(Safety))

#Calculating rating for each city whether they are above or below average
Overall_score=ifelse(CQuality$CityQuality>80, "Above average","Below average")
Rating=cbind(CQuality, Overall_score)
View(Rating)


#Data manipulation :-

#Selecting Random Fraction of Rows in case needed for surveying data
sample_frac(CQuality,0.2)


#Selecting column names starting with a specific character
x = select(CQuality, starts_with("U"))
View(x)

#starts_with()	Starts with a prefix
#ends_with()	Ends with a prefix
#contains()	Contains a literal string
#matches()	Matches a regular expression
#num_range()	Numerical range like x01, x02, x03.
#one_of()	Variables in character vector.
#everything()	All variables.

#Selecting row names starting with a specific character
filtered_df <- CQuality %>% dplyr::filter(substr(UA_Name,1,1) == "A")
fix(filtered_df)


#summary of all numeric variables in the dataset
e <- summarise_all(df2, funs(n(),mean,median))
View(e)


#Grouping continent with the average sum of financial data
FinancialData <- CQuality %>%
  group_by(UA_Continent) %>%
  summarise(FinancialData = mean(Economy, na.rm = TRUE)+mean(Taxation, na.rm = TRUE)+
              mean(Venture.Capital, na.rm = TRUE)+mean(Business.Freedom,na.rm = TRUE))
View(FinancialData)


#Grouping continent with the average sum of Real estate data
RealEstateData <- CQuality %>%
  group_by(UA_Continent) %>%
  summarise(AccomodationData = mean(Cost.of.Living,na.rm = TRUE)+mean(Healthcare,na.rm = TRUE)+
              mean(Housing,na.rm = TRUE)+mean(Safety,na.rm = TRUE)+mean(Travel.Connectivity,na.rm = TRUE))
View(RealEstateData)


#Grouping continent with the average sum of Educational data
EducationalData <- CQuality %>%
  group_by(UA_Continent) %>%
  summarise(AccomodationData = mean(Education,na.rm = TRUE)+mean(Healthcare,na.rm = TRUE)+
              mean(Housing,na.rm = TRUE)+mean(Internet.Access,na.rm = TRUE))
View(EducationalData)


#Selecting city names with A 
a = filter(CQuality,grepl("A",UA_Name,fixed = TRUE))
fix(a)


#Plots for visualizations


#Continent wise City Quality colored on the basis of safety
qplot(data=CQuality, x=UA_Continent, y=CityQuality, colour=Safety,main = "Continent wise City Quality")

#Visualizing outliers using boxplot for each continent
boxplot(CityQuality~UA_Continent,data=CQuality,main="Visualizing outliers using boxplot for each continent") 


#Visualizations using Plot_ly 

#Scatter plot for Health care vs Safety 
fig<-plot_ly(data=CQuality,x=~Healthcare,y=~Safety) %>% 
  layout(title = "HealthCare vs Safety",                
         xaxis = list(title = "HealthCare"),
         yaxis = list(title = "Safety"))

fig

#Distribution of City Quality as per Safety parameter
fig<-plot_ly(CQuality,x=~Safety,y=~CityQuality,
             color = ~CityQuality,size = ~CityQuality) %>% 
  layout(title = "Safety vs CityQualtiy",                
         xaxis = list(title = "Safety"),
         yaxis = list(title = "CityQuality"))
fig

#Calculating mean of Crucial Parameters grouped by Continents and storing them in a new 
#dataframe Score

score <- CQuality %>%
  group_by(UA_Continent) %>%
  summarise(CityQuality =mean(CityQuality,na.rm = TRUE),Cost = mean(Cost.of.Living, na.rm = TRUE), 
            Safety = mean(Safety,na.rm = TRUE),Education = mean(Education,na.rm = TRUE), 
            Economy = mean(Economy, na.rm = TRUE))
View(score)

#Showing transition of Crucial Parameters based on Continents
fig <- plot_ly(data = score, x = ~UA_Continent, name = "Trends")
fig <- fig %>% add_trace(y = ~Cost,type = 'scatter', name = 'Cost of Living', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~Safety,type = 'scatter', name = 'Safety', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~Education,type = 'scatter', name = 'Education', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~Economy,type = 'scatter', name = 'Economy', mode = 'lines+markers')
fig <- fig %>% layout(title = "Trend of Parameter",                
                      xaxis = list(title = "UA_Continent"),
                      yaxis = list(title = "Score"))
fig


#Visualizing Parameters on tooltip for each Continent
fig = plot_ly(
  CQuality, x = ~UA_Continent, y = ~Cost.of.Living,
  text = ~paste("Cost of Living : ",Cost.of.Living,'<br>Internet :',Internet.Access,
                '<br>Housing :',Housing,'<br>City :',UA_Name),
  color = ~UA_Continent, size=~Cost.of.Living) %>% 
  layout(title = "Information about Parameters for each Continent",                
         xaxis = list(title = "UA_Continent"),
         yaxis = list(title = "CityQuality"))
fig
  

#Integration of R with Tableau
#install.packages("Rserve")
library(Rserve)
Rserve(debug = TRUE)
