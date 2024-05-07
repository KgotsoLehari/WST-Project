# Practical 5

# Name: Kgotso Lehari

# Student Number: 20530545

# WST 212 Project


library(sqldf)
library(stringr)
library(RH2)
library(lubridate)
library(rJava)
library(RJDBC)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

Crime <- read_csv('SouthAfricaCrimeStats_v2.csv')
Province <- read_csv('ProvincePopulation.csv')

q1 <- sqldf("SELECT Category
            FROM Crime ")

q2<- sqldf("select Province , count(Category) AS No_of_crimes
            FROM Crime
           WHERE Category = 'Drug-related crime' 
           GROUP BY Province
           ORDER BY No_of_crimes DESC")


q3 <- sqldf("select Category, count(Category) AS Num_of_crimes
            FROM Crime
            WHERE Province = 'Western Cape' 
            GROUP BY Category")

# Question 4
area <- Province$Area
pop <- Province$Population

q4 <-  ggplot(data = Province, mapping = aes(x = area, y = pop )) + 
  geom_point(shape = '*', size = 5 ) +
  scale_x_continuous( labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)


q4b<- q4 + labs(title = "The effect of population based on area in different provinces", x = "Area", y = "Population") +
geom_smooth(method = lm , fill = 'Blue') +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5))

# Question 5

q5 <- ggplot(aes(x = Density),  data = Province) +
  geom_histogram(bins = 50,color = 'blue') + 
  ggtitle('Histogram of the distribution of the density')


summary(Crime$`2015-2016`)

