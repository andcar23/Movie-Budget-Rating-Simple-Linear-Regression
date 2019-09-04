#Andrew Carroll
#Project 3
#April 23, 2018

library(summarytools)
library(ggplot2)
#Import Data
movie <- read.csv(file.choose())

#recode variable to drop outliers
descr(movie$budget)
movies <- subset(movie, movie$budget < 400000000 & movie$country == 'USA')

#Vieing varibale to see if it was imported correctly
View(movies)
str(movies)
descr(movie)
descr(movies)

#Putting movvies variables into their own object and getting descriptive statistics
budget <- movies$budget
score <- movies$imdb_score
descr(budget)
descr(score)

#Make OLS model into own object and check
model <- lm(score ~ budget)
model

#Check OLS asumptions
plot(model)

#Summary Statistics of OLS model
summary(model)
cor(budget, score)

#Plot Data
xtick <- c(0, 100, 200, 300, 400)

plot(budget,score, main = "Relationship Between \n Movies' Budget and Score",
     xlab = "Budget (Millions of Dollars)", ylab = "IMDb Score", xaxt = 'n')  
axis(side = 1, at= c(0, 1e+08, 2e+08, 3e+08, 4e+08), labels = c("0","100","200","300", "400"))
abline(model, cex = 3, col = "blue")








