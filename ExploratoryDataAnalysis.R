rm(list = ls())

data <- read.csv('Student_Performance.csv')

summary(data)

sum(is.na(data))

table(data$Extracurricular.Activities)

data$Extracurricular.Activities <- factor(data$Extracurricular.Activities,levels = c('No','Yes'))
data$Extracurricular.Activities <- as.numeric(data$Extracurricular.Activities)

corr.matrix <- cor(data)
library(corrplot)
corrplot(corr = corr.matrix,method = 'number')


library(ggplot2)

prop.table(data$Extracurricular.Activities, 2)


ggplot(data, aes(x = Hours.Studied,y = Performance.Index)) + geom_point(size =  2, shape = 1)
