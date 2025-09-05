library(tidyverse)

m425 <- read.csv("C:/Users/Jared/OneDrive/Desktop/Math 425/Math425PastGrades.csv", stringsAsFactors=TRUE)

m425 <- m425 %>%
  mutate(f70 = ifelse(FinalExam > 70, 1, 0))



pairs(m425, panel=panel.smooth, col=rgb(.2,.2,.2,.2))



glm1 <- glm(f70 ~ Midterm, data=m425, family=binomial)
summary(glm1)
AIC(glm1)

glm2 <- glm(f70 ~ Midterm + MagicTwoGroups, data=m425, family=binomial)
summary(glm2)
AIC(glm2)

glm3 <- glm(f70 ~ Midterm + MagicTwoGroups + Midterm:MagicTwoGroups, data=m425, family=binomial)
summary(glm3)
AIC(glm3)


#error due to empty comma
glm4 <- glm(f70 ~ Midterm + MagicTwoGroups + Midterm:MagicTwoGroups + , data=m425, family=binomial)
summary(glm4)


m425$ClassActivitiesCompletedPerfectly
glm5 <- glm(f70 ~ Midterm*AssessmentQuizzes*MagicTwoGroups*ClassActivitiesCompletedPerfectly, data=m425, family=binomial)
summary(glm5)




glm5 <- glm(f70 ~ Midterm*MagicTwoGroups + AssessmentQuizzes  + ClassActivitiesCompletedPerfectly, 
            data=m425, family=binomial)
summary(glm5)

predict(glm5, data.frame(Midterm=70, AssessmentQuizzes=33, ClassActivitiesCompletedPerfectly="Y",MagicTwoGroups=1),
        type="response")

summary(m425[,c("Midterm","AssessmentQuizzes","MagicTwoGroups","ClassActivitiesCompletedPerfectly")])


library(ResourceSelection)

hoslem.test(glm5$y, glm5$fit, g=10)

plot(f70 ~ Midterm, data=m425)
b <- coef(glm5)
b

drawit <- function(Magic=1, AQ=33, CA=1){
  curve(1/(1+exp(-(b[1] + b[2]*Midterm + b[3]*Magic + b[4]*AQ + b[5]*CA + b[6]*Magic*Midterm))), add=TRUE, xname="Midterm")
}

drawit(Magic=1, AQ=33, CA=1)

points(70, 0.21, col="green", cex=4, pch=16)




keep <- sample(1:nrow(m425), 90)

mytrain <- m425[keep,]
mytest <- m425[-keep,]

glm.test <- glm(f70 ~ Midterm*MagicTwoGroups + AssessmentQuizzes  + ClassActivitiesCompletedPerfectly, 
            data=mytrain, family=binomial)

mypreds <- predict(glm.test, newdata=mytest, type="response")

mydecs <- ifelse(mypreds > 0.5, 1, 0)

cm <- table(mydecs, mytest$f70)

pcc <- (cm[1] + cm[4]) / sum(cm)
pcc

