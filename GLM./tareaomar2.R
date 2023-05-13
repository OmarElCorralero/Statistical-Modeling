library(nlme)
data("Glucose2")


head(Glucose2)
str(Glucose2)



plot(Glucose2$glucose,Glucose2$Time,xlab="Time",ylab="Glucose")

modeloglu0<-lm(Glucose2$Time~Glucose2$glucose)
abline(modeloglu0,col='red')


plot(Glucose2)



# Ajuste individual
library(ggplot2)
ggplot(data = Glucose2,
       mapping = aes(y = glucose, x = Time)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) + 
    facet_wrap(. ~ Subject)



ggplot(data = Glucose2,
       mapping = aes(y = glucose,
                     x = Time,
                     colour = Subject)) +
    geom_line() + facet_wrap(. ~ Date)



which(is.na(Glucose2)==T)
boxplot(glucose ~ Time*Subject,
        col=c("white","lightgray"),Glucose2)


library(lme4)
glu.model = lmer(glucose ~ Time + (1|Subject) + (1|Date), data=Glucose2)

summary(glu.model)

VarCorr(glu.model)

coef(summary(glu.model))

glu.null = lmer(glucose ~ Time +(1|Subject) + (1|Date), data=Glucose2, 
                       REML=FALSE)

anova(glu.null,glu.model)

coef(glu.model)

glu.model1 = lmer(glucose ~Time +(1+Time|Subject) + (1+Time|Date), 
                  data=Glucose2,
                  REML=FALSE)

summary(glu.model1)

coef(glu.model1)

anova(glu.model,glu.model1)

predict(glu.model)
predict(glu.model1)

