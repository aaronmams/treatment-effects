#This script will walk through the estimation of average treatment effects using
# inverse probability weights
library(ggplot2)
library(dplyr)

#start with the same fake data we used to illustrate the POM estimator
age=rnorm(100,30,5)
psmoke <-  pnorm((age-mean(age))/sd(age))
smoke <- rbinom(100,1,psmoke)
z <- data.frame(age=age,smoke=smoke,bw=3000+(5*age)+(25*smoke) + rnorm(100,100,25))

ggplot(z,aes(x=age,y=bw,color=factor(smoke))) + geom_point() + geom_smooth(method='lm')


#calculate probability weights: fit a logit model and use the fitted values as 
# probabilities
logit.bw <- glm(smoke~age,data=z,family='binomial')
pi <- predict(logit.bw,newdata=z,type="response")

#weight smokers by 1/p(i) so that weight is large when probability of being a smoker is
# small.  Weight observations on non-smokers by 1/(1-p(i)) so weights are large when 
# probability is small
z <- tbl_df(z) %>% mutate(w=pi) %>% 
    mutate(weight=ifelse(smoke==1,1/pi,1/(1-pi)))

ggplot(z,aes(x=age,y=bw,color=factor(smoke),size=weight)) + geom_point(shape=1) + 
  scale_color_manual(values=c("red","black")) 

