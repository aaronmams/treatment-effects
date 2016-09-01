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
probit.bw <- glm(smoke~age,data=z,family=binomial(link='probit'))
pi <- predict(probit.bw,newdata=z,type="response")

#weight smokers by 1/p(i) so that weight is large when probability of being a smoker is
# small.  Weight observations on non-smokers by 1/(1-p(i)) so weights are large when 
# probability is small
z <- tbl_df(z) %>% mutate(w=pi) %>% 
    mutate(weight=ifelse(smoke==1,1/w,1/(1-w)))

ggplot(z,aes(x=age,y=bw,color=factor(smoke),size=weight)) + geom_point(shape=1) + 
  scale_color_manual(values=c("red","black")) 
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#Now switch over to the actual data

#read the Cattaneo2.dta data set in
df <- read.csv("data/cattaneo2.csv")

#use the probit link for probability weights like they do in the STATA blog
probit.bw <- glm(mbsmoke~mage,data=df,family='binomial'(link='probit'))
pi <- predict(probit.bw,newdata=df,type="response")

#add inverse probability weights to the data frame
df <- tbl_df(df) %>% mutate(w=pi) %>% mutate(weight=ifelse(mbsmoke=='smoker',1/w,1/(1-w)),
                                             z=ifelse(mbsmoke=='smoker',1,0))

#ATE based on: 
#http://onlinelibrary.wiley.com/doi/10.1002/sim.6607/epdf
weighted.mean.smoker <- (1/(sum(df$z/df$w)))*sum(df$z*df$bweight/df$w)
weight.mean.ns <- (1/sum(((1-df$z)/(1-df$w))))*(sum(((1-df$z)*df$bweight)/(1-df$w)))
#calculate the weighted mean for smokers and non-smokers
df <- df %>% mutate(bw=bweight*weight)


