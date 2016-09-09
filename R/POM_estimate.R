
#illustrative example - what if older women were more likely to give birth to 
# heavier babies and more likely to smoke during pregnancy
data.frame(m)


#read the Cattaneo2.dta data set in
getwd()
df <- read.csv("data/cattaneo2.csv")

#generate some data to illustrate the potential observed mean, probability of being a 
# smoker increases with age
age=rnorm(100,30,5)
psmoke <-  pnorm((age-mean(age))/sd(age))
smoke <- rbinom(100,1,psmoke)
z <- data.frame(age=age,smoke=smoke,bw=3000+(5*age)+(25*smoke) + rnorm(100,100,25))

ggplot(z,aes(x=age,y=bw,color=factor(smoke))) + geom_point() + geom_smooth(method='lm')

#Next we move on to analyzing the actual birthweight data from Cattaneo 2010

#IMPORTANT NOTE: In our example we pretended like older women were more likely to smoke during 
# pregnancy.  In the actual data it is the case that younger women are more likely to smoke 
# during pregnancy.

#we can see this by running a logistic regression to explain smoking status
# using age
summary(glm(factor(mbsmoke)~mage,data=df,family='binomial'))

#note the negative coefficient estimate on age indicating that older women
# were less likely to smoke during pregnancy

#-----------------------------------------------------------------------------------
#Potentially observed mean (POM) is just derived by estimating a separate regression
# for the treatment group and control group.  Then take the entire sample and assign them
# to the treatment group and get the fitted value...do the same thing assigning everyone in 
# the sample to the control group.  If you think about it visually, what you have done is 
# created two different slopes (least squares lines through different clusters of the data).  For
# any particular age women you can intersect that x-value with the regression lines to get the 
# predicted birthweight if that women were a smoker and if that woman were a non-smoker.


lm.smoker <- lm(bweight~mage,data=df[df$mbsmoke=='smoker',])
pred.smoker <- predict(lm.smoker,newdata=df)
mean(pred.smoker)

lm.ns <- lm(bweight~mage,data=df[df$mbsmoke!='smoker',])
pred.ns <- predict(lm.ns,newdata=df)
mean(pred.ns)

ate <- mean(pred.smoker)-mean(pred.ns)

#compare the POM estimate to a simple regression on age, smoker status and interaction
bw.lm <- lm(bweight~mage+factor(mbsmoke)+factor(mbsmoke)*mage,data=df)
df$pred.bw <- predict(bw.lm,newdata=df)
mean(df$pred.bw[df$mbsmoke=='smoker'])-mean(df$pred.bw[df$mbsmoke=='nonsmoker'])
#-----------------------------------------------------------------------------------
