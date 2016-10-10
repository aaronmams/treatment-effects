library(dplyr)
library(ggplot2)
library(MatchIt)

#read the Cattaneo2.dta data set in
df <- tbl_df(read.csv("data/cattaneo2.csv")) %>% 
      mutate(smoker=ifelse(mbsmoke=='smoker',1,0))

#use mother's age, marital status, and education level to predict smoke/non-smoke
smoke.model <- glm(smoker~mage+medu+mmarried,family=binomial, data=df)

pr.df <- data.frame( pr_score = predict(smoke.model, type = "response"),
                      smoke = df$smoker )

#The 'MatchIt' package will perform the actual propensity score matching for us:
m.out <- matchit(smoker ~ mage + medu + mmarried,
                 method = "nearest", data = df)

#match.data creates a dataframe with only the matched obs
matched <- match.data(m.out)

#inspect the covariate balance
ggplot(matched,aes(x=distance,y=mage,color=mbsmoke)) + geom_point(alpha=0.4,size=1.5) + geom_smooth() +
  theme_bw() + scale_color_manual(values=c('red','black'))

ggplot(matched,aes(x=distance,y=medu,color=mbsmoke)) + geom_point(alpha=0.4,size=1.5) + geom_smooth() +
  theme_bw() + scale_color_manual(values=c('red','black'))

#t-test of means
t.test(matched$mage[matched$mbsmoke=="smoker"],matched$mage[matched$mbsmoke!="smoker"])
t.test(matched$medu[matched$mbsmoke=="smoker"],matched$medu[matched$mbsmoke!="smoker"])

with(matched, t.test(bweight ~ mbsmoke))
