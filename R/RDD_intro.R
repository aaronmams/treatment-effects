

library(memisc)
library(dplyr)

test <- tbl_df(as.data.frame(as.data.set(spss.portable.file('data/RDD_data/04091-0001-Data.por'))))

# difference in IQ score for DC_TRT groups
summary(lm(sbiq24~dc_trt,data=test))
t.test(x=test$sbiq24[test$dc_trt=='Treatment'],y=test$sbiq24[test$dc_trt=='Control'])


lm_rct1 = lm(sbiq48 ~ dc_trt, test)
summary(lm_rct1)

#===========================================================================
#subset data to create artifical sharp RDD
#here assuming that only mothers with an IQ lower than the median IQ (85) are
#selected for the treament
dat_s = filter(test,
               (momwais0 >= median(momwais0) & dc_trt=='Control') |
                 (momwais0 < median(momwais0) & dc_trt=='Treatment'),!is.na(sbiq48))
#==========================================================================

