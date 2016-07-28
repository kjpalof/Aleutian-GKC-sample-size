## Aleutian GKC sample size and power review
# K. Palof ADF&G 
# 5-23-16
# Power Analysis - need results of explore_code for this.

library(pwr)
help(pwr)
pwr.t.test(n=5, d = .2, sig.level = 0.05, type = "one.sample")

#Effect size 
#https://en.wikipedia.org/wiki/Effect_size#Difference_family:_Effect_sizes_based_on_differences_between_means

#calculate Cohen's d to determine what effect size I have currently - this has to be done between two means
# 
s = sqrt(((5-1)*(0.0254^2)+(5-1)*(0.06597^2))/(5+5)) #using sample size of 5 
d= ((0.05503-0.14419)/s)
#d = -1.99
# effect size was determine by using the first two strings in the series.  Maybe a better way to do this?
# using the first two string with total crab

# now for legal crab
s = sqrt(((5-1)*(0.02389^2)+(5-1)*(0.0319^2))/(5+5))
d= ((0.0530-0.09236)/s)
# d = -1.56

pwr.t.test(d = 1.56, power = .85, sig.level = 0.05, type = "two.sample") # sample size of 8 would give you 85% power
pwr.t.test(n = 5, d = 1.56, sig.level = 0.05, type = "two.sample")
# power to detect a difference between strings is 0.58
# 58% of the time 5 samples is enough to determine a significant difference in mean legal crab per string.

# power to detect a change in CPUE from year to year - what kind of change?
#LEGAL CRAB 
# difference of 0.5 crabday? 0.0208
s_region = 0.03898039
d_region= ((0.0208)/s_region)
# overall regional CPUE - here 18 strings. - in reality there are 70
#d region 0.5336
pwr.t.test(d = 0.53, power = .85, sig.level = 0.05, type = "two.sample") # sample size of 65 would give you 85% power
pwr.t.test(n = 70, d = 0.53, sig.level = 0.05, type = "two.sample")
# with a sample size of 70 string you have 88% power to detect a difference of 0.5 crabs per day

# difference of 0.25 crabday ? 0.0104
s_region = 0.03898039
d_region= ((0.0104)/s_region)
# overall regional CPUE - here 18 strings. - in reality there are 70
#d region 0.5336
pwr.t.test(d = 0.27, power = .85, sig.level = 0.05, type = "two.sample") # sample size of 247 would give you 85% power
pwr.t.test(n = 66, d = 0.27, sig.level = 0.05, type = "two.sample")
# with a sample size of 70 string you have 35% power to detect a difference of 0.25 crabs per day
pwr.t.test (n=66, sig.level= 0.05, power= 0.85, type =("two.sample"))
