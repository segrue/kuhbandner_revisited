#import packages
require("data.table")
require("ggplot2")
require("stringr")

#change locale to prevent compatibility issues with date conversions
lct <- Sys.getlocale("LC_TIME") 
Sys.setlocale("LC_TIME", "C")

#read data
dat <- fread("data_kuhbandner.csv") #data set provided by Kuhbandner
colnames(dat)
colnames(dat) <- c("country","date","week","tests_day","new_infect_day","death_day") #rename columns for easier handling
dat$rel_pos <- dat$new_infect_day/dat$tests_day #calculate relative amount of positive tests per day
dat[rel_pos > 1]$rel_pos <- NA #replace entries with relative positivity rate of larger 1 with NA (USA has such an entry -> faulty data entry)
dat[,date:=as.Date(dat$date,"%b %d, %Y")] #transform variable "date" into Date-Time-Format

dat2 <- dat[!(country=="Germany" | country=="France")] #remove Germany and France for daily plots because number of tests are only given weekly

#plot relative positive test rate over time by country 
# that's essentially what Kuhbandner does, but for daily instead of for weekly numbers: 
# simple division of number of positive results by total number of tests
p1 <- ggplot(dat2, aes(x=date, y = rel_pos)) + geom_line() + facet_wrap( ~ country) +
  labs(title="Percentage of positive tests over time by country", x ="Time", y = "Percentage of positive tests")
p1 

#plot infectious numbers over time by country
p2 <- ggplot(dat2, aes(x=date, y = new_infect_day)) + geom_line() + facet_wrap( ~ country) +
  labs(title="Absolute number of positive tests over time by country", x ="Time", y = "Absolute number of positive tests")
p2

#plot infectious numbers over time by country without keeping y-axis fixed (reduced comparibility, enhanced resolution per country)
p3 <- ggplot(dat2, aes(x=date, y = new_infect_day)) + geom_line() + facet_wrap( ~ country, scales="free_y") +
  labs(title="Absolute number of positive tests over time by country", x ="Time", y = "Absolute number of positive tests")
p3

#calculate at weekly rates of tests and newly detected infections
dat$neg_test <- dat$tests_day - dat$new_infect_day #calculate absolute amount of negative tests per day
dat[,weekly_tests:=sum(tests_day,na.rm=T),by=.(week,country)] #sum weekly tests
dat[,weekly_pos:=sum(new_infect_day,na.rm=T),by=.(week,country)] #sum weekly positive cases
dat[,weekly_neg:=weekly_tests-weekly_pos] #weekly negative cases

dat3 <- na.omit(dat) #remove all rows which contain NA (i.e. keep only entries for which we have data for all countries)
dat3 <- unique(dat3[, c("country", "week","weekly_tests","weekly_pos","weekly_neg")]) #keep only weekly data
dat3[,rel_pos_weekly:=weekly_pos/weekly_tests]

#plot relative positive test rate per week per country
p4 <- ggplot(dat3, aes(x=week, y = rel_pos_weekly)) + geom_line() + facet_wrap( ~ country) +
  labs(title="Percentage of positive tests per week per country", x ="Calendar week", y = "Percentage of positive tests")
p4 

#transform data to recreate Kuhbandner's graph in fig3 left-hand side (exact replication is p6)
dat4 <- unique(melt(dat3, id.vars = c("country",
                                     "week"),measure.vars=c("weekly_pos","weekly_neg")))

#stacked plot of positive and negative tests by country
p5 <- ggplot(dat4, aes(x=week, y = value, fill=factor(variable))) +
  geom_bar(stat="identity",position=position_stack(reverse = TRUE)) + facet_wrap( ~ country) +
  labs(title="Absolute number of positive and negative tests over time by country", 
       x ="Calendar week", y = "Absolute number of tests", fill = "Test result") +
  scale_fill_discrete(labels=c("Positive tests","Negative tests"))
p5

#stacked plot of positive and negative tests by country without keeping y-axis fixed (reduced comparibility, enhanced resolution per country)
p6 <- ggplot(dat4, aes(x=week, y = value, fill=factor(variable))) +
  geom_bar(stat="identity",position=position_stack(reverse = TRUE)) + 
  facet_wrap( ~ country, scales="free_y") +
  labs(title="Absolute number of positive and negative tests over time by country", 
       x ="Calendar week", y = "Absolute number of tests", fill = "Test result")  +
  scale_fill_discrete(labels=c("Positive tests","Negative tests"))
p6 #reproduction of figure 3, left-hand side

#dodged plot of positive and negative tests by country
p7 <- ggplot(dat4, aes(x=week, y = value, fill=factor(variable))) +
  geom_bar(stat="identity",position="dodge") + facet_wrap( ~ country) + +
  labs(title="Absolute number of positive and negative tests over time by country", 
       x ="Calendar week", y = "Absolute number of tests", fill = "Test result")  +
  scale_fill_discrete(labels=c("Positive tests","Negative tests"))
p7

#dodged plot of positive and negative tests by country without fixed y-axis (reduced comparibility, enhanced resolution per country)
p8 <- ggplot(dat4, aes(x=week, y = value, fill=factor(variable))) +
  geom_bar(stat="identity",position="dodge") + facet_wrap( ~ country, scales="free_y") +
  labs(title="Absolute number of positive and negative tests over time by country", 
       x ="Calendar week", y = "Absolute number of tests", fill = "Test result") +
  scale_fill_discrete(labels=c("Positive tests","Negative tests"))
p8

#attempt to reproduce figure 3, righthandside
#according to the description on page 12, the relative increases are calculated 
#by scaling the increase of the number of cases to week 10
dat3[,scaled_increase_rel:= rel_pos_weekly/rel_pos_weekly[week==10],by="country"]
dat3[,scaled_increase_abs:= weekly_pos/weekly_pos[week==10],by="country"]

#transform data to recreate Kuhbandner's graphs in fig 3 right-hand side
dat5 <- unique(melt(dat3, id.vars = c("country",
                                      "week"),measure.vars=c("scaled_increase_rel","scaled_increase_abs")))

#dodged plot of relative and absolute increase of case numbers, scaled by week 10 numbers
p9 <- ggplot(dat5, aes(x=week, y = value, fill=factor(variable))) +
  geom_bar(stat="identity",position="dodge") + facet_wrap( ~ country) +
  labs(title="Relative increase of tests results scaled by week 10", 
       x ="Calendar week", y = "x-fold increase compared week 10", fill = "Type of measure") +
  scale_fill_discrete(labels=c("Percentage of positive tests","Absolute numbers of positive tests"))
p9

#dodged plot of relative and absolute increase of case numbers, scaled by week 10 numbers, without fixed y-axis (reduced comparibility, enhanced resolution per country)
p10 <- ggplot(dat5, aes(x=week, y = value, fill=factor(variable))) +
  geom_bar(stat="identity",position="dodge") + facet_wrap( ~ country, scales="free_y") +
  labs(title="Relative increase of tests results scaled by week 10", 
       x ="Calendar week", y = "x-fold increase compared week 10", fill = "Type of measure") +
  scale_fill_discrete(labels=c("Percentage of positive tests","Absolute numbers of positive tests"))
p10 #reproduction of figure 3, right-hand side, but with the two meausures next
#to each other instead of on top of each other to improve comparability

# the million dollar question: can we get infection numbers as reported by the countries
# under the assumption that the lower growth rates postulated by Kuhbandner are accurate (spoiler: we can't)?

# In Kuhbandner's eyes the "true" number of new infections is the relative
# increase of the percentage of positive tests in week x scaled to the percentage of
# positive tests in week 10, where x = [11, 12, 13, 14]. The "artificial" number of new infections
# is the relative increase of the absolute number of positive tests in week x scaled to the 
# absolute number of positive tests in week 10. Setting aside the obvious problem that 
# comparing increases in percentages and increases in absolute numbers is utterly misleading and deeply 
# problematic from a theoretical point of view (see explanation below) we can check whether it is possible to get 
# such high absolute numbers of new cases as were observed *if* we use only the relative growth of percentages of positive
# in week x scaled to week 10. Below I use the example of Italy, but you can just replace "ctr_name <- "Italy" with any other country. 
# I checked it for all of the countries. Except for Austria, the number of actually detected cases in week 14 is way above the number of cases that 
# one would get if one would assume the low growth rates that Kuhbandner claims.

ctr_name <- "Italy"
ctr <- dat4[country==ctr_name]
ctr_growth <- dat5[country==ctr_name & variable=="scaled_increase_rel"]

# note that in the following I'm making a charitable assumption, namely that 
# that the growth rate from week 9 to week 10 is the same as from week 10 to week 11 (even though
# the epidemic was less spread back in week 9)
new_cases_week_10 <- ctr[variable=="weekly_pos"]$value[1]-ctr[variable=="weekly_pos"]$value[1]/ctr_growth$value[2]
new_cases_week_11 <- new_cases_week_10*ctr_growth$value[2]
new_cases_week_12 <- new_cases_week_11*ctr_growth$value[3]
new_cases_week_13 <- new_cases_week_12*ctr_growth$value[4]
new_cases_week_14 <- new_cases_week_13*ctr_growth$value[5]
detected_new_cases_week_14 <- ctr[variable=="weekly_pos"]$value[5]
diff <- detected_new_cases_week_14- new_cases_week_14
diff

# As we can see, there is a considerable difference of the actually observed cases to the theoretically predicted cases given
# Kuhbandner's lower growth rates. Within Kuhbandner's scenario, this could only be explained if we assume that there
# is a considerable number of people who got tested positive in week 14 but who were already infected in week 9. Only then could we
# explain why the number of newly detected cases in week 14 exceeds the number of theoretically predicted new cases in that week. 
# Given a median incubation period of somewhere around a week and given a duration of RT-PCR
# positivity of 1-2 weeks for asymptomatic cases and up to 3 weeks for mild to moderate cases
# (https://www.who.int/news-room/commentaries/detail/transmission-of-sars-cov-2-implications-for-infection-prevention-precautions)
# we can assume that all those who were infected in week 9 could be detected up to 13, but no much beyond.
# One could argue that the severe cases, which have of course a much longer RT-PCR positivity duration
# could account for the additional cases after week 13. However, this assumption is unsubstantiated as it would necessitate
# severe cases to go undetected for four weeks which is insofar unrealistic as severe cases need - by definition - 
# have such severe symptoms that they usually require medical treatment making it highly unlikely that a large part of 
# severe cases go undetected for over a month. 

# Now on to the theoretical criticism: Why is a comparison of a relative increase of percentage with a relative increase of absolute numbers misleading? 
# Because the relative increase of percentages is bounded above whereas the relative increase of absolute numbers isn't. 
# Hence, you will *always* find a lower growth rate of percentages than of absolute numbers when you increase the number of tests conducted.
# and scale it by some starting value. But the problems with Kuhbandner's reasoning go much deeper. 

# If we had a scenario in which the number of new cases is growing exponentially *and* we have an exponential growth
# of the number of tests with the same growth rate in both cases, we'd had an exponential increase of the scaled number of
# absolute cases, but *no increase whatsoever* in the scaled percentage numbers, as the following example shows: 

# We assume that the number of tests grows exponentially with the same rate as the epidemic (i.e. can just "keep pace" with 
# the epidemic) and that we detect 10% of the new infections. In this scenario, the driver of the increased infection numbers 
# is obviously the epidemic, not the number of tests. But even in this scenario, the relative increase of the 
# percentages of positive tests (scaled to the initial percentage at t = 0) does *not* increase, but remains fixed at 1. If we now
# follow Kuhbandner's advice and simply look at the relative increase of the percentage of positive tests scaled to t = 0, we'd 
# come to the absurd conclusion, that there is no increase in "true" new infections - which is obviously wrong since we said that 
# there *is* an exponential growth of cases at the onset of the example.

# Now let us assume that the number of tests grows *quicker* than the epidemic with a growth rate that is double the one of the
# growth of the epidemic. This scenario is akin to the claim that Kuhbandner makes, namely that the number of tests increases quicker
# than the number of new cases and the the absence of an increase in the scaled percentages of positive tests scaled to t=0 indicates
# that there is no exponential growth. This conclusion is wrong, as the following simulation shows: 

n_cases_start <- 100 #the initial number of cases
n_intervals <- 100 #growth intervals
growth_rate <- 1.1

growth_function_cases <- function(t){
  return (n_cases_start*(1+growth_rate)^t)
}
growth_function_tests <- function(t){
  return (n_cases_start*10*(1+growth_rate*2)^t)
}
time_steps <- seq(0,n_intervals)
n_cases <- sapply(time_steps, growth_function_cases)
n_tests <- sapply(time_steps, growth_function_tests)

# let us assume that only 33% of the infected people intially present for testing and are detected
# (the percentage is irrelevant - you can plug in all percentages between 1 and 99 and will arrive
# to the same conclusion with regard to fundemantal theoretically flaw in Kuhbandner's reasoning)

# and let us furthermore assume that this percentage approaches 100% exponentially, to make our case even stronger
# this last assumptions gives credit to Kuhbandner's claim that the more we test, the more "hidden" infections are detected
# at some point we conduct so many tests that all actual infections are detected.

exp_decay <- function(t){
  return (2*exp(-0.1*t))
}
decay <- 1+sapply(time_steps,exp_decay)
n_cases_detected <- n_cases/decay
percentage_positive <- n_cases_detected/n_tests
n_cases_scaled <- n_cases_detected/n_cases_detected[1] #scale the number of 
percentage_positive_scaled <- percentage_positive/percentage_positive[1]

dat_sim <- data.table(time_steps,n_cases_scaled,percentage_positive_scaled)
dat_sim <- melt(dat_sim, id.vars = "time_steps")

#plot change of relative growth on linear scale
p_sim <- ggplot(dat_sim, aes(x=time_steps,y=value,colour=variable)) + geom_line() +
  labs(title="Relative change of positive test results scaled by week 10", 
       x ="Time step", y = "relative change per time step scaled by first time step", colour = "Type of measure") +
  scale_colour_discrete(labels=c("Absolute numbers of positive tests","Percentage of positive tests"))
p_sim

#plot change of relative growth on logarithmic scale
p_sim_log <- ggplot(dat_sim, aes(x=time_steps,y=value,colour=variable)) + 
  geom_line() +
  labs(title="Relative change of positive test results scaled by week 10, logarithmic scale", 
       x ="Time step", y = "log(relative change per time step scaled by first time step)", colour = "Type of measure") +
  scale_colour_discrete(labels=c("Absolute numbers of positive tests","Percentage of positive tests")) +  scale_y_log10()
p_sim_log

# As we can see, the percentage of positive cases scaled to the initial percentage of positive cases *actually drops* whereas the number
# of real cases increases exponentially. So even in a scenario in which we *have* (by construction) exponential growth , 
# Kuhbandner's calculations would force him to conclude that there is no exponential growth whatsoever. 
# This is not further surprising: The scenario which Kuhbandner constructs (number of new tests is growing faster than the number of
# new infections), will always lead to a decrease in the percentages of positive tests scaled by the initial percentages of positive tests - 
# even if there is an exponential growth of new infections. The reason: When I increase the number of tests heavily, then I'm bound to 
# test more people who do *not* have the disease (unless we assume that the whole population is infected).

#plot the everything into a pdf
library(gridExtra)
p <-list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p_sim,p_sim_log)
pdf("plots.pdf", onefile = TRUE)
for (i in seq(length(p))){
  print(p[[i]])
}
dev.off()
