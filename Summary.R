#####################################################
#                                                   #
#           Example Data Processing for             #
#        Data Aggregation and Summarization         #
#                                                   #
#####################################################

#This is a test program for summarizing and aggregating data. Data summarization
#can be useful when only one value can be utilized for some sort of display,
#e.g. Graphic of a difference between two individuals/populations or displaying
#statistics on a map.

#clear environment so no data from previous analyses interferes
rm(list=ls())

#filepath of the working directory. This will be different for you!
filepath <- "C:/Users/justi/OneDrive/Desktop/Portfolio/R/Summarization/"

#set the working environment to the desired path.
setwd(filepath)

#We will be utilizing the 'mpg' dataset, which comes pre-loaded with the tidyverse/ggplot2 package.
#This dataset contains gasoline efficiency data for automobile models.
#There are 234 observations of 11 variables, and car models are all of years either 1999 or 2008.
library(tidyverse)
data(mpg)

#also load additional package for later
library(psych)

#let's examine the data
head(mpg)

#now that we have an idea of how the data is structured, let's get to summarization.
#what is the mean mpg for each car model? we will need to summarize the data based on the model
summ_mpg <- mpg %>%
  group_by(manufacturer, model) %>% # group by the manufacturer column
  # calculate the mean mpg for city and highway for each car model
  summarise(avg_cty_mpg = (mean(cty, na.rm=T)), avg_hwy_mpg = (mean(hwy, na.rm = T))) %>% 
  na.omit()

#let's check the output for the code
head(summ_mpg)
#looks like it did what we wanted.

#what if we want to do the same, but only for cars of 2008 model?
mpg08 <- mpg[mpg$year == 2008,]
summ_mpg08 <- mpg08 %>%
  group_by(manufacturer, model) %>% # group by the manufacturer column
  # calculate the mean mpg for city and highway for each car model
  summarise(avg_cty_mpg = (mean(cty, na.rm=T)), avg_hwy_mpg = (mean(hwy, na.rm = T))) %>% 
  na.omit()

#for a more specific question:
#what is the average change in highway mpg for compact cars from 1999 to 2008?
#extract compact cars, one table for both years
mpgcomp08 <- mpg[mpg$class == "compact" & mpg$year == 2008,]
mpgcomp99 <- mpg[mpg$class == "compact" & mpg$year == 1999,]

#for simplicity, we will use the average mpg for each model from the set of each specific year.
summ_mpgcomp08 <- mpgcomp08 %>%
  group_by(manufacturer, model) %>% # group by the manufacturer column
  # calculate the mean mpg for city and highway for each car model
  summarise("2008_cty_mpg" = (mean(cty, na.rm=T)), "2008_hwy_mpg" = (mean(hwy, na.rm = T))) %>% 
  na.omit()

summ_mpgcomp99 <- mpgcomp99 %>%
  group_by(manufacturer, model) %>% # group by the manufacturer column
  # calculate the mean mpg for city and highway for each car model
  summarise("1999_cty_mpg" = (mean(cty, na.rm=T)), "1999_hwy_mpg" = (mean(hwy, na.rm = T))) %>% 
  na.omit()

#we will now combine the two summarized datasets
mpgcomp <- merge(summ_mpgcomp08,summ_mpgcomp99, by = c("manufacturer", "model"))

#we see that nissam altima from 1999 and subaru impreza from 2008 were excluded,
#due to changing classes sometime in between 1999 and 2008.

#calculate the difference in highway mpg
mpgcomp$hwy_change <- mpgcomp$'2008_hwy_mpg' - mpgcomp$'1999_hwy_mpg'
#save the table
write.csv(mpgcomp, file = "./ComapactCarsMileage.csv", row.names = F)

#we now have a new row in the mpg comp tabe which shows the change in highway mpg for
#each compact car model that exists in both years.

###Exploring graphs ----------------------------------------------------------------------

#perhaps some relationships can be found between mpg and other parameters included in the data.

#let's re-code some variables - the class variable is categorical, 
#but we can order them from smallest to largest.
#what are the unique values in class?
unique(mpg$class)
#after observation, we will order them as:
# 2seater<compact<subcompact<midsize<suv<minivan<pickup
# recode variable

mpg$class2[mpg$class=="2seater"] <- 0
mpg$class2[mpg$class=="compact"] <- 1
mpg$class2[mpg$class=="subcompact"] <- 2
mpg$class2[mpg$class=="midsize"] <- 3
mpg$class2[mpg$class=="suv"] <- 4
mpg$class2[mpg$class=="minivan"] <- 5
mpg$class2[mpg$class=="pickup"] <- 6

mpg <- mpg[order(mpg$class2),]

#let's produce a correlation and descriptive statistics table for a few parameters
simpdata <- as.matrix(subset(mpg, select=c(displ, cyl, cty, hwy,class2))) 
ct <- psych::corr.test(simpdata)
c(list(correlation_matrix = round(ct$r,3), sample_size = ct$n, pvalues_matrix = round(ct$p,3)))
des <- psych::describe(simpdata, type=2)
descstat <- t(as.matrix(subset(print(des,digits=3), select=c(mean, sd, skew, kurtosis, min, max)),digits=3))
cormat <- round(ct$r,3)
cormat[upper.tri(cormat, diag = TRUE)] <- NA
expdata <- rbind(cormat, descstat)
expdata

#we see that there is a high correlation (>0.70) between displacement and cylinders,
#displacement and both mileage variables, as well as between the two mileage variables.
#there is also a borderline high correlation between the class and the highway mileage.
#displacement and cylinders are likely to be multicollinear, so we will not examine those.
#hwy and cty are also likely to be multicollinear.

#This is a common example - are there relationships between displacement and mpg?
#we will be looking at city mpg for this plot
ggplot(mpg, aes(x=displ,y=cty)) +
  geom_point()

#adding a loess curve, to see what the approximate relationship is
ggplot(mpg, aes(x=displ,y=cty)) +
  geom_point() + geom_smooth(color="black",se = FALSE) +
  labs(x = "Engine Displacement", y = "City MPG",
       title = "City MPG vs Engine Displacement")
ggsave(
  file = "mpg by displacement.png", 
  dpi = 1200
)

#what about the relative variation in highway mpg based on class?
#we will order based on the class2 variable, but use the class as the name
ggplot(mpg, aes(x=reorder(class,class2),y=hwy))+
  geom_boxplot() +
  labs(x = "Vehicle Class", y = "Highway MPG",
       title = "Highway MPG of Vehicles by Class")
ggsave(
  file = "mpgbyclass.png", 
  dpi = 1200
)




