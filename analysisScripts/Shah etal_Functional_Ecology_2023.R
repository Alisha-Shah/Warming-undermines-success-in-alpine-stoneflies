#Title: Warming undermines emergence success in a threatened alpine stonefly: A multi-trait perspective on vulnerability to climate change 

#Shah et al. 2023, Functional Ecology 

#Annotated code for analyses

################################################################################

#Growth Rate

rm (list=ls()) # Run this to clear R memory if needed

setwd("~/Documents/R/CSV Files/Lednia_Long-Term/Growth")

install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library("tidyverse")


#Read in data (it's in Excel and in separate sheets within a single datasheet, so using the read_xlsx function)
lednia_wk1 <- read_xlsx("Lednia longterm growth data Feb 2020.xlsx", range = "A2:N419", na = c("", 0))
lednia_wk2 <- read_xlsx("Lednia longterm growth data Feb 2020.xlsx", range = "A2:N419", na = c("", 0), sheet = 2)

#Check if got the correct datasets
print(lednia_wk1, n = nrow(lednia_wk1))

# Rename last column to something reasonable:
names(lednia_wk1)[14] <- "dry.mass"
names(lednia_wk2)[14] <- "dry.mass"

#Remove the 10C (variable treatment) rows from the data sets

lednia_wk1a <- filter(lednia_wk1, Temp !="10")
lednia_wk2a <- filter(lednia_wk2, Temp !="10")

#Check if got the correct datasets
print(lednia_wk1a, n = nrow(lednia_wk1a))
print(lednia_wk2a, n = nrow(lednia_wk2a))


# Plotting

## use dplyr to get means by group .

week1.means <- lednia_wk1a %>%
  group_by(Site, Temp, Trt) %>%
  summarize(mean.mass = mean(dry.mass, na.rm = TRUE))

week2.means <- lednia_wk2a %>%
  group_by(Site, Temp, Trt) %>%
  summarize(mean.mass = mean(dry.mass, na.rm = TRUE))

week1and2 <- full_join(week1.means, week2.means, by = c("Site", "Temp", "Trt"))

print(week1and2, n = nrow(week1and2))

## Here, I am calculating growth per week and converting to data frame. Note, to get growth rate per week, I divided the difference in length between week 2 and week 1 by 2.
lednia <- week1and2 %>%
  mutate(growth = (mean.mass.y - mean.mass.x)/2) %>% as.data.frame

## delete last NA line
lednia <- lednia[-60, ]


# Plot

plot(growth~Temp, data = lednia)

# Add performance curve

#Load package
library(nls2)


#Use this formula for growth
growth ~ a * exp(-((x-b)^2)/(2*c^2))

#Write function for TPC
gaussian <- function(a, b, c, x){
  return(a * exp(-((x - b)^2)/(2*c^2)))
}

# where
# a is the height of the peak
# b is the location of the peak
# c is the is the standard deviation (width of curve)


#Plot data with temperature sequence and starting parameters
temp.seq <- seq(from = 2, to = 22, length.out = 100)
perf.seq <- gaussian(0.07, 13, 5, temp.seq)

plot(growth~Temp, data = lednia)
lines(temp.seq, perf.seq)        # ok nice

# now try fitting using nls2

fit2 <- nls2(growth  ~ a * exp(-((Temp - b)^2)/(2*c^2)),
             data = lednia,
             start = list(a = 0.07, b = 13, c = 5))

summary(fit2)

################################################################################

#Survival

rm(list=ls()) # Run this to clear R memory if needed

setwd("~/Documents/R/CSV Files/Lednia_Long-Term/Mortality")

library(tidyverse)
library(lme4)
library(car)

dat<- read.csv("Survival_binary.csv")
#Check
head(dat)
str(dat)

#Remove the 10C variable treatment

dat1 <- filter(dat, temp !="10")

#Check
View(dat1)

mod1 <- glmer(survive ~ temp * site + (1|container), family=binomial,
              data=dat1, verbose=1, nAGQ = 0) #We added the nAGQ term to optimize the model for our small sample size

summary(mod1, correlation=FALSE) #the correlation term is just so we don't get the correlation among factors output.

Anova(mod1,type="II",test.statistic="Chisq") #I used Type II because it was recommended for this use. 

# predict values based on model
dat1$pred = predict(mod1, newdata=dat1, re.form = NULL, type="response")

# plot each site separately
dat1 %>%
  ggplot(aes(temp, survive)) + geom_jitter(height=0.1) +
  geom_smooth(aes(temp, pred)) + facet_wrap('site')

# or all together

dat1 %>%
  ggplot(aes(temp, survive, color=site)) + geom_jitter(height=0.1) +
  geom_smooth(aes(temp, pred))

#################################################################

#Emergence

rm(list=ls()) # Run this to clear R memory if needed

setwd("~/Documents/R/CSV Files/Lednia_Long-Term/Emergence")

library(tidyverse)
library(lme4)
library(car)

dat<- read.csv("emergence_binary.csv")
#Check
head(dat)
str(dat)

#Remove the 10C variable treatment

dat1 <- filter(dat, temp !="10")
#Check
View(dat1)

mod1 <- glmer(emerge ~ temp * site + (1|container), family=binomial,
              data=dat1, verbose=1, nAGQ = 0) #We added the nAGQ term to optimize the model for our small sample size

summary(mod1, correlation=FALSE) #the correlation term is just so we don't get the correlation among factors output.

Anova(mod1,type="II",test.statistic="Chisq") #I used Type II because it was recommended for this use. 

# predict values based on model
dat1$pred = predict(mod1, newdata=dat1, re.form = NULL, type="response")

# plot sites separately
dat1 %>%
  ggplot(aes(temp, emerge)) + geom_jitter(height=0.1) +
  geom_smooth(aes(temp, pred)) + facet_wrap('site')

# or all together

dat1 %>%
  ggplot(aes(temp, emerge, color=site)) + geom_jitter(height=0.1) +
  geom_smooth(aes(temp, pred))

################################################################################

# Adult locomotor performance & wing morphology
# Analyses conducted by Anthony Lapsansky (U. British Columbia)

# Import relevant libraries if not already called
library(tidyverse)
library(readxl)
library(nlme)
library(dplyr)

# Remove plyr if needed, as conflicts arise in some R versions
# detach("package:plyr", unload=TRUE)

# Import morphological data and convert temperatures to factors
wing_data <- read.csv('wingData.csv')
wing_data$temp <- factor(wing_data$temp, levels = c(1,4,7,13,'var'))

# Test for significant relationship between wing morphology and skimming speed
# This uses adj speed manually transferred to the Shahetal_2023_wingData.csv
# for convenience

lm_flVspeed <- lm(speed_adj ~ front.length, data = wing_data)
anova(lm_flVspeed)

lm_rlVspeed <- lm(speed_adj ~ rear.length, data = wing_data)
anova(lm_rlVspeed)

# To test for a relationship between speed and rearing temperature,
# Import the x-y points that correspond to the skimming of each stonefly. 
data <- read_xlsx('speedData.xlsx')

# Convert ID to a factor and reorder the levels or plotting
data$ID <- as.factor(data$ID)
data$temp <- factor(data$temp, levels = c('1C', '4C', '7C', '13C', 'Variable'))

# Use a single point of data for each individual by averaging trial data
data_sum <- data %>%
  group_by(ID) %>%
  summarize(speed_adj = mean(speed_adj), stream = stream, temp = temp)
data_sum = data_sum[!duplicated(data_sum$ID),]

# Test for relationship between speed and rearing temp
lm_sum <- lm(speed_adj ~ temp, data = data_sum)
anova(lm_sum)

# Test for relationship between speed and site, with rearing temp as a cofactor
lm_int <- lm(speed_adj ~ temp:stream, data = data_sum)
anova(lm_int)

# We find no significant effect of stream, temperature, or their interaction

# Plot the mean adjusted speed (speed adjusted to ambient temperature)
figure5e <- ggplot(data_sum, aes(x = temp, y = speed_adj)) +
  geom_jitter(aes(fill = stream), size = 6, pch = 21, stroke = 2, width = 0.1, height = 0) +
  scale_fill_manual(values = c("#a76228","#dfc07b","#7fcdc0","#098772","#f7f9fa")) +
  ylim(0,0.4)+
  labs(x="Incubation temperature (\u00B0C)", y = "Mean skimming speed (m / s)", fill = "Population") +
  theme_classic() +
  theme(
    axis.title.x=element_text(size = 14),
    axis.title.y=element_text(size = 14),
    axis.text.x=element_text(size = 12),
    axis.text.y=element_text(size = 12),
    legend.position = c(0.95, 0.75),
    legend.justification = c(1, 0)) 

figure5e

##############################################################################

#Wing length

rm(list=ls()) # Use if need to clear R's memory

#Set WD
setwd("~/Documents/R/CSV Files/Lednia_Long-Term/Flight performance")

#Read in final data set, make sure no white spaces in columns.
data <- read.csv("wing_length.csv")

#Created this new dataset to include 1, 4 and 7C temp treatments
wing <- read.csv("wing_length_collapsed.csv")

head(wing)
str(wing)
View(wing)

#Set temperatures as factors
wing$temp <- factor(wing$temp , levels=c("1", "4", "7"))

#Front wing length
#wing.mod<-subset(wing, wing$graph=="graph")

#Look at data (front wing length)
boxplot(front.length~wing$temp,data=wing, main="Front wing length",
        xlab="Temperature (ºC)", ylab="Wing length (mm)")


#Is there an effect of temperature on front wing length? Use a one-way ANOVA and if necessary Bonferroni correction for multiple testing if p-value significant

fit1<-aov(front.length ~ temp, data=wing)
summary(fit1)
anova(fit1) # Nope!

#Rear wing length
boxplot(rear.length~wing$temp,data=wing, main="Rear wing length",
        xlab="Temperature (ºC)", ylab="Wing length (mm)")

#Is there an effect of temperature on rear wing length?

fit2<-aov(rear.length ~ temp, data=wing)
summary(fit2)
anova(fit2) # Also no!

# Correlation between front wing length and wing area to show why we only used one wing parameter in the analysis

cor.test(wing$front.1, wing$front.area, method=("pearson"))

#Conduct t.test to obtain confidence intervals for 1 and 4C lengths & 4 and 7C lengths to respond to Reviewer comment.

#Forewings


temp.1<-subset(wing, wing$temp=="1")
temp.4<-subset(wing, wing$temp=="4")
temp.7<-subset(wing, wing$temp=="7")

#Run t.test

t.test(temp.1$front.length, temp.4$front.length, conf.level = 0.95)$conf.int
t.test(temp.4$front.length, temp.7$front.length, conf.level = 0.95)$conf.int

t.test(temp.1$front.length, temp.4$front.length, mu=0,
       alternative = "two.sided", conf.level =0.95)

t.test(temp.4$front.length, temp.7$front.length, mu=0,
       alternative = "two.sided", conf.level =0.95)

#Rear wing
t.test(temp.1$rear.length, temp.4$rear.length, mu=0,
       alternative = "two.sided", conf.level =0.95)

t.test(temp.4$rear.length, temp.7$rear.length, mu=0,
       alternative = "two.sided", conf.level =0.95)

############################# End of code ###################################