####################################################################################################################
# 1 preparation
####################################################################################################################

#Install the required package
install.packages('googlesheets4')
install.packages('nortest')
install.packages('dplyr')
install.packages('car')
install.packages('coin')
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ltm")

#Load the required library 
library(googlesheets4)
library(nortest)
library(dplyr)
library(car)
library(coin)
library(ggplot2)
library(ggpubr)
library(ltm)

####################################################################################################################
# 2 Loading data
####################################################################################################################

#Reads data into R
df <- read_sheet('https://docs.google.com/spreadsheets/d/1q1ojC3pgj0mXpoiu1rWQOQ6Q1ptJbt3rlrT2vrdwIOQ/edit#gid=1806672803')



#HO1 --> no significant difference between the userfriendly average of the displays
#HO2 --> no significatn difference between the usertimes of the displays


####################################################################################################################
# 3 Calculations
####################################################################################################################

#Cronbachs alpha
cronbach.alpha(df, CI=TRUE )


####################################################################################################################
# 4 Grouping
####################################################################################################################


####################################################################################################################
# 5 variance testing
####################################################################################################################

# 5.1 Independence: Yes
#-----------------------

# 5.2 Normality test
#--------------------
#Kolmogorov-Smirnov test (each group)
# if p < 0.05 this means there is a statistically significant difference between the distribution of the data and a normal distribution
# if p < 0.05 -> Non parametric


tapply(df$Q_GEM,df$display,lillie.test)
tapply(df$tijd,df$display,lillie.test)

# 5.3 Homegenious variances test
#--------------------------------
# leveneTest
# H0: All populations variances are equal
# H1: At least two of them differ
# if p-value > 0.05 -> non parametric

leveneTest(df$Q_GEM ~ df$display, data = df)
leveneTest(df$tijd ~ df$display, data = df)
####################################################################################################################
# 6 main experimental statistical tests
####################################################################################################################

# if variance testing criteria all have been met -> ANOVA
# else -> Kruskal-Wallis

# ANOVA
#-------
#H0 there is a significant difference in strength between the groups
#H1 there is no significant difference in strength between the groups



#post hoc
#---------
TukeyHSD(anova)

# Mann-Whitney U
#---------------

#p-value: this is the alpha. If this value is below 0,05 the differences are significant.
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the groups.

wilcox.test(df$tijd ~ df$display, data = df, exact = FALSE)

wilcox.test(df$Q_GEM ~ df$display, data = df, exact = FALSE)



####################################################################################################################
# 7 visualisation
####################################################################################################################

boxplot(df$tijd ~ df$display,
        data = df,
        main = "Tijd per display",
        xlab = "Display",
        ylab = "Tijd (seconden)",
        col = "steelblue",
        border = "black")

boxplot(df$Q_GEM ~ df$display,
        data = df,
        main = "Tevredenheid per display",
        xlab = "Display",
        ylab = "tevredenheid (0 = niet tevreden, 5 = heel tevreden)",
        col = "steelblue",
        border = "black")

