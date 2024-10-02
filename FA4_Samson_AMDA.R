#FA4_Samson_AMDA

# Load necessary packages
library(tidyverse)
library(car)  # For Anova
library(ggplot2)
library(ggpubr)
library(rstatix)
library(lsr)


# Load the dataset (Assuming the dataset is called 'auto_pollution_filter_noise.csv')
data <- read.csv("C:\\Users\\User\\OneDrive\\Personal docs\\FRESHMAN\\4th yr - 1st Sem\\Applied Multivariate Data Analysis\\Auto Pollution Filter Noise.csv")

# Check the structure of the data
str(data)

#-------------------------------------------------------------------------------------------

# 1.
# Convert variables to factors
data$Size <- as.factor(data$Size)
data$Type <- as.factor(data$Type)
data$Side <- as.factor(data$Side)

# Check for normality of residuals
model <- aov(Noise ~ Size * Type * Side, data = data)
residuals <- residuals(model)
shapiro.test(residuals)

plot(model, which = 2)  # Q-Q plot of residuals

# Check for homogeneity of variance
leveneTest(Noise ~ Size * Type * Side, data = data)

# Check for independence (Durbin-Watson test for autocorrelation of residuals)
durbinWatsonTest(model)

# Plot residuals vs fitted values to check for linearity
plot(model, which = 1)

# Check for outliers
plot(model, which = 4)  # Plot Cook's distance

#Given that we are employing a between-subjects design in your instance, sphericity does not apply. 
#This assumption is pertinent for repeated measures ANOVA.

#-------------------------------------------------------------------------------------------

# 2
# Run Three-Way ANOVA
anova_result <- aov(Noise ~ Size * Type * Side, data = data)
summary(anova_result)

# Check for three-way interaction
interaction.plot(data$Size, data$Type, data$Noise, col=c("red","blue"), xlab="Vehicle Size", ylab="Noise Level", trace.label="Type")

#-------------------------------------------------------------------------------------------

#3
# Interaction plot for Size and Type
interaction.plot(data$Size, data$Type, data$Noise, col=c("red","blue"), xlab="Vehicle Size", ylab="Noise Level", trace.label="Type")

# Interaction plot for Size and Side
interaction.plot(data$Size, data$Side, data$Noise, col=c("red","blue"), xlab="Vehicle Size", ylab="Noise Level", trace.label="Side")

# Interaction plot for Type and Side
interaction.plot(data$Type, data$Side, data$Noise, col=c("red","blue"), xlab="Type", ylab="Noise Level", trace.label="Side")

#-------------------------------------------------------------------------------------------

#4
# Check for main effects
summary(anova_result)

# Post-hoc test for vehicle size effect
TukeyHSD(anova_result, "Size")

# Post-hoc test for type effect
TukeyHSD(anova_result, "Type")

# Post-hoc test for side effect
TukeyHSD(anova_result, "Side")


#-------------------------------------------------------------------------------------------

#5
# Calculate effect size (eta squared)
anova_table <- Anova(anova_result, type = "III")
anova_table

# Eta-squared calculation
eta_squared <- etaSquared(anova_result)
eta_squared

#-------------------------------------------------------------------------------------------

#6
# Perform pairwise comparisons using Tukey's HSD
posthoc_size <- TukeyHSD(anova_result, "Size")
posthoc_size

# Visualize post-hoc results
plot(posthoc_size)

#-------------------------------------------------------------------------------------------
