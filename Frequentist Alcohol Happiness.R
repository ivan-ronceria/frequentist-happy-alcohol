#Stat Methods 3 Project
install.packages(c("readxl", "bestNormalize", "fastDummies", "car"), dependencies = TRUE)


library(readxl)
library(bestNormalize)
library(fastDummies)
library(car)
set.seed(0)

#Read the dataset
df<-read_xlsx("C:/Users/Ronceria/Desktop/HappyAlcohol.xlsx")

#Let bestNormalize use the Box-Cox Transformation to create optimally normal versions
#of continuous variables.
df$bcHDI = bestNormalize(df$HDI, allow_orderNorm = FALSE)$x.t
df$bcHS = bestNormalize(df$HappinessScore, allow_orderNorm = FALSE)$x.t
df$bcGDP = bestNormalize(df$GDP_PerCapita, allow_orderNorm = FALSE)$x.t
df$bcBeer <- bestNormalize(df$Beer_PerCapita, allow_orderNorm = FALSE)$x.t
df$bcWine <- bestNormalize(df$Wine_PerCapita, allow_orderNorm = FALSE)$x.t
df$bcSpirit <- bestNormalize(df$Spirit_PerCapita, allow_orderNorm = FALSE)$x.t

#Indicate to R that these variables are catagorical
df$Region = factor(df$Region)
df$Hemisphere = factor(df$Hemisphere)

#Proposed interaction terms. There are many, but these were deemed the most reasonable to hypothesize 
#right off the bat
df$GDP_HDI <- df$HDI * df$GDP_PerCapita
df$Beer_Wine_Spirit <- df$Beer_PerCapita * df$Wine_PerCapita * df$Spirit_PerCapita

#Serves no analytical purpose in model fitting. Even if converted to dummy, the model would suffer from overfitting
df$Country = NULL

#Creates a new dataset that contains dummy versions of catagorical variables
df2 = dummy_cols(df, remove_first_dummy = TRUE, )

#If desired run this to export the new dataset
#write.csv(df2, file = "C:/*insert directory*/newHappyAlcohol.csv")



#We use bidirectional stepAIC for feature selection. We remove bcHS as it is just
#a transformed version of HappinessScore and Region because we have created dummy versions
#of Region already. We also remove HDI and GDP_PerCapita as we transformed versions
#that are more suitable.

#To select features, we must include all the variables into the models except the ones
#that were mentioned above

mod0 = lm(HappinessScore ~ (. -Region - bcHS -HDI -GDP_PerCapita
                            -Wine_PerCapita -Spirit_PerCapita -Beer_PerCapita), data = df2)


#Decent R^2 and residuals, but VIF will not run because
#perfect multicolinearity is present. Let us see what stepAIC converges to.
summary(mod0)
plot(mod0)
stepAIC(mod0)
shapiro.test(residuals(mod0))


#StepAIC converged to this model
mod1 = lm(formula = HappinessScore ~ bcHDI + bcWine + GDP_HDI + `Region_Central and Eastern Europe` + 
            `Region_Eastern Asia` + `Region_Middle East and Northern Africa` + 
            `Region_Southeastern Asia` + `Region_Sub-Saharan Africa` + 
            `Region_Western Europe`, data = df2)

#Better adjusted R^2, residuals, with less variables, thus mod1 will be our final model
summary(mod1)
plot(mod1)

shapiro.test(mod1$residuals)

#VIF shows no variables exceeds the threshold of 10.
#Therefore, the final model does not suffer from multicollinearity.
vif(mod1)

mod1$coefficients
