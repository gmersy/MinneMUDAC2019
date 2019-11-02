# Time series Analysis
# Author: Christian Sorensen

install.packages("regclass")
library("regclass")

#Data
##########################################################################################################################
#July Data
julyData = read.csv(file.choose())
julyCloseData = julyData$closePrice

#May Data
mayData = read.csv(file.choose())
mayCloseData = mayData$closePrice

#March Data
marchData = read.csv(file.choose())
marchCloseData = marchData$closePrice
##########################################################################################################################


#Visuals/Plots
##########################################################################################################################
#Time Series Plots for Close Price of Each Contract Month Given
plot(ts(julyCloseData), xlab = "Days", ylab = "Closing Price", main = "Time Series for July 2020 Contract")
plot(ts(mayCloseData), xlab = "Days", ylab = "Closing Price", main = "Time Series for May 2020 Contract")
plot(ts(marchCloseData), xlab = "Days", ylab = "Closing Price", main = "Time Series for March 2020 Contract")

#Time Series Plot of Merged Contract Months
plot(ts(julyCloseData), xlab = "Days", ylab = "Closing Price", main = "Time Series for July 2020 Contract", col = "Red")
lines(ts(mayCloseData), col = "Green")
lines(ts(marchCloseData), col = "Blue")
legend(0, 950, legend = c("March", "May", "July"), col = c("Blue", "Green", "Red"), lty = 1:2, cex = .8)


#Plot of Closing Prices During Growing Seasons vs. Non-Growing Seasons

##########################################################################################################################



#Determining Optimal Model
##########################################################################################################################
#Dependent Variable

CLOSE_PRICE = julyData$closePrice

#Explanatory Variables

OPEN_PRICE = julyData$openPrice
SOYBEAN_MEAL_SUPPLY = julyData$totalSoybeanMealSupply
SOYBEAN_MEAL_DEMAND = julyData$totalSoybeanMealDemand
SOYBEAN_OIL_SUPPLY = julyData$soybeanOilSupply
SOYBEAN_OIL_DEMAND = julyData$soybeanOilDemand
SOYBEAN_OIL_PRICE = julyData$soybeanOilPrice
SUNFLOWER_SEED_PRICE = julyData$sunflowerSeedPrice
CANOLA_PRICE = julyData$canolaPrice
PEANUT_PRICE = julyData$peanutsPrice
FLAXSEED_PRICE = julyData$flaxseedPrice
COTTONSEED_OIL_PRICE = julyData$cottonseedOilPrice
SUNFLOWERSEED_OIL_PRICE = julyData$sunflowerseedOilPrice
CANOLA_OIL_PRICE = julyData$canolaOilPrice
PEANUT_OIL_PRICE = julyData$peanutOilPrice
CORN_OIL_PRICE = julyData$cornOilPrice
SOYBEAN_MEAL_PRICE = julyData$soybeanMealPrice
COTTONSEED_MEAL_PRICE = julyData$cottonseedmealPrice
SUNFLOWERSEED_MEAL_PRICE = julyData$sunflowerseedMealPrice
LINSEED_MEAL_PRICE = julyData$linseedMealPrice
MONTH = julyData$Month

#Simple Model
##########################################################################################################################
fit = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_SUPPLY + SOYBEAN_MEAL_DEMAND
        + SOYBEAN_OIL_SUPPLY + SOYBEAN_OIL_DEMAND + SOYBEAN_OIL_PRICE + SUNFLOWER_SEED_PRICE
        + CANOLA_PRICE + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
        + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + CORN_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
        + SUNFLOWERSEED_MEAL_PRICE + LINSEED_MEAL_PRICE + MONTH)

summary(fit)
plot(fit)
##########################################################################################################################


#Variance Inflation Factor Testing (drop anything with VIF > 5)
##########################################################################################################################

VIF(fit)
#drop SOYBEAN_OIL_PRICE

fit2 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_SUPPLY + SOYBEAN_MEAL_DEMAND
         + SOYBEAN_OIL_SUPPLY + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
         + CANOLA_PRICE + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
         + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + CORN_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
         + SUNFLOWERSEED_MEAL_PRICE + LINSEED_MEAL_PRICE + MONTH)

VIF(fit2)
#drop SOYBEAN_OIL_SUPPLY

fit3 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_SUPPLY + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + CANOLA_PRICE + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + CORN_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
          + SUNFLOWERSEED_MEAL_PRICE + LINSEED_MEAL_PRICE + MONTH)

VIF(fit3)
#drop CORN_OIL_PRICE

fit4 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_SUPPLY + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + CANOLA_PRICE + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
          + SUNFLOWERSEED_MEAL_PRICE + LINSEED_MEAL_PRICE + MONTH)

VIF(fit4)
#drop SUNFLOWERSEED_MEAL_PRICE

fit5 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_SUPPLY + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + CANOLA_PRICE + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
          + LINSEED_MEAL_PRICE + MONTH)

VIF(fit5)
#drop SOYBEAN_MEAL_SUPPLY

fit6 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + CANOLA_PRICE + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
          + LINSEED_MEAL_PRICE + MONTH)

VIF(fit6)
#drop CANOLA_PRICE

fit7 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + SOYBEAN_MEAL_PRICE + COTTONSEED_MEAL_PRICE
          + LINSEED_MEAL_PRICE + MONTH)

VIF(fit7)
#drop COTTONSEED_MEAL_PRICE

fit8 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE + SOYBEAN_MEAL_PRICE
          + LINSEED_MEAL_PRICE + MONTH)

VIF(fit8)
#drop SOYBEAN_MEAL_PRICE

fit9 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + CANOLA_OIL_PRICE + PEANUT_OIL_PRICE
          + LINSEED_MEAL_PRICE + MONTH)

VIF(fit9)
#drop CANOLA_OIL_PRICE

fit10 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
          + SOYBEAN_OIL_DEMAND + SUNFLOWER_SEED_PRICE
          + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
          + PEANUT_OIL_PRICE
          + LINSEED_MEAL_PRICE + MONTH)

VIF(fit10)
#drop SUNFLOWER_SEED_PRICE

fit11 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
           + SOYBEAN_OIL_DEMAND
           + PEANUT_PRICE + FLAXSEED_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
           + PEANUT_OIL_PRICE
           + LINSEED_MEAL_PRICE + MONTH)

VIF(fit11) 
#drop FLAXSEED_PRICE

fit12 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
           + SOYBEAN_OIL_DEMAND
           + PEANUT_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
           + PEANUT_OIL_PRICE
           + LINSEED_MEAL_PRICE + MONTH)

VIF(fit12) 
#drop PEANUT_OIL_PRICE

fit13 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
           + SOYBEAN_OIL_DEMAND
           + PEANUT_PRICE + COTTONSEED_OIL_PRICE + SUNFLOWERSEED_OIL_PRICE
           + LINSEED_MEAL_PRICE + MONTH)

VIF(fit13) 
#drop COTTONSEED_OIL_PRICE

fit14 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
           + SOYBEAN_OIL_DEMAND
           + PEANUT_PRICE + SUNFLOWERSEED_OIL_PRICE
           + LINSEED_MEAL_PRICE + MONTH)

VIF(fit14) 
#drop SOYBEAN_OIL_DEMAND

fit15 = lm(formula = CLOSE_PRICE ~ OPEN_PRICE + SOYBEAN_MEAL_DEMAND
           + PEANUT_PRICE + SUNFLOWERSEED_OIL_PRICE
           + LINSEED_MEAL_PRICE + MONTH)

VIF(fit15) 
##########################################################################################################################



#Interaction Model
##########################################################################################################################
importantVars = julyData[c(2, 7, 11, 13, 17, 24)] #explanatory vars. with lowest VIF's

int_model = lm(formula = CLOSE_PRICE ~.^2, data = importantVars)
##########################################################################################################################

#Stepwise Regression
##########################################################################################################################
bestModel = step(int_model)
step(fit15)
##########################################################################################################################

