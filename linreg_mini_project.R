## Exercise: least squares regression

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?


#########################################################
#########################################################

states.data <- readRDS("dataSets/states.rds") 

#########################################################
#########################################################


#############################################################################
###########################          1.          ############################
#############################################################################

library(ggplot2)
ggplot(states.data, aes(x = metro, y = energy)) + geom_point()

#############################################################################
###########################          2.          ############################
#############################################################################

model_1 <- lm(energy ~ metro, data = states.data)
summary(model_1)

### the model indicates a significantly negative relationship between
### metro area population % and per capita energy consumed: a 1% increase
### in metro area population % leads to a decrease in energy usage by
### 2.2871 btu

#############################################################################
###########################          3.          ############################
#############################################################################


ggplot(model_1, aes(x = model_1$residuals)) + geom_histogram(bins = 20)
ggplot(states.data, aes(x = metro, y = energy)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)
plot(model_1, which = c(1, 2))

### the residual histogram doesn't appear to violate normality

### the residual and QQ plots appear well behaved, save for the few
### outliers

### to further gauge whether the residuals are normally distributed, a check 
### of the one-sample K-S test reveals:

ks.test(model_1$residuals, "pnorm")

### the K-S test reveals a p-value very close to zero, indicating that the
### residuals depart from normality and hence one of the key assumptions
### that underlies appropriate use of linear regression is violated


          #################################################
          #################################################


### we now add 'density' (people per square mile) as a predictor:

model_2 <- lm(energy ~ metro + density, data = states.data)
summary(model_2)

### the inclusion of density alongside metro makes for no significant
### predictor variables

ks.test(model_2$residuals, "pnorm")

### further, K-S again shows that the residuals depart from normality

### hence, density should be discarded as a predictor and the univariable
### model should be retained



#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################



## Exercise: interactions and factors

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?


#############################################################################
###########################          1.          ############################
#############################################################################

model_3 <- lm(energy ~ metro * density, data = states.data)
summary(model_3)

### the summary output shows that not one of the predictors or the
### interaction term is significant

#############################################################################
###########################          2.          ############################
#############################################################################

### region is added to the original (significant) model:

model_4 <- lm(energy ~ metro + region, data = states.data)
summary(model_4)

### the base region for the model is "West"

### metro now regains its significant, and out of the three regions,
### only Northeast is significant

### each region must be interpreted in the context of West

### we can interpret Northeast's estimate as the difference in per capita
### energy consumed by living in the Northeast versus the West

### Northeast's estimate indicates that if we live in the Northeast, then
### we will consume about 133 btu less in energy than if we live in the West