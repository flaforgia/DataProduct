# Load and read the dataset
downloadURL <- "https://www.data.gouv.fr/s/resources/emissions-de-co2-et-de-polluants-des-vehicules-commercialises-en-france/20151015-121340/fic_etiq_edition_40-mars-2015.zip"
download.file(downloadURL,destfile = "dataset.zip", method="curl")
dataset.csv <- unzip('dataset.zip')
cars.2015 <- read.csv(dataset.csv, sep=";", encoding="latin1")
colnames(cars.2015)

# Filter and rename the dataset
select.2015 <- c("lib_mrq_doss", "lib_mod_doss", "energ", "hybride", "puiss_max", "typ_boite_nb_rapp", 
                 "conso_urb_93", "conso_exurb" , "conso_mixte", "co2_mixte", "co_typ_1", 
                 "hc", "nox", "hcnox", "ptcl", "masse_ordma_min", "masse_ordma_max")
cars.2015 <- cars.2015[,select.2015]
names.2015 <- c("maker", "model", "gas", "hybrid", "max_hp", "gear", "city_gas_mileage", "hghy_gas_mileage" , "mix_gas_mileage", 
                "co2", "co_typ_1", "hc", "nox", "hcnox", "ptcl", "min_wght", "max_wght")

names(cars.2015) <- names.2015

# Subset the dataset per fuel types
library(dplyr)
cars.ES <- filter(cars.2015, gas=="ES ")
cars.GO <- filter(cars.2015, gas=="GO ")

cars.GO <- cars.GO[complete.cases(cars.GO$co2),]
cars.ES <- cars.ES[complete.cases(cars.ES$co2),]

# Percentage of diesel and petrol cars in the dataset
print (((nrow(cars.2015[cars.2015$gas == "GO ",])+nrow(cars.2015[cars.2015$gas == "ES ",]))/nrow(cars.2015))*100)

# Remove non=numericc variables and calculate the correlation
cars.GO <- select(cars.GO, 
                  mix_gas_mileage, 
                  max_hp, co2, 
                  co_typ_1, nox, 
                  hcnox, ptcl, 
                  min_wght, 
                  max_wght, 
                  hghy_gas_mileage, 
                  city_gas_mileage)

cars.ES <- select(cars.ES, 
                  mix_gas_mileage, 
                  max_hp, co2, 
                  co_typ_1, nox, 
                  hcnox, ptcl, 
                  min_wght, 
                  max_wght, 
                  hghy_gas_mileage, 
                  city_gas_mileage)

cor(cars.GO$co2, cars.GO[,])
cor(cars.ES$co2, cars.ES[,])

# Plot the co2 against the mix_gas_mileage
library(ggplot2)
co2.ES <- ggplot(cars.ES, aes(x=mix_gas_mileage, y=co2))
co2.ES <- co2.ES + geom_point(aes(, colour=mix_gas_mileage)) + scale_colour_gradient(low="blue")
co2.ES <- co2.ES + ggtitle("co2 Emission Vs. Mixte gas mileage in petrol cars")
co2.ES

co2.GO <- ggplot(cars.GO, aes(x=mix_gas_mileage, y=co2))
co2.GO <- co2.GO + geom_point(aes(, colour=mix_gas_mileage)) + scale_colour_gradient(low="blue")
co2.GO <- co2.GO + ggtitle("co2 Emission Vs. Mixte gas mileage in diesel cars")
co2.GO

# Fit the models
modelFit.co2.ES <- lm(co2~mix_gas_mileage, data=cars.ES)
summary(modelFit.co2.ES)
modelFit.co2.GO <- lm(co2~mix_gas_mileage, data=cars.GO)
summary(modelFit.co2.GO)

# Plot the residuals
cars.ES.resid <- resid(modelFit.co2.ES)
cars.ES.yhat <- predict(modelFit.co2.ES)
cars.ES.y <- cars.ES$co2
plot(cars.ES.yhat, cars.ES.resid, xlab = "predicted CO2 value (Petrol)", 
     ylab = "residuals", bg = "lightblue", col = "black", cex = 2, 
     pch = 21, frame=FALSE)
abline(h = 0, lwd = 2)

cars.GO.resid <- resid(modelFit.co2.GO)
cars.GO.yhat <- predict(modelFit.co2.GO)
cars.GO.y <- cars.GO$co2
plot(cars.GO.yhat, cars.GO.resid, xlab = "predicted CO2 value (Diesel)", 
     ylab = "residuals", bg = "lightblue", col = "black", cex = 2, 
     pch = 21, frame=FALSE)
abline(h = 0, lwd = 2)

# plot the outcomes prediction interval
newset.ES <- data.frame(mix_gas_mileage= seq(min(cars.ES$mix_gas_mileage), max(cars.ES$mix_gas_mileage), length=100))
newset.GO <- data.frame(mix_gas_mileage= seq(min(cars.GO$mix_gas_mileage), max(cars.GO$mix_gas_mileage), length=100))

predict1.co2.ES <- data.frame(predict(modelFit.co2.ES, newset.ES, interval="confidence"))
predict2.co2.ES <- data.frame(predict(modelFit.co2.ES, newset.ES, interval="prediction"))
predict1.co2.ES$interval <- "confidence"
predict2.co2.ES$interval <- "prediction"
predict1.co2.ES$mix_gas_mileage <- newset.ES$mix_gas_mileage
predict2.co2.ES$mix_gas_mileage <- newset.ES$mix_gas_mileage
data <- rbind(predict1.co2.ES, predict2.co2.ES)
names(data)[1] <- "y"
plot.co2.ES <- ggplot(data, aes(x = mix_gas_mileage, y = y))
plot.co2.ES <- plot.co2.ES + xlab("gas mileage in liter per 100 kms")
plot.co2.ES <- plot.co2.ES + ylab("co2 prediction")
plot.co2.ES <- plot.co2.ES + geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2)
plot.co2.ES <- plot.co2.ES + geom_line()
plot.co2.ES <- plot.co2.ES + geom_point(data=data.frame(x=cars.ES$mix_gas_mileage, y=cars.ES$co2), aes(x=x, y=y), size=0.5)
plot.co2.ES <- plot.co2.ES + ggtitle("Outcomes plot for the petrol cars")
plot.co2.ES

predict1.co2.GO <- data.frame(predict(modelFit.co2.GO, newset.GO, interval="confidence"))
predict2.co2.GO <- data.frame(predict(modelFit.co2.GO, newset.GO, interval="prediction"))
predict1.co2.GO$interval <- "confidence"
predict2.co2.GO$interval <- "prediction"
predict1.co2.GO$mix_gas_mileage <- newset.GO$mix_gas_mileage
predict2.co2.GO$mix_gas_mileage <- newset.GO$mix_gas_mileage
data <- rbind(predict1.co2.GO, predict2.co2.GO)
names(data)[1] <- "y"
plot.co2.GO <- ggplot(data, aes(x = mix_gas_mileage, y = y))
plot.co2.GO <- plot.co2.GO + xlab("gas mileage in liter per 100 kms")
plot.co2.GO <- plot.co2.GO + ylab("co2 prediction")
plot.co2.GO <- plot.co2.GO + geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2)
plot.co2.GO <- plot.co2.GO + geom_line()
plot.co2.GO <- plot.co2.GO + geom_point(data=data.frame(x=cars.GO$mix_gas_mileage, y=cars.GO$co2), aes(x=x, y=y), size=0.5)
plot.co2.GO <- plot.co2.GO + ggtitle("Outcomes plot for diesel cars")
plot.co2.GO

# Coefficient interval
sumCoef.ES <- summary(modelFit.co2.ES)$coefficients
sumCoef.GO <- summary(modelFit.co2.GO)$coefficients
paste("The slope for petrol cars: ", (sumCoef.ES[2,1] + c(-1, 1) * qt(0.975, df = modelFit.co2.ES$df) * sumCoef.ES[2,2])/length(cars.ES)-1)
paste("The slope for diesel cars: ", (sumCoef.GO[2,1] + c(-1, 1) * qt(0.975, df = modelFit.co2.GO$df) * sumCoef.GO[2,2])/length(cars.GO)-1)