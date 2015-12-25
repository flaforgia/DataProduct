library(shiny)
library(dplyr)

#setwd("/Users/flaforgia/Documents/Data products/Polution CO2/Data sets")
# Load dataset
cars.2015 <- read.csv("fic_etiq_edition_40-mars-2015.csv", sep=";", encoding="latin1")

# Filter and rename the dataset
select.2015 <- c("lib_mrq_doss", "lib_mod_doss", "energ", "hybride", "puiss_max", "typ_boite_nb_rapp", 
                 "conso_urb_93", "conso_exurb" , "conso_mixte", "co2_mixte", "co_typ_1", 
                 "hc", "nox", "hcnox", "ptcl", "masse_ordma_min", "masse_ordma_max")
cars.2015 <- cars.2015[,select.2015]
names.2015 <- c("maker", "model", "gas", "hybrid", "max_hp", "gear", "city_gas_mileage", "hghy_gas_mileage" , "mix_gas_mileage", 
                "co2", "co_typ_1", "hc", "nox", "hcnox", "ptcl", "min_wght", "max_wght")

names(cars.2015) <- names.2015

# Subset the dataset per fuel types
cars.ES <- filter(cars.2015, gas=="ES ")
cars.GO <- filter(cars.2015, gas=="GO ")

cars.GO <- cars.GO[complete.cases(cars.GO$co2),]
cars.ES <- cars.ES[complete.cases(cars.ES$co2),]

# Fit the models
modelFit.co2.ES <- lm(co2~mix_gas_mileage, data=cars.ES)
modelFit.co2.GO <- lm(co2~mix_gas_mileage, data=cars.GO)
        
shinyServer(
        function(input,output){
                co2Prediction <- reactive({
                        if (input$gas == 1) {
                                round(predict(modelFit.co2.GO, data.frame(mix_gas_mileage=as.numeric(input$conso))),3)
                        }
                        else {
                                round(predict(modelFit.co2.ES, data.frame(mix_gas_mileage=as.numeric(input$conso))),3)
                        }
                })
                
                output$co2Predict <- renderPrint({ paste("Level of CO2:", co2Prediction(), "g/km")})
        }
)

