library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("CO2 Predictions Per Gas Mileage"),
        sidebarPanel(
                strong("Usage:"),
                helpText("Select the gas type and input the gas mileage in liter per 100 kms (European unit)."),
                helpText("Clic on the Submit button or press Enter. The results are displayed in the main frame on the right."),
                helpText("You must clic on the Submit button each time a parameter is changed."),
                br(),                
                h2("Predictions"),
                selectInput("gas", label=h5("Select type of gas"),
                            choices=list("Diesel"=1, "Gasoline"=2)),
                        
                textInput("conso", label=h6("Enter the gas mileage in liter per 100 kms"),
                          value=5 ),
                
                submitButton("Submit")
                                
                ),
        mainPanel(
        
                h5("Prediction of the level of emission of Carbon Dioxyde (CO2) for this gas mileage:"),
                verbatimTextOutput("co2Predict")
                
                
        ) ))