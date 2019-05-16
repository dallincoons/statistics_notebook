#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# As you go through this file, look for the following...

###----------- EDIT THIS AREA -------------------###

###----------- END EDIT AREA --------------------###

# To know where to edit the file. Of course, you are welcome to edit any
# and all parts of the file. But the "EDIT THIS AREA" statements will 
# help you know what parts you NEED to edit to get the file to do what
# it needs to in order to successfully complete this assignment.


# Load useful libraries:
library(shiny)
options(scipen=999)
library(tidyverse)
library(ggthemes)
library(pander)
###----------- EDIT THIS AREA -------------------###
# Add any other librarys you wish to include.




###----------- END EDIT AREA --------------------###


# Define UI (user interface) for application
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             "title {background-color: gray;}",
             "body {text-align:left;background-color:white;}",
             'h1 {background-color:white;padding:20px;color:darkgray;}'
  ),
  

  # Application title
  h1("Car Price Predictor"),

  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(6,
       fileInput("file1", "Choose Car Data CSV",
                 multiple = FALSE,
                 accept = ".csv")),
    column(6,
          textInput("milesPurchase",
                    "Purchase Mileage:",
                    value = 0)),
    column(6,
           textInput("pricePurchase",
                     "Purchase Price:",
                     value = 0)),
    column(6,
           textInput("milesSelling",
                     "Selling Mileage:",
                     value = 0)),
    style='text-align:center'),
      
  # Show a plot of the generated distribution
  fluidRow(
    column(12,
           plotOutput("scatterPlot"))),
  
  tags$br(),
  tags$br(),

  fluidRow(
     column(12,
            align='center',
            textOutput('message1')),
     style='font-size:30px;border-color:#a1acff;margin:20px;padding:15px;'
  ),
  
  tags$br(),
  tags$br(),
  
  fluidRow(
    column(12,h1('Technical Details'))
  ),
  

  tags$br(),
  tags$br(),
  
  fluidRow(
    column(12,
           textOutput('analysis')),
    style='font-size:20px;border-color:#a1acff;margin:20px;padding:15px;'
  ),
  
  tags$br(),
  tags$br(),
  
  fluidRow(
    column(12,
           plotOutput('diagnostic'))
  )
  )


# Define server logic required...
server <- function(input, output) {
  
  logRegression <- reactive({
    return(car.lm.log <- lm(log(Price) ~ Mileage, data=getdata()))
  })
  
  getdata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    cardata <- read.csv(inFile$datapath, header = TRUE)
    return(cardata)
    })
  
  truncateDecimals <- function(number, decimal_places) {
    return(trunc(number*10^decimal_places)/10^decimal_places)
  }
  
  output$scatterPlot <- renderPlot({
    cardata <- getdata()
    milesPurchase <- as.numeric(input$milesPurchase) #comes from user input
    milesSelling <- as.numeric(input$milesSelling) #comes from user input
    pricePurchase <- as.numeric(input$pricePurchase) #comes from user input
    ###----------- EDIT THIS AREA -------------------###
    # Goal: create scatterplot of cardata Price and Mileage.
    #
    # You will need to run an lm or multiple lm's and then
    # create a scatterplot of the data with the regression(s)
    # and any other reference lines and dots placed on the
    # graph. The data is called "cardata" and contains "Price"
    # and "Mileage" columns.
    
    # Pretend this area is an R-chunk: ```{r}
    lm1 <- lm(Price ~ Mileage, data=cardata)
    plot(Price ~ Mileage, data=cardata)
    points(pricePurchase ~ milesPurchase, pch=15, cex=2, col="skyblue")
    
    car.lm.log <- logRegression()
    curve(exp(car.lm.log$coefficients[1] + car.lm.log$coefficients[2] * x), add=TRUE)
    
    predictedSelling <- exp(car.lm.log$coefficients[1] + car.lm.log$coefficients[2]*milesSelling)
    
    points(predictedSelling ~ milesSelling, pch=15, cex=2, col="firebrick")
    
    legend(1, 95, legend=c("skyblue", "firebrick"),
           col=c("skyblue", "firebrick"), lty=1:2, cex=0.8)
    
    lines(c(milesPurchase, milesSelling), c(pricePurchase, predictedSelling), pch = 16, bg = "black", col ="black")
    
    # ``` That ends right here. Whatever goes into the above
    # area to create output (like a plot or table) will be 
    # displayed in the shiny app.
    ###----------- END EDIT AREA --------------------###      
  })
  
  output$message1 <- renderText({
    cardata <- getdata() #comes from user upload
    milesPurchase <- as.numeric(input$milesPurchase) #comes from user input
    milesSelling <- as.numeric(input$milesSelling) #comes from user input
    pricePurchase <- as.numeric(input$pricePurchase) #comes from user input
    ###----------- EDIT THIS AREA -------------------###
    # To create a customized message that prints out useful
    # information to the user.
    # You will likely want to use say lm(...) and predict(...) 
    # or other useful codes.
    
    car.lm.log <- logRegression()
    
    predictedSelling <- exp(car.lm.log$coefficients[1] + car.lm.log$coefficients[2]*milesSelling)
    
    paste('The predicted value of the car with ',
          milesSelling,
          paste(' miles is $', truncateDecimals(predictedSelling, 2)),
          ' which means you drove the vehicle for ',
          milesSelling - milesPurchase,
          paste(' miles with a net operating', ifelse((pricePurchase - predictedSelling) > 0, 'loss. ', 'gain'),' of', truncateDecimals(abs(pricePurchase - predictedSelling), 2), '. '),
          paste("This means you'll", ifelse((pricePurchase - predictedSelling) > 0, 'lose', 'gain') , "about $", truncateDecimals(abs(diff(c(pricePurchase, predictedSelling)) / diff(c(milesPurchase, milesSelling))), 2) ,"every mile"),
          sep='')
    ###----------- END EDIT AREA --------------------###  
  })
  

  
  output$analysis <- renderText({
    cardata <- getdata()
    ###----------- EDIT THIS AREA -------------------###
    
    paste("I bought a 2003 Toyota Camry with 80,000 miles for 6800, and according to the regression on the data I collected with a log transform applied, I could sell it at 200,000 miles for $3111 for a loss of $3688")
    
    
    ###----------- END EDIT AREA -------------------###
  })
  
  output$diagnostic <- renderPlot({
    cardata <- getdata()
    ###----------- EDIT THIS AREA -------------------###
    lm1 <- lm(Price ~ Mileage, data=cardata)
    plot(lm1, which=1)
    
    
    ###----------- END EDIT AREA -------------------###
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

