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

library(readr)



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
  
  getdata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    cardata <- read.csv(inFile$datapath, header = TRUE)
    return(cardata)
    })
   
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
    lm2 <- lm(sqrt(Price) ~ Mileage, data=cardata)
    plot(Price ~ Mileage, data=cardata)
    points(pricePurchase ~ milesPurchase, pch=15, cex=2, col="skyblue")
    
    
    mypredict <- predict(lm2, newdata = data.frame(Mileage = milesSelling))^2
    
    
    points(mypredict ~ milesSelling, pch = 15, cex=2, col="orange")
    
    
    lines(c(milesPurchase, milesSelling), c(pricePurchase, mypredict), pch = 16, bg = "black", col ="black")
    
    abline(lm1)
    
    coef(lm1)
    b <- coef(lm2)
    curve((b[1] + b[2]*x)^2, add = TRUE, col = "firebrick")
    
    
   
    
    
        
    # ``` That ends right here. Whatever goes into the above
    # area to create output (like a plot or table) will be 
    # displayed in the shiny app.
    ###----------- END EDIT AREA --------------------###      
  })
  
  output$message1 <- renderText({
    cardata <- getdata() #comes from user upload
    milesPurchase <- as.numeric(input$milesPurchase) #comes from user input
    milesSelling <- as.numeric(input$milesSelling) #comes from user input
    
    ###----------- EDIT THIS AREA -------------------###
    # To create a customized message that prints out useful
    # information to the user.
    # You will likely want to use say lm(...) and predict(...) 
    # or other useful codes.
    
    pricePurchase <- as.numeric(input$pricePurchase)
    
    lm2 <- lm(sqrt(Price) ~ Mileage, data=cardata)
    mypredict <- as.numeric(round(predict(lm2, newdata = data.frame(Mileage = milesSelling))^2), 2)
    
    paste('The predicted value of the car with ', 
          milesSelling, 
          ' miles is ... $', 
          mypredict,
          ' which means you drove the vehicle for ',
          milesSelling - milesPurchase,
          ' miles with a net operating loss of $',
          pricePurchase - mypredict,
          sep='')
    ###----------- END EDIT AREA --------------------###  
  })
  

  
  output$analysis <- renderText({
    cardata <- getdata()
    ###----------- EDIT THIS AREA -------------------###
    
    paste("The residuals vs. fitted-values plot: The linear relation is assumed to be satisfied if the red line shows no apparent trends in the plot. The constant variance assumption is also assumed to be satisfied if it is not a megaphone shape.
          The Q-Q Plot: If the residuals appear to be normal, then the error terms are also considered to be normal. This means that if the dots don't go out of bounds, then it should be good. However, if the other two plots except the Q-Q plot are satisfied, then don't rely on the Q-Q plot too much even though it is not normal. 
          The residuals VS. order plot: If there is no problems with general or obvious trends, then the error terms can be assumed to be independent.")
    
    
    ###----------- END EDIT AREA -------------------###
  })
  
  output$diagnostic <- renderPlot({
    cardata <- getdata()
    ###----------- EDIT THIS AREA -------------------###
    lm1 <- lm(Price ~ Mileage, data=cardata)
    par(mfrow=c(1,3))
    plot(lm1, which=1:2)
    plot(lm1$residuals)
    
    
    ###----------- END EDIT AREA -------------------###
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

