# Load R packages
library(shiny)
library(shinythemes)
library(hms)
library(readr)
mydataset <- read_csv("meteorite_landings.csv")
data(mydataset)

mydataset <- na.omit(mydataset)
library(Hmisc)
library(dplyr)
library(plyr)
library(magrittr)
library(plotrix)
library(ggplot2)
packages <-
  c('tidyverse',
    'leaflet',
    'leaflet.extras',
    'DT',
    'ggplot2',
    'htmltools')

for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

# UI

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  navbarPage(
    "Alpha Orionis",
    tabPanel("Dataset",
             mainPanel(dataTableOutput("ds"),)),
    tabPanel(
      "Central Tendency & Dispersion",
      
      mainPanel(
        h1("Median"),
        h2("Mass"),
        verbatimTextOutput("mass"),
        
        h1("Inter Quartile Range"),
        h2("Mass"),
        verbatimTextOutput("iqrmass"),
      )
      
    ),
    tabPanel(
      "Graphical Representaion",
      
      mainPanel(
        h1("Box Plot of Meteorite's Mass"),
        plotOutput("box1"),
        
        h1("Histogram of Meteorite Landing Year"),
        plotOutput("hist1"),
        
        h1("Histogram of Meteorite's Mass"),
        plotOutput("hist2"),
        
        h1("Distribution of Mass"),
        plotOutput("g1"),
        
      )
    ),
    
    
    tabPanel(
      "Probability Distribution",
      
      mainPanel(
        h1("Bernoulli Distribution"),
        verbatimTextOutput("dist"),
        #p probability that it 'fell'
        #k possible outcomes
        #x=0 found
        h6("P(X=x) = (p^x)*((1-p)^(1-x))"),
        h6("Probability that the meteorite fell -> 0.50"),
        h6("Possible outcomes -> 1065/38115"),
        h6("x = 0 -> 'found'"),
        h6("x = 1 -> 'fell'"),
        
      )
    ),
    
    
    tabPanel("Summary of Data",
             mainPanel(dataTableOutput("dt"),)),
    
    tabPanel(
      "Regression Modelling",
      mainPanel(
        h1("Linear Regression on Mass & Year"),
        textInput("txt1", "Year:", ""),
        h2("Mass:"),
        verbatimTextOutput("reg1"),
      )
    ),
    
  )
)


#server
server <- function(input, output) {
  output$ds <- renderDataTable({
    DT::datatable(mydataset, options = list(pageLength = 100))
  })
  
  output$mass <- renderText({
    x <- mydataset$`mass (g)`
    median(x)
  })
  
  output$iqrmass <- renderText({
    IQR(mydataset$`mass (g)`)
  })
  
  output$box1 <- renderPlot({
    massofmeteorite = mydataset$`mass (g)`
    boxplot(massofmeteorite)
  })
  
  output$hist1 <- renderPlot({
    landingyear = mydataset$year
    hist(
      landingyear,
      col = blues9,
      ylim = c(0, 25000),
      xlim = c(1800, 2300),
      breaks = 18,
      main = "Histogram of Meteorite Landing Year"
    )
  })
  
  output$g1 <- renderPlot({
    massofmeteorite = mydataset$`mass (g)`
    fillColor = "#FFFFFF"
    fillColor2 = "#FFA500"
    mydataset %>%
      ggplot(aes(massofmeteorite)) +
      geom_histogram(fill = fillColor2) +
      scale_x_log10() +
      scale_y_log10() +
      labs(
        x = 'Mass in gms' ,
        y = 'Count',
        title = paste("Distribution of", "mass")
      ) +
      theme_bw()
  })
  
  output$hist2 <- renderPlot({
    new = mydataset
    massofmeteorite = new$`mass (g)`
    hist(
      massofmeteorite,
      col = blues9,
      ylim = c(0, 40000),
      xlim = c(0, 60000000),
      breaks = 18
    )
  })
  
  output$dist <- renderText({
    #applying bernoulli distribution
    #p probability that it 'fell'
    #k possible outcomes
    #x=0 found
    p = 0.5
    k = 1065
    x = 1
    prob = (p ^ x) * ((1 - p) ^ (1 - x))
    prob
  })
  
  output$reg1 <- renderText({
    y = mydataset$`mass (g)`
    x <- mydataset$year
    xbar = mean(x)
    ybar = mean(y)
    n1 <- (x - xbar) * (y - ybar)
    n1 = sum(n1)
    
    n2 <- (x - xbar) ^ 2
    n2 = sum(n2)
    
    bhat <- n1 / n2
    ahat <- ybar - (bhat * xbar)
    x = as.numeric(input$txt1)
    yhat = ahat + bhat * x
    yhat
  })
  output$reg2 <- renderPlot({
    plot(mydataset$`mass (g)`)
  })
  
  output$dt <- renderDataTable({
    #filtering columns
    mydataset2 <-
      select(
        mydataset,
        -fall,
        -GeoLocation,
        -recclass,
        -name,
        -nametype,
        -id,
        -reclat,
        -reclong
      )
    DT::datatable(summary(mydataset2) ,
                  options = list(lengthMenu = c(6, 12), pageLength = 6))
  })
  
  
  output$distPlot <- renderPlot({
    y = mydataset$`mass (g)`
    x <- mydataset$year
    xbar = mean(x)
    ybar = mean(y)
    n1 <- (x - xbar) * (y - ybar)
    n1 = sum(n1)
    
    n2 <- (x - xbar) ^ 2
    n2 = sum(n2)
    
    bhat <- n1 / n2
    ahat <- ybar - (bhat * xbar)
    yhat = ahat + bhat * x
    
  })
  
}


# Application
shinyApp(ui = ui, server = server)