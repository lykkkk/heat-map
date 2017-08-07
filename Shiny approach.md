### Heatmap Shiny approach

#### R code

##### UI Part

```R
library(shiny)

shinyUI(fluidPage(
  titlePanel("Heatmap Shiny App"),
  sidebarLayout(
    sidebarPanel(h4(style = "font-family:Impact",
                    "See data files and codes in my",
                    a("GitHub.",
                      href = "https://github.com/lykkkk/heat-map")
                    ),
                 numericInput(inputId = "zip",
                              label = "Input a zipcode",
                              value=60060, min=10000, max=99999),
                 textInput(inputId = "RUG",
                           label = "Input RUG category",
                           value = "RUA"),
                 sliderInput(inputId = "mile",
                             label = "Choose Range(miles)",
                             value = 50, min=1, max = 1000),
                 actionButton(inputId = "go",
                              label = "Update"),
                 br(),
                 br(),
                 h4("The distribution of RUG level"),
                 p("There are 5 level of RUG categories"),
                 p("Ultra High: RUA, RUB, RUC, RUL, RUX"),
                 p("Very High: RVA, RVB, RVC, RVL, RVX"),
                 p("High: RHA, RHB, RHC, RHL, RHX"),
                 p("Medium: RMA, RMB, RMC, RML, RMX"),
                 p("Other: Other RUG categories")
                 ),
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", plotOutput("heatmap")),
        tabPanel("List table(top 10)", tableOutput("list")),
        tabPanel("Full list table", tableOutput("fulllist")),
        tabPanel("Appedix",
                 img(src="https://github.com/lykkkk/heat-map/raw/master/1.png",height=600,width=700))
        )
      )
    )
  )
)

```


##### SERVER Part

``` R
library(shiny)
library(zipcode)
library(ggplot2)
library(maps)
library(viridis)
library(geosphere)
library(ggmap)
test <- read.csv("https://github.com/lykkkk/heat-map/raw/master/RUG_provider(2013%262014).csv")

shinyServer(function(input, output) {
  datazip <- eventReactive(input$go, {
    input$zip
    })
  dataRUG <- eventReactive(input$go, {
    input$RUG
    })
  datamile <- eventReactive(input$go, {
    input$mile
    })
  output$heatmap <- renderPlot({
    data(zipcode)
    test <- test[which(test$RUG==dataRUG()),]
    test$ZipCode <- clean.zipcodes(test$ZipCode)
    test.zip <- list(ZipCode=zipcode$zip, longitude=zipcode$longitude, latitude=zipcode$latitude)
    test0 <- merge(test, test.zip, by='ZipCode')
    
    testzip <- zipcode[which(zipcode$zip==datazip()),]
    points1 <- cbind(test0$longitude, test0$latitude)
    points2 <- cbind(testzip$longitude, testzip$latitude)
    r=3959
    distance <- distHaversine(points1, points2, r)
    test0$distance <- distance
  
    z=datamile()
    test1 <- test0[test0$distance<=z,]
    testtop10 <- (test1[order(test1$Efficiency),])[1:10,]
    testother <- (test1[order(test1$Efficiency),])[-(1:10),]
    zoomnum <- floor(log(11520/z)/log(2)+1)
  
    testmap <- get_map(location=c(testzip$longitude, testzip$latitude), 
                     zoom=zoomnum, maptype = "roadmap")
    ggmap(testmap, extent="device") + 
    geom_point(aes(x=longitude, y=latitude), alpha=0.8, color="red", size=5, data=testzip) + 
    geom_point(aes(x=longitude, y=latitude), color="green", alpha=0.8, size=2*zoomnum/(8*testtop10$Efficiency), data=testtop10) + 
    geom_point(aes(x=longitude, y=latitude), color="blue", alpha=0.5, size=2*zoomnum/(8*testother$Efficiency), data=testother)
  })
  
  output$list <- renderTable({
    data(zipcode)
    test <- test[which(test$RUG==dataRUG()),]
    test$ZipCode <- clean.zipcodes(test$ZipCode)
    test.zip <- list(ZipCode=zipcode$zip, longitude=zipcode$longitude, latitude=zipcode$latitude)
    test0 <- merge(test, test.zip, by='ZipCode')
  
    testzip <- zipcode[which(zipcode$zip==datazip()),]
    points1 <- cbind(test0$longitude, test0$latitude)
    points2 <- cbind(testzip$longitude, testzip$latitude)
    r=3959
    distance <- distHaversine(points1, points2, r)
    test0$distance <- distance
  
    z=datamile()
    test1 <- test0[test0$distance<=z,]
    test1[,19] <- test1[,13]-test1[,12]
    list <- (test1[order(test1$Efficiency),])[1:10,c(2:6,1,19,18)]
    list <- cbind(c(1:nrow(list)),list)
    colnames(list) <- c("Num", "Facility ID", "Name", "Street", "City", "State",
                        "Zipcode", "Expected Saving($)", "Distance(miles)")
    print(list)
  })

  output$fulllist <- renderTable({
    data(zipcode)
    test <- test[which(test$RUG==dataRUG()),]
    test$ZipCode <- clean.zipcodes(test$ZipCode)
    test.zip <- list(ZipCode=zipcode$zip, longitude=zipcode$longitude, latitude=zipcode$latitude)
    test0 <- merge(test, test.zip, by='ZipCode')
  
    testzip <- zipcode[which(zipcode$zip==datazip()),]
    points1 <- cbind(test0$longitude, test0$latitude)
    points2 <- cbind(testzip$longitude, testzip$latitude)
    r=3959
    distance <- distHaversine(points1, points2, r)
    test0$distance <- distance
  
    z=datamile()
    test1 <- test0[test0$distance<=z,]
    test1[,19] <- test1[,13]-test1[,12]
    fulllist <- (test1[order(test1$Efficiency),])[,c(2:6,1,19,18)]
    fulllist <- cbind(c(1:nrow(fulllist)),fulllist)
    colnames(fulllist) <- c("Num", "Facility ID", "Name", "Street", "City", "State",
                            "Zipcode", "Expected Saving($)", "Distance(miles)")
    print(fulllist)
  })
})

```