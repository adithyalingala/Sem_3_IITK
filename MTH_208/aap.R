library(shiny)
library(ggplot2)
library(gridExtra)
library(leaflet)
Public_Banks<-read.csv("public_banks.csv")
Private_Banks = read.csv("private_banks.csv")
Regional_Rural_Banks = read.csv("regional_rural_banks.csv")


ui <- fluidPage(
  titlePanel(title = "Group 15 project"),
  sidebarLayout(
    sidebarPanel(
      selectInput("bank", "Select bank type",c("Public_Banks", "Private_Banks", "Regional_Rural_Banks"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("view")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Asset_Plot", plotOutput("asset") ),
                  tabPanel("Revenue_Plot",plotOutput("revenue")),
                  tabPanel("Govt_share",plotOutput("gov_share")),
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Assets_vs_Revenue", plotOutput("avr"))
                  
      )
    )
  )
)


server <- function(input, output){
  bankInput = reactive({
    switch(input$bank,
           "Public_Banks" = list(Public_Banks,1),
           "Private_Banks" = list(Private_Banks,2),
           "Regional_Rural_Banks" = list(Regional_Rural_Banks,3)  )
  })
  
  output$view <- renderTable({
    bankInput()[[1]]
  })
  
  output$summary<-renderPrint({
    bank <- bankInput()
    summary(bank[[1]])
  })
  
  output$asset<-renderPlot({
    bank=bankInput()
    
    if(bank[[2]]!=3)
   {ggplot(bank[[1]], 
                 aes(x=bank[[1]]$Bank_name, y=bank[[1]]$Total.Assets))+
   geom_point() +
   xlab("Bank Name")+
   ylab("Assets")+
   ggtitle("Bank Name vs Assets")}

  })
  
  output$revenue=renderPlot({
    bank=bankInput()
    if(bank[[2]]!=3)
      {ggplot(bank[[1]], 
                    aes(x=bank[[1]]$Bank_name, y=bank[[1]]$Revenues))+
      geom_point() +
      xlab("Bank Name")+
      ylab("Revenues")+
      ggtitle("Bank Name vs Revenues ")}
    
    
    
  })
  
  output$gov_share=renderPlot({
    bank=bankInput()
    if(bank[[2]]==1)
     {ggplot(bank[[1]], 
                   aes(x=bank[[1]]$Bank_name, y=bank[[1]]$Government..Shareholding))+
      geom_point() +
      xlab("Bank Name")+
      ylab("Government Shareholding")+
      ggtitle("Bank Name vs Government Shareholding ")}
    
  })
  
  output$map= renderLeaflet({
    bank=bankInput()[[1]]
    leaflet() %>% addTiles() %>% addMarkers(lat = bank$latitude, lng =bank$longitude, popup = bank$Bank_name)
    
  })
  
  output$avr=renderPlot({
    bank=bankInput()
    if(bank[[2]]!=3)
    {ggplot(bank[[1]], 
            aes(x=bank[[1]]$Total.Assets , y=bank[[1]]$Revenues))+
        geom_point() +
        xlab("Total.Assets ")+
        ylab("Revenues")+
        ggtitle("Total.Assets  vs Revenues ")}
    
    
  
  })
}


shinyApp(ui = ui, server = server)