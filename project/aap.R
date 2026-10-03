library(shiny)
library(ggplot2)
library(gridExtra)
library(leaflet)
Public_Banks<-read.csv("public_banks.csv")
Private_Banks = read.csv("private_banks.csv")
Regional_Rural_Banks = read.csv("regional_rural_banks.csv")


Public_Banks$Branches=  gsub("," ,  ""  ,Public_Banks$Branches)
Private_Banks$Branches=gsub("," ,  ""  ,Private_Banks$Branches)
Regional_Rural_Banks$Branches= gsub(",", "",Regional_Rural_Banks$Branches )



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
                  tabPanel("Assets_vs_Revenue", plotOutput("avr")),
                  tabPanel("Branches", plotOutput("branches"))
                  
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
    {ggplot(bank[[1]], aes(x = bank[[1]]$Bank_name, y = bank[[1]]$Total.Assets)) +
        geom_point() +
        geom_segment(aes(xend = bank[[1]]$Bank_name, yend = 0, color = bank[[1]]$Bank_name), size = 0.5) +
        xlab("Bank Name") +
        ylab("Assets (in cr)") +
        ggtitle("Bank Name vs Assets") +
        scale_color_manual(values = rainbow(length(unique(bank[[1]]$Bank_name)))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))}

  })
  
  output$revenue=renderPlot({
    bank=bankInput()
    if(bank[[2]]!=3)
    {ggplot(bank[[1]], aes(x = bank[[1]]$Bank_name, y = bank[[1]]$Revenues)) +
        geom_point() +
        geom_segment(aes(xend = bank[[1]]$Bank_name, yend = 0, color = bank[[1]]$Bank_name), size = 0.5) +
        xlab("Bank Name") +
        ylab("Revenues (in cr)") +
        ggtitle("Bank Name vs Revenues") +
        scale_color_manual(values = rainbow(length(unique(bank[[1]]$Bank_name)))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      
      
      
    }
    
    
    
  })
  
  output$gov_share=renderPlot({
    bank=bankInput()
    if(bank[[2]]==1)
    {ggplot(bank[[1]], aes(x = bank[[1]]$Bank_name, y = bank[[1]]$Government..Shareholding)) +
        geom_point() +
        geom_segment(aes(xend = bank[[1]]$Bank_name, yend = 0, color = bank[[1]]$Bank_name), size = 1) +
        xlab("Bank Name") +
        ylab("Government Shareholding (in %)") +
        ggtitle("Bank Name vs Government Shareholding") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = rainbow(length(unique(bank[[1]]$Bank_name))))}
    
  })
  
  output$map= renderLeaflet({
    bank=bankInput()[[1]]
    leaflet() %>% addTiles() %>% addMarkers(lat = bank$latitude, lng =bank$longitude, popup =paste(bank$Bank_name,br(),
                                                                                                   "Headquarter:", bank$Headquarters, br(),
                                                                                                 "Branches:", bank$Branches, br(),
                                                                                                 "Established Year:", bank$Established,br(),
                                                                                                 "Government Shareholding :", bank$Government..Shareholding, "%"))
    
  })
  
  output$avr=renderPlot({
    bank=bankInput()
    if(bank[[2]]!=3)
    {ggplot(bank[[1]], 
            aes(x=bank[[1]]$Total.Assets , y=bank[[1]]$Revenues))+
        geom_point() +
        geom_smooth(method = "lm",se = FALSE) +
        xlab("Total.Assets (in cr) ")+
        ylab("Revenues (in cr) ")+
        ggtitle("Total.Assets  vs Revenues ")}
    
    
  
  })
  
  output$branches=renderPlot({
    bank=bankInput()[[1]]
    
    ggplot(bank, aes(x = Bank_name, y = Branches, fill = Bank_name)) +
      geom_bar(stat = "identity") +
      labs(x = "Bank Name", y = "Number of Branches", title = "Number of Branches in different banks") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = terrain.colors(length(unique(bank$Bank_name))))
    
  })
                                                                                              
}


shinyApp(ui = ui, server = server)