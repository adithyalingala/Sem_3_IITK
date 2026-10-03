library(shiny)
library(ggplot2)
library(leaflet)
Public_Banks<-read.csv("public_banks.csv")
Private_Banks = read.csv("private_banks.csv")
Regional_Rural_Banks = read.csv("regional_rural_banks.csv")
Assets_Revenue =read.csv("Assets_Revenue.csv")

Public_Banks$Branches=  gsub("," ,  ""  ,Public_Banks$Branches)
Private_Banks$Branches=gsub("," ,  ""  ,Private_Banks$Branches)
Regional_Rural_Banks$Branches= gsub(",", "",Regional_Rural_Banks$Branches )




ui <- fluidPage(
  
  titlePanel(title = "Analysis of Commercial Banks of India "),
  sidebarLayout(
    sidebarPanel(
      selectInput("bank", "Select bank type",c("Public_Banks", "Private_Banks", "Regional_Rural_Banks"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Summary", verbatimTextOutput("summary")),
                  tabPanel("Asset_Revenue_comparision", plotOutput("asset") ),
                  tabPanel("Govt_share",plotOutput("gov_share")),
                  tabPanel("Bank_Location", leafletOutput("map")),
                  tabPanel("Assets_Revenue_Correlation", plotOutput("avr")),
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
  
 
  output$summary<-renderPrint({
    bank <- bankInput()
    summary(bank[[1]])
  })
  
  output$asset<-renderPlot({
    bank=bankInput()
    
    if(bank[[2]]!=3)
    {
      df_long <- pivot_longer(bank[[1]], cols = c(Total.Assets, Revenues), names_to = "Variable", values_to = "Value")
    
    ggplot(df_long, aes(x = Bank_name, y = Value, fill = Variable)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      ggtitle(paste("Comparision of Assets with Revenues in different",bank)) +
      xlab("Bank_name") +
      ylab("Total.Assets / Revenues (in Cr)") +
      scale_fill_manual(values = c("Total.Assets" = "yellow", "Revenues"  = "green")) +
      theme_minimal()
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
    b= makeAwesomeIcon(markerColor = "blue")
    r= makeAwesomeIcon(markerColor = "red")
    g= makeAwesomeIcon(markerColor = "green")
    leaflet() %>% addTiles() %>%
      addAwesomeMarkers(lat =Regional_Rural_Banks$latitude, lng =Regional_Rural_Banks$longitude, icon = b, popup =paste(Regional_Rural_Banks$Bank_name,br(),
                                                                                                                        "Headquarter:", Regional_Rural_Banks$Headquarters, br(),
                                                                                                                        "Branches:", Regional_Rural_Banks$Branches, br(),
                                                                                                                        "Established Year:", Regional_Rural_Banks$Established,br(),
                                                                                                                        "Government Shareholding :",Regional_Rural_Banks$Government..Shareholding, "%")) %>%
      addAwesomeMarkers(lat =Public_Banks$latitude, lng =Public_Banks$longitude, icon = g,popup =paste(Public_Banks$Bank_name,br(),
                                                                                                       "Headquarter:", Public_Banks$Headquarters, br(),
                                                                                                       "Branches:", Public_Banks$Branches, br(),
                                                                                                       "Established Year:",Public_Banks$Established,br(),
                                                                                                       "Government Shareholding :", Public_Banks$Government..Shareholding, "%")) %>%
      addAwesomeMarkers(lat =Private_Banks$latitude, lng =Private_Banks$longitude, icon = r, popup =paste(Private_Banks$Bank_name,br(),
                                                                                                          "Headquarter:", Private_Banks$Headquarters, br(),
                                                                                                          "Branches:", Private_Banks$Branches, br(),
                                                                                                          "Established Year:", Private_Banks$Established,br(),
                                                                                                          "Government Shareholding :", Private_Banks$Government..Shareholding, "%")) %>%
      addLegend(position = "topright", colors = c("red", "green","blue"), labels = c("Private Banks","Public Banks","Regional Rural Banks"))
    
  })
  
  output$avr=renderPlot({
   
    
    
    
    ggplot(Assets_Revenue, aes(x = Total.Assets, y = Revenues, color = Bank_Type)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      xlab("Total.Assets (in cr)") +
      ylab("Revenues (in cr)") +
      ggtitle("Total.Assets vs Revenues") +
      geom_smooth(aes(group = Bank_Type), method = "lm", se = FALSE) +
      scale_color_manual(values = c("red", "blue"))  # Adjust colors as needed
    
    
    
    
  })
  
  output$branches=renderPlot({
    bank=bankInput()
    
    if(bank[[2]]==1){
      ggplot(bank[[1]]) +
        geom_bar(aes(x = Bank_name, y = Branches, fill = Headquarters), stat = "identity") +
        geom_line(aes(x = Bank_name, y = Revenues/100000, group = 2, color = "Revenue"), stat = "identity") +
        labs(x = "Bank Name", y = "Number of Branches", title = "Number of Branches in different banks",
             color = "Revenue ( in Cr)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = terrain.colors(length(unique(bank[[1]]$Headquarters)))) +
        scale_color_manual(values = c("Revenue" = "red")) +
        geom_text(aes(label = Revenues, x = Bank_name, y = Revenues/100000))
    }
    else if(bank[[2]]==2){
      ggplot(bank[[1]]) +
        geom_bar(aes(x = Bank_name, y = Branches, fill = Headquarters), stat = "identity") +
        geom_line(aes(x = Bank_name, y = Revenues/10000, group = 2, color = "Revenue"), stat = "identity") +
        labs(x = "Bank Name", y = "Number of Branches", title = "Number of Branches in different banks",
             color = "Revenue ( in Cr)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = terrain.colors(length(unique(bank[[1]]$Headquarters)))) +
        scale_color_manual(values = c("Revenue" = "red")) +
        geom_text(aes(label = Revenues, x = Bank_name, y = Revenues/10000))
    }
    else
    {
      ggplot(bank[[1]]) +
        geom_bar(aes(x = Bank_name, y = Branches, fill = Headquarters), stat = "identity") +
        labs(x = "Bank Name", y = "Number of Branches", title = "Number of Branches in different banks") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = terrain.colors(length(unique(bank[[1]]$Headquarters)))) 
        
    }
    
      
   #geom_text(aes(label=Bat_Ave, x=Player_name, y=Bat_Ave*10), colour="white",vjust = -0.5, nudge_y = 0.2, size = 3) 
  })
  
}
# geom_line( aes(x=Bank_name,y=Revenues,group=2),color="red",stat = "identity")+

shinyApp(ui = ui, server = server)