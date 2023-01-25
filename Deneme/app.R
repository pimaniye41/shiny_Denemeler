library(shiny)
library(tidyverse)
source("global.R")
ui <- fluidPage()
server <- function(input,output){}
shinyApp(ui = ui, server = server)
ui <- fluidPage(
  titlePanel(title = "Sektorel Karşılaştırma"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "sector", label = "Bir Sektör Seçin:", choices = c("ACCOMMODATION AND FOOD SERVICE ACTIVITIES","ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES","AGRICULTURE, FORESTRY AND FISHING","ARTS, ENTERTAINMENT AND RECREATION","CONSTRUCTION","ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY","FINANCIAL AND INSURANCE ACTIVITIES","HUMAN HEALTH AND SOCIAL WORK ACTIVITIES","INFORMATION AND COMMUNICATION","MANUFACTURING","MINING AND QUARRYING","PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES","REAL ESTATE ACTIVITIES","TRANSPORTATION AND STORAGE","WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES","WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES")),
      selectInput(inputId = "date",label = "Yıl seçin",choices = c("2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-12-31","2021-12-31")),
      selectInput(inputId = "type",label = "Y eksenini seçin",choices = c("Profit Margin","Log of Sales")),
      sliderInput(inputId = "emp_int",label = "Çalışan aralığı",min = 1,max = max(dataset$`Number of Employees`,na.rm = T),value = 1,dragRange = T),
      actionButton(inputId = "save",label = "kaydet")),
    mainPanel(plotOutput(outputId = "comp_plot"))
              )
  )

server <- function(input,output){
  observeEvent(input$save,{
    my_data <- reactive(dataset)
  output$comp_plot <- renderPlot({
    ggplot(my_data%>%
             filter(Dates %in% date),
           aes(x = `Number of Employees`,
               y = `Profit Margin`))+
      geom_point(aes(color = Sector))
  })
  }
)
}
shinyApp(ui = ui, server = server)

