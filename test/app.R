library(shiny)
library(ggplot2)
library(readxl)

data <- read_excel("~/R_files/r4ds_final-main/xutum_data.xlsx")
data[,c(4:11)] <- sapply(data[,c(4:11)], as.numeric)
data$Dates <- as.character(data$Dates)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year", label = "Select year:", 
                  choices = c("2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-12-31","2021-12-31"),    
                  selected = 2021),
      selectInput(inputId = "min_employees", "Select min number of employees:",
                  choices = seq(1,140000,by=1000),
                  selected = 1),
      selectInput(inputId = "max_employees", label = "Select max number of employees:",
                  choices = seq(1,140000,by=1000),
                  selected = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define the server logic for the app
server <- function(input, output) {
  # Create a reactive expression to filter the data based on the input
  filtered_data <- reactive({
    data[data$Dates == input$year,]
    data[input$min_employees < data$`Number of Employees` & data$`Number of Employees` < input$max_employees,]
  })
  # Create the plot
  output$plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = `Number of Employees`, y = `Profit Margin`)) +
      geom_point(aes(color = Sector))+
      ylim(c(-100,300))+
      xlim(c(input$min_employees,input$max_employees))
  })
}

shinyApp(ui = ui, server = server)
