library(shiny)
library(tidyverse)
library(readxl)
dataset <- read_xlsx("xutum_data.xlsx")
dataset[,c(4:11)] <- sapply(dataset[,c(4:11)], as.numeric)
dataset$Dates <- as.character(dataset$Dates)
str(dataset)

ui <- fluidPage(
    titlePanel("BISTTUM Data"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "emp_int",
                        label = "Employee Interval:",
                        min = 1,
                        max = max(dataset$`Number of Employees`,na.rm = T),
                        value = c(1,max(dataset$`Number of Employees`,na.rm = T))
                        ),
        selectInput(inputId = "year",
                    label = "Choose a year:",
                    choices = c(unique(dataset$Dates)) 
                    )
        ),
        mainPanel(
           plotOutput("plot")
           )
        )
)

server <- function(input, output) {
  
    output$plot <- renderPlot(
      {
      f_dataset <- filter(dataset, 
                          dataset$Dates %in% input$year & 
                            dataset$`Number of Employees` >=input$emp_int[1] & 
                            dataset$`Number of Employees` <= input$emp_int[2])
        ggplot(f_dataset,
               aes(x = `Number of Employees`, y = `Profit Margin`))+
          geom_point(aes(color = Sector), show.legend = T)+
          theme(
            legend.text = element_text(size = 6,vjust = 0.5, hjust = 0.5, margin = margin(r = 11)), 
            legend.position = "right",
            legend.key.width = unit(0.1, "cm"),
            legend.key.size = unit(0.1, "cm"),
            legend.spacing = unit(0.01, "cm"))+
          scale_colour_viridis_d(option = "turbo",
                                 end = 1,
                                 labels = scales::label_wrap(20),
                                 guide = guide_legend(nrow = 16))+
          lims(x = input$emp_int)+
          labs(title = "Profit - No. of Employee",
               subtitle = input$year)
    }
    )
}

shinyApp(ui = ui, server = server)

