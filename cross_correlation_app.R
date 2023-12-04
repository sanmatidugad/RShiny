# Install and load necessary packages
#if (!require("shiny")) install.packages("shiny")
#if (!require("heatmap.2")) install.packages("gplots")

library(shiny)
library(gplots)

# Define UI
options(shiny.maxRequestSize = 30*1024^2)  # Set to 30 MB (adjust as needed)
ui <- fluidPage(
  titlePanel("Cross-Correlation Plotter"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      fileInput("first_dataset", "Choose your first dataset",
                accept = c(".txt")),
      fileInput("second_dataset", "Choose your second dataset",
                accept = c(".csv")),
      textInput("plot_title", "Enter Plot Title", ""),
      textInput("x_axis_label", "Enter X-Axis Label", "Pascal normalized"),
      textInput("y_axis_label", "Enter Y-Axis Label", "Java normalized"),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      plotOutput("heatmap", width = "100%", height = "800px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read data based on user input
  pascal_normalized <- reactive({
    req(input$first_dataset)
    data1 = read.table(input$first_dataset$datapath, sep = "\t", row.names = 1)
    colnames(data1) = data1[1,]
    data1 = data.frame(sapply(data1[-1,], as.numeric))
    data1
    #data1 = data.frame(sapply(pascal_normalized[-1,], as.numeric))
    #head(pascal_normalized)
    
  })
  
  java_normalized <- reactive({
    req(input$second_dataset)
    data2 = read.csv(input$second_dataset$datapath, row.names = 1)
    data2
  })
  
  observeEvent(input$submit_button, {
    output$heatmap <- renderPlot({
      # Use the reactive data objects
      pascal_data <- pascal_normalized()
      java_data <- java_normalized()
      
      #perform cross-correlation
      correlation_matrix <- round(cor(pascal_data, java_data), 2)
      
      # Get user-specified labels
      plot_title <- input$plot_title
      xlab <- input$x_axis_label
      ylab <- input$y_axis_label
      
      
      par(mar=c(10,10,10,10)) 
      heatmap.2(correlation_matrix,
                trace = "none",
                dendrogram = 'none',
                Colv = FALSE,    #Turn off column dendrograms
                Rowv = FALSE,    #Turn off row dendrograms
                main = plot_title,
                xlab = xlab,
                ylab = ylab,
                density.info = "none",    #turn off density plots
                cellnote = correlation_matrix,    #add values to the block
                notecol = "black",    #color of the values
                key = TRUE,    #show the color key
                keysize = 0.5,    #adjust the key size
                key.title = "Correlation",
                cexCol = 1.2,
                cexRow = 1.2,
                margins = c(14,14),
      )
    })
  })
  # Perform cross-correlation and generate heatmap
  
}

# Run the app
shinyApp(ui, server)
