library(shiny)
ui <- fluidPage(
  titlePanel("Interactive EOQ Demo"),
  # Sliders to change variable in sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("demand", "Demand (D)",
                  min = 0, max = 1000,
                  value = 250, step = 10,
                  animate = animationOptions(interval = 250, loop = TRUE)),
      
      sliderInput("ordering", "Ordering costs (K)",
                  min = 0, max = 50,
                  value = 10, step = 1,
                  pre = "€", sep = ",",
                  animate = animationOptions(interval = 1000, loop = TRUE)),
      
      sliderInput("carrying", "Holding costs (h)",
                  min = 0, max = 25,
                  value = 2, step = 1,
                  pre = "€", sep = ",",
                  animate = animationOptions(interval = 1000, loop = TRUE)),
    ),
    # Plots in main panel
    mainPanel(
      plotOutput("plotOrders"),
      plotOutput("plotQuantity")
    )
  )
)
server <- function(input, output) {
  # A reactive plot (it changes when the sliders change)
  sliderPlot <- reactive({
    # Make a dataframe for the total cost and EOQ
    TC = rep(0, 50)
    
    # For 1 to 50 orders, calculate the total cost with the variables from the sliders.  
    for (n in 1:50) {
      TC[n] = n * input$ordering + input$demand * input$carrying / (2 * n)
    }
    EOQ = sqrt(2 * input$demand * input$ordering / input$carrying)

    #Make plot
    plot(1:50, TC, ylim=c(0,max(TC)), main = "Total cost vs. number of orders", ylab = "Total cost", xlab = "Number of oders")
    # Add line at the minimum
    abline(h = min(TC), col = "blue")
    mtext(paste("EOQ is ", round(EOQ, digits=2)))
  })
  
  sliderPlot2 <- reactive({
    
    # For a order quantity between 0 and 200, calculate the total cost with the variable from the sliders.
    cost = rep(0, 200)
    for (q in 1:200) {
      cost[q] = input$demand*input$ordering/q + 0.5*input$carrying*q
    }
    
    # Make a plot with a ylim so 98% of the values are viewable, but details are still there
    limit = tail(sort(cost),5)[1]
    plot(1:200, cost, ylim=c(0,limit), main = "Total cost vs. order quantity", ylab = "Total cost", xlab = "Quantity per order")
    # Add line at the minimum
    abline(h = min(cost), col = "blue")
  })
  
  output$plotOrders <- renderPlot({
    sliderPlot()
  })
  output$plotQuantity <- renderPlot({
    sliderPlot2()
  })
}
shinyApp(ui, server)