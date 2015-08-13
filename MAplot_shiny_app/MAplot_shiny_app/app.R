#### shiny server
server <- function(input, output) {
  
  # MA plot from sample names entered by user
  # NB: plots for ggplot2 are commented out because brushed points were
  #     either unrecognized or returned more rows than enclosed points; does
  #     not seem to be a problem with base plots
  
  output$MAplot <- renderPlot({
    
    coord.x <- as.numeric(c(input$x.axis.min, input$x.axis.max))
    coord.y <- as.numeric(c(input$y.axis.min, input$y.axis.max))
    
    # values entered for x-axis only
    if (all(!is.na(coord.x)) && any(is.na(coord.y))) {
       plot(MA$av.log2cpm, MA$log2FC, pch=20, col=MA$col, xlim=coord.x, 
            xlab="log2(mean expression)", ylab="log2(fold change")
    } 
    
    # values entered for y-axis only
    if (any(is.na(coord.x)) && all(!is.na(coord.y))) {
       plot(MA$av.log2cpm, MA$log2FC, pch=20, col=MA$col, ylim=coord.y, 
            xlab="log2(mean expression)", ylab="log2(fold change")
    }
    
    # values entered for both x- and y-axis
    if (all(!is.na(coord.x)) && all(!is.na(coord.y))) {
       plot(MA$av.log2cpm, MA$log2FC, pch=20, col=MA$col, xlim=coord.x, ylim=coord.y, 
            xlab="log2(mean expression)", ylab="log2(fold change")
    }
    
    # default plot if no user entered values for axes
    if (all(is.na(coord.x)) && all(is.na(coord.y))) {
       plot(MA$av.log2cpm, MA$log2FC, pch=20, col=MA$col, 
            xlab="log2(mean expression)", ylab="log2(fold change")
    }
  })
  
 # returns individual data of bushed points
  output$info <- renderPrint({
    brushedPoints(MA, input$plot_brush, xvar = "av.log2cpm", yvar = "log2FC")
  })
}

#### shiny user interface (UI)
ui <- fluidPage(
  
  title = "MAplot",
  
  sidebarLayout(
    
    sidebarPanel(

      # input field for x- and y-axis limits
      numericInput(inputId="x.axis.min", label="x-axis min.", value=NA),
      numericInput(inputId="x.axis.max", label="x-axis max.", value=NA),
      numericInput(inputId="y.axis.min", label="y-axis min.", value=NA),
      numericInput(inputId="y.axis.max", label="y-axis max.", value=NA)
    ),
    
    # output of MA plot
    mainPanel(
      plotOutput("MAplot", brush = "plot_brush"),
      verbatimTextOutput("info")
    )
  )  
)

# runs app as a single-file (in contrast to separate UI and server files)
shinyApp(ui = ui, server = server)





