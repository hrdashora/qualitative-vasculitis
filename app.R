library(shiny)
library(gridExtra)
source("main_loop.R")
source("crop.R")
source("heatmap.R")
source("stacked.R")
source("hist.R")
source("stats.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel( h1("Qualitative Analysis of Vasculitis", align = "center")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      helpText("Angiogram Scorecard"),
      
      # Input: Select a file ----
      fileInput(inputId = "angio",
                label = "Choose AVaDaS CSV File", #Angiogram Vascular Damage Score
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-seperated-values,text/plain",
                         ".csv"),
                placeholder = "No file selected..."),
      
      # Input: Submit button ----
      actionButton(inputId = "angio.submit",
                   label = "Submit"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox group for choosing subtype ----
      checkboxGroupInput(inputId = "subtype",
                  label = "Choose a subtype:",
                  choices = list("GCA",
                                 "TAK"),
                  selected = c("GCA","TAK")),
      
      # Input: Select box for vascular region ----
      selectInput(inputId = "region",
                  label = "Select a vascular region:",
                  choices = c("ALL",
                              "Ascend Aorta" = "AscAo", 
                              "Aortic Arch" = "AorArch", 
                              "Descend Aorta" = "DescAo",
                              "Abdom. Aorta" = "AbdAo",
                              "R. Iliac" = "Riliac",
                              "L. Iliac" = "Liliac",
                              "R. Femoral" = "Rfem",
                              "L. Femoral" = "Lfem",
                              "R. Carotid" = "RCA",
                              "L. Carotid" = "LCA",
                              "Innominate" = "Innom",
                              "R. Subclavian" = "Rsub",
                              "L. Subclavian" = "Lsub",
                              "R. Axillary" = "Rax", 
                              "L. Axillary" = "Lax",
                              "R. Vertebral" = "Rvert",
                              "L. Vertebral" = "Lvert"),
                  selected = "ALL", multiple = TRUE),
      
      # Horizontal line ----
      tags$hr(style = "border-color: black;"),
      
      helpText("PET Scorecard"),
      
      # Input: Select a file ----
      fileInput(inputId = "pet",
                label = "Choose PETVAS CSV File (NOT IMPLEMENTED)",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-seperated-values,text'plain",
                           ".csv"),
                placeholder = "No file selected..."),
      
      # Input: Submit button ----
      actionButton(inputId = "pet.submit",
                   label = "Submit"),
      
      width = 3),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Heatmap",
                           plotOutput("heat"),
                           downloadButton('downloadheat','Download')
                           ),
                  tabPanel("Stacked Bar",
                           plotOutput("stack", width = "75%"),
                           downloadButton('downloadstack','Download')
                           ),
                  tabPanel("Histogram",
                           plotOutput("hist"),
                           tableOutput("histnum")
                           ),
                  tabPanel("Statistics",
                           tableOutput("lesionstats"),
                           tableOutput("basestats")
                           )
      )
    )
  )
)

# Define server logic required ----
server <- function(input, output) {
  
  # Run the following function upon the pressing of the 'submit' button
  loop.output <- eventReactive(input$angio.submit,{
    req(input$angio) #require a filepath to be provided
    showModal(modalDialog("Loading...", footer = NULL)) #display a dialog box while the computation runs
    inFile <- input$angio
    output <- main_loop(inFile$datapath) #output list of 2 elements: 1) heatmap data, 2) subtype data
    removeModal()
    return(output)
  })
  
  # Crop the raw data in response to changes in panel input
  crop.output <- reactive({
    req(input$subtype, input$region)
    output <- crop(loop.output(), input$subtype, input$region)
    return(output)
  })
  
  
  output$heat <- renderPlot({
    req(input$subtype, input$region)
    heatmap(crop.output())
  })
  
  
  output$downloadheat <- downloadHandler(
    filename = function() {
      paste("heatmap.csv")
    },
    content = function(file) {
      sep <- ","
      write.csv(crop.output()[[1]], file, row.names = FALSE)
    }
  )
  
  output$stack <- renderPlot({
    req(input$subtype, input$region)
    plot.stack <- stacked(crop.output())[[1]]
    plot.stack
  })
  
  output$downloadstack <- downloadHandler(
    filename = function() {
      paste("stackedbarchart.csv")
    },
    content = function(file) {
      sep <- ","
      write.csv(stacked(crop.output())[[2]], file, row.names = TRUE)
    }
  )
  
  output$hist <- renderPlot({
    req(input$subtype, input$region)
    hist(crop.output())
  })
  
  output$histnum <- renderTable({
    req(input$subtype, input$region)
    tnt <- crop.output()[[2]]
    num.hist <- tnt %>% group_by(LVV_Type) %>% summarise(Median = median(Elapsed_Time),
                                                         Q1 = quantile(Elapsed_Time, probs = 0.25),
                                                         Q2 = quantile(Elapsed_Time, probs = 0.75))
    num.hist[3,2:4] <- tnt %>% summarise(median(Elapsed_Time),
                                         quantile(Elapsed_Time, probs = 0.25),
                                         quantile(Elapsed_Time, probs = 0.75))
    num.hist[3,1] <- "ALL"
    num.hist[,2:4] <- round(num.hist[,2:4]/365, digits = 2)
    colnames(num.hist)[1] <- "Subtype"
    num.hist
  }, rownames = FALSE)
  
  output$lesionstats <- renderTable({
    req(input$subtype, input$region)
    stats.output <- stats(crop.output())
    tabledata <- stats.output[[1]]
    colnames(tabledata) <- c("Lesion Count", "Unique Patients")
    rownames(tabledata) <- c("Territory Not Involved","No Change in Damage",
                           "New Lesion","Increased Severity of Damage",
                           "Resolved Lesion","Decreased Severity of Damage", "TOTAL")
    tabledata
  }, rownames = TRUE)
  
  output$basestats <- renderTable({
    req(input$subtype, input$region)
    stats.output <- stats(crop.output())
    tabledata <- stats.output[[2]]
    colnames(tabledata) <- c("Lesion Count", "Unique Patients")
    rownames(tabledata) <- c("Active Baseline","Inactive Baseline","Improved Baseline")
    tabledata
  }, rownames = TRUE)
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)