shinyUI(fluidPage(
  fluidRow(column(12,h1("Line Selector"))),
  fluidRow(column(12,h4("Upload a csv file that has these names: 
                       Entr, Row,	Col, Cruza, Historia, KgHa. The column names need to be
                       EXACT (although there can be other columns in there). ENSURE THERE ARE 
                       NO ACCENTS IN A COLUMN NAME."))),
  fluidRow(column(4,  fileInput("file", label = h3("Upload csv file here"))),
           column(3,  numericInput("num", label = h3("# lines between controls"), value = 10)),
           column(4,
           sliderInput("slider1", label = h3("Select the minimum Normalized yield"), min = -1000, 
                       max = 2000, value = 750))),
  
  hr(),
  
  ## OUTPUT
  
  if(!is.na(a)){
    fluidRow(
      column(3,h2("Plot of controls... check for evenness"),
                    plotOutput(outputId="figControl")),
      column(6,h2("These Items were higher than the selected Normalized yield"),
             h2(verbatimTextOutput("PERC")),
             plotOutput(outputId="fig1")),
      column(3,h2("Trials over control"),
             h4("Green are trials, brown is controls"),
             plotlyOutput("Plot3", height = "600px"))
      )},
  if(!is.na(a)){
    fluidRow(h2("List of Items higher than selected Normalized yield"),
             downloadButton('downloadData', 'Download',),
             dataTableOutput(outputId="table1"))
  }
))