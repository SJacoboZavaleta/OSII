# PRELIMINARY VERSION of scoring an open science impact score (OSIS)
library(shiny)
indexWeights = c(0.3,0.2,0.3,0.1,0.1,
                 0.1,0.5,0.4,
                 0.2,0.2,0.2,0.1,0.3)

report1 = array(sample(1:5, 13, replace=T), c(13,1))
report2 = array(sample(1:5, 13, replace=T), c(13,1))
report3 = array(sample(1:5, 13, replace=T), c(13,1))
report4 = array(sample(1:5, 13, replace=T), c(13,1))

  
reportedData = data.frame(
  scores = c("DFC","DS","DA","EDA","LL","UBD","GD","WTA","NTA","TDU","TUE","SSE","URQ"),
  study1 = report1,
  study2 = report2,
  study3 = report3,
  study4 = report4)

colnames(reportedData) = c("Partial scores","Study 1", "Study 2", "Study 3", "Study 4")

ui <- fluidPage(
  titlePanel("Open Science Impact Index (OSII) Simulator"),
  h3(tags$a("For the NASA SPACE APPS CHALLENGES",href="https://www.spaceappschallenge.org/")),
  tags$div(class = "h4", checked = NA,
           tags$a(href = "https://github.com/SJacoboZavaleta/nasaspaceapss2023", "Foundation and Space Team. Find our code here!")),
  hr(style = "border-top: 3px solid #000000;"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("AI",
          h4("Accesibility Impact"),
          sliderInput("DFC", "Data Format Compatibility:", min=0, max=5, value=5),
          sliderInput("DS", "Data source:", min=0, max=5, value=5),
          sliderInput("DA", "Data Accesibility:", min=0, max=5, value=5),
          sliderInput("EDA", "Ease of Data Discovery:", min=0, max=5, value=5),
          sliderInput("LL", "Language Localization:", min=0, max=5, value=5)
        ),
        tabPanel("II",
          h4("Inclusivity Impact"),
          sliderInput("UBD", "User Background Diversity:", min=0, max=5, value=5),
          sliderInput("GD", "Geographic Diversity:", min=0, max=5, value=5),
          sliderInput("WTA", "Website and Tool Accessibility:", min=0, max=5, value=5)
        ),
        tabPanel("RI",
          h4("Reproducibility Impact"),
          sliderInput("NTA", "Number of Tools Available:", min=0, max=5, value=5),
          sliderInput("TDU", "Tool Documentation & Usability:", min=0, max=5, value=5),
          sliderInput("TUE", "Tool Update Frequency & Community Engagement:", min=0, max=5, value=5),
          sliderInput("SSE", "Support Service Effectiveness :", min=0, max=5, value=5),
          sliderInput("URQ", "User Resourse Quality:", min=0, max=5, value=5)
        ),
        tabPanel("Weights",
          h4("For OSAI's components"),
          numericInput("ai_weight1", "AI_weight1",value=indexWeights[1]),
          numericInput("ai_weight2", "AI_weight2",value=indexWeights[2]),
          numericInput("ai_weight3", "AI_weight3",value=indexWeights[3]),
          numericInput("ai_weight4", "AI_weight4",value=indexWeights[4]),
          numericInput("ai_weight5", "AI_weight5",value=indexWeights[5]),
          numericInput("ii_weight1", "II_weight1",value=indexWeights[6]),
          numericInput("ii_weight2", "II_weight2",value=indexWeights[7]),
          numericInput("ii_weight3", "II_weight3",value=indexWeights[8]),
          numericInput("ri_weight1", "RI_weight1",value=indexWeights[9]),
          numericInput("ri_weight2", "RI_weight2",value=indexWeights[10]),
          numericInput("ri_weight3", "RI_weight3",value=indexWeights[11]),
          numericInput("ri_weight4", "RI_weight4",value=indexWeights[12]),
          numericInput("ri_weight5", "RI_weight5",value=indexWeights[13])
        )
      )
    ),
    
    mainPanel(
      h3("OSII Score:"),
      column(12,align = "center",textOutput("OSAI")),
      tags$head(tags$style("#OSAI{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
      )),
      h3("Multiple OSII reported studies:"),
      column(12,align = "center",tableOutput('reportTable')),
      column(12,align = "center",plotOutput('OSAIcomparison'))
    )
  )
)

server <- function(input, output) {
  observe({
    AI <- (input$ai_weight1*input$DFC + input$ai_weight2*input$DS + input$ai_weight3*input$DA + input$ai_weight4*input$EDA + input$ai_weight5*input$LL) / 5
    II <- (input$ii_weight1*input$UBD + input$ii_weight2*input$GD + input$ii_weight3*input$WTA) / 3
    RI <- (input$ri_weight1*input$NTA + input$ri_weight2*input$TDU + input$ri_weight3*input$TUE + input$ri_weight4*input$SSE + input$ri_weight5*input$URQ) / 5
    OSAI <- (AI + II + RI) / 3
    
    output$OSAI <- renderText({
      round(OSAI, 2)})
      
    output$reportTable <- renderTable(reportedData)
    
    output$OSAIcomparison <- renderPlot({
      AI <- colSums(reportedData[1:5,2:5]*indexWeights[1:5])/5
      II <- colSums(reportedData[6:8,2:5]*indexWeights[6:8])/3
      RI <- colSums(reportedData[9:13,2:5]*indexWeights[9:13])/5
      OSAI <- (AI + II + RI) / 3
      barplot(OSAI,col = c(1,2,3,4),width = 0.1)
      },height = 400,width = 500)
  })
}

shinyApp(ui=ui, server=server)
