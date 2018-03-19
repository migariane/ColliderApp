library(shiny)
library(shinythemes)
library(ggplot2)
library(readr) # For write_csv
library(dplyr)
library(visreg) # Model visualization

generateData <- function(beta1,beta2,n){
    age <- rnorm(n, 65, 5)
    sodium  <- age/15 + rnorm(n) 
    sbp <- 10*sodium + 1.25 * age + rnorm(n)
    proteinurie  <-  beta1 * sbp + beta2*age - 0.9 * sodium + rnorm(n)
    data.frame(sbp,age,sodium,proteinurie)
}
ui <- fluidPage(theme=shinytheme("cosmo"),
  titlePanel(HTML("<b>Collider</b>: a Shiny app"),windowTitle = "Collider"),
  sidebarLayout(
    sidebarPanel(width = 3,

      # Inicialización MathJax para poder incluir fórmulas en LaTeX
      withMathJax(),

      # Model Selection
      h3("Model selection:"),
      checkboxInput(inputId = "modelA", 
                    label = div(h6(withMathJax("$$\\text{SBP}=\\beta_{0}+\\beta_{1}\\text{AGE}$$")),style="margin-top:-10px"),
                    value=TRUE
                    ),
      
      checkboxInput(inputId = "modelB", 
                    label = div(h6(withMathJax("$$\\text{SBP}=\\beta_{0}+\\beta_{1}\\text{AGE}+\\beta_{2}\\text{SOD}$$")),style="margin-top:-10px"),
                    value=TRUE
                    ),
      
      checkboxInput(inputId = "modelC", 
                    label = div(h6(withMathJax("$$\\text{SBP}=\\beta_{0}+\\beta_{1}\\text{ AGE}+\\beta_{2}\\text{SOD}+\\beta_{3}\\text{PRO}$$")),style="margin-top:-10px"),
                    value=TRUE
                    ),
      
      hr(), 
      
      # Slider coeficientes
      h5(withMathJax("Collider Model: $$\\text{PRO}=\\alpha_{0}+\\alpha_{1}\\text{AGE}+\\alpha_{2}\\text{SBP}+\\alpha_{3}\\text{SOD}$$")),
      h6("Move the input slider to visualize the collider"),
      
      sliderInput(inputId = "beta1", 
                  label = h5(withMathJax("$$\\alpha_1\\text{(Effect of SBP on PRO)}$$")),
                  min=0.5,
                  max=5,
                  step=0.05,
                  value=1.2
                  # animate = animationOptions(interval = 10, loop = FALSE)
                  ),
     
      sliderInput(inputId = "beta2", 
                  label = h5(withMathJax("$$\\alpha_2\\text{(Effect of AGE on PRO)} $$")),
                  min=0.5,
                  max=5,
                  step=0.05,
                  value=1.8
                  #animate = animationOptions(interval = 10, loop = FALSE)
                  ),
     
     #Leyenda
     wellPanel(tags$b("Legend:"),br(),
               "PRO = Proteinurie (mg)",br(),
               "AGE = Age (years)",br(),
               "SOD = Sodium intake (g)",br(),
               "SBP = Systolic Blood Pressure (mmHg)")
               ),
    
    # Outputs: panel tabs
    mainPanel(
      tabsetPanel(
        
        # Tab 0: Welcome
        tabPanel("Welcome",br(),br(),
                 div(img(src="logo.png",width="450px"),style="text-align:center;")
                 ),
        
        # Tab 1: DAG
        tabPanel("DAGs",
                 h2("Directed Acyclic Graphs"),br(),
                 
                 # No model
                 conditionalPanel(condition="input.modelA==false && input.modelB==false &&input.modelC==false",
                                  h3(tags$b("Please select a model")),
                                  hr()
                                  ),

                 # Model A
                 conditionalPanel(condition="input.modelA==true",
                                  h4(withMathJax("$$\\text{Model 1, SBP} = \\beta_{0} + \\beta_{1} \\text{ AGE}$$")),
                                  div(img(src="dagA.png",width="600px"),style="text-align:center;"),
                                  hr()
                                  ),

                 # Model B
                 conditionalPanel(condition="input.modelB==true",
                                  h4(withMathJax("$$\\text{Model 2, SBP} = \\beta_{0} + \\beta_{1} \\text{ AGE} + \\beta_{2} \\text{SOD}$$")),
                                  div(img(src="dagB.png",width="600px"),style="text-align:center;"),
                                  hr()
                                  ),
               
                 # Model C
                 conditionalPanel(condition="input.modelC==true",
                                  h4(withMathJax("$$\\text{Model 3, SBP} = \\beta_{0} + \\beta_{1}  \\text{ AGE} + \\beta_{2} \\text{SOD} + \\beta_{3} \\text{PRO}$$")),
                                  div(img(src="dagC.png",width="600px"),style="text-align:center;"),
                                  hr()
                                  )
                 ),
                 
        # Tab 2: Graphs
        tabPanel("Collider Visualization",
                 h3("Effect of Age on SPB for different models' specifications: collider m3"),br(),
                 # No model
                 conditionalPanel(condition="input.modelA==false && input.modelB==false && input.modelC==false",
                                  h3(tags$b("Please select a model")),
                                  hr()
                                  ),

                 # Models: formula + plot + beta1
                 fluidRow(column(4,h4(uiOutput("formulaA"))),
                          column(4,h4(uiOutput("formulaB"))),
                          column(4,h4(uiOutput("formulaC")))
                          ),
                 
                 fluidRow(column(4,plotOutput("gA1"),style='padding:0px;'),
                          column(4,plotOutput("gB1"),style='padding:0px;'),
                          column(4,plotOutput("gC1"),style='padding:0px;')
                          ),
                 
                 fluidRow(column(4,h4(uiOutput("coefA"))),
                          column(4,h4(uiOutput("coefB"))),
                          column(4,h4(uiOutput("coefC")))
                 ),

                 hr()
        ),
        
        # Tab 3: Code for data generation
        tabPanel("Data generation",
                 h2("Code for data generation"), br(),
                 
                 # Code
                 tags$p("generateData <- function(beta1,beta2,n){",br(),
                        "age <- rnorm(n, 65, 5)",br(),
                        "sodium  <- age/15 + rnorm(n)",br(), 
                        "sbp <- 10*sodium + 1.25*age + rnorm(n)",br(),
                        "proteinurie  <- beta1*sbp + beta2*age - 0.9*sodium + rnorm(n)",br(),
                        "data.frame(sbp,age,sodium,proteinurie)",br(),
                        "}",br(),br(),
                        "set.seed=777", br(),br(),
                        "head(generateData(1000))",br(),
                        tableOutput("table_generateData"),
                        style="font-family: 'Courier New'"
                        ),
                 
                 downloadButton(outputId="download_data", label=tags$b("Download 1.000 simulations (.csv)")),
                 
                 hr(),
                 
                 # Distribution graphs
                 h3(withMathJax("$$\\text{SBP} = \\beta_{0} + \\beta_{1} \\text{ AGE} + \\beta_{2} \\text{PRO} + \\beta_{3} \\text{SOD}$$")),
                
                 fluidRow(column(4,plotOutput("data.generation.plot1"),style='padding:0px;'),
                          column(4,plotOutput("data.generation.plot2"),style='padding:0px;'),
                          column(4,plotOutput("data.generation.plot3"),style='padding:0px;')
                          ),
                 hr()
                 ),
        
        # Tab 4: Article
        tabPanel("Article",br(),br(),
                 uiOutput("article"))
        ,
        
        # Tab 5: Authorship & Acknowledgment
        tabPanel("Credits & Acknowledgment",
                 
                 #Authorship
                 h2("Autorship"),br(),
                 fluidRow(column(2,img(src="logo_MALF.png",width="100px")),
                          column(10,h4(tags$b("Miguel Angel Luque-Fernandez")),
                          h4("Biomedical Research Institute of Granada, Non‐Communicable and Cancer Epidemiology Group (ibs.Granada), University of Granada."),
                          h4("London School of Hygiene & Tropical Medicine"),
                          tags$i(h5("miguel.luque.easp at juntadeandalucia.es")))
                          ),
                 
                 hr(),
                 
                 fluidRow(column(2,img(src="logo_DRS.png",width="100px")),
                          column(10,h4(tags$b("Daniel Redondo Sánchez")),
                          h4("Biomedical Research Institute of Granada, Non‐Communicable and Cancer Epidemiology Group (ibs.Granada), University of Granada."),
                          h4("Andalusian School of Public Health"),
                          tags$i(h5("daniel.redondo.easp at juntadeandalucia.es")))
                          ),
                 
                 hr(),
                 
                 fluidRow(column(2,img(src="logo_MS.png",width="100px")),
                          column(10,h4(tags$b("Michael Schomaker")),
                                 h4("The University of Cape Town, School of Public Health and Family Medicine,
                                    Center for Infectious Disease Epidemiology and Research. Cape Town, South Africa"),
                                 br(),
                                 tags$i(h5("michael.schomaker at uct.ac.za")))
                 ),
                 
                 hr(),
                 
                 #Acknowledgment
                 h2("Acknowledgment"),
                 tags$b("Funding information"),br(),
                 "Carlos III Institute of Health, Grant/Award Number: CP17/00206",br(),br(),
                 fluidRow(column(5,img(src="logofeder.png",width="75%")),
                          column(5,img(src="logoibs.png",width="75%"))
                          )
                 )
                  
      ))))

#Server function
server <- function(input, output) {

#Formulae and figures
output$formulaA<-renderUI({
    if(input$modelA==TRUE)  withMathJax("$$\\text{SBP} = \\beta_{0} + \\beta_{1} \\text{AGE}$$")
})

output$formulaB<-renderUI({
    if(input$modelB==TRUE)  withMathJax("$$\\text{SBP} = \\beta_{0} + \\beta_{1} \\text{AGE} + \\beta_{2} \\text{SOD}$$")
})

output$formulaC<-renderUI({
    if(input$modelC==TRUE)  withMathJax("$$\\text{SBP} = \\beta_{0} + \\beta_{1} \\text{AGE} + \\beta_{2} \\text{SOD} + \\beta_{3} \\text{PRO}$$")
})



#Data generation
ObsData <- reactive({set.seed(777)
                     generateData(input$beta1,input$beta2,n=1000)})

# Head from simulated data 
output$table_generateData<-renderTable(head(ObsData()))

# Linear models fits + graphs
fit1 <- reactive({lm(sbp ~ age, data = ObsData())})
fit2 <- reactive({lm(sbp ~ age + sodium, data = ObsData())})
fit3 <- reactive({lm(sbp ~ age + proteinurie + sodium, data = ObsData())})

grafico1 <- reactive({visreg(fit1(), points=list(cex = 1.5, pch = 1), jitter = 10, bty = "n")})
grafico2 <- reactive({visreg(fit2(), points=list(cex = 1.5, pch = 1), jitter = 10, bty = "n")})
grafico3 <- reactive({visreg(fit3(), points=list(cex = 1.5, pch = 1), jitter = 10, bty = "n")})

# Figures
output$gA1<-renderPlot({
  if(input$modelA==TRUE) plot(grafico1(),gg=TRUE,ylab="SBP (mmHg)",xlab="Age (years)",
                              points=list(size = 2, pch = 1, alpha = 0.4,col = "snow3"), line=list(col = "blue",size = 1.3)) + theme_classic()
})

output$gB1<-renderPlot({
  if(input$modelB==TRUE) plot(grafico2()[[1]],gg=TRUE,ylab="SBP (mmHg)",xlab="Age (years)",
                              points=list(size=2, pch=1,alpha=0.4,col="snow3"), line=list(col="blue",size=1.3)) + theme_classic()
})

# Positive slope -> blue; negative -> red
output$gC1<-renderPlot({
        if(input$modelC==TRUE)
          if(fit3()$coefficients["age"]>0) plot(grafico3()[[1]], gg = TRUE, ylab = "SBP (mmHg)", xlab = "Age (years)",
                                                points = list(size = 2, pch = 1, alpha = 0.4, col = "snow3"), line = list(col = "blue",size=1.3)) + theme_classic()
          else 
            plot(grafico3()[[1]], gg = TRUE, ylab = "SBP (mmHg)", xlab = "Age (years)",
                 points = list(size = 2, pch = 1, alpha = 0.4, col = "snow3"), line = list(col = "red", size = 1.3)) + theme_classic()
})

# Coefficients
output$coefA<-renderUI({
  if(input$modelA==TRUE)  withMathJax(sprintf("$$ \\beta_{1} = %.03f$$", fit1()$coefficients["age"]))
})

output$coefB<-renderUI({
  if(input$modelB==TRUE)  withMathJax(sprintf("$$ \\beta_{1} = %.03f$$", fit2()$coefficients["age"]))
})

output$coefC<-renderUI({
  if(input$modelC==TRUE)  withMathJax(sprintf("$$ \\beta_{1} = %.03f$$", fit3()$coefficients["age"]))
})

# Figures for model 3 in data generation tab
output$data.generation.plot1 <- renderPlot({
  plot(grafico3()[[1]], gg = TRUE, ylab = "SBP (mmHg)",xlab = "Age (years)",
       line=list(col = "darkmagenta", size = 1.3),points = list(size = 2, pch = 1, alpha = 0.4,col = "snow3"))+ theme_classic()
})

output$data.generation.plot2 <- renderPlot({
  plot(grafico3()[[2]],gg = TRUE,ylab = "SBP (mmHg)", xlab = "Proteinurie (mg)",
       points=list(size = 2, pch = 1,alpha = 0.4, col = "snow3"), line = list(col = "darkmagenta", size = 1.3)) + theme_classic()
})

output$data.generation.plot3 <- renderPlot({
  plot(grafico3()[[3]], gg = TRUE, ylab ="SBP (mmHg)",xlab = "Sodium (g)",
       points=list(size = 2, pch = 1, alpha = 0.4,col = "snow3"), line=list(col = "darkmagenta", size = 1.3)) + theme_classic()
})

# Download data
output$download_data <- downloadHandler(
          filename = function() {
            paste0("data.csv")
          },
          content = function(file) { 
            write_csv(ObsData(), file) 
          }
    )

# Article
output$article <- renderUI({
    tags$iframe(style = "height:700px; width:100%", src = "Paper_Collider.pdf")
})

}

# Building the shiny application
shinyApp(ui = ui, server = server)