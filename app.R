## app.R ##
library(shinydashboard)
rm(list = ls())
library(shiny)
library (reticulate )
library(dplyr)
library(class)
library(caret)
library(shiny)
library(Metrics)
library(scales)
library(RColorBrewer)
library(grid)
library(DataExplorer)
library(corrplot)
library(labelled)
library(ROSE)
library(e1071)
library(nnet)
library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
source("bloomfilter.R")

#install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit"))
#devtools::install_github("rstudio/shinydashboard")
#devtools::install_github("jcheng5/bubbles")
#devtools::install_github("hadley/shinySignals")
#-----------------------------------------------------------------------------------------------

ui <- fluidPage(
    
    titlePanel("MINI PROJET TITANIC"),
    
    sidebarLayout(
        
        sidebarPanel(
          
            fileInput("file1", "Choose input data"),
            uiOutput("category1"),          
            uiOutput("category2"),
            uiOutput("balanceType"),
            checkboxInput("scale","scaled data",FALSE),
            
            sliderInput("k",
                        "number of neighbors (K of KNN)",
                        min = 1,
                        max = 20,
                        value = 5),
            
            fluidRow(
                column(12, 
                       htmlOutput(outputId = "tp")  
                )
            ),
            
            
            
        ),
        mainPanel(
            fluidRow(
                column(12, 
                       mainPanel(
                           tabsetPanel(
                               id = 'dataset',
                               tabPanel("Car details", 
                                        DT::dataTableOutput("mytable3")
                               ),
                               tabPanel("Summary", 
                                        verbatimTextOutput("summary")
                                        
                               ),
                               tabPanel("BoxPlot",
                                        plotOutput(outputId = "boxplot"),
                                        plotOutput(outputId = "boxplotSpecies")
                               ),
                               tabPanel("Pie", 
                                        fluidRow(
                                            column(12, 
                                                   plotOutput(outputId = "Pie")
                                            )
                                        )
                                        
                               ),
                               tabPanel("Hostogram", 
                                        fluidRow(
                                            column(12, 
                                                   plotOutput(outputId = "HistogramPW"),
                                                   sliderInput("bins",
                                                               "Number of bins:",
                                                               min = 1,
                                                               max = 100,
                                                               value = 5)
                                            )
                                        )
                                        
                               ),
                        
                               tabPanel("Coorelation", 
                                        
                                        plotOutput(outputId = "Coorelation")
                                        
                               ),
                               tabPanel("Variables distribution in Transmission attrition", 

                                        fluidRow(
                                            column(12,
                                                   plotOutput(outputId = "barplotBi")
                                            ),
                                        fluidRow(      
                                            column(12,
                                                   plotOutput(outputId = "barplotProfils")
                                                  
                                                   
                                                )
                                            )
                                        )
                                        
                               ),
                               tabPanel("Nuage",
                                        
                                        plotOutput("ScatterPlot")
                                        
                                        ),
                               tabPanel("KNN", 
                                        h2("KNN With original data :"),
                                        dataTableOutput('confusionMatrix'),
                                        verbatimTextOutput("value"),
                                        
                               ),
                               tabPanel("LR",
                                        verbatimTextOutput("LR"),
                                        verbatimTextOutput("LRBalanced"),
                               ),
                               tabPanel("SVM",
                                        verbatimTextOutput("SVM"),
                               ),
                               tabPanel("Réseaux de neurones",
                                        verbatimTextOutput("ANN"),
                               )
                               
                           )
                       )
                )
            )
            
        ))
)
ui1 <- dashboardPage(
  dashboardHeader(title = "MINI PROJET TITANIC"),
  dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Choose input data", tabName = "rawdata")
    ),
    fileInput("file1", "Choose input data"),
    uiOutput("category1"),          
    uiOutput("category2"),
    uiOutput("balanceType"),
    checkboxInput("scale","scaled data",FALSE)
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Popularity by package (last 5 min)",
                  bubblesOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 4, status = "info",
                  title = "Top packages (last 5 min)",
                  tableOutput("packageTable")
                )
              )
      ),
      tabItem("rawdata",
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Data Loading :",
                HTML(
                  paste("<h4>Upload you CSV file </h4>"),
                ),
                downloadButton("file1", "Download as CSV")
        
      )
    )
  )
))
#------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
    
    options(shiny.maxRequestSize=30*1024^2)
    #----------------------iNNONCE DU PROJET------------------------------------------
    
    output$tp<- renderUI({
        HTML(
            paste("<h2>MiniProjet: Car details </h2>"),
            paste("<h3>Objectif: </h3>"),
            paste("<h4> Interface Shiny / Dashboard pour l’exploration de donn´ees.</h4>" ),
            
        )
    })
    
    myData <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        
        data <- read.csv(inFile$datapath, header = TRUE,sep = ";",stringsAsFactors=T)
    })
    
    
    output$category1 <- renderUI({
        selectizeInput('cat1', 'Choose one variable', choices = c("All",sort(as.character(unique(names(myData()))))),selected = "age")
    })
    output$category2 <- renderUI({
        selectizeInput('cat2', 'Choose the seconde variable', choices = c("All",sort(as.character(unique(names(myData()))))),selected = "age")
    })
    
    #----------------------DATASET------------------------------------------
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(myData())
        
    })
    #----------------------BOXPLOT------------------------------------------
    output$boxplot <- renderPlot({
        boxplot(myData()[input$cat1],
                at = c(1),
                names = c("Petal.W"),
                col = c("orange"),
                main = input$cat1,
                xlab = input$cat1,las = 1,
                border = "brown",
                notch = TRUE,
                horizontal = TRUE
        )
    })
    #----------------------Histogram------------------------------------------
    
    output$HistogramPW <- renderPlot({
        
        pw = myData()[[input$cat1]]
        
        bins <- seq(min(pw), max(pw), length.out = input$bins + 1)
        hist(pw,
             breaks = bins,
             col = "orange",
             main = input$cat1,
             xlab = input$cat1,
             ylab = "Number ")
        
    })
    #----------------------Summary------------------------------------------
    output$summary <- renderPrint({
        dataset <- myData()
        summary(dataset)
    })
    
    #-------------------------Pie--------------------------------------
    output$Pie <- renderPlot({
        pie(table(myData()[input$cat1]), labels = names(table(myData()[input$cat1])), 
            main = input$cat1, col=c())    
    })
    
    #-----------------------NUAGE---------------------------------------
    library(ggplot2)
    
    output$Nuage <- renderPlot({
        # Basic scatter plot
        p <- ggplot(myData(), aes(x=myData()[[input$cat1]], y=myData()[[input$cat2]])) + geom_point()
        p + labs(x = input$cat1,y = input$cat2)
        
    })
    
    
    #-----------------------CORRELATION--------------------------------------------
    output$Coorelation <- renderPlot({
        plot_correlation(myData())
        
    })
    #---------------------- Barplot---------------------------------------
    #----------------------Variable ciblé(age)------------------------------------------
    output$barplotBi <- renderPlot({
        # Diagramme en barres entre les variables 'Level' et 'Sex
        p <- ggplot(myData(), aes(x = myData()[[input$cat1]], fill = Survived )) + geom_bar()

        p + labs(x = input$cat1)
    })
    
    output$ScatterPlot <- renderPlot({
        ggplot(myData(), aes_string(x = myData()[[input$cat2]], 
                                    y = myData()[[input$cat1]], 
                                    color = "factor(input$cat2)")) + 
            
            geom_point(size = 2, position = position_jitter(w = 0.1, h = 0.1)) + 
            labs(x = input$cat2,
                 y = input$cat1) +
            fte_theme() + 
            scale_color_manual(name = "--------",values=c("#7A99AC", "#E4002B")) 
    })          
    #----------------------Histogram------------------------------------------
    output$barplotProfils <- renderPlot({
        # Diagramme de profils entre les variables 'Level' et 'Sex'
        p <- ggplot(myData(), aes(x = myData()[[input$cat1]], fill = Survived )) + geom_bar(position = "fill")
        p + labs(x = input$cat1)
         
    })
    
    #----------------------------------LOGISTIC REGRESSION--------------------------------
    output$LR <- renderPrint({
      #li=c(1,11,12,13,14,16,17,18,19,20)
      dataset <- myData()
      dataset <- na.omit(dataset)
      dataset[] <- lapply(dataset, function(x) as.numeric(x))
      #logreg <-glm(as.formula(paste(dataset[,-21], collapse = " ")),family=binomial(),data=dataset)
      dataset$Survived <- factor(dataset$Survived)
      glm.fit <- glm(dataset$Survived ~ .-Survived, data = dataset, family = "binomial")
      print("---------------LOGISTIC REGRESSION SUMMARY----------------")
      print(summary(glm.fit))
      glm.probs <- predict(glm.fit,type = "response")
      glm.pred <- ifelse(glm.probs > 0.5, "2", "1")
      print("-----------------------CONFUSION MATRIX------------------------")
      print(table(glm.pred,dataset$Survived))
      plot(table(glm.pred,dataset$Survived))
      
      print("-----------------------SCORE------------------------")
      mean(glm.pred == dataset$Survived)
    })
    
   
    #----------------------SVM--------------------------------------------
      output$SVM <- renderPrint({
      #dat = data.frame(x, y = as.factor(y))
      dataset <<- myData()
      dat = dataset
      x <- dataset[,-2]
      training.index <- caret::createDataPartition(dataset$Survived, p = .8,list = F)
      train.X <- x[training.index,]
      test.X  <- x[-training.index,]
      train.Y <- dataset$Survived[training.index]
      test.Y  <- dataset$Survived[-training.index]
      svmfit = svm(train.Y ~ ., data = train.X, kernel = "linear", cost = 10, scale = FALSE)
      pred <- predict(svmfit,test.X)
      print(summary(svmfit))
      print(svmfit)
      table(pred, test.Y)
      #plot(svmfit, dat, Sex ~ Pclass)
      
      
    })
    #----------------------Réseaux de neurones----------------------------
    
    output$ANN <- renderPrint({
    dataset <<- myData()
    x <- dataset[,-2]
    y <- sapply(dataset$Survived, unclass)
    y <- y-1
    model <- nnet(x,y, size = 10)
    pred <- predict(model,x)
    print("*******************************************************")
    print(pred[1:10])
    print("*******************************************************")
    print(y[1:10])
    print("*******************************************************")
    pred <- ifelse(pred > 0.5, "1", "0")
    #matrice de confusion 
    mc <- table(pred,y) 
    print("Matrice de confusion") 
    print(mc) 
    
    
    #posLabel=1
    #taux d'erreur 
    err <- 1-sum(diag(mc))/sum(mc) 
    print(paste("Taux d'erreur =", err)) 
    posLabel=1
    #rappel 
    recall <- mc[posLabel,posLabel]/sum(mc[posLabel,]) 
    print(paste("Rappel =", round(recall,3))) 
    
    #precision 
    precision <- mc[posLabel,posLabel]/sum(mc[,posLabel]) 
    print(paste("Precision =",round(precision,3))) 

      
    })
    
    #----------------------KNN--------------------------------------------
    
    output$confusionMatrix <- renderDataTable({
        
        data= myData()
        data <- na.omit(data)
        data[] <- lapply(data, function(x) as.numeric(x))
        standardized.X <- data[,-2]
        if(input$scale == TRUE){standardized.X <- scale(standardized.X)}
        y <- sapply(data$Survived, unclass)           
        # Convert categorical variables
        
        set.seed(55)
        # create training and test sets
        training.index <- caret::createDataPartition(y, p = .8,list = F)
        train.X <- standardized.X[training.index,]
        test.X  <- standardized.X[-training.index,]
        train.Y <- y[training.index]
        test.Y <- y[-training.index]
        set.seed(1)
        knn.pred <- knn(data.frame(train.X[,]),data.frame(test.X[,]),train.Y, k = input$k)
        # modify this to show title - confusion matrix
        # /false positive/positive false negative/negative
        true.positive    <- sum(knn.pred == "2" & test.Y == "2")
        false.positive   <- sum(knn.pred == "1" & test.Y == "2")
        true.negative    <- sum(knn.pred == "1" & test.Y == "1")
        false.negative   <- sum(knn.pred == "2" & test.Y == "1")
        row.names <- c("Pre-FALSE", "Pre-TRUE" )
        col.names <- c("Ref-FALSE", "Ref-TRUE","Accuracy")
        accuracy <- (true.positive+true.negative)/(true.positive+true.negative+false.negative+false.positive)
        cbind(Outcome = row.names, as.data.frame(matrix( 
            c(true.negative, false.negative, false.positive, true.positive,accuracy,accuracy) ,
            nrow = 2, ncol = 3, dimnames = list(row.names, col.names))))
    }, options = table.settings
    )

    
    table.settings <- list(searching = F, pageLength = 5, bLengthChange = F,
                           bPaginate = F, bInfo = F )
    
    
}

#----------------------------------THEME--------------------------------------
fte_theme <- function() {
    
    # Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Greys", n=9)
    color.background = palette[2]
    color.grid.major = palette[3]
    color.axis.text = palette[9]
    color.axis.title = palette[9]
    color.title = palette[9]
    text.size <- 14
    
    # Begin construction of chart
    theme_bw(base_size=9) +
        
        # Set the entire chart region to a light gray color
        theme(panel.background=element_rect(fill=color.background, color=color.background)) +
        theme(plot.background=element_rect(fill=color.background, color=color.background)) +
        theme(panel.border=element_rect(color=color.background)) +
        
        # Format the grid
        theme(panel.grid.major=element_line(color=color.grid.major,size=.50)) +
        theme(panel.grid.minor=element_blank()) +
        theme(axis.ticks=element_blank()) +
        
        # Format the legend, but hide by default
        theme(legend.background = element_rect(fill=color.background)) +
        theme(legend.text = element_text(size=text.size,color=color.axis.title)) +
        theme(legend.title = element_text(size=text.size,color=color.axis.title)) +
        theme(legend.position = "bottom") +
        theme(legend.direction = "vertical") +
        # Set title and axis labels, and format these and tick marks
        theme(plot.title=element_text(color=color.title, size=text.size, vjust=1.25)) +
        theme(axis.text.x=element_text(size=text.size,color=color.axis.text)) +
        theme(axis.text.y=element_text(size=text.size,color=color.axis.text)) +
        theme(axis.title.x=element_text(size=text.size,color=color.axis.title, vjust=0)) +
        theme(axis.title.y=element_text(size=text.size,color=color.axis.title, vjust=1.25)) +
        
        # Plot margins
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
shinyApp(ui1, server)


