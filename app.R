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

ui <- fluidPage(
    
    titlePanel("MINI PROJET"),
    
    
    
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
                               tabPanel("Nuage", 
                                        fluidRow(
                                            column(12, 
                                                   plotOutput(outputId = "Nuage")
                                                   
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
                               tabPanel("Deux Variable Quantitatives",
                                        
                                        plotOutput("ScatterPlot")
                                        
                                        ),
                               tabPanel("KNN", 
                                        h2("KNN With original data :"),
                                        dataTableOutput('confusionMatrix'),
                                        verbatimTextOutput("value"),
                                        h2("KNN With balanced data :"),
                                        dataTableOutput('confusionMatrixbalanced'),
                                        verbatimTextOutput("valuebalanced")
                               ),
                               tabPanel("LR",
                                        verbatimTextOutput("LR"),
                                        verbatimTextOutput("LRBalanced"),
                               )
                               
                           )
                       )
                )
            )
            
        ))
)

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
        p <- ggplot(myData(), aes(x = myData()[[input$cat1]], fill = transmission )) + geom_bar()

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
        p <- ggplot(myData(), aes(x = myData()[[input$cat1]], fill = transmission )) + geom_bar(position = "fill")
        p + labs(x = input$cat1)
         
    })
    
    #----------------------------------LOGISTIC REGRESSION--------------------------------
    output$LR <- renderPrint({
        #li=c(1,11,12,13,14,16,17,18,19,20)
        dataset <- myData()
        dataset <- na.omit(dataset)
        dataset[] <- lapply(dataset, function(x) as.numeric(x))
        #logreg <-glm(as.formula(paste(dataset[,-21], collapse = " ")),family=binomial(),data=dataset)
        dataset$y <- factor(dataset$y)
        glm.fit <- glm(dataset$y ~ .-y, data = dataset, family = "binomial")
        print("---------------LOGISTIC REGRESSION SUMMARY----------------")
        print(summary(glm.fit))
        glm.probs <- predict(glm.fit,type = "response")
        glm.pred <- ifelse(glm.probs > 0.5, "2", "1")
        print("-----------------------CONFUSION MATRIX------------------------")
        print(table(glm.pred,dataset$y))
        print("-----------------------SCORE------------------------")
        mean(glm.pred == dataset$y)
    })
    #-----------------------------balance select type--------------------------------------
    output$balanceType <- renderUI({
        selectizeInput('type', 'Choose the type of Data balancing ', choices = c("Both","Under","Over"),selected = "Both")
    })
    
    #-------------------------LOGISTIC REGRESSION Balanced Data--------------------------------
    output$LRBalanced <- renderPrint({
        dataset <<- myData()
        
        if(isTRUE(input$type == "Both"))
        {
            dataset <<- myData()
            dataset <- na.omit(dataset)
            dataset <- ovun.sample(y ~ .-y, data = dataset, method ="both",p=0.5,N=nrow(dataset), seed =1)$data
        }
        else if(isTRUE(input$type == "Under"))
        {
            dataset <<- myData()
            dataset <- na.omit(dataset)
            dataset <- ovun.sample(y ~ .-y, data = dataset, method ="under",N=nrow(dataset)/4, seed =1)$data
        }
        else
        {
            dataset <<- myData()
            dataset <- na.omit(dataset)
            dataset <- ovun.sample(y ~ .-y, data = dataset, method ="over",N=nrow(dataset)*2, seed =1)$data
            
        }
        
        dataset <- na.omit(dataset)
        dataset[] <- lapply(dataset, function(x) as.numeric(x))
        #logreg <-glm(as.formula(paste(dataset[,-21], collapse = " ")),family=binomial(),data=dataset)
        dataset$y <- factor(dataset$y)
        glm.fit <- glm(dataset$y ~ .-y, data = dataset, family = "binomial")
        print("---------------LOGISTIC REGRESSION SUMMARY----------------")
        print(summary(glm.fit))
        glm.probs <- predict(glm.fit,type = "response")
        glm.pred <- ifelse(glm.probs > 0.5, "2", "1")
        print("---------------------CONFUSION MATRIX---------------------")
        print(table(glm.pred,dataset$y))
        print("-----------------------SCORE------------------------------")
        mean(glm.pred == dataset$y)
        
        
    })
    
    #----------------------KNN--------------------------------------------
    
    output$confusionMatrix <- renderDataTable({
        
        data= myData()
        data <- na.omit(data)
        data[] <- lapply(data, function(x) as.numeric(x))
        standardized.X <- data[,-21]
        if(input$scale == TRUE){standardized.X <- scale(standardized.X)}
        
        #l'application du scale sur des données non équilibrées améliore la prédiction de la classe dominante
        #contrairment à l'autre classe ce qui confirme la nécessité de l'équilibrage de données
        standardized.X <- na.omit(standardized.X)
        set.seed(55)
        # create training and test sets
        training.index <- caret::createDataPartition(data$y, p = .8,list = F)
        train.X <- standardized.X[training.index,]
        test.X  <- standardized.X[-training.index,]
        train.Y <- data$y[training.index]
        test.Y <- data$y[-training.index]
        set.seed(1)
        knn.pred <- knn(data.frame(train.X[,]),data.frame(test.X[,]),train.Y, k = input$k)
        # modify this to show title - confusion matrix
        # /false positive/positive false negative/negative
        true.positive    <- sum(knn.pred == "2" & test.Y == "2")
        false.positive   <- sum(knn.pred == "1" & test.Y == "2")
        true.negative    <- sum(knn.pred == "1" & test.Y == "1")
        false.negative   <- sum(knn.pred == "2" & test.Y == "1")
        row.names <- c("Pre-FALSE", "Pre-TRUE" )
        col.names <- c("Ref-FALSE", "Ref-TRUE")
        cbind(Outcome = row.names, as.data.frame(matrix( 
            c(true.negative, false.negative, false.positive, true.positive) ,
            nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
    }, options = table.settings
    )
    #-------------------------KNN Balanced -------------------------------------
    output$confusionMatrixbalanced <- renderDataTable({
        
        data= myData()
        data <- na.omit(data)
        if(isTRUE(input$type == "Both"))
            {
            data <- ovun.sample(y ~ .-y, data = data, method ="both",N=nrow(data), seed =1)$data
        }
        if(isTRUE(input$type == "Under")){
            data <- ovun.sample(y ~ .-y, data = data, method ="under",N=nrow(data)/4, seed =1)$data
        }else{
            data <- ovun.sample(y ~ .-y, data = data, method ="over",N=nrow(data)*2, seed =1)$data
            
        }
        data[] <- lapply(data, function(x) as.numeric(x))
        standardized.X <- data[,-21]
        if(input$scale == TRUE){standardized.X <- scale(standardized.X)}
        
        #l'application du scale sur des données non équilibrées améliore la prédiction de la classe dominante
        #contrairment à l'autre classe ce qui confirme la nécessité de l'équilibrage de données
        standardized.X <- na.omit(standardized.X)
        set.seed(55)
        # create training and test sets
        training.index <- caret::createDataPartition(data$y, p = .8,list = F)
        train.X <- standardized.X[training.index,]
        test.X  <- standardized.X[-training.index,]
        train.Y <- data$y[training.index]
        test.Y <- data$y[-training.index]
        set.seed(1)
        knn.pred <- knn(data.frame(train.X[,]),data.frame(test.X[,]),train.Y, k = input$k)
        # modify this to show title - confusion matrix
        # /false positive/positive false negative/negative
        true.positive    <- sum(knn.pred == "2" & test.Y == "2")
        false.positive   <- sum(knn.pred == "1" & test.Y == "2")
        true.negative    <- sum(knn.pred == "1" & test.Y == "1")
        false.negative   <- sum(knn.pred == "2" & test.Y == "1")
        row.names <- c("Pre-FALSE-Balanc", "Pre-TRUE-Balanc" )
        col.names <- c("Ref-FALSE-Balanc", "Ref-TRUE-Balanc")
        cbind(Outcome = row.names, as.data.frame(matrix( 
            c(true.negative, false.negative, false.positive, true.positive) ,
            nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
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
shinyApp(ui, server)


