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
library(shinySignals)    #install.packages("devtools") and then devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles") 
source("bloomfilter.R")
library(shinythemes)
library(fresh)
library(plotly)
library(shiny)
library(htmlwidgets)
library(ggplot2)

# Create the theme
mytheme <- create_theme(
  # adminlte_color(
  #   light_blue = "#434C5E"
  # ),
  adminlte_sidebar(
    dark_bg = "#042e29",
    dark_hover_bg = "#095e54",
    dark_color = "#FFF"
  ),
  adminlte_global(
    content_bg = "#095e54",
    # box_bg = "#D8DEE9", 
    # info_box_bg = "#D8DEE9"
  )
)

#install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit"))
#devtools::install_github("rstudio/shinydashboard")
#devtools::install_github("jcheng5/bubbles")
#devtools::install_github("hadley/shinySignals")
#-----------------------------------------------------------------------------------------------
Lr_accuracy <<- 1
Knn_accuracy <<- 1
Svm_accuracy <<- 1
NN_accuracy <<- 1


# Plotly on hover event ----
addHoverBehavior <- c(
  "function(el, x){",
  "  el.on('plotly_hover', function(data) {",
  "    if(data.points.length==1){",
  "      $('.hovertext').hide();",
  "      Shiny.setInputValue('hovering', true);",
  "      var d = data.points[0];",
  "      Shiny.setInputValue('left_px', d.xaxis.d2p(d.x) + d.xaxis._offset);",
  "      Shiny.setInputValue('top_px', d.yaxis.l2p(d.y) + d.yaxis._offset);",
  "      Shiny.setInputValue('dy', d.y);",
  "      Shiny.setInputValue('dtext', d.text);",
  "    }",
  "  });",
  "  el.on('plotly_unhover', function(data) {",
  "    Shiny.setInputValue('hovering', false);",
  "  });",
  "}")


ui1 <- dashboardPage(skin = "green",
  dashboardHeader(title = "MINI PROJET TITANIC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("",badgeLabel = "load data", badgeColor = "blue"),
      fileInput("file1", "Choose input data"),
      menuItem("Data Visualization", tabName = "rawdata", icon = icon("eye"), badgeColor = "blue"),
      menuItem("Analyse Unidimensionnelle", tabName = "Analyse_Unidimensionnelle", icon = icon("dashboard")),
      menuItem("Analyse Bidimensionnelle", tabName = "Analyse_Bidimensionnelle", icon = icon("dashboard")),
      menuItem("Clustering", tabName = "Clustering", icon = icon("dashboard"))


      #ggplot
      
    )
  ),
  dashboardBody(
    ### changing theme
    # shinyDashboardThemes(
    #   theme = "blue_gradient"
    # ),
    use_theme(mytheme), # <-- use the theme
    
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              fluidRow(
                box(
                  width = 8, status = "success", solidHeader = TRUE,
                  title = "Popularity by package (last 5 min)",
                  bubblesOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 4, status = "success",
                  title = "Top packages (last 5 min)",
                  tableOutput("packageTable")
                )
              )
              ),
      tabItem("rawdata",
              box(
                width = 12, status = "success", solidHeader = TRUE,
                title = "Data Visualization :",
                
              tabsetPanel(
                tabPanel("Visualization", 
              box(
                width = 12, status = "success", solidHeader = TRUE,
                title = "Visualization :",
                DT::dataTableOutput("mytable3")   
              )
              ),tabPanel("Summary", 
              box(
                title = "Summary :",
                width = 12, status = "success", solidHeader = TRUE,
                verbatimTextOutput("summary")
              )
              )
              )
              )
              
      ),
      tabItem("Analyse_Unidimensionnelle",
              
              box(
                width = 12, status = "success", solidHeader = TRUE,
                title = "Choose Your Variable :",
                uiOutput("category1")
              ),
              box(
                width = 6, status = "success", solidHeader = TRUE,
                title = "Box Plot :",
                plotOutput(outputId = "boxplot")
              ),
              box(
                width = 6, status = "success", solidHeader = TRUE,
                title = "Box V2 :",                      
                tags$head(
                        # style for the tooltip with an arrow (http://www.cssarrowplease.com/)
                        tags$style("
               .arrow_box {
                    position: absolute;
                  pointer-events: none;
                  z-index: 100;
                  white-space: nowrap;
                  background: CornflowerBlue;
                  color: white;
                  font-size: 13px;
                  border: 1px solid;
                  border-color: CornflowerBlue;
                  border-radius: 1px;
               }
               .arrow_box:after, .arrow_box:before {
                  right: 100%;
                  top: 50%;
                  border: solid transparent;
                  content: ' ';
                  height: 0;
                  width: 0;
                  position: absolute;
                  pointer-events: none;
               }
               .arrow_box:after {
                  border-color: rgba(136,183,213,0);
                  border-right-color: CornflowerBlue;
                  border-width: 4px;
                  margin-top: -4px;
               }
               .arrow_box:before {
                  border-color: rgba(194,225,245,0);
                  border-right-color: CornflowerBlue;
                  border-width: 10px;
                  margin-top: -10px;
               }")
                      ),
                      div(
                        style = "position:relative",
                        plotlyOutput("myplot"),
                        uiOutput("hover_info")
                      )
              ),
              box(
                width = 6, status = "success", solidHeader = TRUE,
                title = "Pie :",
                plotOutput(outputId = "Pie")
              ),
              box(
                width = 6, status = "success", solidHeader = TRUE,
                title = "Histogram :",
                plotOutput(outputId = "HistogramPW"),
               
                  sliderInput("bins",
                              "Number of bins:",
                              min = 1,
                              max = 100,
                              value = 5),
                
              )
      ),
      tabItem("Analyse_Bidimensionnelle",
              box(
                width = 6, status = "success", solidHeader = TRUE,color="red",
                title = "Nuage :",
                plotOutput("ScatterPlot"),
                box(
                  width = 6, status = "success", solidHeader = TRUE,
                  title = "Choose first Variable :",
                  uiOutput("category2")
                ),
                box(
                  width = 6, status = "success", solidHeader = TRUE,
                  title = "Choose second Variable :",
                  uiOutput("category3")
                )
                
              ),
             
              box(
                width = 6, status = "success", solidHeader = TRUE,
                title = "Coorelation :",
                plotOutput(outputId = "Coorelation")
                
              )
              
      ),
      tabItem("Clustering",
              box(
                width = 12, status = "success", solidHeader = TRUE,
                title = "Clustering :",
              
              tabsetPanel(
                tabPanel("KNN", 
                
              
                box(width = 7, status = "success", solidHeader = TRUE,
                    dataTableOutput('confusionMatrix'),
                    ),
                
                box(width = 5, status = "success", solidHeader = TRUE,
                    sliderInput("k",
                                "Number of neighbors",
                                min = 1,
                                max = 20,
                                value = 5),
                    valueBoxOutput("KNNAccuracy"))
                
              ),tabPanel("LR", 

                    box(width = 7, status = "success", solidHeader = TRUE,dataTableOutput('LR')),
                    box(width = 5, status = "success", solidHeader = TRUE,valueBoxOutput("lrAccuracy"))
                  )

              ,tabPanel("SVM", 

             
                   box(width = 7, status = "success", solidHeader = TRUE,dataTableOutput('SVM')),
                   box(width = 5, status = "success", solidHeader = TRUE,uiOutput("kernel"),valueBoxOutput("SvmAccuracy"))
                 )
              ,tabPanel("ANN", 

               
                  box(width = 7, status = "success", solidHeader = TRUE, dataTableOutput('ANN')),
                  box(width = 5, status = "success", solidHeader = TRUE, 
                      sliderInput("size",
                                  "Size of neurons",
                                  min = 1,
                                  max = 100,
                                  value = 10),
                      valueBoxOutput("AnnAccuracy"))
                   )

      )
     
      ))
      
      
      )
    )
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
        selectizeInput('cat1', 'Choose one variable', choices = c("Age",
                                                                            "Sex",
                                                                            "Survived",
                                                                            "Fare",
                                                                            "Embarked", 
                                                                            "Pclass",
                                                                            "Title",
                                                                            "IsAlone"),selected = "Age")
    })
    output$category2 <- renderUI({
        selectizeInput('cat2', 'Choose one variable', choices  = c("Age",
                                                                            "Sex",
                                                                            "Survived",
                                                                            "Fare",
                                                                            "Embarked", 
                                                                            "Pclass",
                                                                            "Title",
                                                                            "IsAlone"),selected = "Age")
    })
    output$category3 <- renderUI({
      selectizeInput('cat3', 'Choose one variable', choices = c("Age",
                                                                          "Sex",
                                                                          "Survived",
                                                                          "Fare",
                                                                          "Embarked", 
                                                                          "Pclass",
                                                                          "Title",
                                                                          "IsAlone"),selected = "Age")
    })
    
    output$size <- renderUI({
      selectizeInput('size', 'Choose the size for the ANN :', choices = c("linear", "polynomial", "radial"),selected = "linear")
    })    
    output$kernel <- renderUI({
      selectizeInput('kernel', 'Choose the kernel for the SVM :', choices = c("linear", "polynomial", "radial"),selected = "linear")
    })
    #----------------------DATASET------------------------------------------
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(myData(),
                      options = list(pageLength = 6, autoWidth = TRUE)
        )
      
        
    })
    #----------------------BOXPLOT------------------------------------------
    
    output$boxplot <- renderPlot({
      pw = myData()[[input$cat1]]
      
      if(class(pw) != "integer") {
        pw <- unclass(pw)
      }
       
      boxplot(pw,
                at = c(1),
                names = c("Petal.W"),
                col = c("#3CB371"),
                main = input$cat1,
                xlab = input$cat1,las = 1,
                border = "black",
                notch = TRUE,
                horizontal = TRUE
      )
    })
    
    #----------------------hisogram-------------------------
    
    output$HistogramPW <- renderPlot({
      pw = myData()[[input$cat1]]
      if(class(pw) != "integer" & class(pw) != "numeric") { 
        barplot(table(pw) 
                )}
      
      else {
      
      bins <- seq(min(pw), max(pw), length.out = input$bins + 1)
      hist(pw,
           breaks = bins,
           col = "#3CB371",
           main = input$cat1,
           xlab = input$cat1,
           ylab = "Number ")
      }
      
    })
    #----------------------Summary------------------------------------------
    output$summary <- renderPrint({
        dataset <- myData()
        summary(dataset)
    })
    
    #-------------------------Pie--------------------------------------
    output$Pie <- renderPlot({
      pw = table(myData()[[input$cat1]])
      df<-as.data.frame(pw) 
      data <- data.frame(
        class = df[["Var1"]],
        value = df[["Freq"]]
        
      )
      data <- data %>% 
        arrange(desc(class)) %>%
        mutate(prop = value / sum(data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop )
      
      # Basic piechart
      ggplot(data, aes(x="", y=prop, fill=class)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() + 
        theme(legend.position="none") +
        
        geom_text(aes(y = ypos, label = class), color = "white", size=6) +
        scale_fill_brewer(palette="Set1")
     #pie(table(myData()[input$cat1]), labels = names(table(myData()[input$cat1])), 
      #   main = input$cat1, col=c())    
    })
    
    #-----------------------NUAGE---------------------------------------
   # library(ggplot2)
    
    #output$Nuage <- renderPlot({
        # Basic scatter plot
     #   p <- ggplot(myData(), aes(x=myData()[[input$cat2]], y=myData()[[input$cat3]])) + geom_point()
      #  p + labs(x = input$cat2,y = input$cat3)
        
    #})
    
    
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
                                    y = myData()[[input$cat3]], 
                                    color = "factor(input$cat2)")) + 
            
            geom_point(size = 2, position = position_jitter(w = 0.1, h = 0.1)) + 
            labs(x = input$cat2,
                 y = input$cat3) +
            fte_theme() + 
            scale_color_manual(name = "--------",values=c("#7A99AC", "#E4002B")) 
    })          
    #----------------------Histogram------------------------------------------
    output$barplotProfils <- renderPlot({
        # Diagramme de profils entre les variables 'Level' et 'Sex'
        p <- ggplot(myData(), aes(x = myData()[[input$cat1]], fill = Survived )) + geom_bar(position = "fill")
        p + labs(x = input$cat1)
         
    })
    #-------------------------------------------------------------------------------------
    #----------------------------------BoxPlot v2-----------------------------------------
    output$myplot <- renderPlotly({
      dataset <- myData()
      
      #xx <- dataset[,-2]
      #yy <- sapply(data$Survived, unclass) 
      plot_ly(dataset, 
              type = "box", 
              x=input$cat1,y = dataset[[input$cat1]], 
              #text = paste0("<b> group: </b>", dataset$Age, "<br/>",
              #"<b> sample: </b>", dataset$Survived, "<br/>"),
              hoverinfo = "y") %>%
        onRender(addHoverBehavior)
    })
    output$hover_info <- renderUI({
      if(isTRUE(input[["hovering"]])){
        style <- paste0("left: ", input[["left_px"]] + 4 + 5, "px;", # 4 = border-width after
                        "top: ", input[["top_px"]] - 24 - 2 - 1, "px;") # 24 = line-height/2 * number of lines; 2 = padding; 1 = border thickness
        div(
          class = "arrow_box", style = style,
          p(HTML(input$dtext, 
                 "<b> value: </b>", formatC(input$dy)), 
            style="margin: 0; padding: 2px; line-height: 16px;")
        )
      }
    })
    
    #----------------------------------LOGISTIC REGRESSION--------------------------------
    output$LR <- renderDataTable({
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
      #print("-----------------------CONFUSION MATRIX------------------------")
      #print(table(glm.pred,dataset$Survived))
      #plot(table(glm.pred,dataset$Survived))
      tb <- table(glm.pred,dataset$Survived)
      true_positive    <- tb[2,2]
      false_positive   <- tb[2,1]
      true_negative    <- tb[1,1]
      false_negative   <- tb[1,2]
      
      row.names <- c("Pre-FALSE", "Pre-TRUE" )
      col.names <- c("Ref-FALSE", "Ref-TRUE")
      Lr_accuracy <<- (true_positive+true_negative)/(true_positive+true_negative+false_negative+false_positive)
      cbind(Outcome = row.names, as.data.frame(matrix( 
        c(true_negative, false_negative, false_positive, true_positive) ,
        nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
    }, options = table.settings)

    output$lrAccuracy <- renderValueBox({
      # The downloadRate is the number of rows in pkgData since
      # either startTime or maxAgeSecs ago, whichever is later.
      
      valueBox(
        value = formatC(Lr_accuracy, digits = 4, format = "f"),
        subtitle = "Accuracy",
        icon = icon("area-chart"),
        color = "aqua"
      )
    })
    
   
    #----------------------SVM--------------------------------------------
    output$SVM <- renderDataTable({
      #dat = data.frame(x, y = as.factor(y))
      dataset <<- myData()
      dat = dataset
      x <- dataset[,-2]
      training.index <- caret::createDataPartition(dataset$Survived, p = .8,list = F)
      train.X <- x[training.index,]
      test.X  <- x[-training.index,]
      train.Y <- dataset$Survived[training.index]
      test.Y  <- dataset$Survived[-training.index]
      svmfit = svm(train.Y ~ ., data = train.X, kernel = input$kernel, cost = 10, scale = FALSE)
      pred <- predict(svmfit,test.X)
      print(summary(svmfit))
      print(svmfit)
      table(pred, test.Y)
      
      tb <- table(pred, test.Y)
      print(tb)
      print(tb["Survived","Died"])
      true_positive    <- tb[2,2]
      false_positive   <- tb[2,1]
      true_negative    <- tb[1,1]
      false_negative   <- tb[1,2]
      
      row.names <- c("Pre-FALSE", "Pre-TRUE" )
      col.names <- c("Ref-FALSE", "Ref-TRUE")
      Svm_accuracy <<- (true_positive+true_negative)/(true_positive+true_negative+false_negative+false_positive)
      cbind(Outcome = row.names, as.data.frame(matrix( 
        c(true_negative, false_negative, false_positive, true_positive) ,
        nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
    }, options = table.settings)
    
    
    output$SvmAccuracy <- renderValueBox({
      # The downloadRate is the number of rows in pkgData since
      # either startTime or maxAgeSecs ago, whichever is later.
      k <-input$kernel
      valueBox(
        value = formatC(Svm_accuracy, digits = 4, format = "f"),
        subtitle = "Accuracy",
        icon = icon("area-chart"),
        color = "aqua"
      )
    })
      
      
    
    #----------------------Réseaux de neurones----------------------------
    
    output$ANN <- renderDataTable({
    
    s <- input$size
    dataset <<- myData()
    x <- dataset[,-2]
    y <- sapply(dataset$Survived, unclass)
    y <- y-1
    model <- nnet(x,y, size = input$size)
    pred <- predict(model,x)
    
    pred <- ifelse(pred > 0.5, "1", "0")
    print("pred")
    print(pred)
    
    #matrice de confusion 
    tb <- table(pred,y) 
    print("Matrice de confusion") 
    print(tb)
    
    
    true_positive    <- tb[2,2]
    false_positive   <- tb[2,1]
    true_negative    <- tb[1,1]
    false_negative   <- tb[1,2]
    
    row.names <- c("Pre-FALSE", "Pre-TRUE" )
    col.names <- c("Ref-FALSE", "Ref-TRUE")
    Ann_accuracy <<- (true_positive+true_negative)/(true_positive+true_negative+false_negative+false_positive)
    
    
      err <- 1-sum(diag(tb))/sum(tb) 
      print(paste("Taux d'erreur =", err)) 
      posLabel=1
      #rappel 
      recall <- tb[posLabel,posLabel]/sum(tb[posLabel,]) 
      print(paste("Rappel =", round(recall,3))) 
      
      #precision 
      precision <- tb[posLabel,posLabel]/sum(tb[,posLabel]) 
      print(paste("Precision =",round(precision,3))) 
      cbind(Outcome = row.names, as.data.frame(matrix( 
        c(true_negative, false_negative, false_positive, true_positive) ,
        nrow = 2, ncol = 2, dimnames = list(row.names, col.names)))) 
    }, options = table.settings)

    output$AnnAccuracy <- renderValueBox({
      # The downloadRate is the number of rows in pkgData since
      # either startTime or maxAgeSecs ago, whichever is later.
      s <- input$size
      
      valueBox(
        value = formatC(Ann_accuracy, digits = 4, format = "f"),
        subtitle = "Accuracy",
        icon = icon("area-chart"),
        color = "aqua"
      )
    })
    #----------------------KNN--------------------------------------------
    
    output$confusionMatrix <- renderDataTable({
        
        data= myData()
        data <- na.omit(data)
        data[] <- lapply(data, function(x) as.numeric(x))
        standardized.X <- data[,-2]
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
        col.names <- c("Ref-FALSE", "Ref-TRUE")
        Knn_accuracy <<- (true.positive+true.negative)/(true.positive+true.negative+false.negative+false.positive)
        cbind(Outcome = row.names, as.data.frame(matrix( 
            c(true.negative, false.negative, false.positive, true.positive) ,
            nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
        
    }, options = table.settings
    )
    
    output$KNNAccuracy <- renderValueBox({
      # The downloadRate is the number of rows in pkgData since
      # either startTime or maxAgeSecs ago, whichever is later.
      a <- input$k
      valueBox(
        value = formatC(Knn_accuracy, digits = 4, format = "f"),
        subtitle = "Accuracy",
        icon = icon("area-chart"),
        color = "aqua"
      )
    })

    
    table.settings <- list(searching = F, pageLength = 5, bLengthChange = F,
                           bPaginate = F, bsuccess = F )

    
    
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


#______________________theme___________________________
# 
# create_theme(
#   theme = "default",
#   bs_vars_navbar(
#     default_bg = "#3f2d54",
#     default_color = "#FFFFFF",
#     default_link_color = "#FFFFFF",
#     default_link_active_color = "#FFFFFF"
#   ),
#   bs_vars_color(
#     gray_base = "#354e5c",
#     brand_primary = "#75b8d1",
#     brand_success = "#c9d175",
#     brand_info = "#758bd1",
#     brand_warning = "#d1ab75",
#     brand_danger = "#d175b8"
#   ),
#   bs_vars_state(
#     success_text = "#FFF",
#     success_bg = "#c9d175",
#     success_border = "#c9d175",
#     info_text = "#FFF",
#     info_bg = "#3f2d54",
#     info_border = "#3f2d54",
#     danger_text = "#FFF",
#     danger_bg = "#d175b8",
#     danger_border = "#d175b8"
#   ),
#   bs_vars_wells(
#     bg = "#FFF",
#     border = "#3f2d54"
#   ),
#   output_file = "www/mytheme.css"
# )
# 
# 
# mytheme <- create_theme(
#   bs4dash_vars(
#     navbar_light_color = "#bec5cb",
#     navbar_light_active_color = "#FFF",
#     navbar_light_hover_color = "#FFF"
#   ),
#   bs4dash_yiq(
#     contrasted_threshold = 10,
#     text_dark = "#FFF", 
#     text_light = "#272c30"
#   ),
#   bs4dash_layout(
#     main_bg = "#353c42"
#   ),
#   bs4dash_sidebar_light(
#     bg = "#272c30", 
#     color = "#bec5cb",
#     hover_color = "#FFF",
#     submenu_bg = "#272c30", 
#     submenu_color = "#FFF", 
#     submenu_hover_color = "#FFF"
#   ),
#   bs4dash_status(
#     primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
#   ),
#   bs4dash_color(
#     gray_900 = "#FFF"
#   )
# )

shinyApp(ui1, server)


