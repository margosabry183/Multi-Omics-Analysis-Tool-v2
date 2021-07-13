##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# ui.R file                      #
##################################

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

library(shiny)
library(shinyWidgets)

library(tools)
library(ggplot2)
library(ggpubr)
library(MOFA2)


library(shiny)
###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="Multi-Omics Analysis Tool", titleWidth = 300),
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo.png' width = '120'></a>",
          "<br>",
          "<p style = 'text-align: center;'><small><a href='https://www.nps.gov/subjects/hfc/arrowhead-artwork.htm' target='_blank'></a></small></p>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        # menuItem("Parks Map", tabName = "map", icon = icon("thumbtack")),
        # menuItem("Machine Learning Model", tabName = "table", icon = icon("table")),
        menuItem("Machine Learning Model", tabName = "tree", icon = icon("table")),
        menuItem("Visualization", tabName = "charts", icon = icon("stats", lib = "glyphicon")),
        menuItem("MEFISTO Analysis", tabName = "charts2", icon = icon("tasks")),
        # menuItem("Species Choropleth Map", tabName = "choropleth", icon = icon("map marked alt")),
        # menuItem("Releases", tabName = "releases", icon = icon("tasks")),
        HTML(paste0(
          "<br><br><br><br><br><br><br><br><br>",
          "<table style='margin-left:auto; margin-right:auto;'>",
            "<tr>",
              "<td style='padding: 5px;'><a href='https://www.facebook.com/nationalparkservice' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.youtube.com/nationalparkservice' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.twitter.com/natlparkservice' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.instagram.com/nationalparkservice' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.flickr.com/nationalparkservice' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
            "</tr>",
          "</table>",
          "<br>"),
        HTML(paste0(
          "<script>",
            "var today = new Date();",
            "var yyyy = today.getFullYear();",
          "</script>",
          "<p style = 'text-align: center;'><small>&copy; - <a href='https://alessiobenedetti.com' target='_blank'>alessiobenedetti.com</a> - <script>document.write(yyyy);</script></small></p>")
        ))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "map",
        
          # parks map section
          leafletOutput("parksMap") %>% withSpinner(color = "green")
                
        ),
        
        tabItem(
          # species data section
          tabName = "table", dataTableOutput("speciesDataTable") %>% withSpinner(color = "green")
          
        ),
        
        tabItem(tabName = "tree",
              
          # collapsible species tree section
         
          # column(3, uiOutput("parkSelectComboTree")),
          # column(3, uiOutput("categorySelectComboTree")),
          # collapsibleTreeOutput('tree', height='700px') %>% withSpinner(color = "green")


           titlePanel("Machine Learning Model"),
          # a(href="model.hdf5", "Download Trained model", download=NA, target="_blank"),
          

            includeMarkdown("www/tree.md"),
    # Sidebar with main model parameters choices
     sidebarLayout(
         sidebarPanel(
    #         # Model file picker (HDF5 or RDS file)
             fileInput(inputId = "data_file",
                       label = "Data file (.csv or .txt):",
                       buttonLabel = "Data file",
                       accept = c("csv", "txt")
    #         uiOutput("viewsChoice"),
    #         uiOutput("groupsChoice"),
    #         uiOutput("factorsChoice"),
    #         uiOutput("colourChoice"),
    #         div(id="descriptionMofa", checked=NA
    #             # span("More information on MOFA+ is "),
    #             # a(href="https://github.com/bioFAM/MOFA2", " on GitHub", target="_blank"),
    #             # TODO: add link to the paper when available
    #             # br(),
    #             # span("Source code of this app is available "),
    #             # a(href="https://github.com/gtca/mofaplus-shiny", "here", target="_blank"),
             ),
             width = 3
   
     
         ),

    #     # Show a plot of the generated distribution
        mainPanel(

          tabsetPanel(
                tabPanel("Training model", 
                    p("", class="description"),
                    plotOutput("trainingData")
                ),
          tabsetPanel(
                   downloadButton("Download Trained model",
                    href="model.hdf5",
                    id = "tabs",
                    download=NA,
                    target="_blank")
          )
         
         
          )
          
          
        )

                
        
         
   
     )   
        ),

          
    
    
      
        tabItem(tabName = "charts",
          
          # ggplot2 species charts section
          # includeMarkdown("www/charts.md"),
          # fluidRow(column(3, uiOutput("categorySelectComboChart"))),
          # column(6, plotOutput("ggplot2Group1") %>% withSpinner(color = "green")),
          # column(6, plotOutput("ggplot2Group2") %>% withSpinner(color = "green"))

              # Application title
    titlePanel("Visualization"),

     includeMarkdown("www/charts.md"),
    

    # Sidebar with main model parameters choices
    sidebarLayout(
        sidebarPanel(
            # Model file picker (HDF5 or RDS file)
            fileInput(inputId = "model_file",
                      label = "Model file (.hdf5 or .rds):",
                      buttonLabel = "Model file",
                      accept = c("hdf5", "rds")),
            uiOutput("viewsChoice"),
            uiOutput("groupsChoice"),
            uiOutput("factorsChoice"),
            uiOutput("colourChoice"),
            div(id="descriptionMofa", checked=NA
                # span("More information on MOFA+ is "),
                # a(href="https://github.com/bioFAM/MOFA2", " on GitHub", target="_blank"),
                # TODO: add link to the paper when available
                # br(),
                # span("Source code of this app is available "),
                # a(href="https://github.com/gtca/mofaplus-shiny", "here", target="_blank"),
            ),
            width = 3
        ),
    

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Data overview", 
                    p("", class="description"),
                    plotOutput("dataOverviewPlot")
                ),
                tabPanel("Variance", 
                    p("", class="description"),
                    # TODO: selector x, y
                    # TODO: selector group_by
                    # TODO: embed multiple plots on one page
                    plotOutput("varianceExplainedPlot")
                ),
                tabPanel("Weights", 
                    p("Visualize factor weights as a first step to interpret factors.", class="description"),
                    fluidRow(
                       column(2, uiOutput("weightsViewSelection")),
                       column(3, sliderInput(inputId = "nfeatures_to_label",
                                             label = "Number of top features to label:",
                                             min = 0,
                                             max = 100,
                                             value = 10,
                                             step = 1)),
                       column(3, uiOutput("weightsFeatureSelection"))
                    ),
                    plotOutput("weightsPlot"),
                    p("Top features per factor in the current view are displayed below:", class="description"),
                    plotOutput("topWeightsPlot")
                ),
                tabPanel("Factor exploration",
                    p("Explore factors one by one by plotting original data values for their top weights.", class="description"),
                    fluidRow(
                       column(2, uiOutput("dataFactorSelection")),
                       column(2, uiOutput("dataViewSelection")),
                       column(3, sliderInput(inputId = "nfeatures_to_plot",
                                             label = "Number of top features to plot:",
                                             min = 0,
                                             max = 100,
                                             value = 10,
                                             step = 1)),
                       column(3, uiOutput("dataFeatureSelection"))
                    ),
                    plotOutput("dataHeatmapPlot"),
                    plotOutput("dataScatterPlot")
                ),
                tabPanel("Factors beeswarm", 
                    p("Visualize factor values and explore their distribution in different sets of samples", class="description"),
                    fluidRow(
                       column(2, uiOutput("factorsAxisChoice_x")),
                       column(2, switchInput(inputId = "factorsRotateLabelsX", label = "X&nbsp;axis&nbsp;labels&nbsp;90°", value = FALSE),
                                 style = "margin-top: 25px; margin-right: 5px;")
                    ),
                    fluidRow(
                       column(1, switchInput(inputId = "factorsAddDots", label = "Points", value = TRUE),
                                 style = "margin-top: 25px; margin-right: 25px;"),
                       column(2, sliderInput(inputId = 'factorsDotSize', label = 'Point size', value = 2, min = 1, max = 8, step = .5),
                                 style = "margin-right: 5px;"),
                       column(2, sliderInput(inputId = 'factorsDotAlpha', label = 'Point alpha', value = 1, min = .1, max = 1, step = .1),
                                 style = "margin-right: 5px;"),
                    
                       column(1, switchInput(inputId = "factorsAddViolins", label = "Violins", value = FALSE),
                                 style = "margin-top: 25px; margin-right: 25px;"),
                       column(2, sliderInput(inputId = 'factorsViolinAlpha', label = 'Violin alpha', value = 1, min = .1, max = 1, step = .1),
                                 style = "margin-right: 5px;")
                    ),
                    hr(),
                    plotOutput("factorsPlot")
                ),
                tabPanel("Factors scatter", 
                    p("Visualize pairs of factors to study how they separate different sets of samples.", class="description"),
                    fluidRow(
                       column(2, uiOutput("factorChoice_x")),
                       column(1, actionButton("swapEmbeddings", "", 
                                              icon("exchange-alt"),
                                              style = "margin: 25px auto; display: flex;")),
                       column(2, uiOutput("factorChoice_y")),
                       column(2, sliderInput(inputId = 'factorDotSize', label = 'Point size', value = 2, min = 1, max = 8, step = .5)),
                       column(2, sliderInput(inputId = 'factorDotAlpha', label = 'Point alpha', value = 1, min = .1, max = 1, step = .1),
                                 style = "margin-right: 5px;")
                       # column(2, uiOutput("factorsGroupsChoice_xy"))
                    ),
                    hr(),
                    plotOutput("embeddingsPlot", 
                               brush = brushOpts(id = "plot_factors", fill = "#aaa")),
                    verbatimTextOutput("embeddingsInfo")
                ),
                tabPanel("Embeddings", 
                    p("Run non-linear dimensionality reduction method on factors.", class="description"),
                    fluidRow(
                       column(2, uiOutput("manifoldChoice")),
                       column(2, sliderInput(inputId = 'manifoldDotSize', label = 'Point size', value = 2, min = 1, max = 8, step = .5))
                    ),
                    hr(),
                    plotOutput("dimredPlot")
                )
            ),
            width = 9
        )
    )
          
        ),

    
    
    tabItem(tabName = "charts2",
            
            # ggplot2 species charts section
            # includeMarkdown("www/charts.md"),
            # fluidRow(column(3, uiOutput("categorySelectComboChart"))),
            # column(6, plotOutput("ggplot2Group1") %>% withSpinner(color = "green")),
            # column(6, plotOutput("ggplot2Group2") %>% withSpinner(color = "green"))
            
            # Application title
            titlePanel("MEFISTO Analysis"),
            
            includeMarkdown("www/charts2.md"),
            
            
            # Sidebar with main model parameters choices
            sidebarLayout(
              sidebarPanel(
                # Model file picker (HDF5 or RDS file)
                fileInput(inputId = "model_file2",
                          label = "MEFISTO model (.hdf5 or .rds):",
                          buttonLabel = "MEFISTO model",
                          accept = c("hdf5", "rds")
                ),
                width = 3
              ),
              
              
              # Show a plot of the generated distribution
              mainPanel(
                tabsetPanel(
                  tabPanel("Variance decomposition", 
                           p("", class="description"),
                           plotOutput("variancedecomposition")
                  ),
                  tabPanel("Analyses of weights", 
                           p("", class="description"),
                           plotOutput("Analysesofweights")
                  ),
                  tabPanel("Analyses of factors", 
                           p("", class="description"),
                           # TODO: selector x, y
                           # TODO: selector group_by
                           # TODO: embed multiple plots on one page
                           plotOutput("varianceExplainedPlot2")
                  )
                  ),

                width = 9
              )
            )
            
    ),
    
  
        
        tabItem(tabName = "choropleth",
          
          # choropleth species map section
          includeMarkdown("www/choropleth.md"),
          fluidRow(
            column(3, uiOutput("statesSelectCombo")),
            column(3, uiOutput("categorySelectComboChoro"))
          ),
          fluidRow(
            column(3,tableOutput('stateCategoryList') %>% withSpinner(color = "green")),
            column(9,leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "green"))
          )
          
        ),
        
        tabItem(tabName = "releases", includeMarkdown("www/releases.md"))
              
      )
    
    ) # end dashboardBody
  
  )# end dashboardPage

))