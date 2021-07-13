##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(data.table)



library(data.table)
library(purrr)
library(ggplot2)
library(ggpubr)
library(ggrepel)

# MOFA
library(MOFA2)

#####################
# SUPPORT FUNCTIONS #
#####################

options(shiny.maxRequestSize=200*1024^2) 
# function to retrieve a park image from the park wiki page
park_image <- function (park_Name){
  
  #bug1_fix#
  park_WikiUrl <- gsub(" ","_",paste0("https://en.wikipedia.org/wiki/",park_Name))
  #bug1_fix#
  park_Img <- read_html(park_WikiUrl)
  park_Img <- park_Img %>% html_nodes("img")
  
  list_park_Img <- (grepl("This is a featured article", park_Img) | grepl("Question_book-new.svg.png", park_Img) | grepl("Listen to this article", park_Img) | grepl("This is a good article", park_Img))
  park_Img <- park_Img[min(which(list_park_Img == FALSE))]
  
  park_Img <- gsub("\"","'",park_Img)
  park_Img <- gsub("//upload.wikimedia.org","https://upload.wikimedia.org",park_Img)
  park_Img <- sub("<img","<img style = 'max-width:100%; max-height:200px; margin: 10px 0px 0px 0px; border-radius: 5%; border: 1px solid black;'",park_Img)
  
  return(park_Img)
  
}
  



################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
   
 
 
    datafile <- reactive({
        input_data <- input$data_file
        if (is.null(input_data)) return(NULL)
        print(file_ext(input_data$name))
        if (file_ext(input_data$name) %in% c("txt", "csv" ,"gz")) {
          return(input_data$datapath)
        } 
         else {
          return(NULL)
        }
    })

        ### TRAINING MODEL ###
    output$trainingData <- renderPlot({
      
       
        #hideTab(inputId = "tabs", target = "Download Trained model")
        m <- datafile()
        if (is.null(m)) return(NULL)
        # Use custom colour palette
        #shiny_palette <- c("#B1BED5", "#BFD8D5", "#DFDFDF", "#6392A3", "f4f3f3")
        ## -----------------------------------------------------------------------------
        #data <- make_example_data(
        #n_views = 2, 
        #n_samples = 200, 
        #n_features = 1000, 
        #n_factors = 10
        #)[[1]]
        
        #lapply(data,dim)

        ## ----message=FALSE------------------------------------------------------------
        #MOFAobject <- create_mofa(data)
        #plot_data_overview(MOFAobject)
        
        
        ## ----message=FALSE------------------------------------------------------------
        #N = ncol(data[[1]])
        #groups = c(rep("A",N/2), rep("B",N/2))
        
        #MOFAobject <- create_mofa(data, groups=groups)
        ## -----------------------------------------------------------------------------
        #plot_data_overview(MOFAobject)
        
        
        
        dt = fread(m)
        
        
        
        
        
        #transcriptome <- read.table (m, sep = "\t", header = FALSE)
        
        #dt <- data.frame(transcriptome)
        
        #file <- system.file("extdata", "test_data.RData", package = "MOFA2")
        
        # Load data (in long data.frame format)
        
        head(dt)
        
        dt[,group:=NULL]
        
        ## -----------------------------------------------------------------------------
        MOFAobject <- create_mofa(dt)
        print(MOFAobject)

        ## ----out.width = "80%"--------------------------------------------------------
        #plot_data_overview(MOFAobject)
        
        
        ## -----------------------------------------------------------------------------
        data_opts <- get_default_data_options(MOFAobject)
        head(data_opts)

        ## -----------------------------------------------------------------------------
        model_opts <- get_default_model_options(MOFAobject)
        model_opts$num_factors <- 10
        head(model_opts)

        ## -----------------------------------------------------------------------------
        train_opts <- get_default_training_options(MOFAobject)
        head(train_opts)

        ## ----message=FALSE------------------------------------------------------------
        MOFAobject <- prepare_mofa(
        object = MOFAobject,
        data_options = data_opts,
        model_options = model_opts,
        training_options = train_opts
        )

        ## -----------------------------------------------------------------------------
        outfile = file.path(getwd(),"www/model.hdf5")
        MOFAobject.trained <- run_mofa(MOFAobject, outfile)
        # shinyjs::show(id = "download")

        
        
        #showTab(inputId = "tabs", target = "Download Trained model")
        

        output$download <- downloadHandler(
        filename = "model.hdf5"
        )
        
        
        
        
        plot(sample)+
          theme(strip.text.x = element_text(size = 16, colour = "#333333"), 
                axis.text.y = element_text(size = 16, colour = "#333333"))
        
        
        
        
  
        #############################################################
        
        
        
       

        ## -----------------------------------------------------------------------------
        sessionInfo()

 

    })
    
    
   


 
    model <- reactive({
        input_model <- input$model_file
        if (is.null(input_model)) return(NULL)
        print(file_ext(input_model$name))
        if (file_ext(input_model$name) %in% c("hdf5", "h5")) {
          load_model(input_model$datapath)
        } else if (file_ext(input_model$name) %in% c("rds", "RDS")) {
          readRDS(input_model$datapath)
        } else {
          return(NULL)
        }
    })
    
    
    
    model2 <- reactive({
      input_model <- input$model_file2
      if (is.null(input_model)) return(NULL)
      print(file_ext(input_model$name))
      if (file_ext(input_model$name) %in% c("hdf5", "h5")) {
        load_model(input_model$datapath)
      } else if (file_ext(input_model$name) %in% c("rds", "RDS")) {
        readRDS(input_model$datapath)
      } else {
        return(NULL)
      }
    })
    
    factorsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        factors_names(m)
    })
    
    viewsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        views_names(m)
    })
    
    groupsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        groups_names(m)
    })
    
    metaChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        metadata_names <- colnames(samples_metadata(m))
        metadata_names[metadata_names != "sample"]
    })

    metaAndFeatureChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        choices_names <- colnames(samples_metadata(m))
        choices_names <- choices_names[choices_names != "sample"]
        c(choices_names, features_names(m))
    })

    metaFeatureFactorChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        meta_names <- list()
        meta_names <- colnames(samples_metadata(m))
        meta_names <- meta_names[meta_names != "sample"]
        if (length(meta_names) > 1)
            # There's more information than just a group
            meta_names <- list("Metadata" = meta_names)
        c(meta_names, 
          features_names(m),
          list("Factors" = factors_names(m)))
    })
    
    dimredChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        if (!.hasSlot(m, "dim_red") || (length(m@dim_red) == 0)) 
            return(c("UMAP", "TSNE"))  # to be computed
            # no t-SNE due to its frequent problems with 
            # perplexity too high for small datasets
        names(m@dim_red)
    })

    featuresChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        features_names(m)
    })

    ### Remembering selected subset of views, groups, factors, etc.

    # viewsSelection <- reactive({
    #     if (is.null(input$viewsChoice))
    #         return("all")
    #     input$viewsChoice
    # })

    groupsSelection <- reactive({
        if (is.null(input$groupsChoice))
            return("all")
        input$groupsChoice
    })

    factorsSelection <- reactive({
        if (is.null(input$factorsChoice))
            return("all")
        input$factorsChoice
    })
    
    colourSelection <- reactive({
        if (is.null(input$colourChoice))
            return("group_name")
        input$colourChoice
    })

    ### LOADINGS ###

    weightsViewSelection <- reactive({
        if (is.null(input$weightsViewSelection))
            return(1)
        input$weightsViewSelection
    })

    weightsFeatureSelection <- reactive({
        if (is.null(input$weightsFeatureSelection))
            return(NULL)
        input$weightsFeatureSelection
    })

    ### DATA ###

    dataFactorSelection <- reactive({
        if (is.null(input$dataFactorSelection))
            return(1)
        input$dataFactorSelection
    })

    dataViewSelection <- reactive({
        if (is.null(input$dataViewSelection))
            return(1)
        input$dataViewSelection
    })

    dataFeatureSelection <- reactive({
        if (is.null(input$dataFeatureSelection))
            return(NULL)
        input$dataFeatureSelection
    })
    
    ### EMBERDDINGS ###
    
    factorSelection_x <- reactive({
        selected_global <- input$factorsChoice
        if (is.null(selected_global)) {
            return(factorsChoice()[1])
        } else if (length(selected_global) >= 1) {
            return(selected_global[1])
        } else {
            return(factorsChoice()[1])
        }
        input$factorChoice_x
    })
    
    factorSelection_y <- reactive({
        selected_global <- input$factorsChoice
        if (is.null(selected_global)) {
            return(factorsChoice()[2])
        } else if (length(selected_global) > 1) {
            return(selected_global[2])
        } else {
            return(factorsChoice()[2])
        }
        input$factorChoice_y
    })

    ### FACTOR VALUES ###

    factorsAxisSelection_x <- reactive({
        selected_global <- input$factorsAxisChoice_x
        if (is.null(selected_global)) {
            return(metaChoice()[1])
        } else if (length(selected_global) >= 1) {
            return(selected_global[1])
        } else {
            return(metaChoice()[1])
        }
    })


    factorsGroupsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        as.character(unique(samples_metadata(m)[,factorsAxisSelection_x()]))
    })


    ### MANIFOLD VALUES ###
    manifoldSelection <- reactive({
        selected_global <- input$manifoldChoice    
        if (is.null(selected_global)) {
            return(dimredChoice()[1])
        } else if (length(selected_global) >= 1) {
            return(selected_global[1])
        } else {
            return(dimredChoice()[1])
        }
    })


    #################
    ### RENDERING ###
    #################
    
    
    output$viewsChoice <- renderUI({
        selectInput('viewsChoice', 'Views:', choices = viewsChoice(), selected = viewsChoice(), multiple = TRUE, selectize = TRUE)
    })

    output$groupsChoice <- renderUI({
        selectInput('groupsChoice', 'Groups:', choices = groupsChoice(), multiple = TRUE, selectize = TRUE)
    })

    output$factorsChoice <- renderUI({
        selectInput('factorsChoice', 'Factors:', choices = factorsChoice(), multiple = TRUE, selectize = TRUE)
    })
    
    output$colourChoice <- renderUI({
        selectInput('colourChoice', 'Colour samples:', choices = metaFeatureFactorChoice(), selected = "group", multiple = FALSE, selectize = FALSE)
    })

    ### MODEL OVERVIEW ###
    
    output$dataOverviewPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        # Use custom colour palette
        shiny_palette <- c("#B1BED5", "#BFD8D5", "#DFDFDF", "#6392A3", "f4f3f3")
        n_views <- get_dimensions(m)$M
        if (n_views <= length(shiny_palette)) {
            shiny_colours <- shiny_palette[seq_len(n_views)] 
        } else {
            shiny_colours <- rainbow(n_views)
        }
        names(shiny_colours) <- views_names(m)
        plot_data_overview(m, colors = shiny_colours) +
            theme(strip.text.x = element_text(size = 16, colour = "#333333"), 
                  axis.text.y = element_text(size = 16, colour = "#333333"))
    })

    ### VARIANCE EXPLAINED ###
    
    output$varianceExplainedPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_variance_explained(m, 
                                factors = factorsSelection(), 
                                plot_total = FALSE, use_cache = TRUE) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  strip.text.x = element_text(size = 14, colour = "#333333"))
    })
    
    output$variancedecomposition <- renderPlot({
      m2 <- model2()
      if (is.null(m2)) return(NULL)
      
      mefisto <- m2
      
      r2 <- get_variance_explained(mefisto, views="RNA")[["r2_per_factor"]][[1]]
      factors <- names(which(r2[,"RNA"]>1))
      mefisto <- subset_factors(mefisto, factors)
      mefisto
      
      plot_variance_explained(mefisto, x="view", y="factor", max_r2 = 9)
      
    })
    
    
    
    output$varianceExplainedPlot2 <- renderPlot({
      m2 <- model2()
      if (is.null(m2)) return(NULL)
      
      mefisto <- m2
      
      sample_metadata <- fread("scnmt_sample_metadata.txt")
      colnames(sample_metadata)
      
      
      #plot_smoothness(m2)
      
      
      factors.dt <- get_factors(mefisto, factors=c(1,3,4))[[1]] %>% 
        as.data.table(keep.rownames = T) %>% 
        setnames("rn","sample")
      
      to.plot <- sample_metadata[,c("sample","UMAP1","UMAP2")] %>% 
        merge(factors.dt, by="sample") %>%
        melt(id.vars=c("UMAP1","UMAP2","sample"), variable.name="factor") %>%
        .[,value:=value/max(abs(value)),by="factor"]
      
      ggscatter(to.plot, x="UMAP1", y="UMAP2", fill="value", shape=21, stroke=0.15) +
        facet_wrap(~factor) +
        scale_fill_gradient2(low = "gray50", mid="gray90", high = "red") +
        theme_classic() +
        ggplot2:: theme_bw()
      
      
    })
    
    
    
    
    
    output$Analysesofweights <- renderPlot({
      m2 <- model2()
      if (is.null(m2)) return(NULL)
      
      mefisto <- m2
      
      
      w.met <- get_weights(mefisto, views="motif_met",  factors=c(1,3), as.data.frame=T) %>% 
        as.data.table %>% .[,feature:=gsub("_met","",feature)] %>%
        .[,value:=value/max(abs(value)),by=c("factor")]
      
      w.acc <- get_weights(mefisto, views="motif_acc", factors=c(1,3), as.data.frame=T) %>% 
        as.data.table %>% .[,feature:=gsub("_acc","",feature)] %>%
        .[,value:=value/max(abs(value)),by=c("factor")]
      
      # Merge loadings
      w.dt <- merge(
        w.met[,c("feature","factor","value")], 
        w.acc[,c("feature","factor","value")], 
        by = c("feature","factor")
      ) %>% .[,feature:=strsplit(feature,"_") %>% map_chr(c(1))]
      
      head(w.dt)
      
      
      
      for (i in unique(w.dt$factor)) {
        
        to.plot <- w.dt[factor==i]
        
        to.label <- w.dt %>% 
          .[factor==i] %>%
          .[,value:=abs(value.x)+abs(value.y)] %>% setorder(-value) %>% head(n=10)
        
        p <- ggscatter(to.plot, x="value.x", y="value.y", size=1.5, add="reg.line", conf.int=TRUE) +
          coord_cartesian(xlim=c(-1,1), ylim=c(-1,1)) +
          scale_x_continuous(breaks=c(-1,0,1)) +
          scale_y_continuous(breaks=c(-1,0,1)) +
          geom_text_repel(data=to.label, aes(x=value.x, y=value.y, label=feature), size=3,  max.overlaps=100) +
          geom_vline(xintercept=0, linetype="dashed") +
          geom_hline(yintercept=0, linetype="dashed") +
          stat_cor(method = "pearson") +
          labs(x="Methylation weights", y="Accessibility weights")
        
        print(p)
      }
      

    })
    
    
    
    
    
    
    
    
    
    

    ### WEIGHTS (LOADINGS) ###

    output$weightsViewSelection <- renderUI({
        selectInput('weightsViewSelection', 'View:', choices = viewsChoice(), multiple = FALSE, selectize = TRUE)
    })

    output$weightsFeatureSelection <- renderUI({
        selectInput('weightsFeatureSelection', 'Label manually:', choices = featuresChoice()[weightsViewSelection()], multiple = TRUE, selectize = TRUE)
    })

    output$weightsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        if (!is.null(weightsFeatureSelection()) && (length(weightsFeatureSelection()) > 0)) {
            # Some features are selected manually
            plot_weights(m, view = weightsViewSelection(), factors = factorsSelection(), manual = weightsFeatureSelection())
        } else {
            plot_weights(m, view = weightsViewSelection(), factors = factorsSelection(), nfeatures = input$nfeatures_to_label)
        }
    })

    output$topWeightsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_top_weights(m, view = weightsViewSelection(), 
                         factors = factorsSelection(), 
                         nfeatures = input$nfeatures_to_label)
    })

    # output$weightsInfo <- renderPrint({
    #     m <- model()
    #     if (is.null(m)) return(NULL)
    #     nearPoints(plot_weights(m, factors = factorsSelection(), nfeatures = input$nfeatures_to_label, return_data = TRUE),
    #                input$weightsHover,
    #                threshold = 10, maxpoints = 3, addDist = TRUE)
    # })

    ### FACTOR VALUES ###
    
    output$factorsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        p <- plot_factor(m, factors = factorsSelection(), color_by = colourSelection(), group_by = factorsAxisSelection_x(), 
                    groups = groupsSelection(), add_dots = input$factorsAddDots, add_violin = input$factorsAddViolins,
                    dot_size = input$factorsDotSize, dot_alpha = input$factorsDotAlpha, violin_alpha = input$factorsViolinAlpha)
        if (input$factorsRotateLabelsX) {
            p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        } else {
            p
        }
    })

    output$factorsAxisChoice_x <- renderUI({
        selectInput('factorsAxisChoice_x', 'X axis:',
                    choices = metaChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorsAxisSelection_x())
    })

    ### DATA (SINGLE FACTOR EXPLORATION) ###

    output$dataFactorSelection <- renderUI({
        selectInput('dataFactorSelection', 'Factor:', choices = factorsChoice(), multiple = FALSE, selectize = TRUE)
    })

    output$dataViewSelection <- renderUI({
        selectInput('dataViewSelection', 'View:', choices = viewsChoice(), multiple = FALSE, selectize = TRUE)
    })

    output$dataFeatureSelection <- renderUI({
        selectInput('dataFeatureSelection', 'Select features manually:', 
                    choices = featuresChoice()[dataViewSelection()], multiple = TRUE, selectize = TRUE)
    })

    output$dataHeatmapPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)

        # Figure out if samples should be annotated
        annotation_samples <- NULL
        if (colourSelection() %in% colnames(samples_metadata(m))) annotation_samples <- colourSelection()

        # Figure out if features are provided manually
        selection_features <- input$nfeatures_to_plot
        if (!is.null(dataFeatureSelection()) && (length(dataFeatureSelection()) > 0))
            selection_features <- dataFeatureSelection()

        plot_data_heatmap(m, view = dataViewSelection(), groups = groupsSelection(), 
                          factor = dataFactorSelection(), features = selection_features,
                          annotation_samples = annotation_samples)
    })

    output$dataScatterPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)

        # Figure out if features are provided manually
        selection_features <- input$nfeatures_to_plot
        if (!is.null(dataFeatureSelection()) && (length(dataFeatureSelection()) > 0))
            selection_features <- dataFeatureSelection()

        plot_data_scatter(m, view = dataViewSelection(), groups = groupsSelection(), 
                          factor = dataFactorSelection(), features = selection_features)
    })


    ### FACTORS SCATTERPLOT (EMBEDDINGS) ###
    
    output$factorChoice_x <- renderUI({
        selectInput('factorChoice_x', 'X axis factor:', 
                    choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorSelection_x())
    })
    
    output$factorChoice_y <- renderUI({
        selectInput('factorChoice_y', 'Y axis factor:', 
                    choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorSelection_y())
    })
    
    output$embeddingsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_factors(m, groups = groupsSelection(), factors = c(input$factorChoice_x, input$factorChoice_y), color_by = colourSelection(),
                     dot_size = input$factorDotSize, alpha = input$factorDotAlpha)
    })
    
    output$embeddingsInfo <- renderPrint({
        m <- model()
        if (is.null(m)) return(NULL)
        df <- plot_factors(m, groups = groupsSelection(), factors = c(input$factorChoice_x, input$factorChoice_y), color_by = colourSelection(), return_data = TRUE) 
        brushedPoints(df, input$plot_factors)
    })

    # output$factorsGroupsChoice_xy <- renderUI({
    #     selectInput('factorsGroupsChoice_xy', 'Display groups:',
    #                 choices = factorsGroupsChoice(), multiple = TRUE, selectize = TRUE,
    #                 selected = factorsGroupsChoice())
    # })
    
    
    observeEvent(input$swapEmbeddings, {
        x_sel <- input$factorChoice_x
        y_sel <- input$factorChoice_y
        if (!is.null(x_sel) && !is.null(y_sel)) {
            output$factorChoice_y <- renderUI({
                selectInput('factorChoice_y', 'Factor on Y axis:', 
                            choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                            selected = x_sel)
            })
            
            output$factorChoice_x <- renderUI({
                selectInput('factorChoice_x', 'Factor on X axis:', 
                            choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                            selected = y_sel)
            })
        }
    })


    ### DIMENSIONALITY REDUCTION PLOT ###

    output$manifoldChoice <- renderUI({
        selectInput('manifoldChoice', 'Manifold:',
                    choices = dimredChoice(), multiple = FALSE, selectize = TRUE,
                    selected = manifoldSelection())
    })
    
    output$dimredPlot <- renderPlot({
        m <- model()
        method <- manifoldSelection()
        if (is.null(m) || is.null(method) || (method == "")) {
            return(NULL)
        } else {
            set.seed(1)
            if (method == "TSNE") {
                plot_dimred(m, method, groups = groupsSelection(), factors = factorsSelection(), color_by = colourSelection(),
                        # Provide perplexity for t-SNE since the default one is typically too high for small datasets
                        perplexity = 10, dot_size = input$manifoldDotSize)     
            } else {
                plot_dimred(m, method, groups = groupsSelection(), factors = factorsSelection(), color_by = colourSelection(),
                            dot_size = input$manifoldDotSize)
            }
        }
    })

    # Saving the plots
    output$saveButtonLoadings <- downloadHandler(
        filename = "mofa2_plot.pdf",
        content = function(file) {
            ggsave(file, device = "pdf",
                   plot = plot_weights(model(), view = loadingsViewSelection(), factors = factorsSelection(), nfeatures = input$nfeatures_to_label))
        }
    )

    
 
 # leaflet choropleth
 output$statesSelectCombo <- renderUI({
   selectInput("statesCombo","Select a state:", paste0(state.name[match(speciesStates,state.abb)]," (",speciesStates,")"))
 })
 
 output$categorySelectComboChoro <- renderUI({
   selectInput("selectedCategoryChoro","Select a category:", speciesCategories)
 })
 
 selectedChoroCategory <- reactive(speciesCategoriesByState[speciesCategoriesByState$Category==input$selectedCategoryChoro,])
 selectedChoroCategoryJoinStates <- reactive(geo_join(states, selectedChoroCategory(), "STUSPS", "ParkState"))
 
 output$stateCategoryList <- renderTable({
   speciesCategoriesByState[speciesCategoriesByState$ParkState == substr(input$statesCombo,nchar(input$statesCombo)-2,nchar(input$statesCombo)-1), c("Category","n")]
 },colnames = FALSE) 
 
   
 output$choroplethCategoriesPerState <- renderLeaflet({

   leaflet(options = leafletOptions(zoomControl = FALSE)) %>% htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
     addProviderTiles("CartoDB.PositronNoLabels") %>%
     setView(-98.483330, 38.712046, zoom = 4) %>%
     addPolygons(data = selectedChoroCategoryJoinStates(),
                 fillColor = colorNumeric("Greens", domain=selectedChoroCategoryJoinStates()$n)(selectedChoroCategoryJoinStates()$n),
                 fillOpacity = 0.7,
                 weight = 0.2,
                 smoothFactor = 0.2,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "#666",
                   fillOpacity = 0.7,
                   bringToFront = TRUE),
                 label = paste0("Total of ", as.character(selectedChoroCategoryJoinStates()$n)," species in ",as.character(selectedChoroCategoryJoinStates()$NAME)," (",as.character(selectedChoroCategoryJoinStates()$STUSPS),").")) %>%
     addLegend(pal = colorNumeric("Greens", domain=selectedChoroCategoryJoinStates()$n),
               values = selectedChoroCategoryJoinStates()$n,
               position = "bottomright",
               title = input$selectedCategoryChoro)

 })

})