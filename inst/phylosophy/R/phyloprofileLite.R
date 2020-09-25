#' PhyloProfile module
library(PhyloProfile)

phyloprofileLiteUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        wellPanel(
            fluidRow(
                column(
                    2,
                    radioButtons(
                        inputId = ns("xAxis"),
                        label = "Choose type of x-axis:",
                        choices = list("taxa", "genes"),
                        selected = "taxa",
                        inline = TRUE
                    ),
                    actionButton(ns("mainPlotConfig"), "Appearance")
                ),
                column(
                    1,
                    createPlotSize(ns("width"), "Width (px)", 600)
                ),
                column(
                    1, createPlotSize(ns("height"), "Height (px)", 600)
                ),
                column(
                    2, uiOutput(ns("var1Cutoff.ui"))
                ),
                column(
                    2, uiOutput(ns("var2Cutoff.ui"))
                ),
                column(
                    2, uiOutput(ns("percentCutoff.ui"))
                ),
                column(
                    2,
                    numericInput(
                        ns("coortholog"),
                        "Max co-orthologs",
                        min = 1,
                        max = 999,
                        step = 1,
                        value = 999,
                        width = 150
                    ),
                    bsButton(
                        ns("resetMain"),
                        "Reset cutoffs",
                        style = "danger",
                        icon = icon("backward")
                    )
                )
            )
        ),
        sidebarLayout(
            # sidebar panel for input/options -----------------
            sidebarPanel(
                width = 3,
                strong("Input files"),
                checkboxInput(
                    ns("useHamstr"), em("Use HaMStR output"),
                    value = FALSE
                ),
                conditionalPanel(
                    condition = "input.useHamstr", ns = ns,
                    "PhyloProfile input",
                    verbatimTextOutput(ns("inputPP")),
                    selectInput(
                        ns("direction"),
                        "FAS direction",
                        choices = list("Forward" = "fwd", "Reverse" = "rev"),
                        selected = "fwd"
                    ),
                    "Domain input",
                    verbatimTextOutput(ns("inputDomain"))
                ),
                uiOutput(ns("input.ui")),
                hr(),
                
                strong("Set variable names"),
                fluidRow(
                    column(
                        6, uiOutput(ns("var1ID.ui"))
                    ),
                    column(
                        6, uiOutput(ns("var2ID.ui"))
                    )
                ),

                conditionalPanel(
                    condition = "output.unkTaxaStatus == 0", ns = ns,
                    strong("Seed (super)taxon:"),
                    uiOutput(ns("rankSelect.ui")),
                    uiOutput(ns("taxSelect.ui")),
                    bsButton(
                        ns("do"),
                        "PLOT",
                        type = "action",
                        style = "success",
                        disabled = FALSE
                    )
                )
            ),
            # main panel ----------------------------
            mainPanel(
                width = 9,
                conditionalPanel(
                    condition = "output.unkTaxaStatus == 1", ns = ns,
                    # uiOutput(ns("unkMsg.ui"))
                    # strong("PLEASE USE THE FULL VERSION OF PHYLOPROFILE")
                    htmlOutput(ns("unkMsg")),
                    DT::dataTableOutput(ns("unkTaxaFull")),
                    tags$head(tags$style("#unkTaxaFull{color: red;
                                 font-size: 20px;
                                         font-style: italic;}"
                         )
                    )
                ),
                conditionalPanel(
                    condition = "input.do > 0", ns = ns,
                    uiOutput(ns("plot.ui")),
                    br(),
                    downloadButton(ns("profileDownload"),"Download profile",
                                   class = "butDL"),
                    tags$head(
                        tags$style(HTML(
                            ".butDL{background-color:#476ba3;} .butDL{color: white;}"))
                    )
                )
            )
        ),
        
        # popup for setting Main plot configurations -------------------------
        bsModal(
            "mainPlotConfigBs",
            "Plot appearance configuration",
            ns("mainPlotConfig"),
            size = "small",
            column(
                6, createTextSize(ns("xSize"), "X-axis label size (px)", 8, 100)
            ),
            column(
                6, createTextSize(ns("ySize"), "Y-axis label size (px)", 8, 100)
            ),
            column(
                6,
                createTextSize(ns("legendSize"), "Legend label size (px)", 8, 150)
            ),
            column(
                6,
                selectInput(
                    ns("mainLegend"), label = "Legend position:",
                    choices = list("Right" = "right",
                                   "Left" = "left",
                                   "Top" = "top",
                                   "Bottom" = "bottom",
                                   "Hide" = "none"),
                    selected = "right",
                    width = 150
                )
            ),
            column(
                12,
                HTML("<strong>Angle for x-axis label</strong>:<br>"),
                sliderInput(
                    ns("xAngle"),
                    "",
                    min = 0,
                    max = 90,
                    step = 10,
                    value = 60,
                    width = 250
                )#,
                # br()
            ),
            column(
                12,
                HTML("<strong>Zooming factor (α) for dots on
                     profile</strong>:<br>"),
                sliderInput(
                    ns("dotZoom"), "",
                    min = -1,
                    max = 3,
                    step = 0.1,
                    value = 0,
                    width = 250
                ),
                HTML("<em>dot size = (1+α)*defaultSize<br>defaultSize
                     =[0:5]</em>"),
                uiOutput(ns("dotSizeInfo")),
                br()
            ),
            column(
                12,
                HTML("<strong>Change default colors</strong>:<br>"),
                actionButton(ns("setColor"), "Set color")
            )
        ),
        
        # popup for setting plot colors (profiles) ---------------------------
        bsModal(
            "color",
            "Set colors for profile",
            ns("setColor"),
            size = "small",
            colourpicker::colourInput(
                ns("lowColorVar1"),
                "Low variable 1 (dot)",
                value = "darkorange"
            ),
            colourpicker::colourInput(
                ns("highColorVar1"),
                "High variable 1 (dot)",
                value = "steelblue"
            ),
            actionButton(
                ns("defaultColorVar1"),
                "Default",
                style = "padding:4px; font-size:100%"
            ),
            hr(),
            colourpicker::colourInput(
                ns("lowColorVar2"),
                "Low variable 2 (background)",
                value = "grey95"
            ),
            colourpicker::colourInput(
                ns("highColorVar2"),
                "High variable 2 (background)",
                value = "khaki"
            ),
            actionButton(
                ns("defaultColorVar2"),
                "Default",
                style = "padding:4px; font-size:100%"
            ),
            hr(),
            colourpicker::colourInput(
                ns("paraColor"),
                "Color for inparalogs",
                value = "#07d000"
            ),
            actionButton(
                ns("defaultColorPara"),
                "Default",
                style = "padding:4px; font-size:100%"
            )
        ),
        
        # POINT INFO BOX =======================================================
        absolutePanel(
            bottom = 5, left = 30,
            fixed = TRUE,
            draggable = TRUE,
            h5("Point's info:"),
            verbatimTextOutput(ns("pointInfo")),
            conditionalPanel(
                condition = "output.pointInfoStatus == 0", ns = ns,
                bsButton(
                    ns("doDomainPlot"),
                    "Show domain architecture",
                    style = "success",
                    disabled = FALSE
                )
            ),
            style = "opacity: 0.80"
        ),
        
        # popup for plotting domain architecture plot ------------------------
        bsModal(
            "plotArchi",
            "Domain architecture",
            ns("doDomainPlot"),
            size = "large",
            fluidRow(
                column(
                    3, 
                    createPlotSize(ns("archiHeight"), "Plot height(px)", 400),
                    createPlotSize(ns("archiWidth"), "Plot width(px)", 800)
                ),
                column(
                    3,
                    createTextSize(
                        ns("titleArchiSize"), "Title size(px)", 11, 150
                    ),
                    createTextSize(
                        ns("labelArchiSize"), "SeqID size(px)", 11, 150
                    )
                ),
                column(
                    6,
                    uiOutput(ns("seedID.ui")),
                    uiOutput(ns("queryID.ui"))
                )
            ),
            hr(),
            uiOutput(ns("archiPlot.ui")),
            br(),  br(),
            downloadButton(ns("archiDownload"), "Download plot")
        )
    )
}

phyloprofileLite <- function(input, output, session, hamstrOut) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # type of input (manually or directly obtained from hamstr output) -------
    output$input.ui <- renderUI({
        if (input$useHamstr == FALSE) {
            tagList(
                fileInput(ns("mainInput"), "Upload phyloprofile input:"),
                fileInput(ns("fileDomainInput"), "Upload domain file:")
            )
        }
    })
    output$inputPP <- renderText({
        if (file.exists(hamstrOut()[2])) return(hamstrOut()[2])
    })
    output$inputDomain <- renderText({
        if (input$useHamstr == TRUE) {
            if (input$direction  == "rev")
                return(hamstrOut()[4])
            else
                return(hamstrOut()[3])
            if (!file.exists(inputDomain)) return()
        }
    })
    
    # get main input ---------------------------------------------------------
    getMainInput <- reactive({
        withProgress(message = 'Reading main input...', value = 0.5, {
            if (input$useHamstr == TRUE) {
                inputPP <- hamstrOut()[2]
                if (!file.exists(inputPP)) return()
            } else {
                filein <- input$mainInput
                if (is.null(filein)) return()
                inputPP <- filein$datapath
            }
            inputType <- checkInputValidity(inputPP)
            if (inputType == "oma") {
                if (input$getDataOma[1] == 0) return()
                longDataframe <- createProfileFromOma(finalOmaDf())
                longDataframe <- as.data.frame(unclass(longDataframe))
            } else longDataframe <- createLongMatrix(inputPP)
            
            # convert geneID, ncbiID and orthoID into factor and
            # var1, var2 into numeric
            for (i in seq_len(3)) {
                longDataframe[, i] <- as.factor(longDataframe[, i])
            }
            if (ncol(longDataframe) > 3) {
                for (j in seq(4, ncol(longDataframe))){
                    longDataframe[,j] <- suppressWarnings(
                        as.numeric(as.character(longDataframe[,j]))
                    )
                }
            }
            
            # remove duplicated lines
            longDataframe <- longDataframe[!duplicated(longDataframe),]
            return(longDataframe)
        })
    })
    
    # get domain input -------------------------------------------------------
    getDomainInformation <- reactive({
        withProgress(message = 'Reading domain input...', value = 0.5, {
            if (input$useHamstr == TRUE) {
                if (input$direction  == "rev")
                    inputDomain <- hamstrOut()[4]
                else
                    inputDomain <- hamstrOut()[3]
                if (!file.exists(inputDomain)) return()
            } else {
                filein <- input$fileDomainInput
                if (is.null(filein)) return()
                inputDomain <- filein$datapath
            }
            domainDf <- parseDomainInput(
                NULL,
                inputDomain,
                "file"
            )
            return(domainDf)
        })
    })
    
    # check the status of unkTaxa --------------------------------------------
    unkTaxa <- reactive({
        withProgress(message = 'Checking for unknown taxa...', value = 0.5, {
            longDataframe <- getMainInput()
            req(longDataframe)
            
            pathToPhyloprofile <- paste0(
                path.package("PhyloProfile"), "/PhyloProfile"
            )
            rankListFile <- paste0(pathToPhyloprofile, "/data/rankList.txt")
            nameFullFile <- paste0(
                pathToPhyloprofile, "/data/preProcessedTaxonomy.txt"
            )
            
            inputTaxa <- levels(longDataframe$ncbiID)
            inputTaxa <- unlist(strsplit(inputTaxa, split = "\t"))
            
            if (inputTaxa[1] == "geneID") {
                # remove "geneID" element from vector inputTaxa
                inputTaxa <- inputTaxa[-1]
            }
            
            if (!file.exists(isolate(rankListFile))) {
                return(inputTaxa)
            } else {
                info <- file.info(rankListFile)
                if (info$size == 0) {
                    return(inputTaxa)
                } else {
                    # rankListFile <- paste0(getwd(), "/data/rankList.txt")
                    allTaxa <- as.factor(
                        unlist(
                            data.table::fread(file = rankListFile, select = 1)
                        )
                    )
                    
                    # list of unknown taxa
                    unkTaxa <- inputTaxa[!(inputTaxa %in% allTaxa)]
                    if (identical(unkTaxa, character(0))) return()
                    
                    # get non-ncbi taxa
                    unkTaxa <- data.frame(TaxonID = unkTaxa)
                    unkTaxa$id <- substring(unkTaxa$TaxonID, 5)
                    unkTaxa$Source <- "ncbi"
                    
                    ncbiTaxa <- as.factor(
                        unlist(
                            data.table::fread(file = nameFullFile, select = 1)
                        )
                    )
                    
                    ncbiID <- levels(ncbiTaxa)
                    maxNCBI <- max(sort(as.numeric(ncbiID[ncbiID != "ncbiID"])))
                    
                    if (nrow(unkTaxa[!(unkTaxa[,"id"] %in% ncbiTaxa),]) > 0) {
                        unkTaxaId <- unkTaxa[!(unkTaxa$id %in% ncbiTaxa),]$id
                        unkTaxa[unkTaxa$id %in% unkTaxaId,]$Source <- "unknown"
                        if (any(unkTaxaId < maxNCBI)) {
                            unkTaxa[
                                unkTaxa$id %in% unkTaxaId 
                                & unkTaxa$id < maxNCBI,]$Source <- "invalid"
                        }
                    }
                    
                    # return list of unkTaxa
                    return(unkTaxa)
                }
            }
        })
    })
    
    output$unkTaxaStatus <- reactive({
        unkTaxa <- unkTaxa()
        if (length(unkTaxa) > 0) {
            return(1)
        } else return(0)
    })
    outputOptions(output, "unkTaxaStatus", suspendWhenHidden = FALSE)
    
    output$unkMsg <- renderText({
        msg <- paste(
            "Invalid taxa found in your input file.",
            "Please use the full version of PhyloProfile!"
        )
        paste("<font color=\"#FF0000\"><b>", msg, "</b></font>")
    })

    output$unkTaxaFull <- DT::renderDataTable(
        options = list(searching = FALSE, pageLength = 10),{
            if (length(unkTaxa()) > 0) {
                tb <- unkTaxa()
                tb[, c("TaxonID", "Source")]
            }
        }
    )
    
    # check if data is loaded and "plot" button is clicked -------------------
    v <- reactiveValues(doPlot = FALSE)
    observeEvent(input$do, {
        v$doPlot <- input$do
        if (input$useHamstr == TRUE) {
            inputPP <- hamstrOut()[2]
            if (!file.exists(inputPP)) {
                v$doPlot <- FALSE
                updateButton(session, ns("do"), disabled = TRUE)
            }
        } else {
            filein <- input$mainInput
            if (is.null(filein)) {
                v$doPlot <- FALSE
                updateButton(session, ns("do"), disabled = TRUE)
            }
        }
    })
    
    # get ID list of input taxa from main input ------------------------------
    inputTaxonID <- reactive({
        if (length(unkTaxa()) == 0) {
            withProgress(message = 'Getting input taxon IDs...', value = 0.5, {
                longDataframe <- getMainInput()
                inputTaxa <- getInputTaxaID(longDataframe)
            })
        } else return()
    })
    
    # get NAME list of all (super)taxa ---------------------------------------
    inputTaxonName <- reactive({
        req(input$rankSelect)
        if (length(unkTaxa()) > 0) return()
        if (input$rankSelect == "") return()
        withProgress(message = 'Getting input taxon names...', value = 0.5, {
            inputTaxaName <- getInputTaxaName(input$rankSelect, inputTaxonID())
            return(inputTaxaName)
        })
    })
    
    # render textinput for Variable 1 & 2 ------------------------------------
    output$var1ID.ui <- renderUI({
        longDataframe <- getMainInput()
        if (is.null(longDataframe)) {
            textInput(
                ns("var1ID"),
                h5("1st variable:"),
                value = "Variable 1",
                width = "100%",
                placeholder = "Name of first variable"
            )
        } else {
            textInput(
                ns("var1ID"), h5("1st variable:"),
                value = colnames(longDataframe)[4],
                width = "100%",
                placeholder = "Name of first variable"
            )
        }
    })
    
    output$var2ID.ui <- renderUI({
        longDataframe <- getMainInput()
        if (is.null(longDataframe)) {
            textInput(
                ns("var2ID"),
                h5("2st variable:"),
                value = "Variable 2",
                width = "100%",
                placeholder = "Name of second variable"
            )
        } else {
            textInput(
                ns("var2ID"), h5("2st variable:"),
                value = colnames(longDataframe)[5],
                width = "100%",
                placeholder = "Name of second variable"
            )
        }
    })
    
    # render filter slidebars for Main plot ----------------------------------
    output$var1Cutoff.ui <- renderUI({
        createSliderCutoff(
            ns("var1"), paste(input$var1ID, "cutoff:"), 0.0, 1.0, input$var1ID
        )
    })
    
    output$var2Cutoff.ui <- renderUI({
        createSliderCutoff(
            ns("var2"), paste(input$var2ID, "cutoff:"), 0.0, 1.0, input$var2ID
        )
    })
    
    output$percentCutoff.ui <- renderUI({
        createSliderCutoff(
            ns("percent"), "% of present taxa:", 0.0, 1.0, "percent"
        )
    })
    
    # reset cutoffs of Main plot ---------------------------------------------
    observeEvent(input$resetMain, {
        shinyjs::reset("var1")
        shinyjs::reset("var2")
        shinyjs::reset("percent")
        shinyjs::reset("coortholog")
    })
    
    # render list of taxonomy ranks ------------------------------------------
    output$rankSelect.ui <- renderUI({
        selectInput(
            ns("rankSelect"), label = "Select taxonomy rank:",
            choices = getTaxonomyRanks(),
            selected = "species"
        )
    })
    
    # render list of (super)taxa ---------------------------------------------
    output$taxSelect.ui <- renderUI({
        choice <- inputTaxonName()
        choice$fullName <- as.factor(choice$fullName)
        selectInput(
            ns("inSelect"), "Choose (super)taxon of interest:",
            as.list(levels(choice$fullName)),
            levels(choice$fullName)[1]
        )
    })
    
    # sort taxonomy data of input taxa ---------------------------------------
    sortedtaxaList <- reactive({
        req(v$doPlot)
        withProgress(message = 'Sorting input taxa...', value = 0.5, {
            # get input taxonomy tree
            inputTaxaTree <- NULL
            treeIn <- input$inputTree
            if (!is.null(treeIn)) {
                inputTaxaTree <- read.tree(file = treeIn$datapath)
            }
            
            # sort taxonomy matrix based on selected refTaxon
            sortedOut <- sortInputTaxa(
                taxonIDs = inputTaxonID(),
                rankName = input$rankSelect,
                refTaxon = input$inSelect,
                taxaTree = inputTaxaTree
            )
            # return
            return(sortedOut)
        })
    })
    
    # * count taxa for each supertaxon -----------------------------------------
    getCountTaxa <- reactive({
        taxaCount <- plyr::count(sortedtaxaList(), "supertaxon")
        return(taxaCount)
    })
    
    # get subset data (default: first 30 genes) for plotting -----------------
    preData <- reactive({
        req(v$doPlot)
        longDataframe <- getMainInput()
        req(longDataframe)
        withProgress(message = 'Subseting data...', value = 0.5, {
            data <- longDataframe
            
            if (ncol(data) < 5) {
                for (i in seq_len(5 - ncol(data))) {
                    data[paste0("newVar", i)] <- 1
                }
            }
            
            # return preData
            if (nrow(data) == 0) return()
            colnames(data) <- c("geneID", "ncbiID", "orthoID", "var1", "var2")
            return(data)
        })
    })
    
    # * creating main dataframe for subset taxa (in species/strain level) ------
    # * get (super)taxa names
    # * max/min/mean/median VAR1 and VAR2
    getFullData <- reactive({
        req(v$doPlot)
        req(preData())
        req(getCountTaxa())
        req(sortedtaxaList())
        withProgress(message = 'Parsing profile data...', value = 0.5, {
            fullMdData <- parseInfoProfile(
                inputDf = preData(),
                sortedInputTaxa = sortedtaxaList(),
                taxaCount = getCountTaxa(),
                var1AggregateBy = "max",
                var2AggregateBy = "max"
            )
            return(fullMdData)
        })
    })
    
    # * heatmap data input -----------------------------------------------------
    dataHeat <- reactive({
        req(v$doPlot)
        withProgress(message = 'Creating data for plotting...', value = 0.5, {
            percentCutoff <- input$percent
            coorthologCutoffMax <- input$coortholog
            var1Cutoff <- input$var1
            var2Cutoff <- input$var2
            
            # get selected supertaxon name
            split <- strsplit(as.character(input$inSelect), "_")
            inSelect <- as.character(split[[1]][1])
            # create data for heatmap plotting
            filteredDf <- filterProfileData(
                DF = getFullData(),
                taxaCount = getCountTaxa(),
                refTaxon = inSelect,
                percentCutoff,
                coorthologCutoffMax,
                var1Cutoff,
                var2Cutoff,
                "protein",
                "protein",
                groupByCat = FALSE,
                catDt = NULL
            )
            dataHeat <- reduceProfile(filteredDf)
            return(dataHeat)
        })
    })
    
    # ** create profiles for calculating distance matrix -----------------------
    getProfiles <- reactive({
        withProgress(message = 'Getting data for cluster...', value = 0.5, {
            req(dataHeat())
            profiles <- getDataClustering(
                dataHeat(),
                "binary",
                "max",
                "max"
            )
            return(profiles)
        })
    })
    
    # ** calculate distance matrix ---------------------------------------------
    getDistanceMatrixProfiles <- reactive({
        withProgress(message = 'Calculating distance matrix...', value = 0.5, {
            req(dataHeat())
            distanceMatrix <- getDistanceMatrix(getProfiles(), "mutualInformation")
            return(distanceMatrix)
        })
    })
    
    # clustered heatmap data -------------------------------------------------
    clusteredDataHeat <- reactive({
        req(v$doPlot)
        dataHeat <- dataHeat()
        withProgress(message = 'Clustering profile data...', value = 0.5, {
            dat <- getProfiles()
            # do clustering based on distance matrix
            row.order <- hclust(
                getDistanceMatrixProfiles(), method = input$clusterMethod
            )$order

            # re-order distance matrix accoring to clustering
            datNew <- dat[row.order, ] #col.order

            # return clustered gene ID list
            clusteredGeneIDs <- as.factor(row.names(datNew))

            # sort original data according to clusteredGeneIDs
            dataHeat$geneID <- factor(dataHeat$geneID, levels =clusteredGeneIDs)
            
            dataHeat <- dataHeat[!is.na(dataHeat$geneID),]
            return(dataHeat)
        })
    })
    
    # parameters for the main profile plot -----------------------------------
    getParameterInputMain <- reactive({
        inputPara <- list(
            "xAxis" = input$xAxis,
            "var1ID" = input$var1ID,
            "var2ID"  = input$var2ID,
            "lowColorVar1" =  input$lowColorVar1,
            "highColorVar1" = input$highColorVar1,
            "lowColorVar2" =  input$lowColorVar2,
            "highColorVar2" = input$highColorVar2,
            "paraColor" = input$paraColor,
            "xSize" = input$xSize,
            "ySize" = input$ySize,
            "legendSize" = input$legendSize,
            "mainLegend" = input$mainLegend,
            "dotZoom" = input$dotZoom,
            "xAngle" = input$xAngle,
            "guideline" = 1,
            "width" = input$width,
            "height" = input$height,
            "colorByGroup" = FALSE
        )
        return(inputPara)
    })
    
    # reset profile plot colors ----------------------------------------------
    observeEvent(input$defaultColorVar2, {
        shinyjs::reset("lowColorVar2")
        shinyjs::reset("highColorVar2")
    })
    
    observeEvent(input$defaultColorVar1, {
        shinyjs::reset("lowColorVar1")
        shinyjs::reset("highColorVar1")
    })
    
    observeEvent(input$defaultColorPara, {
        shinyjs::reset("paraColor")
    })
    
    # render dot size to dotSizeInfo ---------------------------------------
    output$dotSizeInfo <- renderUI({
        req(v$doPlot)
        
        dataHeat <- dataHeat()
        dataHeat$presSpec[dataHeat$presSpec == 0] <- NA
        presentVl <- dataHeat$presSpec[!is.na(dataHeat$presSpec)]
        
        minDot <- (floor(min(presentVl) * 10) / 10 * 5) * (1 + input$dotZoom)
        maxDot <- (floor(max(presentVl) * 10) / 10 * 5) * (1 + input$dotZoom)
        
        em(paste0("current point's size: ", minDot, " - ", maxDot))
    })
    
    # plot main profile ------------------------------------------------------
    dataPlot <- reactive({
        if (is.null(dataHeat())) stop("Profile data is NULL!")
        dataPlot <- dataMainPlot(dataHeat())
        return(dataPlot)
    })
    
    # render heatmap profile ---------------------------------------------------
    output$plot <- renderPlot({
        if (is.null(dataHeat())) stop("Profile data is NULL!")
        withProgress(message = 'PLOTTING...', value = 0.5, {
            highlightProfilePlot(
                dataPlot(),
                getParameterInputMain(),
                "none",
                input$rankSelect,
                "none"
            )
        })
    })
    
    output$plot.ui <- renderUI({
        ns <- session$ns
        # shinycssloaders::withSpinner(
        plotOutput(
            ns("plot"),
            width = input$width,
            height = input$height,
            click = ns("plotClick")
        )
        # )
    })
    
    output$profileDownload <- downloadHandler(
        filename = function() {
            c("profile.pdf")
        },
        content = function(file) {
            ggplot2::ggsave(
                file,
                plot = highlightProfilePlot(
                    dataPlot(),
                    getParameterInputMain(),
                    "none",
                    input$rankSelect,
                    "none"
                ),
                
                width = input$width * 0.056458333,
                height = input$height * 0.056458333,
                units = "cm", dpi = 300, device = "pdf", limitsize = FALSE
            )
        }
    )
    
    # get info of clicked point on heatmap plot --------------------------------
    selectedpointInfo <- reactive({
        # get selected supertaxon name
        taxaList <- getNameList()
        rankName <- input$rankSelect
        inSelect <- taxaList$ncbiID[taxaList$fullName == input$inSelect]
        
        dataHeat <- dataPlot()
        if (is.null(dataHeat)) stop("Data for heatmap is NULL!")
        
        # get values
        if (is.null(input$plotClick$x)) return()
        else {
            # get cooridiate point
            if (input$xAxis == "genes") {
                corX <- round(input$plotClick$y);
                corY <- round(input$plotClick$x)
            } else {
                corX <- round(input$plotClick$x);
                corY <- round(input$plotClick$y)
            }
            
            # get geneID
            genes <- levels(dataHeat$geneID)
            geneID <- toString(genes[corY])
            # get supertaxon (spec)
            supertaxa <- levels(dataHeat$supertaxon)
            spec <- toString(supertaxa[corX])
            # get var1, percentage of present species and var2 score
            var1 <- NA
            if (!is.na(dataHeat$var1[dataHeat$geneID == geneID
                                     & dataHeat$supertaxon == spec][1])) {
                var1 <- max(
                    na.omit(dataHeat$var1[dataHeat$geneID == geneID
                                          & dataHeat$supertaxon == spec])
                )
            }
            Percent <- NA
            if (!is.na(dataHeat$presSpec[dataHeat$geneID == geneID
                                         & dataHeat$supertaxon == spec][1])) {
                Percent <- {
                    max(
                        na.omit(
                            dataHeat$presSpec[dataHeat$geneID == geneID
                                              & dataHeat$supertaxon == spec]
                        )
                    )
                }
            }
            var2 <- NA
            if (!is.na(dataHeat$var2[dataHeat$geneID == geneID
                                     & dataHeat$supertaxon == spec][1])) {
                var2 <- {
                    max(na.omit(dataHeat$var2[dataHeat$geneID == geneID
                                              & dataHeat$supertaxon == spec]))
                }
            }
            
            # get ortholog ID
            orthoID <- dataHeat$orthoID[dataHeat$geneID == geneID
                                        & dataHeat$supertaxon == spec]
            if (length(orthoID) > 1) {
                orthoID <- paste(orthoID, collapse = ",") #paste0(orthoID[1], ",...")
            }
            
            if (is.na(Percent)) return()
            else {
                info <- c(geneID,
                          as.character(orthoID),
                          spec,
                          round(var1, 2),
                          round(Percent, 2),
                          round(var2, 2))
                return(info)
            }
        }
    })
    
    # show info into "point's info" box --------------------------------------
    output$pointInfo <- renderText({
        info <- selectedpointInfo()
        req(info)
        orthoIDlist <- unlist(strsplit(info[2], ",")) # info[2]
        if (length(orthoIDlist) > 1) orthoID <- paste0(orthoIDlist[1], ",...")
        else orthoID <- as.character(info[2])
        
        if (is.na(orthoID)) return()
        else {
            a <- toString(paste("Seed-ID:", info[1]))
            b <- toString(paste0(
                "Hit-ID: ", orthoID,
                " (", info[3], ")"
            ))
            c <- ""
            if (input$var1ID != "") {
                c <- toString(paste(
                    input$var1AggregateBy, input$var1ID, ":", info[4]
                ))
            }
            d <- ""
            if (input$var2ID != "") {
                d <- toString(paste(
                    input$var2AggregateBy, input$var2ID, ":", info[6]
                ))
            }
            e <- toString(paste("% present taxa:", info[5]))
            paste(a, b, c, d, e, sep = "\n")
        }
    })
    
    # get status of pointInfo for activating Domain Plot button --------------
    output$pointInfoStatus <- reactive({
        req(selectedpointInfo())
        return(is.null(selectedpointInfo()))
    })
    outputOptions(output, "pointInfoStatus", suspendWhenHidden = FALSE)

    output$seedID.ui <- renderUI({
        info <- selectedpointInfo()
        req(info)
        selectInput(
            ns("seedID"), "Seed ID", choices = c(info[1])
        )
    })
    output$queryID.ui <- renderUI({
        info <- selectedpointInfo()
        req(info)
        selectInput(
            ns("queryID"), "Query ID", choices = unlist(strsplit(info[2], ","))
        )
    })
    
    # plot domain architectur --------------------------------------------------
    output$archiPlot <- renderPlot({
        req(selectedpointInfo())
        g <- createArchiPlot(
            c(input$seedID, input$queryID), 
            getDomainInformation(), input$labelArchiSize, input$titleArchiSize
        )
        if (any(g == "No domain info available!")) {
            msgPlot()
        } else {
            grid.draw(g)
        }
    })
    
    output$archiPlot.ui <- renderUI({
        ns <- session$ns
        plotOutput(
            ns("archiPlot"),
            height = input$archiHeight,
            width = input$archiWidth
        )
    })
    
    output$archiDownload <- downloadHandler(
        filename = function() {
            paste0(input$seedID, "_", input$queryID, "_domains.pdf")
        },
        content = function(file) {
            g <- createArchiPlot(
                c(input$seedID, input$queryID), 
                getDomainInformation(), input$labelArchiSize, 
                input$titleArchiSize
            )
            grid.draw(g)
            ggsave(
                file, plot = g,
                width = input$archiWidth * 0.056458333,
                height = input$archiHeight * 0.056458333,
                units = "cm", dpi = 300, device = "pdf", limitsize = FALSE
            )
        }
    )
}

