#' DCC module

dccAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for DCC input/options -----------------
        sidebarPanel(
            width = 3,
            conditionalPanel(
                condition = "output.checkDccStatus == 0", ns = ns,
                h2(em("dcc2 not found! Please install it first!")),
                bsButton(
                    "installDcc", "Install DCC",
                    onclick = "window.open('https://bionf.github.io/dcc2/#how-to-install', '_blank')"
                ),
                hr()
            ),
            
            h3("Input and configurations"),
            hr(),

            radioButtons(
                ns("inputTyp"), strong("Select your input type"),
                choices = list(
                    "NCBI Taxonomy ID" = "ncbiID",
                    "Scientific name" = "speciesName",
                    "Input taxon ID list" = "inputFile",
                    "OMA Group ID" = "OmaId",
                    "OMA standalone file" = "omaFile"
                ),
                selected = "ncbiID"
            ),
            
            conditionalPanel(
                condition = "input.inputTyp == 'omaFile'", ns = ns,
                shinyFilesButton(
                    ns("omaFileInput"),
                    "Upload OMA file (orthoXML)" ,
                    title = "Please provide OMA file in orthoXML format",
                    multiple = FALSE,
                    buttonType = "default"
                ),
                br(), br(),
                
                selectInput(
                    ns("taxType"), strong("Get genne sets from:"),
                    c("Local directory", "OMA Database"),
                    selected = "Local directory"
                ),
                
                conditionalPanel(
                    condition = "input.taxType == 'Local directory'", 
                    ns = ns,
                    shinyDirButton(
                        ns("genomeDir"), "Gene set directory" ,
                        title = "Please select gene set directory",
                        buttonType = "default", class = NULL
                    ),
                    bsPopover(
                        ns("genomeDir"),
                        "",
                        paste(
                            "Directory contains organisms's sequences"
                        ),
                        "bottom"
                    ),
                    br(), br(),
                ),
                
                shinyFilesButton(
                    ns("taxMappingFile"),
                    "TaxID mapping file" ,
                    title = "Please provide taxon ID mapping file",
                    multiple = FALSE,
                    buttonType = "default"
                ),
                bsPopover(
                    ns("taxMappingFile"),
                    "",
                    paste(
                        "Tab-delimited file containing 3 columns ",
                        "<NCBI ID> <Taxon name in orthoXML file>",
                        "<Abbr. taxon name>"
                    ),
                    "bottom"
                ),
                br(), br(),
            ),
            
            conditionalPanel(
                condition = "input.inputTyp != 'omaFile'", ns = ns,
                # oma version
                uiOutput(ns("version")),
                # list of avail oma spec
                uiOutput(ns("omaSpec")),
                # oma type (appears only when 2 taxa are selected)
                uiOutput(ns("omaType")),
                # No. of missing species allowed a common OmaGroup
                uiOutput(ns("nrMissingSpecies"))
            ),
            
            # MSA tool selection
            radioButtons(
                ns("MSA"),
                strong("MSA tool"), choices = c("MAFFT", "MUSCLE"), 
                selected = "MAFFT"
            ),
            
            # Number of CPUs
            numericInput(
                ns("cpus"), 
                strong("Number of CPU cores for multiprocessing"),
                min = 1, max = 999, step = 1, value = 4
            ),
            
            # option for running FAS annotation
            checkboxInput(
                ns("doAnno"), strong("Include FAS annotation"),
                value = FALSE
            ),
            bsPopover(
                ns("doAnno"),
                "",
                paste(
                    "Include feature annotation.",
                    "Note: it can take time!"
                ),
                "bottom"
            ),
            br(),
            
            shinyDirButton(
                ns("outAnnoDir"), "Output directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            hr(),
            
            # job ID
            textInput(ns("dccJob"), strong("Job ID"), value = randFn(1)),
            bsPopover(
                ns("dccJob"),
                "",
                paste(
                    "Name of job and log file(s)"
                ),
                "bottom"
            ),
            bsButton(ns("newDcc"), label = "New job", disabled = TRUE)
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            conditionalPanel(
                condition = "output.checkRunDcc", ns = ns,
                bsButton(
                    ns("submit"), "Run DCC",
                    style = "success", disabled = FALSE
                ),
                bsButton(ns("stopDcc"), label = "Stop", disabled = TRUE),
                hr(),
                
                # list of taxa selected by names or IDs
                DT::dataTableOutput(ns("speciesTable")),
                
                # list of taxa from user input file
                DT::dataTableOutput(ns("checkTax")),
                
                # list of taxa in the given oma group
                DT::dataTableOutput(ns("omaGroupSpeciesTable")),
                uiOutput(ns("speciesSelectionOmaGroup")),
                
                # error msg for invalid input taxa
                span(textOutput(ns("error")), style = "color:red"),
                
                # number of obtained common oma groups
                verbatimTextOutput(ns("nrOmaGroups")),
                
                # arguments of orthoxmlParser 
                conditionalPanel(
                    condition = "input.inputTyp == 'omaFile'", ns = ns,
                    strong("Standalone OMA parser options:"),
                    uiOutput(ns("omaParserOptions")),
                    br(), br(),
                ),
                
                strong("Command"),
                verbatimTextOutput(ns("dccCmdText")),
            
                strong("Progress"),
                verbatimTextOutput(ns("dccLog")),
                br(),
                
                # finishing msg
                # uiOutput(ns("end")),
                uiOutput(ns("end.ui"))
            )
        )
    )
}

dccApp <- function (input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # check if dcc2 installed ==================================================
    output$checkDccStatus <- reactive({
        dccLocation <- suppressWarnings(
            system("which prepareDcc", intern = TRUE)
        )
        if (!is.na (dccLocation[1])){
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkDccStatus", suspendWhenHidden = FALSE)
    
    python <- reactive({
        if (try(system("python -V") < "Python 3")) {
            return("python3")
        } else {
            return("python")
        }
    })
    
    # get local OMA data path ==================================================
    getOmaPath <- reactive({
        omaDataPath <- suppressWarnings(
            system("prepareDcc -o ~/ -g", intern = TRUE)
        )
        if (length(omaDataPath) > 1){
            return(omaDataPath[2])
        } else {
            return("ERROR NO PATHCONFIG FOUND!")
        }
    })
    
    # render oma version =======================================================
    output$version <- renderUI({
        req(getOmaPath())
        currVersion <- OmaDB::getVersion()$oma_version
        if (!file.exists(paste0(getOmaPath(),"/oma-groups.txt"))) {
            prepareDcc <- a(
                "prepareDcc function",
                href="https://github.com/BIONF/dcc2#setup-dcc2"
            )
            tagList(
                "No OMA data found! Please run ", prepareDcc, " first!"
            )
        } else {
            localVersion <- str_replace_all(
                str_match(
                    readLines(paste0(getOmaPath(),"/oma-groups.txt"), n = 1),
                    "of .+"
                )[1],
                "of ", ""
            )
            if (localVersion == currVersion) {
                HTML(paste0(
                    "<p>OMA data found at ", getOmaPath(), "</p>",
                    "<p><em>Version <span style=\"color: #ff0000;\">",
                    currVersion, "</span></em></p>"
                ))
            } else {
                HTML(paste0(
                    "<p><em>This version <span style=\"color: #ff0000;\">",
                    localVersion, "</span> is outdated! ",
                    "Current OMA version is <span style=\"color: #ff0000;\">",
                    currVersion,"</span>.</em></p>"
                ))
            }
        }
    })
    
    # generate new job ID ======================================================
    observeEvent(input$newDcc, {
        jobID <- randFn(1)
        updateTextInput(session, "dccJob", strong("Job ID"), value = jobID)
        updateButton(session, ns("submit"), disabled = FALSE)
        updateButton(session, ns("stopDcc"), disabled = TRUE)
        output$verbatimTextOutput <- renderText({NULL})
        system2("rm", "*.dcc.log")
    })

    # get standalone OMA info ==================================================
    # * oma input path ========================================================
    getLocalOma <- reactive({
        shinyFileChoose(
            input, "omaFileInput", roots = homePath, session = session
        )
        req(input$omaFileInput)
        file_selected <- parseFilePaths(homePath, input$omaFileInput)
        return(as.character(file_selected$datapath))
    })
    # * data set directory ===================================================
    getLocalDataset <- reactive({
        shinyDirChoose(
            input, "genomeDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$genomeDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    # * mapping file path =====================================================
    getTaxMapping <- reactive({
        shinyFileChoose(
            input, "taxMappingFile", roots = homePath, session = session
        )
        file_selected <- parseFilePaths(homePath, input$taxMappingFile)
        return(as.character(file_selected$datapath))
    })

    # * read taxonomy info from mapping file ==================================
    readTaxMapping <- reactive({
        req(getTaxMapping())
        taxFile <- read.table(
            getTaxMapping(), sep = "\t", header = FALSE, comment.char = "",
            stringsAsFactors = FALSE
        )
        colnames(taxFile) <- c("NCBI ID", "Gene set name", "Abbr specices name")
        return(taxFile)
    })

    # * render orthoxmlParser options =========================================
    omaParserOptions <- reactive({
        inFile <- ""
        if (length(getLocalOma()) > 0)
            inFile <- paste("--inFile", getLocalOma())
        outPath <- ""
        if (length(getOutputPath()) > 0)
            outPath <- paste("--outPath", getOutputPath())
        data <- ""
        if (length(getLocalDataset()) > 0)
            data <- paste("--geneSet", getLocalDataset())
        mapping <- ""
        if (length(getTaxMapping()) > 0)
            mapping <- paste("--mappingFile", getTaxMapping())
        tool <- paste("--alignTool", input$MSA)
        anno <- ""
        if (input$doAnno  == TRUE)
            anno <- "--annoFas"
        jobName <- ""
        if(lenght(input$dccJob) > 0)
            jobName <- paste("--jobName", input$dccJob)
        parserOption <- c(inFile, outPath, data, mapping, tool, anno, jobName)
        return(
            parserOption[unlist(lapply(parserOption, function (x) x != ""))]
        )
    })

    output$omaParserOptions <- renderUI({
        HTML(paste(omaParserOptions(), collapse = "<br/>"))
    })

    # process dowloaded OMA database ===========================================
    # * load the oma-species file from OmaDb ===================================
    readOmaSpec <- reactive({
        req(getOmaPath())
        if (!file.exists(paste0(getOmaPath(), "/oma-species.txt"))) return()
        taxTable <- fread(
            paste0(getOmaPath(), "/oma-species.txt"), header = FALSE, skip = 2,
            sep = "\t"
        )
        colnames(taxTable) <- c(
            "OMAcode", "TaxonID", "ScientificName", "GenomeSource",
            "Version/Release"
        )
        return(taxTable)
    })

    # * load the transformed oma-groups-tmp.txt ================================
    readOmaGroup <- reactive({
        req(getOmaPath())
        groupTable <- fread(paste0(getOmaPath(), "/oma-groups-tmp.txt"))
        return(groupTable)
    })

    # * render list of avail taxa based on selected input type =================
    # * except OMA standalone
    output$omaSpec <- renderUI({
        omaSpecTable <- readOmaSpec()
        if (input$inputTyp == "ncbiID") {
            selectInput(
                inputId = ns("speciesList"),
                label = strong("Select up to 10 species"),
                multiple = TRUE,
                choices = omaSpecTable$TaxonID
            )
        } else if (input$inputTyp == "speciesName") {
            selectInput(
                inputId = ns("speciesList"),
                label = strong("Select up to 10 species"),
                multiple = TRUE,
                choices = omaSpecTable$ScientificName
            )
        } else if (input$inputTyp == "inputFile") {
            fileInput(ns("taxFile"), "Choose File")
        } else if (input$inputTyp == "OmaId") {
            x = readOmaGroup()
            numericInput(
                inputId = ns("omaGroupId"),
                value = 1,
                label = strong(
                    paste("Select a Oma Group Id between 1 and", nrow(x))
                ),
                min = 1,
                max = nrow(x),
                step = 1
            )
        }
    })

    # * render OMA type ========================================================
    output$omaType <- renderUI({
        req(input$speciesList)
        if (length(input$speciesList) == 2){
            checkboxInput(
                ns("omaPair"), 
                strong("Use Oma Pair (requires Internet)"), value = FALSE
            )
        }
    })

    # * render number of allowed missing species ===============================
    # * except OMA standalone
    output$nrMissingSpecies <- renderUI({
        if (input$inputTyp != "OmaId") {
            if (input$inputTyp != "inputFile") {
                selectInput(
                    ns("nrMissingSpecies"),
                    strong("How many species can be missed in an OmaGroup"),
                    choices = seq(0,(length(input$speciesList)-2))
                )
            } else if (input$inputTyp == "inputFile") {
                inFile <- input$taxFile
                if (!(is.null(inFile))) {
                    taxaInFile <- read.table(inFile$datapath, header = FALSE)
                    selectInput(
                        ns("nrMissingSpecies"),
                        strong("How many species can be missed in an OmaGroup"),
                        choices = seq(0,(nrow(taxaInFile)-2))
                    )
                }
            }
        }
    })

    # render table of selected taxa ===========================================
    # * from taxa FILE ========================================================
    # load input file
    getFileTable <- reactive({
        inFile <- input$taxFile
        speciesTable <- readOmaSpec()
        if (is.null(inFile)) {
            DF <- NULL
            return(DF)
        } else if (input$inputTyp == "inputFile") {
            taxaInFile <- read.table(inFile$datapath, header = FALSE)
            colnames(taxaInFile) <- "TaxonID"
            DF <- merge(
                taxaInFile, speciesTable[,c("TaxonID", "ScientificName")],
                by = "TaxonID",
                all.x = TRUE
            )
            return(DF)
        }
    })

    # get valid & invalid taxa from input file
    makeWarningWindow <- reactive({
        req(input$taxFile)
        DF <- getFileTable()
        notAvailableTaxons <- DF$TaxonID[is.na(DF$ScientificName)]
        if (length(notAvailableTaxons) > 0) {
            shinyalert(
                title = "warning",
                text = paste(
                    "The following TaxonIds aren't available in Oma. Do you",
                    "want to continue without them?", notAvailableTaxons
                ),
                type = "warning", showConfirmButton = TRUE,
                showCancelButton = TRUE
            )
        }
    })

    getTaxaFromFile <- reactive({
        req(input$taxFile)
        DF <- getFileTable()
        notAvailableTaxons <- DF$TaxonID[is.na(DF$ScientificName)]
        DF$ScientificName[is.na(DF$ScientificName)] <- "not available yet"
        if (length(notAvailableTaxons) > 0) {
            makeWarningWindow()
        }
        return(DF)
    })

    # render taxa list from input file
    output$checkTax <- DT::renderDataTable({
        req(input$taxFile)
        fileTable <- getTaxaFromFile()
    })

    # * from OMA group ID =====================================================
    createOmaIdOutput <- reactive({
        tableGroup <- readOmaGroup()
        speciesTable <- readOmaSpec()
        if (is.null(input$omaGroupId)) {
            return(NULL)
        } else {
            speciesList <- strsplit(
                tableGroup$V2[tableGroup$V1 == input$omaGroupId], ","
            )
            specCode <- data.frame(OMAcode = speciesList[[1]])
            DF <- merge(
                specCode,
                speciesTable[,c("TaxonID", "ScientificName", "OMAcode")],
                by = "OMAcode",
                all.x = TRUE
            )
        }
        return(DF)
    })

    output$omaGroupSpeciesTable <- DT::renderDataTable({
        if (input$inputTyp == "OmaId") {
            speciesTable <- createOmaIdOutput()
        }
    })

    output$speciesSelectionOmaGroup <- renderUI({
        if (input$inputTyp == "OmaId") {
            speciesTable <- createOmaIdOutput()
            selectInput(
                inputId = ns("GroupSpecies"),
                label = strong("Select up to 10 species"),
                multiple = TRUE,
                choices = speciesTable$TaxonID,
                selected = speciesTable$TaxonID[1]
            )
        }
    })

    # * from selected taxon NAMES or IDs or standalone OMA ====================
    createTableOutput <- reactive({
        speciesTable <- readOmaSpec()
        DF <- NULL
        if (input$inputTyp == "ncbiID") {
            if (is.null(input$speciesList) == TRUE) {
                DF <- data.table(
                    TaxonmyIDs = "Nothing selected",
                    ScientificNames = "Nothing selected",
                    OmaCode = "Nothing selected")
            } else {
                if (sum(speciesTable$TaxonID %in% input$speciesList) > 0) {
                    DF <- data.table(
                        TaxonomyIDs = speciesTable$TaxonID[
                            speciesTable$TaxonID %in% input$speciesList],
                        ScientificNames = unlist(
                            speciesTable$ScientificName[
                                speciesTable$TaxonID %in% input$speciesList]
                        ),
                        OmaCode = unlist(
                            speciesTable$OMAcode[
                                speciesTable$TaxonID %in% input$speciesList]
                        )
                    )
                }
            }
        } else if (input$inputTyp == "speciesName") {
            if (is.null(input$speciesList) == TRUE) {
                DF <- data.table(
                    TaxonmyIDs = "Nothing selected",
                    ScientificNames = "Nothing selected",
                    OmaCode = "Nothing selected")
            } else {
                if (sum(speciesTable$ScientificName %in% input$speciesList)>0) {
                    DF <- data.table(
                        TaxonomyIDs = speciesTable$TaxonID[
                            speciesTable$ScientificName %in% input$speciesList],
                        # ScientificNames = unlist(input$speciesList),
                        ScientificNames = speciesTable$ScientificName[
                            speciesTable$ScientificName %in% input$speciesList],
                        OmaCode = unlist(
                            speciesTable$OMAcode[
                                speciesTable$ScientificName 
                                %in% input$speciesList
                            ]
                        )
                    )
                }
            }
        } else {
            DF <- NULL
        }
        return(DF)
    })

    output$speciesTable <- DT::renderDataTable({
        if (input$inputTyp == "omaFile") {
            readTaxMapping()
        } else {
            speciesTable <- createTableOutput()
        }
    })

    # get OmaCode for chosen species ==========================================
    findOmaCode <- function(outputSpecies, speciesTable) {
        if (input$inputTyp == "ncbiID") {
            omaCode <- speciesTable$OMAcode[
                speciesTable$TaxonID %in% outputSpecies]
        } else if (input$inputTyp == "speciesName") {
            omaCode <- speciesTable$OMAcode[
                speciesTable$ScientificName %in% outputSpecies]
        } else {
            omaCode <- speciesTable$OMAcode[
                speciesTable$TaxonID %in% outputSpecies]
        }
        return(omaCode)
    }

    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outAnnoDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outAnnoDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })

    # run DCC ==================================================================
    # * dcc command ============================================================
    getCmd <- reactive({
        cmd <- ""
        if (input$inputTyp == 'omaFile') {
            # parse standalone OMA
            cmd <- paste(
                python(), "parseOrthoxml",
                paste(omaParserOptions(), collapse = " ")
                #, "-l", 5 # for testing purpose
            )
        } else {
            # parse OMA Browser
            if (input$inputTyp == "inputFile") {
                inFile <- input$taxFile
                taxa <- fread(inFile$datapath, header = FALSE)
                speciesInput <- taxa$V1
            } else if (input$inputTyp == "OmaId") {
                speciesInput <- input$GroupSpecies
            } else {
                speciesInput <- input$speciesList
            }

            taxTable <- readOmaSpec()
            OmaCodes <- findOmaCode(speciesInput, taxTable)
            # taxIds <- findTaxId(speciesInput, taxTable)

            if (input$inputTyp == "OmaId") {
                cmd <- paste(
                    "parseOmaById",
                    "--OG", input$omaGroupId,
                    "--name", paste(OmaCodes, collapse = ","),
                    # "-i", paste(taxIds, collapse = ","),
                    # "-d", getOmaPath(),
                    "--outPath", getOutputPath(),
                    "--alignTool", tolower(input$MSA),
                    "--jobName", input$dccJob,
                    "--cpus", input$cpus
                )
                if (input$doAnno) cmd <- paste(cmd, "-f")
            } else {
                cmd <- paste(
                    "parseOmaBySpec",
                    "--name", paste(OmaCodes, collapse = ","),
                    # "-i", paste(taxIds, collapse = ","),
                    # "-d", getOmaPath(),
                    "--outPath", getOutputPath(),
                    "--missingTaxa", input$nrMissingSpecies,
                    "--alignTool", tolower(input$MSA),
                    "--jobName", input$dccJob,
                    "--cpus", input$cpus
                )
                if (input$doAnno) cmd <- paste(cmd, "--annoFas")
                if (length(input$speciesList) == 2){
                    if (length(input$omaPair) > 0 && input$omaPair == TRUE) 
                        cmd <- paste(cmd, "--omaType pair")
                }
            }
        }
        return(cmd)
    })

    output$dccCmdText <- renderText({
        getCmd()
    })

    # * render submit button ===================================================
    output$checkRunDcc <- reactive({
        if (length(getOutputPath()) == 0) return(FALSE)
        if (input$inputTyp == "omaFile") {
            if (
                length(getLocalOma()) == 0 ||
                length(getLocalDataset()) == 0 ||
                length(getTaxMapping()) == 0
            ) { return(FALSE) }
        } else {
            if (input$inputTyp == "inputFile") {
                if (is.null(input$taxFile)) return(FALSE)
            } else if (input$inputTyp == "OmaId") {
                if (is.null(input$omaGroupId)) return(FALSE)
            } else {
                if (is.null(input$speciesList)) return(FALSE)
            }
            if (length(input$shinyalert) > 0) {
                if (input$shinyalert == FALSE) return(FALSE)
            }
        }
        return(TRUE)
    })
    outputOptions(output, "checkRunDcc", suspendWhenHidden = FALSE)

    rvDcc <- reactiveValues(
        textstream = c(""),
        timer = reactiveTimer(1000),
        started = FALSE
    )

    startPythonScript <- observeEvent(input$submit, {
        rvDcc$started <- TRUE
        updateButton(session, ns("submit"), disabled = TRUE)
        updateButton(session, ns("newDcc"), disabled = FALSE)
        updateButton(session, ns("stopDcc"), disabled = FALSE)

        cmd <- paste(getCmd(), ">>", paste0(input$dccJob, ".dcc.log"))
        system(cmd, wait = FALSE)
        # output$end <- renderUI(
        #     strong("The calculation is finished!")
        # )
    })

    observeEvent(input$stopDcc, {
        rvDcc$started <- FALSE
        system2("rm", "*.dcc.log")
        updateButton(session, ns("stopDcc"), disabled = TRUE)
    })

    observe({
        rvDcc$timer()
        if (isolate(rvDcc$started)) {
            if (file.exists(paste0(input$dccJob, ".dcc.log"))) {
                rvDcc$textstream <- suppressWarnings(
                    readLines(paste0(input$dccJob, ".dcc.log"),  n = -1) %>%
                        tail(10) %>% paste(collapse = "\n")
                )
            }
        }
    })
    output$dccLog <- renderText({
        rvDcc$textstream
    })

    # finishing msg ==========================================================
    output$end.ui <- renderUI({
        req(input$submit)
        paste("Your output will be saved at: ", getOutputPath())
    })
}