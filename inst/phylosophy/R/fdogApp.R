#' fdog module

fdogAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for fdog input/options -----------------
        sidebarPanel(
            width = 3,
            # ** fdog location ===================================
            conditionalPanel(
                condition = "output.checkFdogStatus == 0", ns = ns,
                h2(em("fDOG not found! Please install it first!")),
                bsButton(
                    "installFdog", "Install fDOG", 
                    onclick = "window.open('https://bionf.github.io/fDOG/#how-to-install', '_blank')"
                ),
                hr()
            ),
            
            h3("Input and configurations"),
            
            selectInput(
                ns("inputType"),
                "",
                choices = c("Run with single seed", "Run with multiple seeds"),
                selected = "Run with single seed"
            ),
            
            # ** required options ==================================
            strong("Required options"),
            
            conditionalPanel(
                condition = 'input.inputType == "Run with single seed"',ns = ns,
                shinyFilesButton(
                    ns("fdogInput"), "Input fasta file!" ,
                    title = "Please provide fasta input file:",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("input.ui")),
            ),
            
            conditionalPanel(
                condition ='input.inputType == "Run with multiple seeds"',ns=ns,
                shinyDirButton(
                    ns("fdogInputDir"), "Input seed directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("fdogInputDir.ui"))
            ),
            br(),
            
            uiOutput(ns("refSpec.ui")),
            bsPopover(
                ns("refSpec"),
                "",
                paste(
                    "Determine the reference species for the",
                    "ortholog search. It should be the species the",
                    "seed sequence was derived from."
                ),
                "bottom"
            ),
            
            shinyDirButton(
                ns("outFdogDir"), "Output directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            bsPopover(
                ns("outFdogDir"),
                "",
                paste(
                    "Provide output directory"
                ),
                "top"
            ),
            uiOutput(ns("outFdogDir.ui")),
            br(),
            
            uiOutput(ns("seqName.ui")),
            bsPopover(
                ns("seqName"),
                "",
                paste(
                    "Specifies a name for the search. If not",
                    "defined, a random name will be set."
                ),
                "bottom"
            ),
            bsButton(ns("newSeqName.btn"), "New job name"),
            hr(),
            
            # ** FAS option ========================================
            strong("FAS options"),
            checkboxInput(
                ns("useFAS"),
                strong("Use FAS"),
                value = TRUE,
                width = NULL
            ),
            bsPopover(
                ns("useFAS"),
                "",
                paste(
                    "Turn on/off feature architecture similarity calculation"
                ),
                "right"
            ),
            
            conditionalPanel(
                condition = "input.useFAS", ns = ns,
                checkboxInput(
                    ns("bidirectionalFAS"),
                    strong("Calculate FAS in both directions"),
                    value = TRUE,
                    width = NULL
                ),
                
                selectInput(
                    ns("coreFilter"),
                    strong("FAS filter for core compilation"),
                    choices = c("none", "relaxed", "strict"),
                    selected = "none"
                ),
                bsPopover(
                    ns("coreFilter"),
                    "",
                    paste(
                        "Specifiy mode for filtering core orthologs by FAS",
                        "score. In 'relaxed' mode candidates with insufficient",
                        "FAS score will be disadvantaged. In 'strict' mode",
                        "candidates with insufficient FAS score will be deleted",
                        "from the candidates list."
                    ),
                    "top"
                ),
                
                conditionalPanel(
                    condition = 'input.coreFilter != "none"', ns = ns,
                    numericInput(
                        ns("minScore"), strong("Threshold for coreFilter"),
                        value = 0.75,
                        min = 0,
                        max = 1
                    )
                )
            ),
            hr(),
            
            # ** optional options ==================================
            checkboxInput(
                ns("optOption"),
                strong("Other options"),
                value = FALSE,
                width = NULL
            ),
            
            conditionalPanel(
                condition = "input.optOption", ns = ns,
                # *** Paths options ============================================
                bsButton("cusPath", "Customize data paths"),
                br(),
                
                # *** I/O options ==============================================
                strong("Other I/O options"),
                checkboxInput(
                    ns("force"),
                    strong("Force mode"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("force"),
                    "",
                    paste(
                        "Force the final ortholog search to create",
                        "output file. Existing files will be",
                        "overwritten."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("append"),
                    strong("Append output"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("append"),
                    "",
                    paste(
                        "Set this flag to append the output to",
                        "existing output files"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("cleanup"),
                    strong("Clean up tmp files"),
                    value = TRUE,
                    width = NULL
                ),
                bsPopover(
                    ns("cleanup"),
                    "",
                    paste(
                        "Temporary output will be deleted."
                    ),
                    "bottom"
                ),
                
                textInput(
                    ns("group"), strong("Limit search in systematic group"),
                    value = ""
                ),
                bsPopover(
                    ns("group"),
                    "",
                    paste(
                        "Allows to limit the search to a certain",
                        "systematic group"
                    ),
                    "bottom"
                ),
                
                # checkboxInput(
                #     ns("blast"),
                #     strong("Auto determine seqID and refspec"),
                #     value = FALSE,
                #     width = NULL
                # ),
                # bsPopover(
                #     ns("blast"),
                #     "",
                #     paste(
                #         "Set this flag to determine sequence id",
                #         "and refspec automatically. Note, the",
                #         "chosen sequence id and reference species",
                #         "does not necessarily reflect the species",
                #         "the sequence was derived from."
                #     ),
                #     "bottom"
                # ),
                
                # *** Core compilation options =================================
                strong("Core compilation options"),
                
                selectInput(
                    ns("coreOpt"),
                    strong(""),
                    choices = c("Compile core set", "Reuse existing core-set"),
                    selected = "Compile core set"
                ),
                
                conditionalPanel(
                    condition = "input.coreOpt == 'Compile core set'", ns=ns,
                    checkboxInput(
                        ns("coreOnly"),
                        strong("Compile core only, don't search for orthologs"),
                        value = FALSE,
                        width = NULL
                    ),
                    bsPopover(
                        ns("coreOnly"),
                        "",
                        paste(
                            "Set this flag to compile only the core",
                            "orthologs. These sets can later be used",
                            "for a stand alone ortholog search."
                        ),
                        "bottom"
                    ),
                    selectInput(
                        ns("minDist"), strong("Min systematic distance"),
                        choices = c(
                            "strain",
                            "species",
                            "genus",
                            "family",
                            "order",
                            "class",
                            "phylum",
                            "kingdom",
                            "superkingdom"
                        ),
                        selected = "genus"
                    ),
                    bsPopover(
                        ns("minDist"),
                        "",
                        paste(
                            "Specify the minimum systematic distance of",
                            "primer taxa for the core set compilation."
                        ),
                        "top"
                    ),
                    
                    selectInput(
                        ns("maxDist"), strong("Max systematic distance"),
                        choices = c(
                            "strain",
                            "species",
                            "genus",
                            "family",
                            "order",
                            "class",
                            "phylum",
                            "kingdom",
                            "superkingdom"
                        ),
                        selected = "kingdom"
                    ),
                    bsPopover(
                        ns("maxDist"),
                        "",
                        paste(
                            "Specify the maximum systematic distance of",
                            "primer taxa fto be considered for core set",
                            "compilation."
                        ),
                        "top"
                    ),
                    
                    numericInput(
                        ns("coreOrth"), strong("Number of core orthologs"),
                        value = 5,
                        min = 3,
                        max = 99
                    ),
                    bsPopover(
                        ns("coreOrth"),
                        "",
                        paste(
                            "Specify the number of orthologs added to the",
                            "core set."
                        ),
                        "top"
                    ),
                    
                    uiOutput(ns("coreTaxa.ui")),
                    bsPopover(
                        ns("coreTaxa"),
                        "",
                        paste(
                            "You can provide a list of primer taxa",
                            "that should exclusively be used for the",
                            "compilation of the core ortholog set."
                        ),
                        "bottom"
                    ),
                    
                    checkboxInput(
                        ns("coreStrict"),
                        strong("Strict mode for core set"),
                        value = FALSE,
                        width = NULL
                    ),
                    bsPopover(
                        ns("coreStrict"),
                        "",
                        paste(
                            "Run the compilation of the core set in strict mode."
                        ),
                        "bottom"
                    ),
                    
                    checkboxInput(
                        ns("corecheckCoorthologsRef"),
                        strong("Check co-orthologs for core set"),
                        value = FALSE,
                        width = NULL
                    ),
                    bsPopover(
                        ns("corecheckCoorthologsRef"),
                        "",
                        paste(
                            "Invokes the checkCoorthologsRef",
                            "behavior in the course of the core set",
                            "compilation."
                        ),
                        "bottom"
                    ),
                    
                    checkboxInput(
                        ns("coreRep"),
                        strong("Only most similar sequence for core set"),
                        value = FALSE,
                        width = NULL
                    ),
                    bsPopover(
                        ns("coreRep"),
                        "",
                        paste(
                            "Set this flag to invoke the -rep",
                            "behaviour for the core ortholog",
                            "compilation."
                        ),
                        "bottom"
                    ),
                    
                    numericInput(
                        ns("coreHitLimit"),
                        strong("Number of HMM hits for core set"),
                        value = 3,
                        min = 0,
                        max = 999,
                        step = 1
                    ),
                    bsPopover(
                        ns("coreHitLimit"),
                        "",
                        paste(
                            "Provide an integer specifying the number",
                            "of hits of the initial pHMM based search",
                            "that should be evaluated via a reverse",
                            "search. Default: 3"
                        ),
                        "bottom"
                    ),
                    
                    numericInput(
                        ns("distDeviation"),
                        strong("Distance deviation"),
                        value = 1,
                        min = 0,
                        max = 1,
                        step = 0.01
                    ),
                    bsPopover(
                        ns("distDeviation"),
                        "",
                        paste(
                            "Specify the deviation in score in percent",
                            "(1=100%, 0=0%) allowed for two taxa to be",
                            "considered similar"
                        ),
                        "bottom"
                    )
                ),
                
                # *** Ortholog search options ==================================
                strong("Ortholog search options"),
                
                checkboxInput(
                    ns("strict"),
                    strong("Strict mode"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("strict"),
                    "",
                    paste(
                        "Run the final ortholog search in strict",
                        "mode. An ortholog is only then accepted",
                        "when the reciprocity is fulfilled for",
                        "each sequence in the core set."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("checkCoorthologsRef"),
                    strong("Check co-orthologs"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("checkCoorthologsRef"),
                    "",
                    paste(
                        "During the final ortholog search, accept",
                        "an ortholog also when its best hit in",
                        "the reverse search is not the core",
                        "ortholog itself, but a co-ortholog of it."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("rbh"),
                    strong("Reciprocal best hit"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("rbh"),
                    "",
                    paste(
                        "Requires a reciprocal best hit during the",
                        "ortholog search to accept a new ortholog."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("rep"),
                    strong("Only most similar sequence"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("rep"),
                    "",
                    paste(
                        "Set this flag to obtain only the sequence",
                        "being most similar to the corresponding",
                        "sequence in the core set rather than all",
                        "putative co-orthologs."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("ignoreDistance"),
                    strong("Ignore taxon distance"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("ignoreDistance"),
                    "",
                    paste(
                        "Set this flag to ignore the distance",
                        "between taxa and to choose orthologs",
                        "only based on score"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("lowComplexityFilterOff"),
                    strong("Turn off low complexity filter"),
                    value = FALSE,
                    width = NULL
                ),
                
                numericInput(
                    ns("evalBlast"),
                    strong("Blast e-value cutoff (10^x)"),
                    value = -5,
                    min = -99,
                    max = 0,
                    step = 1
                ),
                bsPopover(
                    ns("evalBlast"),
                    "",
                    paste(
                        "This option allows to set the e-value",
                        "cut-off for the Blast search.",
                        "Default: 10^-5"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("evalHmmer"),
                    strong("HMM search e-value cutoff (10^x)"),
                    value = -5,
                    min = -99,
                    max = 0,
                    step = 1
                ),
                bsPopover(
                    ns("evalHmmer"),
                    "",
                    paste(
                        "This option allows to set the e-value",
                        "cut-off for the HMM search.",
                        "Default: 10^-5"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("evalRelaxfac"),
                    strong("Relax factor for e-value cutoff"),
                    value = 10,
                    min = 1,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("evalRelaxfac"),
                    "",
                    paste(
                        "This options allows to set the factor to",
                        "relax the e-value cut-off (Blast search",
                        "and HMM search) for the final ortholog run.",
                        "Default: 10"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("hitLimit"),
                    strong("Number of HMM hits"),
                    value = 10,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("hitLimit"),
                    "",
                    paste(
                        "Provide an integer specifying the number",
                        "of hits of the initial pHMM based search",
                        "that should be evaluated via a reverse",
                        "search. Default: 10"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("autoLimit"),
                    strong("Auto HMM hit limit"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("autoLimit"),
                    "",
                    paste(
                        "Setting this flag will invoke a lagPhase",
                        "analysis on the score distribution from",
                        "the hmmer search. This will determine",
                        "automatically a hit limit for each query.",
                        "Note, when setting this flag, it will be",
                        "effective for both the core ortholog",
                        "compilation and the final ortholog search."
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("scoreThreshold"),
                    strong("Threshold for HMM score"),
                    value = 10,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("scoreThreshold"),
                    "",
                    paste(
                        "Instead of setting an automatic hit",
                        "limit, you can specify with this flag",
                        "that only candidates with an hmm score no",
                        "less than x percent of the hmm score of",
                        "the best hit are further evaluated.",
                        "Default is x = 10. You can change this",
                        "cutoff with the option -scoreCutoff.",
                        "Note, when setting this flag, it will be",
                        "effective for both the core ortholog",
                        "compilation and the final ortholog search."
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("scoreCutoff"),
                    strong("Percent range of the hmm score"),
                    value = 10,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("scoreCutoff"),
                    "",
                    paste(
                        "In combination with -scoreThreshold you",
                        "can define the percent range of the hmms",
                        "core of the best hit up to which a",
                        "candidate of the hmmsearch will be",
                        "subjected for further evaluation.",
                        "Default: 10%"
                    ),
                    "bottom"
                ),
                
                selectInput(
                    ns("aligner"), "Alignment tool",
                    choices = c(
                        "mafft-linsi",
                        "muscle"
                    ),
                    selected = "muscle"
                ),
                bsPopover(
                    ns("aligner"),
                    "",
                    paste(
                        "Choose between mafft-linsi or muscle for",
                        "the multiple sequence alignment.",
                        "DEFAULT: muscle"
                    ),
                    "top"
                ),
                
                selectInput(
                    ns("alignStrategy"), "Alignment strategy",
                    choices = c(
                        "local",
                        "glocal",
                        "global"
                    ),
                    selected = "local"
                ),
                bsPopover(
                    ns("alignStrategy"),
                    "",
                    paste(
                        "Choose alignment strategy for core ortholog",
                        "compilation. DEFAULT: local"
                    ),
                    "top"
                ),
                
                
                # *** Other options ===========================================
                strong("Other options"),
                
                numericInput(
                    ns("cpu"),
                    strong("Number of CPUs"),
                    value = 4,
                    min = 1,
                    max = 99,
                    step = 1
                ),
                bsPopover(
                    ns("cpu"),
                    "",
                    paste(
                        "Determine the number of threads to be run",
                        "in parallel"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("silent"),
                    strong("Show more output to terminal"),
                    value = FALSE,
                    width = NULL
                ),
                
                
            ),

            # ** bsModal for setting customized paths ==========================
            bsModal(
                "setPath",
                "Customize data & output paths",
                "cusPath",
                
                shinyDirButton(
                    ns("blastDir"), "BlastDB directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("blastDir"),
                    "",
                    paste(
                        "Provide path to Blast DBs directory.",
                        "Reference species and species in the core set must",
                        "be present in this directory!"
                    ),
                    "top"
                ),
                uiOutput(ns("blastDir.ui")),
                br(),
                
                shinyDirButton(
                    ns("genomeDir"), "Genome directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("genomeDir"),
                    "",
                    paste(
                        "Provide path to genomes directory,",
                        "where search/query species can be found."
                    ),
                    "top"
                ),
                uiOutput(ns("genomeDir.ui")),
                br(),
                
                shinyDirButton(
                    ns("weightDir"), "Genome annotation directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("weightDir"),
                    "",
                    paste(
                        "Provide path to weight_dir directory,",
                        "where feature annotations can be found."
                    ),
                    "top"
                ),
                uiOutput(ns("weightDir.ui")),
                br(),
                
                shinyDirButton(
                    ns("coreDir"), "Core ortholog directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("coreDir"),
                    "",
                    paste(
                        "Provide path to core_orthologs directory,",
                        "where the core set will be saved or can be found."
                    ),
                    "top"
                ),
                uiOutput(ns("coreDir.ui"))
            )
        ),
        # * main panel for fdog run ----------------------------
        mainPanel(
            width = 9,
            conditionalPanel(
                condition = "output.checkRun", ns = ns,
                bsButton(
                    ns("doFdog"), "Run fdog",
                    style = "success", disabled = FALSE
                ),
                actionButton(ns("stopFdog"),label = "Stop"),
                actionButton(ns("newFdog"),label = "New fdog job"),
                uiOutput(ns("fdogLocation")),
                conditionalPanel(
                    condition = "input.useFAS", ns = ns,
                    uiOutput(ns("fasLocation.ui"))
                ),
                hr(),
                checkboxInput(
                    ns("showOpts"),
                    strong("Show selected options"),
                    value = FALSE,
                    width = NULL
                ),
                conditionalPanel(
                    condition = "input.showOpts", ns = ns,
                    strong("SELECTED OPTIONS"),
                    br(), br(),
                    strong("Required"),
                    br(), br(),
                    uiOutput(ns("reqOptions.ui")),
                    br(), br(),
                    strong("Optional"),
                    br(), br(),
                    uiOutput(ns("optOptions.ui")),
                    hr()
                ),
                strong("Command"),
                verbatimTextOutput(ns("fdogCmdText")),
                strong("Log file"),
                textOutput(ns("logLocation")),
                br(),
                strong("Output files"),
                checkboxInput(
                    ns("outputDetail"), "Show detailed output files", value = FALSE
                ),
                uiOutput(ns("output.ui")),
                hr(),
                strong("Progress"),
                verbatimTextOutput(ns("fdogLog"))
            )
        )
    )
}

fdogApp <- function(input, output, session, nameFullDf) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns

    # get fdog location ======================================================
    output$checkFdogStatus <- reactive({
        fdogLocation <- suppressWarnings(
            system("which fdog.run", intern = TRUE)
        )
        if (!is.na (fdogLocation[1])){
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkFdogStatus", suspendWhenHidden = FALSE)
    
    getFdogPath <- function() {
        datapath <- system("fdog.setup -o ~/ --getDatapath", intern = TRUE)
        if (length(datapath) > 0) {
            return(datapath)
        } else {
            return(NULL)
        }
    }
    
    output$fdogLocation <- renderUI({
        fdogPath <- getFdogPath()
        if (!is.null(fdogPath)) 
            paste("Default data path of fdog found at", fdogPath)
    })
    
    # get fas location =========================================================
    output$checkFasStatus <- reactive({
        fasLocation <- suppressWarnings(
            system("which calcFAS", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkFasStatus", suspendWhenHidden = FALSE)
    
    getFasPath <- reactive({
        fasLocation <- suppressWarnings(
            system("which calcFAS", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(fasLocation[1])
        } else {
            return(NULL)
        }
    })
    
    output$fasLocation.ui <- renderUI({
        fasPath <- getFasPath()
        if (!is.null(fasPath)) paste("calcFAS found at", fasPath)
    })
    
    # get input fasta ==========================================================
    getInputFile <- reactive({
        shinyFileChoose(
            input, "fdogInput", session = session, roots = homePath, 
            filetypes = c('', 'fa', 'fasta')
        )
        req(input$fdogInput)
        fileSelected <- parseFilePaths(homePath, input$fdogInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$input.ui <- renderUI({
        req(getInputFile())
        if (length(getInputFile()) > 0) {
            outString <- getInputFile()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # get input folder =========================================================
    getInputDir <- reactive({
        shinyDirChoose(
            input, "fdogInputDir", roots = homePath, session = session
        )
        inputPath <- parseDirPath(homePath, input$fdogInputDir)
        return(replaceHomeCharacter(as.character(inputPath)))
    })
    output$fdogInputDir.ui <- renderUI({
        req(getInputDir())
        if (length(getInputDir()) > 0) {
            outString <- getInputDir()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # get list of available refspec ============================================
    getRefspecList <- reactive ({
        # get spec ID from blast_dir
        outDir <- paste0(getFdogPath(), "/blast_dir")
        outDirPath <- list.dirs(
            path = outDir, full.names = TRUE, recursive = FALSE
        )
        outDirTaxIds <- stringr::str_replace(
            outDirPath, paste0(outDir,"/"), ""
        )
        ids <- str_replace_all(str_match(outDirTaxIds, "@.+@"), "@", "")
        blastDf <- data.frame(
            "abbrName" = outDirTaxIds, "ncbiID" = ids
        )
        # get taxon fullnames
        refSpecDf <- merge(blastDf, nameFullDf, by = "ncbiID", all.x = TRUE)
        refSpecDf$fullName[is.na(refSpecDf$fullName)] <- 
            refSpecDf$abbrName[is.na(refSpecDf$fullName)]
        return(refSpecDf)
    })
    
    output$refSpec.ui <- renderUI({
        refspecList <- c("undefined")
        refspecList <- getRefspecList()
        selectInput(
            ns("refSpec"), "Reference species",
            choices = c("undefined", refspecList$fullName),
            selected = "undefined"
        )
    })
    
    output$coreTaxa.ui <- renderUI({
        coreTaxaList <- "all"
        coreTaxaList <- getRefspecList()
        selectInput(
            ns("coreTaxa"), strong("Core taxa"),
            choices = c("all", coreTaxaList$fullName),
            selected = "all",
            multiple = TRUE
        )
    })
    
    # get customized paths =====================================================
    getBlastPath <- reactive({
        shinyDirChoose(
            input, "blastDir", roots = homePath, session = session
        )
        blastPath <- parseDirPath(homePath, input$blastDir)
        return(replaceHomeCharacter(as.character(blastPath)))
    })
    output$blastDir.ui <- renderText({
        if (length(getBlastPath()) > 0) getBlastPath()
        else paste0(getFdogPath(), "/blast_dir")
    })
    
    getGenomePath <- reactive({
        shinyDirChoose(
            input, "genomeDir", roots = homePath, session = session
        )
        genomePath <- parseDirPath(homePath, input$genomeDir)
        return(replaceHomeCharacter(as.character(genomePath)))
    })
    output$genomeDir.ui <- renderText({
        if (length(getGenomePath()) > 0) getGenomePath()
        else paste0(getFdogPath(), "/genome_dir")
    })
    
    getWeightPath <- reactive({
        shinyDirChoose(
            input, "weightDir", roots = homePath, session = session
        )
        weightPath <- parseDirPath(homePath, input$weightDir)
        return(replaceHomeCharacter(as.character(weightPath)))
    })
    output$weightDir.ui <- renderText({
        if (length(getWeightPath()) > 0) getWeightPath()
        else paste0(getFdogPath(), "/weight_dir")
    })
    
    getCorePath <- reactive({
        shinyDirChoose(
            input, "coreDir", roots = homePath, session = session
        )
        corePath <- parseDirPath(homePath, input$coreDir)
        return(replaceHomeCharacter(as.character(corePath)))
    })
    output$coreDir.ui <- renderText({
        if (length(getCorePath()) > 0) getCorePath()
        else paste0(getFdogPath(), "/core_orthologs")
    })
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outFdogDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outFdogDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    output$outFdogDir.ui <- renderUI({
        if (length(getOutputPath()) > 0) {
            if (input$inputType == "Run with single seed") {
                outString <- paste0(getOutputPath(), "/", input$seqName)
            } else {
                outString <- getOutputPath()
            }
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        } else {
            return()
        }
    })
    
    
    # required options =========================================================
    reqOptions <- reactive({
        if (input$inputType == "Run with single seed") {
            req(getInputFile())
            seqFile <- paste("--seqFile", getInputFile())
            seqName <- ""
            if (!is.null(input$seqName)) {
                seqName <- paste("--seqName", input$seqName)
            }
            refSpecDf <- getRefspecList()
            refSpec <- paste(
                "--refspec", refSpecDf$abbrName[refSpecDf$fullName == input$refSpec]
            )
            return(
                c(seqFile, seqName, refSpec)
            )
        } else {
            req(getInputDir())
            seqFile <- paste("--input", getInputDir())
            jobName <- ""
            if (!is.null(input$seqName)) {
                jobName <- paste("--jobName", input$seqName)
            }
            refSpecDf <- getRefspecList()
            refSpec <- paste(
                "--refspec", refSpecDf$abbrName[refSpecDf$fullName == input$refSpec]
            )
            return(
                c(seqFile, jobName, refSpec)
            )
        }
    })
    
    output$reqOptions.ui <- renderUI({
        HTML(paste(reqOptions(), collapse = "<br/>"))
    })
    
    # optional options =========================================================
    optOptions <- reactive({
        minDist <- ""
        if (input$minDist != "genus")
            minDist <- paste("--minDist", input$minDist)
        
        maxDist <- ""
        if (input$maxDist != "kingdom")
            maxDist <- paste("--maxDist", input$maxDist)
        
        coreOrth <- ""
        if (input$coreOrth != 5)
            coreOrth <- paste("--coreOrth", input$coreOrth)
        
        outpath <- ""
        if (length(getOutputPath()) > 0) {
            outpath <- paste("--outpath", getOutputPath())
        }
        
        blastpath <- ""
        if (length(getBlastPath()) > 0) {
            blastpath <- paste("--blastpath", getBlastPath())
        }
        
        searchpath <- ""
        if (length(getGenomePath()) > 0) {
            searchpath <- paste("--searchpath", getGenomePath())
        }
        
        hmmpath <- ""
        if (length(getCorePath()) > 0) {
            hmmpath <- paste("--hmmpath", getCorePath())
        }
        
        weightpath <- ""
        if (length(getWeightPath()) > 0) {
            weightpath <- paste("--weightpath", getWeightPath())
        }
        
        fasoff <- ""
        if (input$useFAS == FALSE) {
            fasoff <- "--fasoff"
        }
        
        counterCheck <- ""
        if (input$useFAS == TRUE) {
            if (input$bidirectionalFAS == TRUE) {
                counterCheck <- "--countercheck"
            }
        }
        
        refSpecDf <- getRefspecList()
        coreTaxa <- ""
        if(input$optOption == TRUE && !is.null(input$coreTaxa)) {
            if (!("all" %in% input$coreTaxa)) {
                coreTaxa <- paste(
                    "--coreTaxa", 
                    paste(
                        refSpecDf$abbrName[refSpecDf$fullName %in% input$coreTaxa],
                        collapse = ","
                    )
                )
            }
        } 
        
        strict <- ""
        if (input$strict == TRUE) {
            strict <- "--strict"
        }
        coreStrict <- ""
        if (input$coreStrict == TRUE) {
            coreStrict <- "--coreStrict"
        }
        checkCoorthologsRef <- ""
        if (input$checkCoorthologsRef == TRUE) {
            checkCoorthologsRef <- "--checkCoorthologsRef"
        }
        corecheckCoorthologsRef <- ""
        if (input$corecheckCoorthologsRef == TRUE) {
            corecheckCoorthologsRef <- "--CorecheckCoorthologsRef"
        }
        
        rbh <- ""
        if (input$rbh == TRUE) {
            rbh <- "--rbh"
        }
        rep <- ""
        if (input$rep == TRUE) {
            rep <- "--rep"
        }
        coreRep <- ""
        if (input$coreRep == TRUE) {
            coreRep <- "--coreRep"
        }
        
        coreOnly <- ""
        if (input$coreOnly == TRUE) {
            coreOnly <- "--coreOnly"
        }
        reuseCore <- ""
        if (input$coreOpt == "Reuse existing core-set") {
            reuseCore <- "--reuseCore"
        }
        
        coreFilter <- ""
        minScore <- ""
        if (input$coreFilter != "none") {
            coreFilter <- paste("--coreFilter", input$coreFilter)
            if (input$minScore != 0.75) {
                minScore <- paste("--minScore", input$minScore)
            }
        }
        
        # blast <- ""
        # if (input$blast == TRUE) {
        #     blast <- "--blast"
        # }
        
        group <- ""
        if (input$group != "") {
            group <- paste("--group", input$group)
        }
        
        evalBlast <- ""
        if (input$evalBlast != "-5") {
            evalBlast <- paste("--evalBlast", 10^input$evalBlast)
        }
        evalHmmer <- ""
        if (input$evalHmmer != "-5") {
            evalHmmer <- paste("--evalHmmer", 10^input$evalHmmer)
        }
        evalRelaxfac <- ""
        if (input$evalRelaxfac != 10) {
            evalRelaxfac <- paste("--evalRelaxfac", input$evalRelaxfac)
        }
        
        hitLimit <- ""
        if (input$hitLimit != "10") {
            hitLimit <- paste("--hitLimit", input$hitLimit)
        }
        coreHitLimit <- ""
        if (input$coreHitLimit != "3") {
            coreHitLimit <- paste("--coreHitLimit", input$coreHitLimit)
        }
        
        autoLimit <- ""
        if (input$autoLimit == TRUE) {
            autoLimit <- "--autoLimit"
        }
        
        scoreThreshold <- ""
        if (input$scoreThreshold != "10") {
            scoreThreshold <- paste("--scoreThreshold", input$scoreThreshold)
        }
        scoreCutoff <- ""
        if (input$scoreCutoff != "10") {
            scoreCutoff <- paste("--scoreCutoff", input$scoreCutoff)
        }
        
        ignoreDistance <- ""
        if (input$ignoreDistance == TRUE) {
            ignoreDistance <- "--ignoreDistance"
        }
        
        lowComplexityFilterOff <- ""
        if (input$lowComplexityFilterOff) {
            lowComplexityFilterOff <- "--lowComplexityFilterOff"
        }
        
        distDeviation <- ""
        if (input$distDeviation != "1") {
            distDeviation <- paste("--distDeviation", input$distDeviation)
        }
        
        cpu <- ""
        if (input$cpu != "4") {
            cpu <- paste("--cpu", input$cpu)
        }
        aligner <- ""
        if (input$aligner != "muscle") {
            aligner <- paste("--aligner", input$aligner)
        }
        alignStrategy <- ""
        if (input$alignStrategy != "local") {
            alignStrategy <- paste0("--", input$alignStrategy)
        }
        
        force <- ""
        if (input$force == TRUE) {
            force <- "--force"
        }
        append <- ""
        if (input$append == TRUE) {
            append <- "--append"
        }
        silent <- ""
        if (input$silent == TRUE) {
            silent <- "--silentOff"
        }
        cleanup <- ""
        if (input$cleanup == TRUE) {
            cleanup <- "--cleanup"
        }
        
        optOptionList <- c(
            minDist, maxDist, coreOrth,
            outpath, blastpath, searchpath, hmmpath, weightpath, fasoff,
            coreTaxa, strict, coreStrict, lowComplexityFilterOff,
            checkCoorthologsRef, corecheckCoorthologsRef, rbh, rep, coreRep, 
            coreOnly, reuseCore, group, evalBlast, evalHmmer, 
            evalRelaxfac, hitLimit, coreHitLimit, autoLimit, scoreThreshold, 
            scoreCutoff,ignoreDistance, distDeviation, cpu, aligner, 
            alignStrategy, force, append, silent, cleanup, counterCheck,
            coreFilter, minScore
        )
        return(
            optOptionList[unlist(lapply(optOptionList, function (x) x != ""))]
        )
    })
    
    output$optOptions.ui <- renderUI({
        HTML(paste(optOptions(), collapse = "<br/>"))
    })
    
    # generate new job ID ======================================================
    output$seqName.ui <- renderUI({
        jobID <- randFn(1)
        textInput(ns("seqName"), "Job name", value = jobID)
    })
    
    observeEvent(input$newSeqName.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "seqName", "Job name", value = jobID)
    })
    
    # RUN fDOG ===============================================================
    output$checkRun <- reactive({
        if (length(getOutputPath()) == 0) {
            return(FALSE)
        } else {
            if (!is.null(input$refSpec) | input$refSpec != "undefined") {
                if (length(reqOptions()) == 3) {
                    return(TRUE)
                }
            }
        }
        return(FALSE)
    })
    outputOptions(output, "checkRun", suspendWhenHidden = FALSE)
    
    observeEvent(input$newFdog, {
        updateButton(session, ns("doFdog"), disabled = FALSE)
        updateButton(session, ns("stopFdog"), disabled = FALSE)
    })
    
    fdogCmd <- reactive({
        fdog <- "fdogs.run"
        if (input$inputType == "Run with single seed") {
            fdog <- "fdog.run"
        }
        return(
            paste(
                fdog,
                paste(reqOptions(), collapse = " "),
                paste(optOptions(), collapse = " ")
            )
        )
    })
    
    output$fdogCmdText <- renderText({
        paste(fdogCmd())
    })
    
    rv <- reactiveValues(
        textstream = c(""),
        timer = reactiveTimer(1000),
        started = FALSE
    )
    observeEvent(input$doFdog, {
        rv$started <- TRUE
        cmd <- paste(
            fdogCmd(),
            ">>",
            paste0(input$seqName, ".fdog.log")
        )
        system(cmd, wait = FALSE)
        updateButton(session, ns("doFdog"), disabled = TRUE)
        updateButton(session, ns("newSeqName.btn"), disabled = TRUE)
    })
    
    observeEvent(input$stopFdog, {
        rv$started <- FALSE
        system(paste("rm -rf", str_replace_all(returnOutput()[1], paste0("/", input$seqName, ".extended.fa"), "")))
        # system2("rm", "*.log")
        updateButton(session, ns("stopFdog"), disabled = TRUE)
    })
    
    observe({
        rv$timer()
        if (isolate(rv$started)) {
            if (file.exists(paste0(input$seqName, ".fdog.log"))) {
                rv$textstream <- suppressWarnings(
                    paste(
                        readLines(paste0(input$seqName, ".fdog.log"),  n = -1) %>% 
                            tail(25) %>% paste(collapse = "\n")
                    )
                )
            }
        }
    })
    output$fdogLog <- renderText({
        rv$textstream
    })
    
    # report results ===========================================================
    output$logLocation <- renderText({
        paste0(getwd(), "/", input$seqName, ".fdog.log")
    })
    
    returnOutput <- reactive({
        req(getFdogPath())
        if (input$inputType == "Run with single seed") {
            req(getInputFile())
            # get output path
            if (length(getOutputPath()) > 0) {
                outpath <- paste0(getOutputPath(), "/", input$seqName)
            } else {
                outpath <- paste0(getwd(), "/", input$seqName)
            }
        } else {
            req(getInputDir())
            # get output path
            if (length(getOutputPath()) > 0) {
                outpath <- getOutputPath()
            } else {
                outpath <- getwd()
            }
        }
        # return output files
        faOut <- paste0(outpath, "/", input$seqName, ".extended.fa")
        ppOut <- paste0(outpath, "/", input$seqName, ".phyloprofile")
        if (input$useFAS == TRUE) {
            domainFwOut <- paste0(outpath, "/", input$seqName, "_forward.domains")
            domainRevOut <- paste0(outpath, "/", input$seqName, "_reverse.domains")
            if (input$bidirectionalFAS  == TRUE) {
                return(c(faOut, ppOut, domainFwOut, domainRevOut))
            } else return(c(faOut, ppOut, domainFwOut))
            
        } else {
            return(c(faOut, ppOut, NULL))
        }
    })
    
    output$output.ui <- renderUI({
        if (input$outputDetail == TRUE) {
            HTML(paste(returnOutput(), sep = "<br/>"))
        } else {
            HTML(str_replace_all(returnOutput()[1], "extended.fa", "*"))
        }
    })
    
    # return
    return(returnOutput)
}
