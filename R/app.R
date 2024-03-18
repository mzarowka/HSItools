# shiny app

# choose capture file
# choose if it needs normalization - TRUE/FALSE (old/new specim)
# if needs normalization then choose if different times for capture and reference - TRUE/FALSE (needs two times/doesn't need)
# choose proxies
# normalize entire core or rois only (computation speed) -> depending on future needs (like full core classification?)
# should I write to script?
# draw and store large roi to crop raster
# draw and store small rois to actually extract data from
# select tube top and tube depth and read depths
# normalize data if needed
# extract rois
# normalize core or rois only
# calculate indices in rois
# smooth data
# recalculate depths
# write data-frames
# write plots with maps
# write entire core pseudocolor rgb
# write script

# showModal(modalDialog(
#   title = "odd",
#   "odd",
#   easyClose = TRUE,
#   footer = NULL
# ))


#' Run shiny app to set up core image analysis parameters
#'
#' @param autoSave save shiny app output to rds file. Defaults to true. Saves in current working directory.
#'
#' @import shiny
#' @import DT
#' @import shinyFiles
#' @importFrom shinyalert shinyalert
#' @return an object with processing settings.
#' @export
run_core <- function(autoSave = TRUE){

  allParams <- list()

  shiny::runApp(
  shiny::shinyApp(
  ui = shiny::fluidPage(
    tags$head(tags$style(HTML('* {font-family: "Georgia"};'))),
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    tags$script(HTML("
          $(document).ready(function() {setTimeout(function() {
            supElement = document.getElementById('scalermarkerPointSize').parentElement;
            $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
          }, 50);})
        ")),
    tags$script(HTML("
          $(document).ready(function() {setTimeout(function() {
            supElement = document.getElementById('scaleOrigImg').parentElement;
            $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
          }, 50);})
        ")),
    tags$script(HTML("
          $(document).ready(function() {setTimeout(function() {
            supElement = document.getElementById('scaleDistImg').parentElement;
            $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
          }, 50);})
        ")),
    tags$script(HTML("
          $(document).ready(function() {setTimeout(function() {
            supElement = document.getElementById('selectionSize').parentElement;
            $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
          }, 50);})
        ")),
    titlePanel("HSItools", windowTitle = "HSItools"),
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    id = "tabset1",
                    tabPanel("Analysis",
                             align="center",
                             br(),
                             headerPanel("Select the analysis methods and settings"),
                             br(),
                             br(),
                             br(),
                             br(),
                             shiny::fluidRow(
                               shiny::column(
                                 4,
                                 shiny::radioButtons("choice_normalize", "Do you need to normalize the data?", choices = list(Yes = "TRUE", No = "FALSE"))
                               ),
                               shiny::column(
                                 4,
                                 shiny::radioButtons("choice_integration", "Is your white reference scanned with different settings than core?", choices = list(Yes = "TRUE", No = "FALSE"), selected = "FALSE")
                               ),
                               shiny::column(
                                 4,
                                 shiny::selectInput("choice_proxies", "Choose proxies to calculate", choices = names(proxies), multiple = TRUE)
                               )
                             ),
                             shiny::fluidRow(
                               style = "text-align:center; font-weight:100;",
                               actionButton('begin','Save Selections and Proceed')
                             )
                    ),
                    tabPanel("Select Data",
                             align="center",
                             shiny::br(),
                             headerPanel("Select the core files and layers to utilize"),
                             br(),
                             br(),
                             br(),
                             br(),
                             uiOutput("file_select"),
                             uiOutput("file_show"),
                             shiny::br(),
                             uiOutput("table1"),
                             shiny::br(),
                             shiny::actionButton("proceed_with_data", "Proceed with selected data")

                    ),
                    tabPanel("Crop Image",
                               align="center",
                               shiny::br(),
                               headerPanel("Crop viable region of image (or skip to use full image)"),
                             br(),
                             br(),
                             br(),
                             br(),
                               shiny::column(
                                 6,
                                 align="left",
                                 actionButton("resetPlot", "Reset selection"),
                               ),
                               shiny::column(
                                 6,
                                 align="right",
                                 actionButton("selectPlotRegion", "Accept crop"),
                               ),
                             shiny::fluidRow(
                               sliderInput(
                                 inputId="scaleOrigImg",
                                 label="Size of Image",
                                 min=-2,
                                 max=2,
                                 value=0,
                                 step = 0.1,
                                 round = FALSE,
                                 ticks = FALSE,
                                 animate = FALSE,
                                 width = NULL,
                                 sep = ",",
                                 pre = NULL,
                                 post = NULL,
                                 timeFormat = NULL,
                                 timezone = NULL,
                                 dragRange = TRUE
                               ),
                             ),
                               shiny::br(),
                               shiny::verbatimTextOutput("color_warning"),
                               shiny::br(),
                                shinycssloaders::withSpinner(shiny::plotOutput(outputId = "core_plot",
                                                                            width = "100%",
                                                                            brush = brushOpts(
                                                                              id = "plotBrush",
                                                                              delay = 5000,
                                                                              fill = "black",
                                                                              stroke = "black",
                                                                              opacity = 0.4
                                                                            )
                                                                            )
                                                             ),
                             ),
                    tabPanel("Select Analysis Regions",
                             align="center",
                             br(),
                             headerPanel("Select specific regions to analyze (or skip to use analyze all)"),
                             br(),
                             br(),
                             br(),
                             br(),
                             mainPanel(
                               shiny::br(),
                               shiny::br(),
                               fixedRow(
                                 column(3,
                                        wellPanel(
                                          id = "Analysisregions",
                                          style = "height:600px; overflow-y: scroll; overflow-x:scroll; max-width: 800px",

                             actionButton("selectAnalysisRegion", "Add selected region"),
                             shiny::br(),
                             shiny::br(),
                             shiny::fluidRow(
                               shiny::column(
                                 6,
                                 "xmin",
                                 shiny::br(),
                                 shiny::verbatimTextOutput("xmin"),
                                 "ymin",
                                 shiny::verbatimTextOutput("ymin"),
                               ),
                               shiny::column(
                                 6,
                                 "xmax",
                                 shiny::br(),
                                 shiny::verbatimTextOutput("xmax"),
                                 "ymax",
                                 shiny::verbatimTextOutput("ymax"),
                               ),
                             ),
                             shiny::fixedRow(
                               align="center",
                               shiny::br(),
                               "Analysis Regions",
                               shiny::br(),
                               column(width=12,
                                DT::DTOutput("analysisRegions",width = "100%"),
                                style = "height:200px; overflow-y: scroll; overflow-x:visible"
                               ),
                             ),
                             shiny::br(),
                             actionButton("removeRegion", "Remove highlighted region"),
                             shiny::br(),
                             actionButton("acceptAnalysisRegions", "Accept all selections"),
                               ),
                               ),
                               column(
                                 5,
                                 wellPanel(
                                   id = "fullCorePanel",style = "height:600px; overflow-y: scroll; overflow-x:scroll; max-width: 800px",
                                 "Full Core",
                                 sliderInput(
                                   inputId="selectionSize2",
                                   label="Size of image",
                                   min=0.5,
                                   max=10,
                                   value=1,
                                   step = 0.5,
                                   round = FALSE,
                                   ticks = FALSE,
                                   animate = FALSE,
                                   width = NULL,
                                   sep = ",",
                                   pre = NULL,
                                   post = NULL,
                                   timeFormat = NULL,
                                   timezone = NULL,
                                   dragRange = TRUE
                                 ),
                             shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cropped_plot",
                                                                            inline = TRUE,
                                                                            brush = brushOpts(
                                                                              id = "plotBrush",
                                                                              delay = 5000,
                                                                              fill = "black",
                                                                              stroke = "black",
                                                                              opacity = 0.4
                                                                            )
                             )
                             ),
                             ),
                               ),
                             column(
                               4,
                               wellPanel(
                                 id = "currentselectionPanel",style = "height:600px; overflow-y: scroll; overflow-x:scroll; max-width: 1600px",
                               "Current Selection (Note image scrollbar below image)",
                               sliderInput(
                                 inputId="selectionSize",
                                 label="Size of image",
                                 min=0.5,
                                 max=10,
                                 value=1,
                                 step = 0.5,
                                 round = FALSE,
                                 ticks = FALSE,
                                 animate = FALSE,
                                 width = NULL,
                                 sep = ",",
                                 pre = NULL,
                                 post = NULL,
                                 timeFormat = NULL,
                                 timezone = NULL,
                                 dragRange = TRUE
                               ),
                             shinycssloaders::withSpinner(shiny::plotOutput(outputId = "selection_plot",
                                                                            inline = TRUE,
                             )
                             ),
                             ),
                             ),
                             ),
                             width = 12,
                             ),
                    ),
                    tabPanel("Distance Calibration",
                             align="center",
                             shiny::br(),
                             headerPanel("Choose the start and end points of the scale and sample"),
                             br(),
                             br(),
                             br(),
                             br(),
                             shiny::fluidRow(
                                 align="center",
                                 style = "text-align:center; font-weight:100;",
                                 shiny::column(
                                   2,
                             sliderInput(
                               inputId="scalermarkerPointSize",
                               label="Size of markers",
                               min=1,
                               max=5,
                               value=3,
                               step = 0.1,
                               round = FALSE,
                               ticks = FALSE,
                               animate = FALSE,
                               width = NULL,
                               sep = ",",
                               pre = NULL,
                               post = NULL,
                               timeFormat = NULL,
                               timezone = NULL,
                               dragRange = TRUE
                               ),
                             ),
                             shiny::column(
                               2,
                               sliderInput(
                                 inputId="scaleDistImg",
                                 label="Size of Image",
                                 min=-2,
                                 max=2,
                                 value=0,
                                 step = 0.1,
                                 round = FALSE,
                                 ticks = FALSE,
                                 animate = FALSE,
                                 width = NULL,
                                 sep = ",",
                                 pre = NULL,
                                 post = NULL,
                                 timeFormat = NULL,
                                 timezone = NULL,
                                 dragRange = TRUE
                               ),
                             ),
                             shiny::column(
                               2,
                               br(),
                               radioButtons("sampleOrScale",label = "Marker Selection",choices = c("Scale","Sample"),selected = "Scale"),
                             ),
                             shiny::column(
                               1,
                               style = "margin-top: 10px;",
                               "start scale (x,y)",
                               shiny::verbatimTextOutput("distCoordA"),
                               "end scale (x,y)",
                               shiny::verbatimTextOutput("distCoordB")
                             ),
                             # shiny::column(
                             #   1,
                             #   style = "margin-top: 10px;",
                             #   "end scale (x,y)",
                             #   shiny::verbatimTextOutput("distCoordB")
                             # ),

                             # shiny::column(
                             #   1,
                             #   style = "margin-top: 10px;",
                             #   "end core (x,y)",
                             #   shiny::verbatimTextOutput("distSamplePointB")
                             # ),
                             shiny::column(
                               2,
                               style = "margin-top: 10px;",
                               "distance along scale",
                               shiny::verbatimTextOutput("scaleDist"),
                               "distance along y axis",
                               shiny::verbatimTextOutput("coreDist")
                             ),
                             # shiny::column(
                             #   1,
                             #   style = "margin-top: 10px;",
                             #   "distance along y axis",
                             #   shiny::verbatimTextOutput("coreDist")
                             # ),
                             shiny::column(
                               2,
                               style = "margin-top: 10px;",
                               "scale bar (mm)",
                               numericInput(inputId = "scaleLength",
                                            step = 1,
                                            min = 1,
                                            label=NULL,
                                            max=10000,
                                            value = 1500),
                               "mm / pixel",
                               shiny::verbatimTextOutput("pixelRatio")
                             ),
                             # shiny::column(
                             #   1,
                             #   style = "margin-top: 10px;",
                             #   "mm / pixel",
                             #   shiny::verbatimTextOutput("pixelRatio")
                             # ),

                             shiny::column(
                               1,
                               style = "margin-top: 10px;",
                               "start sample (x,y)",
                               shiny::verbatimTextOutput("distSamplePointA"),
                               "end sample (x,y)",
                               shiny::verbatimTextOutput("distSamplePointB")
                             ),
                             ),
                             shiny::br(),
                             shiny::fluidRow(
                               align="center",
                               actionButton("done", "Accept distance calibration and Exit", style = "margin-right: 10px; margin-top:10px;"),
                             ),
                             shiny::br(),
                             shiny::plotOutput(outputId = "core_plot2",width = "100%", click="plot_click"),
                             shiny::br(),
                    )
                    )
  ),

  server = function(input, output, session) {

    session$onSessionEnded(function() {
      stopApp()
    })

    countRegions <- reactiveValues(count = 0)
    clickCounter <- reactiveValues(count=1)
    volumes <- c(shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "file_dir", roots = volumes)
    shinyFiles::shinyFileChoose(input, "ref_file", roots = volumes)

    useExample <- reactiveVal(FALSE)

    observeEvent(input$layerTable1_rows_selected,ignoreNULL = FALSE, {
      wavesA <- paste0(names(coreImage())[input$layerTable1_rows_selected],collapse = ", ")
      textA <- paste0("Wavelengths selected: ", wavesA)
      output$currentSelection <- renderText(textA)
    })


    #checkbox to select all layers
    dt_proxy <- DT::dataTableProxy("layerTable1")
    dt_proxy2 <- DT::dataTableProxy("analysisRegions")

    observeEvent(input$dt_sel, {
      custTableIndex(NULL)
      if (isTRUE(input$dt_sel) & isTRUE(input$halfRows)) {
        DT::selectRows(dt_proxy, seq(min(input$layerTable1_rows_all),max(input$layerTable1_rows_all),2))
      } else if (isTRUE(input$dt_sel) & isFALSE(input$halfRows)) {
        DT::selectRows(dt_proxy, input$layerTable1_rows_all)
      } else if (isFALSE(input$dt_sel) & isTRUE(input$halfRows)){
        DT::selectRows(dt_proxy, seq(min(input$layerTable1_rows_all),max(input$layerTable1_rows_all),2))
      } else {
        DT::selectRows(dt_proxy, NULL)
      }
    })
    #checkbox to select every other row
    observeEvent(input$halfRows, {
      custTableIndex(NULL)
      if (isTRUE(input$dt_sel) & isTRUE(input$halfRows)) {
        DT::selectRows(dt_proxy, seq(min(input$layerTable1_rows_all),max(input$layerTable1_rows_all),2))
      } else if (isTRUE(input$dt_sel) & isFALSE(input$halfRows)) {
        DT::selectRows(dt_proxy, input$layerTable1_rows_all)
      } else if (isFALSE(input$dt_sel) & isTRUE(input$halfRows)){
        DT::selectRows(dt_proxy, seq(min(input$layerTable1_rows_all),max(input$layerTable1_rows_all),2))
      } else {
        DT::selectRows(dt_proxy, NULL)
      }
    })

    user_datapath <- reactiveVal()

    user_file <- eventReactive(input$ref_file, {

      user_datapath(shinyFiles::parseFilePaths(volumes, selection = input$ref_file)$datapath)
      return(shinyFiles::parseFilePaths(volumes, selection = input$ref_file))
      #"C:/Users/dce25/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51"
    })
    #capture user directory
    # user_dir <- eventReactive(input$file_dir, {
    #   shinyFiles::parseDirPath(volumes, selection = input$file_dir)
    #   #"C:/Users/dce25/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51"
    # })
    user_dir <- reactiveVal()

    observeEvent(input$file_dir, {
      useExample(FALSE)
      user_dir(shinyFiles::parseDirPath(volumes, selection = input$file_dir))
    })
    #   dir1 <- shinyFiles::parseDirPath(volumes, selection = input$file_dir)
    #   print(dir1)
    #   user_dir(dir1)
    #   print(user_dir(dir1))
    #   files1 <- user_dir() |>
    #     fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)
    #   print(files1)
    #   rasters(files1)
    #   print(rasters(files1))
    #   coreImage(terra::rast(rasters(files1)))
    #   # coreInfo(c(paste0("width: ", ncol(coreImage()), " pixels"), paste0("height: ", nrow(coreImage()), " pixels"), paste0("layers: ", length(names(coreImage())))))
    # })



    observeEvent(input$file_dir_example, {
      useExample(TRUE)
      path1 <- file.path(system.file(package = "HSItools"), "extdata")
      user_dir(path1)
      # rasters(user_dir() |>
      #           fs::dir_ls(type = "file", regexp = ".tif", recurse = TRUE))
      # coreImage(terra::rast(user_file()$datapath))
      # coreInfo(NULL)
    })

    # user_dir <- eventReactive({input$file_dir
    #   input$file_dir_example
    #   }, {
    #   print(paste0("selected: ", input$file_dir))
    #   print(paste0("example: ", input$file_dir_example))
    #   if (!is.null(input$file_dir)){
    #     return(shinyFiles::parseDirPath(volumes, selection = input$file_dir))
    #   } else{
    #     return(paste0(getwd(),"/data"))
    #   }
    #   #"C:/Users/dce25/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51"
    # })

    observeEvent(input$proceed_with_data, {
      #print(input$layerTable1_rows_all)

      if (length(input$layerTable1_rows_selected) < 1){
        shinyalert::shinyalert(title = "No Layers", text = "Please choose at least 1 layer to proceed!")
      } else {

        allParams$layers <<- as.numeric(names(coreImage())[input$layerTable1_rows_selected])

        updateTabsetPanel(session=session,
                          "tabset1",
                          selected = "Crop Image")
      }
    })

    #print file path
    output$core_file_show <- renderTable({
      #if (length(user_file()) != 0){
        user_file()
      #}
    })

    #print directory
    output$core_dir_show <- renderPrint({
      #print(paste0("user_dir(): ", user_dir()))
      #if (length(user_dir()) != 0){
        user_dir()
      #}
    })

    # List raster files in the selected directory
    #rasters <- reactiveVal()
    rasters <- reactive({
      if (length(user_dir()) != 0 & !useExample()){
        user_dir() |>
          fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)
      } else if (length(user_dir()) != 0 & useExample()) {
        user_dir() |>
          fs::dir_ls(type = "file", regexp = ".tif", recurse = TRUE)
      } else {
        NULL
      }
      # else if (length(user_dir2()) != 0){
      #   user_dir() |>
      #     fs::dir_ls(type = "file", regexp = ".tif", recurse = TRUE)
      # }
      })

    files1 <- reactive({
      m <- rasterGuess(rasters())
      shinyFiles::shinyFileChoose(input, 'select_core',roots=shinyFiles::getVolumes())
      shinyFiles::shinyFileChoose(input, 'select_white',roots=shinyFiles::getVolumes())
      shinyFiles::shinyFileChoose(input, 'select_dark',roots=shinyFiles::getVolumes())
      if (length(input$select_core)>=2){
        m$capture <- unname(shinyFiles::parseFilePaths(roots = shinyFiles::getVolumes(), input$select_core)$datapath)
      }
      if (length(input$select_white)>=2){
        m$whiteref <- unname(shinyFiles::parseFilePaths(roots = shinyFiles::getVolumes(), input$select_white)$datapath)
      }
      if (length(input$select_dark)>=2){
        m$darkref <- unname(shinyFiles::parseFilePaths(roots = shinyFiles::getVolumes(), input$select_dark)$datapath)
      }
      m
    })

    rasterGuess = function(dir1){
      f1 <- list()
        for (ii in dir1){
          if (grepl("white", tolower(ii))){
            f1$whiteref <- ii
          } else if (grepl("dark", tolower(ii))){
            f1$darkref <- ii
          } else if (grepl("core", tolower(ii)) || grepl("tif", tolower(ii)) || grepl("raw", tolower(ii))){
            f1$capture <- ii
          }
        }
      return(f1)
    }

    coreImage <- reactive({
      if (length(rasters()) == 0 & length(user_datapath()) == 0){
        NULL
      } else if (length(user_dir()) != 0){
        return(terra::rast(files1()$capture))
        } else if (length(user_datapath()) != 0) {

          return(terra::rast(user_file()$datapath))
        } else {
          return(NULL)
      }
    })

    #coreInfo <- reactiveVal()


    coreInfo <- reactive({
      if (!is.null(coreImage())){
        c(paste0("width: ", ncol(coreImage()), " pixels"), paste0("height: ", nrow(coreImage()), " pixels"), paste0("layers: ", length(names(coreImage()))))
      } else {
        NULL
      }
    })

    layersTable <- reactive({
      if (!is.null(coreImage())){
        data.table::data.table(index=1:length(names(coreImage())), wavelength=as.numeric(names(coreImage())))
      } else {
        data.table::data.table()
      }

    })

    output$layerTable1 <- DT::renderDT(layersTable(), height = 100)

    colorSelection <- reactive({
      if (!is.null(coreImage())){
        all <- defineRGB(names(coreImage()))
        colorNames <- c("red", "green", "blue")
        if (sum(all$flags)>0){
          flags <- c(paste0("Warning, false color image due to lack of ", colorNames[as.logical(all$flags)], " layers."), paste0("Using: ", names(coreImage())[all$layers[as.logical(all$flags)]], " nm"))
        } else {
        flags <- "Using: "
        for (ii in 1:3){
          flags <- paste(flags, paste0(names(coreImage())[all$layers[ii]], " nm for ", colorNames[ii]), sep="\n")
        }
      }
      flags
      }
      })

    RGBlayers <- reactive({
      defineRGB(names(coreImage()))$layers
    })

    output$color_warning <- renderText(colorSelection())

    imgH <- reactive({
      nrow(coreImage()) * 10^(input$scaleOrigImg)
    })

    imgW <- reactive({
      ncol(coreImage()) * 10^(input$scaleOrigImg)
    })

    imgDistH <- reactive({
      nrow(coreImage()) * 10^(input$scaleDistImg)
    })

    imgDistW <- reactive({
      ncol(coreImage()) * 10^(input$scaleDistImg)
    })

    renderFN <- function(fullPath){
      if (length(fullPath)==0){
        ""
      } else {
        basename(fullPath)
      }
    }

    # Print raster files
    output$core_file <- renderText(renderFN(files1()$capture))

    output$white_file <- renderText(renderFN(files1()$whiteref))
    output$dark_file <- renderText(renderFN(files1()$darkref))

    # Print core info
    output$core_info <- renderText(coreInfo(), sep = "\n")

    #make plot
    plot1 <- reactive({
        terra::plotRGB(x = coreImage(), r = RGBlayers()[1], g = RGBlayers()[2], b = RGBlayers()[3], stretch = "hist")
    })

    #render plot
    output$core_plot <- renderPlot(plot1(), height = imgH, width = imgW, res = 20)

    source_coords <- reactiveValues(
        xy=data.frame(x=c(1,1),  y=c(1,1))
      )

    sample_coords <- reactiveValues(
      xy=data.frame(x=c(1,1),  y=c(1,1))
    )

    observeEvent(input$plot_click, {
      clickCounter$count <- clickCounter$count + 1
      if (input$sampleOrScale == "Scale"){
        if (ceiling(clickCounter$count/2) == clickCounter$count/2){
          source_coords$xy[2,] <- c(round(input$plot_click$x), round(input$plot_click$y))
        }else{
          source_coords$xy[1,] <- c(round(input$plot_click$x), round(input$plot_click$y))
        }
      } else {
        if (ceiling(clickCounter$count/2) == clickCounter$count/2){
          sample_coords$xy[2,] <- c(round(input$plot_click$x), round(input$plot_click$y))
        }else{
          sample_coords$xy[1,] <- c(round(input$plot_click$x), round(input$plot_click$y))
        }
      }

    })

    #measure scale bar
    distTot <- reactive({
      round(sum(abs(source_coords$xy[1,]-source_coords$xy[2,])))
    })

    distY <- reactive({
      round(abs(source_coords$xy[1,2]-source_coords$xy[2,2]))
    })

    pointA <- reactive({
      which(max(source_coords$xy[,2]) == source_coords$xy[,2])
    })

    pointB <- reactive({
      which(min(source_coords$xy[,2]) == source_coords$xy[,2])
    })

    output$distCoordA <- renderText(paste0("(", round(unlist(source_coords$xy[pointA(),])[1]), ", ", round(nrow(coreImage()[[1]]) - unlist(source_coords$xy[pointA(),])[2]), ")"))
    output$distCoordB <- renderText(paste0("(", round(unlist(source_coords$xy[pointB(),])[1]), ", ", round(nrow(coreImage()[[1]]) - unlist(source_coords$xy[pointB(),])[2]), ")"))

    output$coreDist <- renderText(distY())

    output$scaleDist <- renderText(distTot())

    output$pixelRatio <- renderText(input$scaleLength/distTot())

    #Sample coords
    samplePointA <- reactive({
      which(max(sample_coords$xy[,2]) == sample_coords$xy[,2])
    })

    samplePointB <- reactive({
      which(min(sample_coords$xy[,2]) == sample_coords$xy[,2])
    })

    output$distSamplePointA <- renderText(paste0("(", round(unlist(sample_coords$xy[pointA(),])[1]), ", ", round(nrow(coreImage()[[1]]) - unlist(sample_coords$xy[pointA(),])[2]), ")"))
    output$distSamplePointB <- renderText(paste0("(", round(unlist(sample_coords$xy[pointB(),])[1]), ", ", round(nrow(coreImage()[[1]]) - unlist(sample_coords$xy[pointB(),])[2]), ")"))


    #plot box selection

    x_range <- function(e) {
      if(is.null(e)) return(c(0,0))
      c(round(e$xmin, 0), round(e$xmax, 0))
    }

    y_range <- function(e) {
      if(is.null(e)) return(c(0,0))
      c(round(e$ymin, 0), round(e$ymax, 0))
    }

    #output$xmin <- reactive({x_range(input$plot_brush)[1]})

    output$xmin <- reactive({x_range(input$plotBrush)[1]})
    output$xmax <- reactive({x_range(input$plotBrush)[2]})

    #output$ymin <- reactive({y_range(input$plot_brush)[1]})

    output$ymin <- reactive({y_range(input$plotBrush)[1]})
    output$ymax <- reactive({y_range(input$plotBrush)[2]})

    brush <- NULL
    makeReactiveBinding("brush")

    observeEvent(input$plotBrush, {
      brush <<- input$plotBrush

    })

    observeEvent(input$clearBrush, {
      session$resetBrush("plotBrush")
    })

    observeEvent(input$resetPlot, {
      session$resetBrush("plotBrush")
      brush <<- NULL
    })

    croppedH <- reactive({
      (abs(allParams$cropImage[4] - allParams$cropImage[3])/2)*input$selectionSize2
    })

    croppedW <- reactive({
      (abs(allParams$cropImage[2] - allParams$cropImage[1])/2)*input$selectionSize2
    })

    selectionH <- reactive({
      if (abs(y_range(input$plotBrush)[1] - y_range(input$plotBrush)[2]) < 50){
        500 * input$selectionSize
      } else {
        abs(y_range(input$plotBrush)[1] - y_range(input$plotBrush)[2]) * input$selectionSize
      }
    })

    selectionW <- reactive({
      if (abs(x_range(input$plotBrush)[1] - x_range(input$plotBrush)[2]) < 50){
        500 * input$selectionSize
      } else {
        abs(x_range(input$plotBrush)[1] - x_range(input$plotBrush)[2]) * input$selectionSize
      }
    })

    observeEvent(input$selectPlotRegion, {
      ext1 <- unname(as.vector(terra::ext(coreImage())))
      if (is.null(brush)){
        allParams$cropImage <<- ext1
      } else {
        allParams$cropImage <<- c(x_range(input$plotBrush)[1], x_range(input$plotBrush)[2],
                                  y_range(input$plotBrush)[1], y_range(input$plotBrush)[2])
      }



    zoomedPlot <- reactive({
      if (!is.null(brush)){
        plotsmall <- terra::plotRGB(x = coreImage(), r = RGBlayers()[1], g = RGBlayers()[2], b = RGBlayers()[3], stretch = "hist",
                       ext=terra::ext(x_range(input$plotBrush)[1], x_range(input$plotBrush)[2], y_range(input$plotBrush)[1], y_range(input$plotBrush)[2])
        )
      } else {
        plotsmall <- terra::plotRGB(x = coreImage(), r = RGBlayers()[1], g = RGBlayers()[2], b = RGBlayers()[3], stretch = "hist")
      }
      plotsmall
    })

    output$cropped_plot <- renderPlot({
      #cat(allParams$cropImage)
      terra::plotRGB(x = coreImage(), r = RGBlayers()[1], g = RGBlayers()[2], b = RGBlayers()[3], stretch = "hist",
                     ext=terra::ext(allParams$cropImage)
                     )
      if (sum(complete.cases(analysisRegions$DT))>0){
        for (i in 1:nrow(analysisRegions$DT)){
          polygon(x=c(analysisRegions$DT[i,1], analysisRegions$DT[i,2], analysisRegions$DT[i,2], analysisRegions$DT[i,1]),
                  y=c(analysisRegions$DT[i,4], analysisRegions$DT[i,4], analysisRegions$DT[i,3], analysisRegions$DT[i,3]),
                  col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 0.5), lwd=3)
        }
      }
      },
      height = croppedH,
      width = croppedW
      )

    output$selection_plot <- renderPlot({
      zoomedPlot()
    },
    height = selectionH,
    width = selectionW
    )


    updateTabsetPanel(session=session,
                      "tabset1",
                      selected = "Select Analysis Regions")

    session$resetBrush("plotBrush")
    brush <<- NULL

    })

    analysisRegions <- reactiveValues()
    analysisRegions$DT <- data.frame("xmin"=NA,
                                     "xmax"=NA,
                                     "ymin"=NA,
                                     "ymax"=NA)
    output$analysisRegions <- DT::renderDT(analysisRegions$DT, rownames = FALSE, options = list(dom = 't',
                                                                                            scrollX=TRUE,
                                                                                            autoWidth = TRUE,
                                                                                            columnDefs = list(list(width = '15px', targets = "_all"))))
    # colnames(analysisRegions$DT) <- c("xmin", "xmax", "ymin", "ymax")


    observeEvent(input$selectAnalysisRegion, {

      #add new set of bounds to saved set
      countRegions$count <- countRegions$count + 1
      # if (countRegions$count == 1){
      #   analysisRegions$DT <- data.frame(matrix(nrow = 1, ncol = 4))
      #
      # }

      analysisRegions$DT <- rbind(analysisRegions$DT, c(x_range(input$plotBrush)[1], x_range(input$plotBrush)[2],
                                                                        y_range(input$plotBrush)[1], y_range(input$plotBrush)[2]))

      if (countRegions$count == 1){
        analysisRegions$DT <- analysisRegions$DT[-1,]
        colnames(analysisRegions$DT) <- c("xmin", "xmax", "ymin", "ymax")
      }

      #allParams$analysisRegions <- analysisRegions()

      output$analysisRegions <- DT::renderDT(analysisRegions$DT, rownames = FALSE, options = list(dom = 't',
                                                                                              autoWidth = TRUE,
                                                                                              columnDefs = list(list(width = '15px', targets = "_all"))))
      #reset brush
      session$resetBrush("plotBrush")
      brush <<- NULL


    })

    observeEvent(input$removeRegion, {
      if (!is.null(input$analysisRegions_rows_selected)) {

        analysisRegions$DT <- analysisRegions$DT[-as.numeric(input$analysisRegions_rows_selected),]
      }
      output$analysisRegions <- DT::renderDT(analysisRegions$DT, rownames = FALSE, options = list(dom = 't',
                                                                                              scrollX=TRUE,
                                                                                              autoWidth = TRUE,
                                                                                              columnDefs = list(list(width = '15px', targets = "_all"))))
    })

    # observeEvent(input$skipSelectAnalysisRegion, {
    #   countRegions$count <- 0
    #   analysisRegions$DT <- data.frame("xmin"=NA,
    #                                    "xmax"=NA,
    #                                    "ymin"=NA,
    #                                    "ymax"=NA)
    #   session$resetBrush("plotBrush")
    #   brush <<- NULL
    #
    #   output$core_plot2 <- renderPlot({
    #     terra::plotRGB(x = coreImage(), r = RGBlayers()[1], g = RGBlayers()[2], b = RGBlayers()[3], stretch = "hist")
    #     #add start/end scale points
    #     points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19)
    #     points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize/3, pch=19, col="white")
    #
    #     points(y=sample_coords$xy[,2], x=sample_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19, col="white")
    #     points(y=sample_coords$xy[,2], x=sample_coords$xy[,1], cex=input$scalermarkerPointSize/3, pch=19)
    #     #points( source_coords$xy[1,1], source_coords$xy[1,2], cex=3, pch=intToUtf8(8962))
    #     #text(source_coords$xy[2,1], source_coords$xy[2,2], paste0("Distance=", dist1), cex=3)
    #   },
    #   height = imgDistH,
    #   width = imgDistW
    #   )
    #
    #   updateTabsetPanel(session=session,
    #                     "tabset1",
    #                     selected = "Distance Calibration")
    #
    # })

    # observeEvent(input$acceptCalibration, {
    #   updateTabsetPanel(session=session,
    #                     "tabset1",
    #                     selected = "Analysis")
    # })

    observeEvent(input$file_dir, {
      output$file_show <- renderUI({
        shiny::fluidRow(
        shiny::column(
          12,
          strong("Selected core directory"),
          shiny::br(),
          shiny::verbatimTextOutput("core_dir_show"),
          shiny::br(),
          strong("Raster files in the directory"),
          shiny::br(),
            shiny::fluidRow(
              shiny::column(
                4,
                "Image",
                shiny::verbatimTextOutput("core_file"),
                shinyFiles::shinyFilesButton(id = "select_core", label="Change Selection",title = "Choose your primary image file",multiple = FALSE),
              ),
              shiny::column(
                4,
                "White Reference",
                shiny::verbatimTextOutput("white_file"),
                shinyFiles::shinyFilesButton(id = "select_white", label="Change Selection",title = "Choose your white reference",multiple = FALSE),
              ),
              shiny::column(
                4,
                "Dark Reference",
                shiny::verbatimTextOutput("dark_file"),
                shinyFiles::shinyFilesButton(id = "select_dark", label="Change Selection",title = "Choose your dark reference",multiple = FALSE),
              ),
            ),
          shiny::br(),
          #"Raster details",
          shiny::br(),
          shiny::verbatimTextOutput("core_info"),
        ),
        strong("Choose layers to subset raster"),
        wellPanel(
          shiny::fluidRow(column(6,
          checkboxInput("dt_sel", "select/deselect all", value = TRUE)),
          column(6,
          checkboxInput("halfRows", "select every other row", value = FALSE))),
          strong("Choose Custom Wavelengths (comma separated)"),
          shiny::fluidRow(column(8,
                                 textInput(label = NULL,"findWavelength", placeholder = "450,550,650",width = "100%")),
                          column(4,actionButton("custom_wavelengths", "Select",width = "100%"))
          ),
          strong("Choose Wavelength Range (min/max)"),
          shiny::fluidRow(column(4,
                                 numericInput(label = NULL,"minWave", value = 550, width = "100%")),
                          column(4,
                                 numericInput(label = NULL,"maxWave", value = 650, width = "100%")),
                          column(4,actionButton("range_waves", "Select",width = "100%"))
          ),
          DT::DTOutput("layerTable1"),
        ),
        wellPanel(
          textOutput("currentSelection"),
        )
        )
      })
    })
    observeEvent(input$file_dir_example, {
      output$file_show <- renderUI({
        shiny::fluidRow(
          shiny::column(
            12,
            strong("Selected core directory"),
            shiny::br(),
            shiny::verbatimTextOutput("core_dir_show"),
            shiny::br(),
            strong("Raster files in the directory"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                4,
                "Image",
                shiny::verbatimTextOutput("core_file"),
              ),
              shiny::column(
                4,
                "White Reference",
                shiny::verbatimTextOutput("white_file"),
              ),
              shiny::column(
                4,
                "Dark Reference",
                shiny::verbatimTextOutput("dark_file"),
              ),
            ),
            shiny::br(),
            strong("Raster details"),
            shiny::br(),
            shiny::verbatimTextOutput("core_info"),
          ),
          strong("Choose layers to subset raster"),
          wellPanel(
            shiny::fluidRow(column(6,
            checkboxInput("dt_sel", "select/deselect all", value = TRUE)),
            column(6,
            checkboxInput("halfRows", "select every other row", value = FALSE))),
            strong("Choose Custom Wavelengths (comma separated)"),
            shiny::fluidRow(column(8,
            textInput(label = NULL,"findWavelength", placeholder = "450,550,650",width = "100%")),
            column(4,actionButton("custom_wavelengths", "Select",width = "100%"))
            ),
            strong("Choose Wavelength Range (min/max)"),
            shiny::fluidRow(column(4,
                                   numericInput(label = NULL,"minWave", value = 550, width = "100%")),
                            column(4,
                                   numericInput(label = NULL,"maxWave", value = 650, width = "100%")),
                            column(4,actionButton("range_waves", "Select",width = "100%"))
            ),
            DT::DTOutput("layerTable1"),
            wellPanel(
              textOutput("currentSelection"),
            )
          )
        )
      })
    })
    observeEvent(input$ref_file, {
      output$file_show <- renderUI({
        shiny::fluidRow(
          shiny::column(
            12,
            "Selected reflectance file",
            shiny::br(),
            shiny::tableOutput("core_file_show"),
            shiny::br(),
            "Raster details",
            shiny::br(),
            shiny::verbatimTextOutput("core_info"),
          ),
          "Choose layers to subset raster",
          wellPanel(
            shiny::fluidRow(column(6,
            checkboxInput("dt_sel", "select/deselect all", value = TRUE)),
            column(6,
            checkboxInput("halfRows", "select every other row", value = FALSE))),
            "Choose Custom Wavelengths (comma separated)",
            shiny::fluidRow(column(8,
                                   textInput(label = NULL,"findWavelength", placeholder = "450,550,650",width = "100%")),
                            column(4,actionButton("custom_wavelengths", "Select",width = "100%"))
            ),
            "Choose Wavelength Range (min/max)",
            shiny::fluidRow(column(4,
                                   numericInput(label = NULL,"minWave", value = 550, width = "100%")),
                            column(4,
                                   numericInput(label = NULL,"maxWave", value = 650, width = "100%")),
                            column(4,actionButton("range_waves", "Select",width = "100%"))
            ),
            DT::DTOutput("layerTable1"),
            wellPanel(
              textOutput("currentSelection"),
            )
          )
        )
      })
    })

    observeEvent(input$begin, {
      #distances <- list()
      #distances$pointA <- round(unlist(source_coords$xy[pointA(),]))
      #distances$pointB <- round(unlist(source_coords$xy[pointB(),]))
      #distances$coreDist <- distY()
      #distances$scaleDist <- distTot()
      #distances$scaleDistmm <- input$scaleLength
      #distances$pixelRatio <- input$scaleLength/distTot()
      #print("2 executing ok")
      analysisOptions <- list()
      #print("3 executing ok")
      analysisOptions$normalize <- as.logical(input$choice_normalize)
      #print("4 executing ok")
      analysisOptions$integration <- as.logical(input$choice_integration)
      #print("5 executing ok")
      analysisOptions$proxies <- input$choice_proxies
      #print("6 executing ok")
      allParams$analysisOptions <<- analysisOptions
      #print("7 executing ok")
      if (analysisOptions$normalize){
        output$file_select <- renderUI({
          # shiny::fluidRow(
          #   align="center",
          #   shinyFiles::shinyDirButton("file_dir", "Select directory with captured data", title = "Select directory"),
          #   shiny::actionButton("file_dir_example", "Use example data"),
          #   shiny::br(),
          #   shiny::br(),
          # ),
          shiny::fluidRow(
            align="center",
            shinyFiles::shinyDirButton("file_dir", "Select directory with captured data", title = "Select directory"),
            shiny::actionButton("file_dir_example", "Use example data"),
            shiny::br(),
            shiny::br(),
          )
        })
      } else {
        output$file_select <- renderUI({
          # shiny::fluidRow(
          #   align="center",
          #   #shinyFiles::shinyDirButton("file_dir", "Select directory with captured data", title = "Select directory"),
          #   shinyFiles::shinyFilesButton("ref_file", "Select reflectance file", title = "Select file",multiple = FALSE),
          #   shiny::br(),
          #   shiny::br(),
          # ),
          shiny::fluidRow(
            align="center",
            shinyFiles::shinyFilesButton("ref_file", "Select reflectance file", title = "Select file",multiple = FALSE),
            shiny::br(),
            shiny::br(),
          )
        })
      }



      updateTabsetPanel(session=session,
                        "tabset1",
                        selected = "Select Data")
      #print("8 executing ok")
    })

    custTableIndex <- reactiveVal()

    observeEvent(input$custom_wavelengths, {

      rows <- c(unlist(lapply(strsplit(input$findWavelength, ","), function(x) as.numeric(x))))

      runIt <- FALSE

      if (length(rows)==1){
        if (is.na(rows)){
          shinyalert::shinyalert(title = "Invalid Input", text = "Check the format of your entry")
        } else {
          runIt <- TRUE
        }
      } else {
        runIt <- TRUE
      }
      if (runIt) {
        spectraIndex <- purrr::map(rows, \(x) which.min(abs(x - as.numeric(names(coreImage()))))) |>
          purrr::as_vector()

        # warn1 <- paste0("Requested ", length(rows), " unique layers, but only found ", length(unique(spectraIndex)))
        #
        # if (length(rows) > length(unique(spectraIndex))) {
        #   shinyalert::shinyalert(warn1)
        # }

        spectraIndex <- c(custTableIndex(),spectraIndex)

        custTableIndex(spectraIndex)

        DT::selectRows(dt_proxy, input$layerTable1_rows_all[spectraIndex])
      }
    })



    observeEvent(input$range_waves, {

      if (input$minWave > input$maxWave){
        shinyalert::shinyalert(title = "Min/Max Error",text = "Max value must be greater than Min")
      } else {
        waves <- as.numeric(names(coreImage()))
        waves <- waves + runif(length(waves),min = 0,max = .001)

        minDiffs <- unlist(lapply(c(waves - input$minWave), function(x) if(x<0){x*-1 + 500}else{x}))
        maxDiffs <- unlist(lapply(c(input$maxWave - waves), function(x) if(x<0){x*-1 + 500}else{x}))

        spectraIndex1 <- which.min(minDiffs)
        spectraIndex2 <- which.min(maxDiffs)

        spectraIndex3 <- c(custTableIndex(),spectraIndex1:spectraIndex2)

        custTableIndex(spectraIndex3)

        DT::selectRows(dt_proxy, input$layerTable1_rows_all[spectraIndex3])
      }
    })

    observeEvent(input$done, {
      distances <- list()
      distances$startCore <- round(unlist(sample_coords$xy[samplePointA(),]))
      distances$endCore <- round(unlist(sample_coords$xy[samplePointB(),]))
      distances$startScale <- round(unlist(source_coords$xy[pointA(),]))
      distances$endScale <- round(unlist(source_coords$xy[pointB(),]))
      distances$coreDist <- distY()
      distances$scaleDist <- distTot()
      distances$scaleDistmm <- input$scaleLength
      distances$pixelRatio <- input$scaleLength/distTot()
      analysisOptions <- list()
      analysisOptions$normalize <- as.logical(input$choice_normalize)
      analysisOptions$integration <- as.logical(input$choice_integration)
      analysisOptions$proxies <- input$choice_proxies

      allParams$simpleRGB <<- plot1()
      allParams$rasterPaths <<- files1()

      if (length(user_dir()) != 0){
        allParams$directory <<- user_dir()
      }
      # else if (length(user_dir2()) != 0){
      #   allParams$directory <<- user_dir2()
      # }
      else {
        allParams$directory <<- dirname(user_file()$datapath)
        #shinyalert::shinyalert(title = "No Data", text = "Please return to the 'Select Data' tab and choose data to analyze.")
      }

      allParams$analysisRegions <<- roi_to_vect(analysisRegions$DT)
      allParams$distances <<- distances
      allParams$analysisOptions <<- analysisOptions
      if (autoSave==TRUE){
        saveLoc <- paste0(allParams$directory,"/HSItools_core.rds")
        saveRDS(allParams, saveLoc)
        cat("\n")
        cat(paste0("Output saved: ", saveLoc))
        cat("\n")
        cat(paste0("Load the rds to use new data (eg. core1A <- readRDS('", saveLoc, "'))"))
        cat("\n")
        cat("\n")
      }
      stopApp(invisible(allParams))
    })

    observeEvent(input$acceptAnalysisRegions, {
      output$core_plot2 <- renderPlot({
        terra::plotRGB(x = coreImage(), r = RGBlayers()[1], g = RGBlayers()[2], b = RGBlayers()[3], stretch = "hist")
        points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19)
        points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize/3, pch=19, col="white")

        points(y=sample_coords$xy[,2], x=sample_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19, col="white")
        points(y=sample_coords$xy[,2], x=sample_coords$xy[,1], cex=input$scalermarkerPointSize/3, pch=19)
        #points( source_coords$xy[1,1], source_coords$xy[1,2], cex=3, pch=intToUtf8(8962))
        #text(source_coords$xy[2,1], source_coords$xy[2,2], paste0("Distance=", dist1), cex=3)
      },
      height = imgDistH,
      width = imgDistW
      )
      ## Source


      ## Destination
      ##

      updateTabsetPanel(session=session,
                        "tabset1",
                        selected = "Distance Calibration")
    })

  }
  )
  )
}

# run_core <- function() {shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))}
