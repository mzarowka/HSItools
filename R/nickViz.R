#' Get depths
#'
#' @param core core
#' @param rast raster
#'
#' @returns depths
#' @export
get_depths <- function(core,rast){
  cal <- pixel_to_distance(core)

  depths <- seq_len(nrow(rast)) * cal$pixel_ratio #+
        #cal$distance -
        #cal$point_zero

  if(mean(depths) < 0){depths <- depths * -1}

  return(depths)
}

#' Look up color ramps
#'
#' @param index
#'
#' @return
#' @export
getColorsByIndex <- function(index) {
  # Define index-to-palette mappings
  palette_map <- c(
    "RABD615" = "GnBu",
    "RABD660" = "BuGn",
    "rabd660670_max" = "Greens",
    "RABD640655" = "YlGn",
    "RABD845" = "Blues",
    "rabd845_strict" = "Blues",
    "R570R630" = "YlOrRd",
    "R590R690" = "Purples",
    "R950R970" = "Oranges"
  )

  # Look up palette
  pall <- palette_map[index]

  # Handle unrecognized index
  if (is.na(pall)) {
    stop(glue::glue("Index {index} not recognized"))
  }

  # Generate color list
  colors <- RColorBrewer::brewer.pal(name = pall, n = 7)

  list(
    line = colors[3],
    smooth = colors[7],
    palette = pall
  )
}


#' Plot a heatmap
#'
#' @param rasDat
#' @param depthScale
#' @param palette
#' @param cmPerPixel
#'
#' @return
#' @export
plotHeatmap <- function(rasDat,depthScale,cmPerPixel,palette = "Greens",palette.direction = 1){
  #depth
  syf <- rev(depthScale)
  # Heatmap
  plotOut <- rasDat %>%
    matrix(ncol = ncol(rasDat),nrow = nrow(rasDat),byrow = TRUE) %>%
    # Data wrangling
    tibble::as_tibble() %>%
    tibble::rowid_to_column(var="depthIndex") %>%
    tidyr::gather(key="X", value="index", -1) %>%

    # Change X to numeric
    dplyr::mutate(X=as.numeric(gsub("V","",X))*cmPerPixel) %>%

    #convert to depth
    dplyr::mutate(depth = syf[depthIndex]) %>%
    ggplot2::ggplot(ggplot2::aes(X, depth, fill= index)) +
    ggplot2::geom_raster() +
    ggplot2::theme(legend.position="none")+
    ggplot2::coord_equal()+
    ggplot2::scale_y_continuous(expand = c(0,0))+
    ggplot2::scale_fill_distiller(palette = palette,
                         direction = palette.direction)+
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank(),
          panel.background = ggplot2::element_blank())

  return(plotOut)

}


#' Plot vertically
#'
#' @param ind
#' @param index.name
#' @param line.color
#' @param smooth.color
#' @param smooth.width
#'
#' @return
#' @export
plotVerticalIndex <- function(ind,
                              index.name = "RABD660",
                              line.color = "gray70",
                              smooth.color = "black",
                              smooth.width = .5){
  linPlot <- ggplot2::ggplot(ind)+
    ggplot2::geom_path(ggplot2::aes_string(y = "depth",x = index.name),color = line.color)+
    ggplot2::geom_path(ggplot2::aes_string(y = "depth",x = paste0("smooth",index.name)),color = smooth.color,size = smooth.width)+
    ggplot2::theme_bw()+
    ggplot2::scale_y_reverse(expand = c(0,0))

  return(linPlot)

}


#' Plot spectral dashboard
#'
#' @param normalized
#' @param ind
#' @param index.name
#' @param depth.label
#' @param plot.width
#' @param tol
#' @param processed.image.dir
#' @param core.width
#' @param page.width
#' @param page.width.multiplier
#' @param page.length.multiplier
#' @param y.tick.interval
#' @param page.units
#' @param output.file.path
#' @param output.dpi
#'
#' @return
#' @export
plotSpectralDashboard <- function(core,
                                  ind,
                                  processed.image.dir = file.path(core$directory,"photos"),
                                  roi_i  = 1,
                                  index.name = names(ind),
                                  depth.label = "Depth (cm)",
                                  smooth.win = NA,
                                  core.width = 4,
                                  plot.width = 8,
                                  page.width = 10,
                                  page.width.multiplier = 1.5,
                                  page.length.multiplier = 3,
                                  y.tick.interval = 5,
                                  page.units = "cm",
                                  roi.box = TRUE,
                                  tol = 1,
                                  output.file.path = NA,
                                  output.dpi = 600){
  #make a composite plot

  #get the image
  if(is.na(processed.image.dir)){
    #select it
  }

  #get the processed image path (want full png with scale so that ROI is in right spot)
  fullPath <- list.files(path = processed.image.dir,pattern = "fullImage_RGB*.png$",full.names = TRUE)
  img <- magick::image_read(fullPath[1])

  bigRoi <- raster::extent(core$cropImage)
  roi <- raster::extent(core$analysisRegions[roi_i,])

  #decide how to crop it.
  xOffset <- min(bigRoi@xmin,roi@xmin)
  yOffset <- roi@ymin #use ROI exactly
  rightPos <- max(bigRoi@xmax,roi@xmax)
  topPos <- roi@ymax #use ROI exactly
  width <- rightPos-xOffset
  height <- topPos-yOffset

  #get roi boundaries in cm
  cmRoi <- roi
  cmRoi@xmin <- max(roi@xmin - xOffset + 1,1)*core$distances$pixelRatio/10
  cmRoi@xmax <- min(roi@xmax - xOffset + 1,rightPos)*core$distances$pixelRatio/10
  cmRoi@ymin <- max(roi@ymin - yOffset + 1,1)*core$distances$pixelRatio/10
  cmRoi@ymax <- min(roi@ymax - yOffset + 1,topPos)*core$distances$pixelRatio/10


  iroi <- magick::geometry_area(width = width,height = height, x_off = xOffset,y_off = yOffset)
  cimg <- magick::image_crop(img,geometry = iroi,gravity = "SouthWest")

  #cimg <- img

  cinfo <- magick::image_info(img)

  c.height <- height*core$distances$pixelRatio/10
  c.width <- width*core$distances$pixelRatio/10

  depth.ticks <- seq(0,c.height,by = y.tick.interval)

  ggimg <- ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string("x","y")) +
    ggplot2::geom_blank() +
    ggplot2::coord_fixed(expand = FALSE, xlim = c(0, c.width),ylim = c(-c.height,0)) +
    ggplot2::annotation_raster(cimg, 0, c.width, -c.height, 0, interpolate = FALSE)+
    ggplot2::scale_y_continuous(depth.label,labels = rev(depth.ticks),breaks = -rev(depth.ticks))


  ticks <- ggplot2::ggplot_build(ggimg)$layout$panel_params[[1]]$y$breaks

  ggimg <- ggimg+
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank())

  if(roi.box){
    ggimg <- ggimg +
    ggplot2::geom_rect(ggplot2::aes(xmin = cmRoi@xmin,
                  xmax = cmRoi@xmax,
                  ymin = -cmRoi@ymin,
                  ymax = -cmRoi@ymax),
              color = "red",
              fill = NA)
  }

  plots <- vector(mode = "list",length = length(index.name)*2+1)
  plots[[1]] <- ggimg
  for(i in 1:length(index.name)){
    depths <- get_depths(core,ind[[i]])

    #calculate indices

    #get colors by index
    cols <- getColorsByIndex(index.name[i])

    #get downcore data
    # Extract the series
    depth_index <- ind[[i]] |>
      extract_spectral_series(
        index = names(ind[[i]])) |>
      dplyr::mutate(depth = depths/10) |>
      dplyr::select(depth,!!names(ind[[i]])) |>
      dplyr::mutate(dplyr::across(-depth, smoother::smth,window = smooth.win,.names = "smooth{.col}"))

    if(i == 1){
      spectralIndices <- depth_index
    }else{
      spectralIndices <- dplyr::bind_cols(spectralIndices,dplyr::select(depth_index,-depth))
    }

    if(!is.na(output.file.path)){
      thisCsvPath <- file.path(dirname(output.file.path),paste0(names(ind[[i]]),"-roi",roi_i,".csv"))
      readr::write_csv(depth_index,file = thisCsvPath)
    }

    # make a line plot
    # line plot
    plots[[2*i+1]] <- plotVerticalIndex(depth_index,
                                        index.name = index.name[i],
                                        line.color = cols$smooth,
                                        smooth.color = cols$smooth,
                                        smooth.width = 0)+
      ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis())

    if(i<length(index.name)){
      plots[[2*i+1]] <- plots[[2*i+1]] +   ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                                                 axis.text.y=ggplot2::element_blank(),
                                                 axis.ticks.y=ggplot2::element_blank())
    }else{
      plots[[2*i+1]] <- plots[[2*i+1]] +
        ggplot2::scale_y_reverse("Depth (cm)",position = "right",expand = c(0,0),breaks = rev(depth.ticks))+
        ggplot2::theme(axis.title.y.right = ggplot2::element_text(angle = 90))
    }

    #make a heatmap
    plots[[2*i]] <- plotHeatmap(ind[[i]],depthScale = depths, cmPerPixel = core$distances$pixelRatio, palette = cols$palette) +
      ggplot2::theme(axis.title.y=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.margin=grid::unit(c(1,-.5,1,-0.5), "cm"))
    #make a dashboard plot
  }

  if(length(ind) > 1){#write out spectral indices if there are multiple indices

  spectralIndices <- spectralIndices |>
    dplyr::select(depth,dplyr::everything(), dplyr::starts_with("smooth")) |>
    readr::write_csv(file = file.path(dirname(output.file.path),"spectralIndices.csv"))

  }


  rel.widths <- c(core.width,rep(c(1,plot.width),times = length(index.name)))
  widths <- grid::unit(rel.widths/sum(rel.widths)*page.width,units = page.units)
  page.length <- rel.widths[1]/sum(rel.widths)*page.width*c.height/c.width

  #egg
  outplot <- egg::ggarrange(plots = plots,nrow = 1,widths = widths,padding = 0,draw = FALSE,clip = "on")

  if(!is.na(output.file.path)){
  ggplot2::ggsave(plot = outplot,
         filename = output.file.path,
         width = page.width*page.width.multiplier,
         height = page.length*page.length.multiplier,
         units = page.units,dpi = output.dpi,
         limitsize = FALSE)
  }

  return(outplot)

}



