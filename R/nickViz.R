get_depths <- function(core,rast){
  cal <- pixel_to_distance(core)

  depths <- seq_len(nrow(rast)) * cal$pixel_ratio +
        cal$distance -
        cal$point_zero

  if(mean(depths) < 0){depths <- depths * -1}

  return(depths)
}

#' Title
#'
#' @param index
#'
#' @return
#' @export
#'
#' @examples
getColorsByIndex <- function(index){

  if("RABD615" == index){
    pall <- "GnBu"
  }
  if("RABD660" == index){
    pall <- "BuGn"
  }
  if("rabd660670_max" == index){
    pall <- "Greens"
  }
  if("RABD640655" == index){
    pall <- "YlGn"
  }
  if("RABD845" == index){
    pall <- "Blues"
  }


  #band ratios
  if("R570R630"== index){
    pall <- "YlOrRd"
  }

  if("R590R690" == index){
    pall <- "Purples"
  }

  if("R950R970" == index){
    pall <- "Oranges"
  }

  cols <- list(line = RColorBrewer::brewer.pal(name = pall,n = 7)[3],
               smooth = RColorBrewer::brewer.pal(name = pall,n = 7)[7],palette = pall)
  return(cols)
}

#' Title
#'
#' @param rasDat
#' @param depthScale
#' @param palette
#' @param cmPerPixel
#'
#' @return
#' @export
#'
#' @examples
plotHeatmap <- function(rasDat,depthScale,cmPerPixel,palette = "Greens",palette.direction = 1){
  #depth
  syf <- rev(depthScale)
  # Heatmap
  plotOut <- rasDat %>%
    matrix(ncol = ncol(rasDat),nrow = nrow(rasDat),byrow = TRUE) %>%
    # Data wrangling
    as_tibble() %>%
    rowid_to_column(var="depthIndex") %>%
    tidyr::gather(key="X", value="index", -1) %>%

    # Change X to numeric
    mutate(X=as.numeric(gsub("V","",X))*cmPerPixel) %>%

    #convert to depth
    mutate(depth = syf[depthIndex]) %>%
    ggplot(aes(X, depth, fill= index)) +
    geom_raster() +
    theme(legend.position="none")+
    coord_equal()+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_distiller(palette = palette,
                         direction = palette.direction)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background = element_blank())

  return(plotOut)

}


#' Title
#'
#' @param ind
#' @param index.name
#' @param line.color
#' @param smooth.color
#' @param smooth.width
#'
#' @return
#' @export
#'
#' @examples
plotVerticalIndex <- function(ind,
                              index.name = "RABD660",
                              line.color = "gray70",
                              smooth.color = "black",
                              smooth.width = .5){
  linPlot <- ggplot(ind)+
    geom_path(aes_string(y = "depth",x = index.name),color = line.color)+
    geom_path(aes_string(y = "depth",x = paste0("smooth",index.name)),color = smooth.color,size = smooth.width)+
    theme_bw()+
    scale_y_reverse(expand = c(0,0))

  return(linPlot)

}


#' Title
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
#'
#' @examples
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


  # iroi <- magick::geometry_area(width = width,height = height, x_off = xOffset,y_off = yOffset)
  # cimg <- magick::image_crop(img,geometry = iroi,gravity = "SouthWest")

  cimg <- img

  cinfo <- magick::image_info(img)

  c.height <- height*core$distances$pixelRatio/10
  c.width <- width*core$distances$pixelRatio/10

  depth.ticks <- seq(0,c.height,by = y.tick.interval)

  ggimg <- ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string("x","y")) +
    ggplot2::geom_blank() +
    ggplot2::coord_fixed(expand = FALSE, xlim = c(0, c.width),ylim = c(-c.height,0)) +
    ggplot2::annotation_raster(cimg, 0, c.width, -c.height, 0, interpolate = FALSE)+
    ggplot2::scale_y_continuous(depth.label,labels = rev(depth.ticks),breaks = -rev(depth.ticks))


  ticks <- ggplot_build(ggimg)$layout$panel_params[[1]]$y$breaks

  ggimg <- ggimg+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    geom_rect(aes(xmin = cmRoi@xmin,
                  xmax = cmRoi@xmax,
                  ymin = -cmRoi@ymin,
                  ymax = -cmRoi@ymax),
              color = "red",
              fill = NA)

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
      mutate(depth = depths) |>
      select(depth,!!names(ind[[i]])) |>
      mutate(across(-depth, smoother::smth,window = smooth.win,.names = "smooth{.col}"))

    if(!is.na(output.file.path)){
      thisCsvPath <- file.path(dirname(output.file.path),paste0(names(ind[[i]]),"-roi",i,".csv"))
      readr::write_csv(depth_index,file = thisCsvPath)
    }

    # make a line plot
    # line plot
    plots[[2*i+1]] <- plotVerticalIndex(depth_index,
                                        index.name = index.name[i],
                                        line.color = cols$smooth,
                                        smooth.color = cols$smooth,
                                        smooth.width = 0)+
      scale_x_continuous(sec.axis = dup_axis())

    if(i<length(index.name)){
      plots[[2*i+1]] <- plots[[2*i+1]] +   theme(axis.title.y=element_blank(),
                                                 axis.text.y=element_blank(),
                                                 axis.ticks.y=element_blank())
    }else{
      plots[[2*i+1]] <- plots[[2*i+1]] +
        scale_y_reverse("Depth (cm)",position = "right",expand = c(0,0),breaks = rev(depth.ticks))+
        theme(axis.title.y.right = element_text(angle = 90))
    }

    #make a heatmap
    plots[[2*i]] <- plotHeatmap(ind[[i]],depthScale = depths, cmPerPixel = core$distances$pixelRatio, palette = cols$palette) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            plot.margin=unit(c(1,-.5,1,-0.5), "cm"))
    #make a dashboard plot
  }

  rel.widths <- c(core.width,rep(c(1,plot.width),times = length(index.name)))
  widths <- unit(rel.widths/sum(rel.widths)*page.width,units = page.units)
  page.length <- rel.widths[1]/sum(rel.widths)*page.width*c.height/c.width

  #egg
  outplot <- egg::ggarrange(plots = plots,nrow = 1,widths = widths,padding = 0,draw = FALSE,clip = "on")

  if(!is.na(output.file.path)){
  ggsave(plot = outplot,
         filename = output.file.path,
         width = page.width*page.width.multiplier,
         height = page.length*page.length.multiplier,
         units = page.units,dpi = output.dpi,
         limitsize = FALSE)
  }

  return(outplot)

}



