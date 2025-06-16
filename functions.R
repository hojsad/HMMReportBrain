#' workhorse function for \code{brainconn()}
#'
#' returns a ggraph object of plotted brain connectivity matrix
#' @author Sidhant Chopra
#' @import ggraph
#' @import ggplot2
#' @import grid

build_plot <- function(
  conmat,
  data,
  data.row = NULL,
  data.col = NULL,
  background,
  node.size,
  node.color = "network",
  thr = NULL,
  uthr = NULL,
  view,
  edge.color,
  edge.alpha,
  edge.width,
  show.legend,
  label.size,
  labels,
  include.vec = NULL,
  scale.edge.width,
  edge.color.weighted,
  label.edge.weight,
  bg_xmin = 0,
  bg_ymin = 0,
  bg_xmax = 0,
  bg_ymax = 0,
  ...
) {
  if (view == "top") {
    x.mni <- data$x.mni
    y.mni <- data$y.mni
    depth <- data$z.mni
    xmax = 70 + bg_xmax
    xmin = -75 + bg_xmin
    ymax = 73 + bg_ymax
    ymin = -107 + bg_ymin
  }

  if (view == "bottom") {
    x.mni <- data$x.mni * -1
    y.mni <- data$y.mni
    depth <- data$z.mni * -1
    xmax = 70 + bg_xmax
    xmin = -70 + bg_xmin
    ymax = 73 + bg_ymax
    ymin = -107 + bg_ymin
  }

  if (view == "front") {
    x.mni <- data$x.mni
    y.mni <- data$z.mni
    depth <- data$y.mni
    xmax = 70 + bg_xmax
    xmin = -70 + bg_xmin
    ymax = 80 + bg_ymax
    ymin = -48 + bg_ymin
  }

  if (view == "back") {
    x.mni <- data$x.mni * -1
    y.mni <- data$z.mni
    depth <- data$y.mni * -1
    xmax = 70 + bg_xmax
    xmin = -70 + bg_xmin
    ymax = 80 + bg_ymax
    ymin = -48 + bg_ymin
  }

  if (view == "left") {
    x.mni <- data$y.mni * -1
    y.mni <- data$z.mni
    depth <- data$x.mni
    xmax = 103 + bg_xmax
    xmin = -72 + bg_xmin
    ymax = 77 + bg_ymax
    ymin = -50 + bg_ymin
  }

  ##fix below
  if (view == "right") {
    x.mni <- data$y.mni
    y.mni <- data$z.mni
    depth <- data$x.mni * -1
    xmax = 103 + bg_xmax
    xmin = -140 + bg_xmin
    ymax = 77 + bg_ymax
    ymin = -50 + bg_ymin
  }

  #is matrix directed (i.e. symetric)
  ifelse(
    isSymmetric.matrix(conmat) == TRUE,
    directed <- FALSE,
    directed <- TRUE
  )

  #is matrix weighed
  ifelse(all(conmat %in% c(0, 1)) == TRUE, weighted <- FALSE, weighted <- TRUE)

  #should edges be colored by weight
  #  ifelse(edge.color=="weight", edge.color.weighted <- T, edge.color.weighted <- F)

  if (!exists("conmat")) stop(print("Please enter a valid connectivity matrix"))

  if (directed == F) {
    conmat[upper.tri(conmat)] <- 0 #only take bottom tri of matrix to stop the edge labels being plotted twice
    layout <- create_layout(graph = conmat, layout = "stress", circular = TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
  }

  if (directed == T) {
    layout <- create_layout(graph = conmat, layout = "stress", circular = TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout$facet <- include.vec
  }

  #make graph

  if (directed == T && weighted == F) {
    p <- ggraph(layout) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_parallel(
        color = edge.color,
        edge_width = edge.width,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = circle((node.size / 2) + 0.6, 'mm')
      ) +
      #  ggraph::geom_edge_loop0(aes(strength=node.size*3), color=edge.color, edge_width = edge.width, arrow = arrow(length = unit(1, 'mm'))) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == T &&
      weighted == T &&
      edge.color.weighted == F &&
      label.edge.weight == F
  ) {
    p <- ggraph(layout) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_parallel(
        aes(width = weight),
        color = edge.color,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = circle(node.size / 2, 'mm')
      ) +
      geom_edge_loop0(
        aes(strength = node.size * 3, width = weight),
        color = edge.color,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm'))
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == T &&
      weighted == T &&
      edge.color.weighted == F &&
      label.edge.weight == T
  ) {
    p <- ggraph(layout) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_parallel(
        aes(width = weight, label = round(weight, 3)),
        color = edge.color,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = circle(node.size / 2, 'mm'),
        angle_calc = 'along',
        alpha = 0,
        label_dodge = unit(2.5, 'mm'),
        label_size = 2,
        fontface = "bold"
      ) +
      geom_edge_loop0(
        aes(strength = node.size * 3, width = weight, label = round(weight, 3)),
        color = edge.color,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm')),
        angle_calc = 'none',
        alpha = 0,
        label_dodge = unit(6, 'mm'),
        label_size = 2,
        vjust = -1,
        fontface = "bold"
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == T &&
      weighted == T &&
      edge.color.weighted == T &&
      label.edge.weight == F
  ) {
    p <- ggraph(layout) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_parallel(
        aes(color = weight),
        edge_alpha = edge.alpha,
        edge_width = edge.width,
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = circle(node.size / 2, 'mm')
      ) +
      geom_edge_loop(
        aes(strength = node.size * 3, color = weight),
        edge_width = edge.width,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm'))
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == T &&
      weighted == T &&
      edge.color.weighted == T &&
      label.edge.weight == T
  ) {
    p <- ggraph(layout) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_parallel(
        aes(color = weight, label = round(weight, 3)),
        #color=edge.color,
        edge_alpha = edge.alpha,
        edge_width = edge.width,
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = circle(node.size / 2, 'mm'),
        angle_calc = 'along',
        alpha = 0,
        label_dodge = unit(2.5, 'mm'),
        label_size = 2,
        fontface = "bold"
      ) +
      geom_edge_loop(
        aes(strength = node.size * 3, color = weight, label = round(weight, 3)),
        edge_width = edge.width,
        edge_alpha = edge.alpha,
        arrow = arrow(length = unit(3, 'mm')),
        angle_calc = 'none',
        alpha = 0,
        label_dodge = unit(6, 'mm'),
        label_size = 2,
        vjust = -1,
        fontface = "bold"
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (directed == F && weighted == F) {
    p <- ggraph(layout, circular = FALSE) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_link(
        color = edge.color,
        edge_width = edge.width,
        edge_alpha = edge.alpha
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == F &&
      weighted == T &&
      edge.color.weighted == F &&
      label.edge.weight == F
  ) {
    p <- ggraph(layout, circular = FALSE) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_link(
        aes(width = weight),
        color = edge.color,
        edge_alpha = edge.alpha
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == F &&
      weighted == T &&
      edge.color.weighted == F &&
      label.edge.weight == T
  ) {
    p <- ggraph(layout, circular = FALSE) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_link(
        aes(width = weight, label = round(weight, 3)),
        color = edge.color,
        edge_alpha = edge.alpha,
        angle_calc = 'along',
        alpha = 0,
        label_dodge = unit(2.5, 'mm'),
        label_size = 2,
        fontface = "bold"
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == F &&
      weighted == T &&
      edge.color.weighted == T &&
      label.edge.weight == F
  ) {
    p <- ggraph(layout, circular = FALSE) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_link(
        aes(colour = weight),
        edge_width = edge.width,
        edge_alpha = edge.alpha
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  if (
    directed == F &&
      weighted == T &&
      edge.color.weighted == T &&
      label.edge.weight == T
  ) {
    p <- ggraph(layout, circular = FALSE) +
      annotation_custom(
        background,
        xmax = xmax,
        xmin = xmin,
        ymax = ymax,
        ymin = ymin
      ) +
      geom_edge_link(
        aes(colour = weight, label = round(weight, 3)),
        edge_width = edge.width,
        edge_alpha = edge.alpha,
        angle_calc = 'along',
        alpha = 0,
        label_dodge = unit(2.5, 'mm'),
        label_size = 2,
        fontface = "bold"
      ) +
      coord_fixed(xlim = c(-70, 70), ylim = c(-107, 73))
  }

  ##scale edge weight for weighted networks
  if (weighted == T && !is.null(scale.edge.width)) {
    p <- p + scale_edge_width(range = scale.edge.width)
  }

  #adjust xylim for left and right views --- probably can get rid of this with correct inital placement ratios
  if (view == "left") {
    p <- p + coord_fixed(xlim = c(-64, 98), ylim = c(-44, 76))
  }
  if (view == "right") {
    p <- p + coord_fixed(xlim = c(-98, 64), ylim = c(-44, 76))
  }

  #set node size with degree option  #### NOT WORKING ###
  #ifelse(node.size=="degree", node.size <- as.vector((degree(graph_from_adjacency_matrix(conmat)))*0.2), node.size <- node.size)

  #add nodes
  if (directed == T) {
    ifelse(
      node.color == "network",
      p <- p +
        geom_node_point(
          size = node.size,
          aes(colour = as.factor(data$network), filter = as.logical(facet))
        ),
      p <- p + geom_node_point(size = node.size, colour = node.color)
    )
  }
  if (directed == F) {
    ifelse(
      node.color == "network",
      p <- p +
        geom_node_point(
          size = node.size,
          aes(colour = as.factor(data$network))
        ),
      p <- p + geom_node_point(size = node.size, colour = node.color)
    )
  }

  ## add labs
  if (directed == T && labels == T) {
    p <- p +
      geom_node_text(
        aes(label = data$ROI.Name, filter = as.logical(facet)),
        size = label.size,
        repel = TRUE,
        nudge_x = node.size + 2,
        nudge_y = node.size
      )
  }

  if (directed == F && labels == T) {
    p <- p +
      geom_node_text(
        aes(label = data$ROI.Name),
        size = label.size,
        repel = TRUE,
        nudge_x = node.size + 2,
        nudge_y = node.size
      )
  }

  #remove gridlines
  p <- p +
    theme_gray() +
    theme(
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      #panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
    )
  #legend
  if (show.legend == F) {
    p <- p + theme(legend.position = "none")
  }
  if (show.legend == T) {
    p <- p + scale_color_discrete(name = "Network")
  }

  p
}

#' Plots brains
#'
#' \code{brainconn} plots and returns a ggraph object of plotted brain connectivity matrix..
#' @author Sidhant Chopra
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases()} or a \code{data.frame()} that meets specifications, see \code{vignette("brainconn")}
#' @param background 'ICBM152', currently the only background option
#' @param background.alpha Number between 0-1 to  set the transparency of the background.
#' @param view A sting to choose the view. Can be any of these: c("ortho", "top", "bottom", "left", "right")
#' @param conmat A adjacency matrix. Can be binary, weights, directed or undirected. see example_* data.
#' @param node.size A integer that determines the diameter of the nodes. Can also be a vector of integers with a length equal to the number of ROIs in the atlas
#' @param node.color A string that sets the node color. e.g. "blue". If set to "network", then nodes will be colored according to the network column of the atlas
#' @param edge.color.weighted A boolean that applies when the conmat is weighted. if \code{TRUE}, then edges will be colored according to the weight \code{FALSE}, the edges will be sized according to weight.
#' @param all.nodes if \code{TRUE}, then all nodes will be shown be hemisphere without ticks. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param edge.color A string that sets the edge color. e.g. "blue".
#' @param edge.alpha Number between 0-1 to  set the transparency of the edges.
#' @param edge.width Number to set the width of the edges.
#' @param labels if \code{TRUE}, ROI labels for all visible nodes will be shown. If \code{FALSE}, then no labes will be shown.
#' @param show.legend if \code{TRUE}, legend will be shown. If \code{FALSE}, then no legend will be shown.
#' @param thr a optional value to set a threshold on the conmat (e.g. edges with a weighted value lower than the one set here will not be shown)
#' @param uthr a optional value to set a upper threshold on the conmat (e.g. edges with a weighted value higher than the one set here will not be shown)
#' @param scale.edge.width If \code{edge.color.weighted=FALSE}, you can use this rescale the edge.width according to weight. e.g. \code{scale.edge.width = c(1,3)}
#' @param label.size If labels=TRUE then, \code{label.size} can can be set as in integer to control the size of the labels.
#' @param label.edge.weight if \code{TRUE}, then the edge weight will be labels along the edge.
#'
#' @return a ggraph object
#'
#' @import ggraph
#' @import cowplot
#' @import grid
#' @import OpenImageR
#' @importFrom grDevices rgb
#' @examples
#' library(brainconn)
#' x <- example_unweighted_undirected
#' brainconn(atlas ="schaefer300_n7", conmat=x, node.size = 3, view="ortho")
#' @export
brainconn2 <- function(
  atlas,
  background = 'ICBM152',
  view = "ortho",
  conmat = NULL,
  # interactive = F,
  node.size = 4,
  node.color = "network",
  all.nodes = FALSE,
  edge.color = "black",
  edge.alpha = 0.8,
  edge.width = 1,
  edge.color.weighted = FALSE,
  labels = FALSE,
  show.legend = TRUE,
  thr = NULL,
  uthr = NULL,
  scale.edge.width = NULL,
  label.size = 1.5,
  label.edge.weight = FALSE,
  background.alpha = 1,
  bg_xmax = 0,
  bg_xmin = 0,
  bg_ymax = 0,
  bg_ymin = 0
) {
  ifelse(is.character(atlas), data <- get(atlas), data <- atlas)

  #set background (add ability to add custom background image)
  if (background != "ICBM152" && view == "ortho") {
    stop(
      "Custom background image detected, view cannot be 'ortho', please select top,
        bottom, left, right, front or back."
    )
  }

  if (!is.null(thr)) {
    conmat[conmat < thr] <- 0
  } #lower threshold graph
  if (!is.null(uthr)) {
    conmat[conmat > thr] <- 0
  } #upper threshold graph
  #loop three times for the three vies that make ortho view

  if (view == "ortho") {
    ortho_list <- list()
    ortho_views <- c("top", "left", "front")
    for (v in 1:3) {
      view <- ortho_views[v]
      bg <- paste0("ICBM152_", view)
      m <- get(bg)
      w <- matrix(
        rgb(m[,, 1], m[,, 2], m[,, 3], m[,, 4] * background.alpha),
        nrow = dim(m)[1]
      )
      background <- rasterGrob(w)
      #} else {stop(paste('please select a valid background: ', as.character(list.backgroud)))
      #}

      #if no conmat is provided, build nparc x  nparc empty one
      nparc <- dim(data)[1]
      if (!exists("conmat")) {
        conmat <- matrix(0L, nrow = nparc, ncol = nparc)
      }

      #convert conmat to matrix
      conmat <- as.matrix(conmat)

      #Remove nodes with no edges
      rownames(conmat) <- colnames(conmat) #this needs to be same same if is.sym to work
      ifelse(
        isSymmetric.matrix(conmat) == TRUE,
        directed <- FALSE,
        directed <- TRUE
      )

      if (all.nodes == FALSE && directed == FALSE) {
        include.vec <- vector(length = dim(data)[1])
        for (i in 1:dim(conmat)[1]) {
          ifelse(
            any(conmat[i, ] != 0),
            include.vec[i] <- 1,
            include.vec[i] <- 0
          )
        }
        data <- data[as.logical(include.vec), , drop = F]
        conmat <- conmat[
          which(rowSums(conmat, na.rm = T) != 0),
          which(colSums(conmat, na.rm = T) != 0),
          drop = F
        ]
      }

      if (all.nodes == FALSE && directed == TRUE) {
        include.vec <- vector(length = dim(data)[1])
        for (i in 1:dim(conmat)[1]) {
          ifelse(
            any(conmat[i, ] != 0) | any(conmat[, i] != 0),
            include.vec[i] <- 1,
            include.vec[i] <- 0
          )
        }
      }

      if (all.nodes == TRUE) {
        include.vec <- vector(length = dim(data)[1])
        include.vec <- rep(1, length = dim(data)[1])
      }

      #in ortho view, only show legend for top view to avoid redundancy
      ifelse(v == 1, show.legend <- F, show.legend <- F)

      ortho_list[[v]] <- build_plot(
        conmat = conmat,
        data = data,
        background = background,
        node.size = node.size,
        view = view,
        node.color = node.color,
        thr = thr,
        uthr = uthr,
        edge.color = edge.color,
        edge.alpha = edge.alpha,
        edge.width = edge.width,
        scale.edge.width = scale.edge.width,
        show.legend = show.legend,
        labels = labels,
        label.size = label.size,
        include.vec = include.vec,
        edge.color.weighted = edge.color.weighted,
        label.edge.weight = label.edge.weight
      )
      if (is.environment(edge.color) == T) {
        ortho_list[[v]] <- ortho_list[[v]] + edge.color
      }
    }

    right_col <- plot_grid(
      ortho_list[[2]],
      ortho_list[[3]],
      nrow = 2,
      rel_heights = c(1, 1.45)
    )
    p <- plot_grid(ortho_list[[1]], right_col, rel_widths = c(1.8, 1.2))
    return(p)
  }

  # If not ortho, then do the below:
  if (background == 'ICBM152') {
    bg <- paste0("ICBM152_", view)
    m <- get(bg)
    w <- matrix(
      rgb(m[,, 1], m[,, 2], m[,, 3], m[,, 4] * background.alpha),
      nrow = dim(m)[1]
    )
  }

  if (background != 'ICBM152') {
    m <- OpenImageR::readImage(background)
  }
  w <- matrix(
    rgb(m[,, 1], m[,, 2], m[,, 3], m[,, 4] * background.alpha),
    nrow = dim(m)[1]
  )
  background <- rasterGrob(w)

  #if no conmat is provided, build nparc x  nparc empty one
  nparc <- dim(data)[1]
  if (!exists("conmat")) {
    conmat <- matrix(0L, nrow = nparc, ncol = nparc)
  }

  #convert conmat to matrix
  conmat <- as.matrix(conmat)

  #Remove nodes with no edges
  rownames(conmat) <- colnames(conmat) #this needs to be same same if is.sym to work
  ifelse(
    isSymmetric.matrix(conmat) == TRUE,
    directed <- FALSE,
    directed <- TRUE
  )

  if (all.nodes == FALSE && directed == FALSE) {
    include.vec <- vector(length = dim(data)[1])
    for (i in 1:dim(conmat)[1]) {
      ifelse(any(conmat[i, ] != 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
    data <- data[as.logical(include.vec), , drop = F]
    conmat <- conmat[
      which(rowSums(conmat, na.rm = T) != 0),
      which(colSums(conmat, na.rm = T) != 0),
      drop = F
    ]
  }

  if (all.nodes == FALSE && directed == TRUE) {
    include.vec <- vector(length = dim(data)[1])
    for (i in 1:dim(conmat)[1]) {
      ifelse(
        any(conmat[i, ] != 0) | any(conmat[, i] != 0),
        include.vec[i] <- 1,
        include.vec[i] <- 0
      )
    }
  }

  if (all.nodes == TRUE) {
    include.vec <- vector(length = dim(data)[1])
    include.vec <- rep(1, length = dim(data)[1])
  }

  #if interactive call build_plot_int, else call build con
  #  source("functions/build_plot.R")
  p <- build_plot(
    conmat = conmat,
    data = data,
    background = background,
    node.size = node.size,
    view = view,
    node.color = node.color,
    thr = thr,
    uthr = uthr,
    edge.color = edge.color,
    edge.alpha = edge.alpha,
    edge.width = edge.width,
    scale.edge.width = scale.edge.width,
    show.legend = show.legend,
    labels = labels,
    label.size = label.size,
    include.vec = include.vec,
    edge.color.weighted = edge.color.weighted,
    label.edge.weight = label.edge.weight,
    bg_xmax = bg_xmax,
    bg_xmin = bg_xmin,
    bg_ymax = bg_ymax,
    bg_ymin = bg_ymin
  )

  #  source("functions/build_plot_int.R")
  #if(interactive==TRUE){p <- build_plot_int(conmat, data, background, node.size=node.size, view,
  #                                             node.color=node.color, thr=NULL, uthr=NULL,
  #                                             edge.color=edge.color,edge.alpha=edge.alpha,
  #                                             edge.width=edge.width,  scale.edge.width=scale.edge.width,
  #                                             show.legend=show.legend, labels=labels, label.size=label.size,
  #                                             include.vec=include.vec, view=view, edge.color.weighted=edge.color.weighted)}
  return(p)
}

psd_df <- function(df, f) {
  dft <- t(df)
  dfib <- dft %>% as_tibble() %>% mutate(fr = f)
}

process_data <- function(p_mean, roi, prefix) {
  p_mean <- data.frame(t(p_mean))
  p_mean[p_mean == 0] <- NA

  colnames(p_mean) <- c(
    "State 1",
    "State 2",
    "State 3",
    "State 4",
    "State 5",
    "State 6"
  )
  states_df <- cbind(roi, p_mean)
  write_csv(states_df, paste0(prefix, "_states_df.csv"))

  states_df_for_hemi <- states_df %>%
    mutate(
      hemi = case_when(
        str_detect(ROI, "_lh") ~ "left",
        str_detect(ROI, "_rh") ~ "right",
        TRUE ~ NA_character_
      ),
      ROI = str_replace(ROI, "_[lr]h", "")
    )

  left_data <- ROI_Glasser %>%
    left_join(states_df_for_hemi %>% filter(hemi == "left"))
  right_data <- ROI_Glasser %>%
    left_join(states_df_for_hemi %>% filter(hemi == "right"))
  tidy_df_hemi <- rbind(left_data, right_data) %>%
    mutate(region = Region)
  write_csv(tidy_df_hemi, paste0(prefix, "_tidy_df_hemi.csv"))

  tidy_df_hemi
}

legend_theme <- theme(
  legend.title.position = "top",
  # Title justification is controlled by hjust/vjust in the element
  legend.title = element_text(hjust = 0.5), #angle = 90,
  legend.text = element_text(size = 5),
  legend.key.size = unit(.25, "cm"),
  legend.key.width = unit(.75, "cm"),
)

create_brain_plot <- function(
  states,
  prefix,
  state_num,
  low_color,
  high_color
) {
  ggplot() +
    geom_brain(
      atlas = states,
      mapping = aes_string(fill = paste0("State", state_num)),
      position = position_brain(side ~ hemi)
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    scale_fill_gradient(low = low_color, high = high_color) +
    labs(x = NULL, y = NULL) +
    guides(x = "none", y = "none") +
    legend_theme
}

power_plot <- function(tidystate) {
  tidystate |>
    ggplot() +
    geom_brain(
      atlas = tidystate,
      position = position_brain(hemi ~ side),
      aes(fill = Power)
    ) +
    #theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank()) +
    #scale_fill_gradient(low = "yellow", high = "red"  ) +
    scale_fill_viridis(option = "H", alpha = .99) +
    labs(x = NULL, y = NULL) +
    guides(x = "none", y = "none") +
    legend_theme +
    facet_wrap(~State, ncol = 2)
  #facet_grid( cols= vars(State))
}

power_plot2 <- function(tidystate) {
  tidystate |>
    ggplot() +
    geom_brain(
      atlas = tidystate,
      position = position_brain(hemi ~ side),
      aes(fill = Power)
    ) +
    #theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank()) +
    #scale_fill_gradient(low = "yellow", high = "red"  ) +
    scale_fill_viridis(option = "H", alpha = .79) +
    labs(x = NULL, y = NULL) +
    guides(x = "none", y = "none") +
    legend_theme +
    #facet_wrap(~Group)
    facet_grid(cols = vars(State), rows = vars(Group))
}

power_diff_plot <- function(tidystate) {
  tidystate |>
    ggplot() +
    geom_brain(
      atlas = tidystate,
      position = position_brain(hemi ~ side),
      size = 0.2,
      aes(fill = Difference)
    ) +
    #theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank()) +
    #scale_fill_gradient(low = "#00FFC8", high = "#F8166F" ,na.value = "#D3D3D3"  ) +
    scale_fill_viridis(option = "H", alpha = 0.7) +
    labs(x = NULL, y = NULL) +
    guides(x = "none", y = "none") +
    legend_theme +
    #theme_void()+

    #facet_wrap(~Group)
    facet_grid(rows = vars(Group), cols = vars(State))
  #facet_wrap(~State, ncol = 2)
}

process_matrix <- function(corr_matrix) {
  dist_matrix <- cor2dist(corr_matrix)
  tresh <- quantile(dist_matrix, 0.05)
  dist_matrix <- dist_matrix * (dist_matrix < tresh)
  return(dist_matrix)
}

create_brainconn_plot <- function(matrix, atlas = "atlas_df") {
  plot <- brainconn2(
    atlas = atlas,
    conmat = matrix,
    node.size = 4,
    #view = c("right"),
    edge.width = 1,
    edge.color.weighted = TRUE,
    scale.edge.width = c(1, 3),
    background.alpha = 0.5,
    show.legend = FALSE
  )
  return(plot)
}

# Function to generate graphs
generate_graphs <- function(mat, name_prefix) {
  gr <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  mst_graph <- mst(gr)
  mst_ggraph <- as_tbl_graph(mst_graph)
  mst_ggraph <- mst_ggraph %>%
    activate(nodes) %>%
    mutate(
      eigen = centrality_eigen(),
      degree = centrality_degree(),
      betweenness = centrality_betweenness(),
      closeness = centrality_closeness()
    )

  betweenness_graph <- ggraph(mst_ggraph, layout = "kk") +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(aes(size = betweenness, color = name)) +
    theme_gray() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  degree_graph <- ggraph(mst_ggraph, layout = "kk") +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(aes(size = degree, color = name)) +
    theme_gray() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  eigen_graph <- ggraph(mst_ggraph, layout = "kk") +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(aes(size = eigen, color = name)) +
    theme_gray() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  closeness_graph <- ggraph(mst_ggraph, layout = "kk") +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(aes(size = closeness, color = name)) +
    theme_gray() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  list(
    betweenness = betweenness_graph,
    degree = degree_graph,
    eigen = eigen_graph,
    closeness = closeness_graph
  )
}

# Function to convert p-value matrix to significance matrix
convert_pvals_to_significance <- function(pval_matrix) {
  # Check if input is a matrix
  if (!is.matrix(pval_matrix)) {
    stop("Input must be a matrix.")
  }

  # Apply cut function to classify p-values in the matrix
  significance_matrix <- apply(pval_matrix, c(1, 2), function(p) {
    cut(
      p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("***", "**", "*", "n.s."),
      right = TRUE
    )
  })

  # Return the transformed matrix
  return(significance_matrix)
}

sum_plot <- function(data) {
  df_max <- data %>%
    group_by(Variable, State, comparison) %>%
    summarise(
      MaxValue = max(Measures),
      Significance = first(Significance),
      .groups = "drop" # Assuming one significance level per state
    )
  custom_colors <- c("LBD" = "#F8766D", "PD" = "#00BFC4", "NC" = "#7CAE00")
  data |>
    ggplot(aes(x = State, y = Measures, fill = Group, label = Significance)) +
    #geom_split_violin(trim = TRUE, alpha = .6)+
    geom_boxplot(
      width = .3,
      alpha = .7,
      #outliers = FALSE,
      outlier.size = 0.25,
      #notch = TRUE,
      position = position_dodge(.4)
    ) +
    #scale_fill_viridis_d() +
    scale_fill_manual(values = custom_colors) +
    geom_text(
      data = df_max,
      aes(x = State, y = MaxValue + MaxValue * 0.05, label = Significance),
      inherit.aes = FALSE,
      vjust = 0
    ) +
    facet_grid(
      cols = vars(comparison),
      rows = vars(Variable),
      scale = "free_y",
    )
  #facet_wrap(~comparison,ncol = 3,  scale = "free", )
  #facet_grid(rows = vars(comparison), cols = vars(Variable), scale = "free", )
}

sum_plot2 <- function(data) {
  df_max <- data %>%
    group_by(Variable, State, comparison) %>%
    summarise(
      MaxValue = max(Measures),
      Significance = first(Significance),
      .groups = "drop" # Assuming one significance level per state
    )
  custom_colors <- c("LBD" = "#F8766D", "PD" = "#00BFC4", "NC" = "#7CAE00")
  data |>
    ggplot(aes(x = State, y = Measures, fill = Group, label = Significance)) +
    #geom_split_violin(trim = TRUE, alpha = .6)+
    geom_boxplot(
      width = .5,
      alpha = .7,
      #outliers = FALSE,
      outlier.size = 0.25,
      position = position_dodge(.7)
    ) +
    #geom_violin(alpha = .4, position = position_dodge(.8))+
    #scale_fill_viridis_d()+
    geom_text(
      data = df_max,
      aes(x = State, y = MaxValue + MaxValue * 0.05, label = Significance),
      inherit.aes = FALSE,
      vjust = 0
    ) +
    labs(x = NULL, y = NULL) +
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_fill_manual(values = custom_colors) +

    #facet_grid(cols = vars(State),
    #rows  = vars(Variable),
    #scale = "free",
    facet_wrap(~Variable, scale = "free")
}

psd_plot <- function(data) {
  data |>
    ggplot() +
    geom_line(aes(x = fr, y = Power, color = Group)) +
    xlab("Frequency (Hz)") +
    ylab("PSD (a.u.)") +
    facet_wrap(~State, scale = "free")
  #facet_grid(cols = vars(State),
  # scale = "free")
}

power_diff_plot2 <- function(tidystate) {
  cortical_pos <- c(
    "left lateral",
    "left medial",
    "right medial",
    "right lateral"
  )
  tidystate |>
    ggplot() +
    geom_brain(
      atlas = tidystate,
      position = position_brain(cortical_pos),
      aes(fill = Difference)
    ) +
    #theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank()) +
    #scale_fill_gradient(low = "blue", high = "red"  ) +
    #scale_fill_viridis_d(option = "magma")+
    scale_fill_viridis(option = "H", alpha = 0.5) +
    labs(x = NULL, y = NULL) +
    guides(x = "none", y = "none") +
    legend_theme +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +

    #theme_tufte(base_size = 32, base_family = "serif", ticks = TRUE)+
    #facet_wrap(~State)
    facet_grid(rows = vars(State))
}

power_diff_plot3 <- function(tidystate, state) {
  #cortical_pos <- c("left lateral", "left medial", "right medial", "right lateral")
  tidystate |>
    filter(State == state) |>
    brain_join(glasser) %>%
    reposition_brain(hemi ~ side) %>%
    ggplot(aes(fill = Difference)) +
    geom_sf(show.legend = FALSE) +
    #scale_fill_gradient(low = "blue", high = "red") +
    scale_fill_viridis(option = "D", alpha = 0.5) +
    labs(x = NULL, y = NULL) +
    guides(x = "none", y = "none") +
    legend_theme +
    ggtitle(state)
}

process_matrix <- function(corr_matrix) {
  dist_matrix <- cor2dist(corr_matrix)
  tresh <- quantile(dist_matrix, 0.05)
  dist_matrix <- dist_matrix * (dist_matrix < tresh)
  return(dist_matrix)
}

tidy_psd <- function(psd, demo) {
  df <- melt(psd)
  colnames(df) <- c(
    "session",
    "State",
    "Region",
    "fr",
    "value"
  )
  df <- df %>%
    mutate(
      hemi = case_when(
        str_detect(Region, "_lh") ~ "left",
        str_detect(Region, "_rh") ~ "right",
        TRUE ~ NA_character_
      ),
      ROI = str_replace(Region, "_[lr]h", "")
    )
  df <- df %>% left_join(demo)
}

LBDNC_powerr_plot1 <- function(i) {
  tfmat <- LBD_NC_C_power_diff != 0
  tfmat <- t(tfmat)
  NC <- NC_psd_mean[i, , ] %>% melt()
  LBD <- LBD_psd_mean[i, , ] %>% melt()
  c_list <- list(NC, LBD)
  c_flag <- c("NC", "LBD") #,"CC","NINANC")
  c_data <- tibble(Group = c_flag, data = c_list)
  c_df <- unnest(c_data)

  unique_values <- unique(c_df$X1)
  tf_map <- setNames(tfmat[, i], unique_values)
  filtered_df <- c_df %>%
    filter(tf_map[X1]) %>%
    group_by(X1, X2, Group) %>%
    summarize(mean_value = mean(value, na.rm = TRUE)) %>%
    ungroup()
  ggplot(filtered_df) +
    geom_line(aes(x = X2, y = mean_value, color = Group)) +
    xlab("Frequency (Hz)") +
    ylab("PSD (a.u.)") +
    facet_wrap(~X1, scale = "free")

  main <- filtered_df %>% select(X1) %>% unique()
  print(main)
  return(main)
}

LBDPD_power_plot <- function() {
  LBD_PD_PowerBand <- list()
  for (i in 1:6) {
    tfLBD_PD_PowerBand <- LBD_PD_power_diff != 0
    tfLBD_PD_PowerBand <- t(tfLBD_PD_PowerBand)
    PD_PowerBand <- PD_psd_mean[i, , ] %>% melt()
    LBD_PowerBand <- LBD_psd_mean[i, , ] %>% melt()
    LBD_PD_PowerBand_list <- list(PD_PowerBand, LBD_PowerBand)
    LBD_PowerBand_flag <- c("PD", "LBD") #,"CC","NINANC")
    LBD_PowerBand_data <- tibble(
      Group = LBD_PowerBand_flag,
      data = LBD_PD_PowerBand_list
    )
    LBD_PD_PowerBand_df <- unnest(LBD_PowerBand_data)

    LBD_PD_PowerBand_unique_values <- unique(LBD_PD_PowerBand_df$X1)

    LBD_PD_PowerBand_tf_map <- setNames(
      tfLBD_PD_PowerBand[, i],
      LBD_PD_PowerBand_unique_values
    )
    LBD_PD_PowerBand_filtered_df <- LBD_PD_PowerBand_df %>%
      filter(LBD_PD_PowerBand_tf_map[X1]) %>%
      group_by(X2, Group) %>%
      summarise(,
        mean_value = mean(value, na.rm = TRUE),
        se = sd(value) / sqrt(n())
      ) %>%
      ungroup()
    LBD_PD_PowerBand[[i]] <- LBD_PD_PowerBand_filtered_df
  }
  LBD_PD_PowerBand_flags <- c(
    "State 1",
    "State 2",
    "State 3",
    "State 4",
    "State 5",
    "State 6"
  )
  LBD_PD_PowerBandfil_data <- tibble(
    States = LBD_PD_PowerBand_flags,
    data = LBD_PD_PowerBand
  )
  LBD_PD_PowerBandfil_df <- unnest(LBD_PD_PowerBandfil_data)
  #fil_tidy <- fil_df %>%
  #pivot_longer(cols = starts_with("State"), names_to = "State", values_to = "Significance")
  custom_colors <- c("LBD" = "#F8766D", "PD" = "#00BFC4")

  ggplot(LBD_PD_PowerBandfil_df, aes(x = X2, y = mean_value, color = Group)) +
    geom_line() +
    geom_errorbar(
      aes(ymin = mean_value - se, ymax = mean_value + se),
      width = 0.4
    ) +
    xlab("Frequency (Hz)") +
    ylab("PSD (a.u.)") +
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+

    #facet_grid(cols = vars(Significance), scale = "free")
    facet_wrap(~States, scale = "free")
}

LBDNC_power_plot1 <- function() {
  filtered_df_state <- list()
  for (i in 1:6) {
    i = 1
    tfmat <- LBD_NC_C_power_diff != 0
    tfmat <- t(tfmat)
    NC <- NC_psd_mean[i, , ] %>% melt()
    LBD <- LBD_psd_mean[i, , ] %>% melt()
    c_list <- list(PD, LBD)
    c_flag <- c("NC", "LBD") #,"CC","NINANC")
    c_data <- tibble(Group = c_flag, data = c_list)
    c_df <- unnest(c_data)

    unique_values <- unique(c_df$X1)
    tf_map <- setNames(tfmat[, i], unique_values)
    filtered_df <- c_df %>%
      filter(tf_map[X1]) %>%
      group_by(X2, Group) %>%
      summarise(, mean_value = mean(value, na.rm = TRUE)) %>%
      ungroup()
    filtered_df_state[[i]] <- filtered_df
  }
  filtered_df_state_flags <- c(
    "State 1",
    "State 2",
    "State 3",
    "State 4",
    "State 5",
    "State 6"
  )
  fil_data <- tibble(States = filtered_df_state_flags, data = filtered_df_state)
  fil_df <- unnest(fil_data)
  fil_tidy <- fil_df %>%
    pivot_longer(
      cols = starts_with("State"),
      names_to = "State",
      values_to = "Significance"
    )
  custom_colors <- c("LBD" = "#CF4446FF", "NC" = "#FD9A1AFF")
  ggplot(fil_df) +
    geom_line(aes(x = X2, y = mean_value, color = Group)) +
    xlab("Frequency (Hz)") +
    ylab("PSD (a.u.)") +
    #facet_grid(cols = vars(Significance), scale = "free")
    scale_color_manual(values = custom_colors) +
    facet_wrap(~State, scale = "free")
}

LBDNC_power_plot <- function() {
  LBD_NC_PowerBand <- list()
  for (i in 1:6) {
    tfLBD_NC_PowerBand <- LBD_NCNINA_power_diff != 0
    tfLBD_NC_PowerBand <- t(tfLBD_NC_PowerBand)
    NC_PowerBand <- NC_psd_mean[i, , ] %>% melt()
    LBD_PowerBand <- LBD_psd_mean[i, , ] %>% melt()
    LBD_NC_PowerBand_list <- list(NC_PowerBand, LBD_PowerBand)
    LBD_NC_PowerBand_flag <- c("NC", "LBD") #,"CC","NINANC")
    LBD_NC_PowerBand_data <- tibble(
      Group = LBD_NC_PowerBand_flag,
      data = LBD_NC_PowerBand_list
    )
    LBD_NC_PowerBand_df <- unnest(LBD_NC_PowerBand_data)

    LBD_NC_PowerBand_unique_values <- unique(LBD_NC_PowerBand_df$X1)

    LBD_NC_PowerBand_tf_map <- setNames(
      tfLBD_NC_PowerBand[, i],
      LBD_NC_PowerBand_unique_values
    )
    LBD_NC_PowerBand_filtered_df <- LBD_NC_PowerBand_df %>%
      filter(LBD_NC_PowerBand_tf_map[X1]) %>%
      group_by(X2, Group) %>%
      summarise(,
        mean_value = mean(value, na.rm = TRUE),
        se = sd(value) / sqrt(n())
      ) %>%
      ungroup()
    LBD_NC_PowerBand[[i]] <- LBD_NC_PowerBand_filtered_df
  }
  LBD_NC_PowerBand_flags <- c(
    "State 1",
    "State 2",
    "State 3",
    "State 4",
    "State 5",
    "State 6"
  )
  LBD_NC_PowerBandfil_data <- tibble(
    States = LBD_NC_PowerBand_flags,
    data = LBD_NC_PowerBand
  )
  LBD_NC_PowerBandfil_df <- unnest(LBD_NC_PowerBandfil_data)
  #fil_tidy <- fil_df %>%
  #pivot_longer(cols = starts_with("State"), names_to = "State", values_to = "Significance")
  custom_colors <- c("LBD" = "#F8766D", "NC" = "#7CAE00")
  ggplot(LBD_NC_PowerBandfil_df, aes(x = X2, y = mean_value, color = Group)) +
    geom_line() +
    geom_errorbar(
      aes(ymin = mean_value - se, ymax = mean_value + se),
      width = 0.4
    ) +
    #geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se), alpha = 0.2) +
    #geom_point() +
    #geom_smooth(method = "loess", se = TRUE) +
    xlab("Frequency (Hz)") +
    ylab("PSD (a.u.)") +
    #facet_grid(cols = vars(Significance), scale = "free")
    scale_color_manual(values = custom_colors) +
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    #theme_minimal()+
    #theme_bw()+
    facet_wrap(~States, scale = "free")
}

correlation_with_column <- function(df, column_name) {
  # Check if the specified column exists in the data frame
  if (!(column_name %in% colnames(df))) {
    stop("Specified column not found in the data frame")
  }

  # Initialize an empty data frame to store results
  results <- data.frame(
    Column = character(),
    Correlation = numeric(),
    P_Value = numeric(),
    Stars = character(),
    stringsAsFactors = FALSE
  )

  # Loop over each column in the data frame
  for (col in colnames(df)) {
    if (col != column_name) {
      # Calculate correlation and p-value
      cor_test <- cor.test(df[[column_name]], df[[col]], use = "complete.obs")
      cor_val <- cor_test$estimate
      p_val <- cor_test$p.value

      # Determine significance level
      stars <- ""
      if (p_val < 0.001) {
        stars <- "***"
      } else if (p_val < 0.01) {
        stars <- "**"
      } else if (p_val < 0.05) {
        stars <- "*"
      } else if (p_val < 0.1) {
        stars <- "."
      }

      # Add results to the data frame
      results <- rbind(
        results,
        data.frame(
          Column = col,
          Correlation = cor_val,
          P_Value = p_val,
          Stars = stars,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  return(results)
}
