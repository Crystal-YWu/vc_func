library(ggplot2)
library(cowplot)

.abf_ylab <- function(abf, chan_id)
  paste(abf$ChannelNameGuess[chan_id], "/", abf$ChannelUnit[chan_id])

#' Plot_Channel plots a single channel
#'
#' @param abf data loaded from abf file
#' @param chan_id channel id to plot
#'
#' @return a ggplot object
#' @export
#'
#' @examples Plot_Channel(abf, 2)
Plot_Channel <- function(abf, chan_id) {
  g <- ggplot(data = abf$ByChannel[[chan_id]], aes(x = abf$X_ticks))
  for (i in 1:abf$NumOfEpisodes)
    g <- g + geom_line(aes_string(y = EpisodeName(i)))
  g <- g + ylab(.abf_ylab(abf, chan_id))
  g <- g + xlab("Time Ticks")

  g
}

#' Plot_ChannelWithIntv plots a single channel with two dotted lines indicating an interval
#'
#' @param abf data loaded from abf file
#' @param chan_id channel id to plot
#' @param intv a vector of 2 numerics describing an interval
#'
#' @return a ggplot object
#' @export
#'
#' @examples intv <- FindSamplingInterval(abf); Plot_ChannelWithIntv(abf, 1, intv)
Plot_ChannelWithIntv <- function(abf, chan_id, intv) {
  g <- Plot_Channel(abf, chan_id)
  g <- g + geom_vline(xintercept = intv[1], linetype = "dotted")
  g <- g + geom_vline(xintercept = intv[2], linetype = "dotted")

  g
}

#' Plot_IVChannel plots both current and voltage channels on a same graph
#'
#' @param abf data loaded from abf file
#'
#' @return a ggplot object
#' @export
#'
#' @examples Plot_IVChannel(abf)
Plot_IVChannel <- function(abf) {
  g1 <- Plot_Channel(abf, 1)
  g2 <- Plot_Channel(abf, 2)
  g <- plot_grid(g1, g2, ncol = 2)

  g
}

#' Plot_IVChannelWithIntv plots both current and voltage channels on a same graph along with two dotted lines indicating an interval
#'
#' @param abf data loaded from abf file
#' @param intv a vector of 2 numerics describing an interval
#'
#' @return a ggplot object
#' @export
#'
#' @examples intv <- FindSamplingInterval(abf); Plot_IVChannelWithIntv(abf, intv)
Plot_IVChannelWithIntv <- function(abf, intv) {
  g1 <- Plot_ChannelWithIntv(abf, 1, intv)
  g2 <- Plot_ChannelWithIntv(abf, 2, intv)
  g <- plot_grid(g1, g2, ncol = 2)

  g
}

GetAll_UniformYLim <- function(abf_list, chan_id) {
  ylimit = c(0, 0)
  n <- length(abf_list)
  for (i in 1:n) {
    nepi <- ncol(abf_list[[i]]$ByChannel[[chan_id]])
    mid <- nrow(abf_list[[i]]$ByChannel[[chan_id]]) %/% 2
    mid_val <- as.vector(abf_list[[i]]$ByChannel[[chan_id]][mid, ])
    upper <- 2 * mid_val[nepi] - mid_val[nepi - 1]
    lower <- 2 * mid_val[1] - mid_val[2]
    ylimit <- range(ylimit, mid_val, upper, lower)
  }

  return(ylimit)
}
GetAll_Channel_G <- function(abf_list, chan_id, uniform_y) {

  g <- list()
  n <- length(abf_list)
  if (uniform_y)
    ylimit <- GetAll_UniformYLim(abf_list, chan_id)

  for (i in 1:n) {
    g[[i]] <- Plot_Channel(abf_list[[i]], chan_id)
    if (uniform_y)
      g[[i]] <- g[[i]] + ylim(ylimit)
  }

  return(g)
}

#' PlotAll_Channel plots same channel for all abf data in abf_list
#'
#' @param abf_list a list of abf data
#' @param chan_id channel id to plot
#' @param label OPTIONAL, determines if labels are added to subplots
#' @param uniform_y OPTIONAL, determines if y axes are unified to same scale
#'
#' @return a ggplot object
#' @export
#'
#' @examples abf_list <- abf2.load_in_folder(path, files); PlotAll_Channel(abf_list, 1)
PlotAll_Channel <- function(abf_list, chan_id, label = TRUE, uniform_y = TRUE) {
  g <- GetAll_Channel_G(abf_list, chan_id, uniform_y)
  n <- length(g)
  if (label)
    for (i in 1:n)
      g[[i]] <- g[[i]] + ggtitle(as.character(i))
  ncols <- ceiling(sqrt(n))
  gg <- plot_grid(plotlist = g, ncol = ncols)

  gg
}

#' PlotAll_ChannelWithIntv plots same channel for all abf data in abf_list as well as intervals
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#' @param chan_id channel id to plot
#' @param label OPTIONAL, determines if labels are added to subplots
#' @param uniform_y OPTIONAL, determines if y axes are unified to same scale
#'
#' @return a ggplot object
#' @export
#'
#' @examples See PlotAll_Channel and Plot_ChannelWithIntv
PlotAll_ChannelWithIntv <- function(abf_list, intv_list, chan_id, label = TRUE, uniform_y = TRUE) {
  g <- GetAll_Channel_G(abf_list, chan_id, uniform_y)
  n <- length(g)
  if (label)
    for (i in 1:n)
      g[[i]] <- g[[i]] + ggtitle(as.character(i))
  for (i in 1:n) {
    g[[i]] <- g[[i]] +
      geom_vline(xintercept = intv_list[[i]][1], linetype = "dotted") +
      geom_vline(xintercept = intv_list[[i]][2], linetype = "dotted")
  }
  ncols <- ceiling(sqrt(n))
  gg <- plot_grid(plotlist = g, ncol = ncols)

  gg
}

#' PlotAll_IVChannel plots both current and voltage channels for all abf data
#'
#' @param abf_list a list of abf data
#' @param title_list OPTIONAL, a list of titles corresponds to abf_list
#' @param uniform_y OPTIONAL, determines if y axes are unified to same scale
#'
#' @return a list of ggplot objects
#' @export
#'
#' @examples see Plot_IVChannel and PlotALL_Channel
PlotAll_IVChannel <- function(abf_list, title_list = NULL, uniform_y = TRUE) {
  g1 <- GetAll_Channel_G(abf_list, 1, uniform_y)
  g2 <- GetAll_Channel_G(abf_list, 2, uniform_y)
  n <- length(abf_list)

  gg <- list()
  for (i in 1:n) {
    if (!is.null(title_list))
      g1[[i]] <- g1[[i]] + ggtitle(title_list[[i]])
    else
      g1[[i]] <- g1[[i]] + ggtitle(as.integer(i))
    gg[[i]] <- plot_grid(g1[[i]], g2[[i]], ncol = 2)
  }

  gg
}

#' PlotAll_IVChannelWithIntv plots both current and voltage channels for all abf data as well as intervals
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#' @param title_list OPTIONAL, a list of titles corresponds to abf_list
#' @param uniform_y OPTIONAL, determines if y axes are unified to same scale
#'
#' @return a list of ggplot objects
#' @export
#'
#' @examples see Plot_IVChannelWithIntv and PlotALL_Channel
PlotAll_IVChannelWithIntv <- function(abf_list, intv_list, title_list = NULL, uniform_y = TRUE) {
  g1 <- GetAll_Channel_G(abf_list, 1, uniform_y)
  g2 <- GetAll_Channel_G(abf_list, 2, uniform_y)
  n <- length(abf_list)

  gg <- list()
  for (i in 1:n) {
    if (!is.null(title_list))
      g1[[i]] <- g1[[i]] + ggtitle(title_list[[i]])
    else
      g1[[i]] <- g1[[i]] + ggtitle(as.integer(i))
    g1[[i]] <- g1[[i]] +
      geom_vline(xintercept = intv_list[[i]][1], linetype = "dotted") +
      geom_vline(xintercept = intv_list[[i]][2], linetype = "dotted")
    g2[[i]] <- g2[[i]] +
      geom_vline(xintercept = intv_list[[i]][1], linetype = "dotted") +
      geom_vline(xintercept = intv_list[[i]][2], linetype = "dotted")
    gg[[i]] <- plot_grid(g1[[i]], g2[[i]], ncol = 2)
  }

  gg
}
