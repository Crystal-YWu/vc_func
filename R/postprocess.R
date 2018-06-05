#' AllSamples_ChannelMeans calculates average channel data for every episode within the given interval
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#' @param chan_id channel id to calculate
#'
#' @return a data.frame of the calculated means.
#' @export
#'
#' @examples
AllSamples_ChannelMeans <- function(abf_list, intv_list, chan_id) {
  n <- length(abf_list)
  cnames <- AvailEpisodeName(abf_list[[1]]$ByChannel[[chan_id]])
  df <- data.frame(matrix(ncol = length(cnames), nrow = 0))
  colnames(df) <- cnames
  for (i in 1:n) {
    intv <- intv_list[[i]][1]:intv_list[[i]][2]
    df[i, ] <- colMeans(abf_list[[i]]$ByChannel[[chan_id]][intv, ])
  }

  return(df)
}

#' AllSamples_VoltageMeans calculates average voltage for every episode within the given interval
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#'
#' @return a data.frame of the calculated means.
#' @export
#'
#' @examples
AllSamples_VoltageMeans <- function(abf_list, intv_list) {

  chan_id <- GuessVoltageChan(abf_list[[1]])
  df <- AllSamples_ChannelMeans(abf_list, intv_list, chan_id)

  return(df)
}

#' AllSamples_VoltageMeans calculates average current for every episode within the given interval
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#'
#' @return a data.frame of the calculated means.
#' @export
#'
#' @examples
AllSamples_CurrentMeans <- function(abf_list, intv_list) {

  chan_id <- GuessCurrentChan(abf_list[[1]])
  df <- AllSamples_ChannelMeans(abf_list, intv_list, chan_id)

  return(df)
}

#' AllSamples_IVSummary calculates average current and voltage for all samples listed in abf_list
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#'
#' @return a data.frame with Voltage, SEM Voltage, Current, SEM Current values
#' @export
#'
#' @examples
AllSamples_IVSummary <- function(abf_list, intv_list) {

  df_current <- AllSamples_CurrentMeans(abf_list, intv_list)
  df_voltage <- AllSamples_VoltageMeans(abf_list, intv_list)
  col_mean_v <- colMeans(df_voltage)
  col_sd_v <- colSds(df_voltage)
  col_mean_i <- colMeans(df_current)
  col_sd_i <- colSds(df_current)
  df <- data.frame(col_mean_v, col_sd_v, col_mean_i, col_sd_i)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")

  return(df)
}

colSds <- function(df) {

  n <- nrow(df)
  #Custom implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df) - colMeans(df)^2
  ret <- sqrt(ret * n / (n -1))

  return(ret)
}
