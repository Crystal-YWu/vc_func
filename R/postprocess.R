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
  nepi <- abf_list[[1]]$NumOfEpisodes
  cnames <- EpisodeName(1:nepi)
  df <- data.frame(matrix(ncol = nepi, nrow = 0))
  colnames(df) <- cnames
  for (i in 1:n) {
    if (any(is.na(intv_list[[i]])))
      cmeans <- rep(NA, nepi)
    else {
      intv <- intv_list[[i]][1]:intv_list[[i]][2]
      cmeans <- colMeans(abf_list[[i]]$ByChannel[[chan_id]][intv, ])
    }
    mnames <- names(cmeans)
    for (j in 1:length(cmeans))
      df[i, mnames[j]] <- cmeans[j]
  }

  return(df)
}

#' AllSamples_VoltageMeans calculates average voltage for every episode within the given interval
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#' @param transpose OPTIONAL, determines if transpose the data.frame so episodes become row vector
#'
#' @return a data.frame of the calculated means.
#' @export
#'
#' @examples
AllSamples_VoltageMeans <- function(abf_list, intv_list, transpose = FALSE) {

  chan_id <- GuessVoltageChan(abf_list[[1]])
  df <- AllSamples_ChannelMeans(abf_list, intv_list, chan_id)
  if (transpose)
    df <- t(df)

  return(df)
}

#' AllSamples_VoltageMeans calculates average current for every episode within the given interval
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of intervals
#' @param transpose OPTIONAL, determines if transpose the data.frame so episodes become row vector
#'
#' @return a data.frame of the calculated means.
#' @export
#'
#' @examples
AllSamples_CurrentMeans <- function(abf_list, intv_list, transpose = FALSE) {

  chan_id <- GuessCurrentChan(abf_list[[1]])
  df <- AllSamples_ChannelMeans(abf_list, intv_list, chan_id)
  if (transpose)
    df <- t(df)

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

  df_current <- AllSamples_CurrentMeans(abf_list, intv_list, transpose = FALSE)
  df_voltage <- AllSamples_VoltageMeans(abf_list, intv_list, transpose = FALSE)
  col_mean_v <- colMeans(df_voltage, na.rm = TRUE)
  col_sd_v <- colSds(df_voltage, na.rm = TRUE)
  col_mean_i <- colMeans(df_current, na.rm = TRUE)
  col_sd_i <- colSds(df_current, na.rm = TRUE)
  df <- data.frame(col_mean_v, col_sd_v, col_mean_i, col_sd_i)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")

  return(df)
}

#' AllSamples_IVRaw aggregates all abf data into a list of data.frame
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of corresponding sampling intervals
#'
#' @return A list of data.frame, each element correspond to an abf data.
#' @export
#'
#' @examples
AllSamples_IVRaw <- function(abf_list, intv_list){

  ret <- list()
  n <- length(abf_list)
  current <- AllSamples_CurrentMeans(abf_list, intv_list, transpose = TRUE)
  voltage <- AllSamples_VoltageMeans(abf_list, intv_list, transpose = TRUE)

  for (i in 1:n) {
    ret[[i]] <- data.frame(voltage[, i], current[, i])
    colnames(ret[[i]]) <- c("Voltage", "Current")
  }

  return(ret)
}

#' AllSamples_IRaw aggregates all abf CURRENT data into a list of data.frame
#'
#' @param abf_list a list of abf data
#' @param intv_list a list of corresponding sampling intervals
#'
#' @return A list of data.frame, each element correspond to an abf CURRENT data.
#' @export
#'
#' @examples
AllSamples_IRaw <- function(abf_list, intv_list) {

  ret <- list()
  n <- length(abf_list)
  current <- AllSamples_CurrentMeans(abf_list, intv_list, transpose = TRUE)

  for (i in 1:n) {
    ret[[i]] <- data.frame(current[, i])
    colnames(ret[[i]]) <- c("Current")
  }

  return(ret)
}

colSds <- function(df, na.rm = FALSE) {

  if (na.rm)
    n <- colSums(!is.na(df))
  else
    n <- nrow(df)
  #Custom implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df, na.rm = na.rm) - colMeans(df, na.rm = na.rm)^2
  ret <- sqrt(ret * n / (n -1))

  return(ret)
}

#' ChangeInterval changes interval list manually
#'
#' @param intv_list interval list
#' @param sample_id sample id
#' @param intv_start interval start position
#' @param intv_length interval length
#'
#' @return a list of intervals that is changed
#' @export
#'
#' @examples intv_list <- ChangeInterval(intv_list, 1, 6000, 200)
ChangeInterval <- function(intv_list, sample_id, intv_start, intv_length) {
  intv_list[[sample_id]][1] <- intv_start
  intv_list[[sample_id]][2] <- intv_start + intv_length -1

  return(intv_list)
}
