#' FindSamplingInterval finds a suitable intervals for sampling current and voltage data of the loaded abf file
#'
#' @param abf data loaded from abf file
#' @param current_chan_id OPTIONAL, current channel id, usually can be identified automatically
#' @param voltage_chan_id OPTIONAL, voltage channel id, usually can be identified automatically
#' @param interval_size OPTIONAL, prefered minimum interval size.
#' @param max_interval_expansion_rate OPTIONAL, determine if the function returns variable interval size.
#' @param allowed_voltage_delta OPTIONAL, allowed maximum deviation of voltage W.R.T protocol setting in the sampling interval.
#' @param epoch_name OPTIONAL, determines which in which epoch to find the interval.
#' @param backward_seach OPTIONAL, perform interval search along negative direction
#' @param backward_search_ratio OPTIONAL, determines how aggressive the backward search is
#'
#' @return a vector of 2 numeric describes an interval
#' @export
#'
#' @examples Usually this function does all the work automatically and no extra parameters are needed: intv <- FindSamplingInterval(abf)
FindSamplingInterval <- function(abf, current_chan_id = 0, voltage_chan_id = 0,
                                 interval_size = 0, max_interval_expansion_rate = 1,
                                 allowed_voltage_delta = 0, epoch_name = "auto",
                                 backward_seach = FALSE, backward_search_ratio = 3) {

  #figure out current channel and voltage channel
  if (current_chan_id == 0) {
    current_chan_id <- GuessCurrentChan(abf)
  }
  if (voltage_chan_id == 0) {
    voltage_chan_id <- GuessVoltageChan(abf)
  }
  if (is.na(current_chan_id) || is.na(voltage_chan_id))
    stop("Failed to find current or voltage channel id. Please provide manually.")

  #figure out which epoch to search
  epoch <- GetEpochId(abf, epoch_name)

  #figure out a reasonable allowed voltage error, 5% of incremental level
  if (allowed_voltage_delta == 0)
    allowed_voltage_delta <- DefaultAllowedVoltageDelta(abf, epoch)

  #figure out a reasonable min_interval_size, 10ms of scan
  if (interval_size == 0)
    interval_size <- floor(10 / abf$SampleInterval_ms)

  epoch_range <- ExpectedEpochRange(abf, epoch, voltage_chan_id, allowed_voltage_delta)
  if (backward_seach) {
    nepoch <- length(epoch_range)
    nepoch_ptr <- floor(nepoch * (1.0 - 1.0/backward_search_ratio))
    epoch_range <- epoch_range[nepoch_ptr:nepoch]
  }

  #We expect current to be stable at some point, so we can simply exploit histogram
  #to extract expexted/target current for each episode (the highest frequency samples)
  target_c <- ExpectedEpiCurrent(abf, epoch_range, current_chan_id)
  chan_c <- abf$ByChannel[[current_chan_id]]

  win <- GetWindow(epoch_range, interval_size, 1, 0)
  best_delta <- abs(as.vector(chan_c[win[1], ]) - target_c)
  best_score <- SimpleSd(chan_c, win)
  best_size <- 1
  best_pos <- 0
  for (i in 1:max_interval_expansion_rate) {
    win_size <- i
    win_pos <- 0
    while (TRUE) {
      win <- GetWindow(epoch_range, interval_size, win_size, win_pos)
      if (!CheckWindow(epoch_range, win))
        break
      delta <- abs(as.vector(chan_c[win[1], ]) - target_c)
      score <- SimpleSd(chan_c, win)
      md_idx <- which.max(best_delta)
      #NOTE:
      # OR used here is not a bug (hopefully). Because we are searching along and
      # positive direction current reading toward later in an epoch is favorable,
      # thus delta is used to eliminate premature stable current readings.
      if (delta[md_idx] < best_delta[md_idx] || all(score < best_score)) {
        best_delta <- delta
        best_score <- score
        best_size <- win_size
        best_pos <- win_pos
      }
      win_pos <- win_pos + 1
    }
  }
  win <- GetWindow(epoch_range, interval_size, best_size, best_pos)

  return(range(win))
}

#' FindAllSampleInterval deos FindSamplingInterval for a list of abf data
#'
#' @param abf_list a list of abf data
#' @param current_chan_id OPTIONAL, current channel id, usually can be identified automatically
#' @param voltage_chan_id OPTIONAL, voltage channel id, usually can be identified automatically
#' @param interval_size OPTIONAL, prefered minimum interval size.
#' @param max_interval_expansion_rate OPTIONAL, determine if the function returns variable interval size.
#' @param allowed_voltage_delta OPTIONAL, allowed maximum deviation of voltage W.R.T protocol setting in the sampling interval.
#' @param epoch_name OPTIONAL, determines which in which epoch to find the interval.
#' @param backward_seach OPTIONAL, perform interval search along negative direction
#' @param backward_search_ratio OPTIONAL, determines how aggressive the backward search is
#'
#' @return a list of intervals found
#' @export
#'
#' @examples
FindAllSamplingInterval <- function(abf_list, current_chan_id = 0, voltage_chan_id = 0,
                                  interval_size = 0, max_interval_expansion_rate = 1,
                                  allowed_voltage_delta = 0, epoch_name = "auto",
                                  backward_seach = FALSE, backward_search_ratio = 3) {
  intv <- list()
  for (i in 1:length(abf_list))
    intv[[i]] <- FindSamplingInterval(abf_list[[i]], current_chan_id, voltage_chan_id,
                                      interval_size, max_interval_expansion_rate,
                                      allowed_voltage_delta, epoch_name, backward_seach,
                                      backward_search_ratio)

  return(intv)
}

GuessVoltageChan <- function(abf) match("Voltage", abf$ChannelNameGuess)
GuessCurrentChan <- function(abf) match("Current", abf$ChannelNameGuess)
GetEpochId <- function(abf, epoch_name) {
  epoch <- 0
  n_epoch <- nrow(abf$Sections$EpochPerDAC)
  epoch_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  if (epoch_name == "auto") {
    if (n_epoch != 3)
      stop("Failed to identify proper earch_epoch. Please provide manually.")
    epoch <- 2
  } else {
    epoch <- match(epoch_name, epoch_names)
    if (is.na(epoch))
      stop("Please provide correct search_epoch. (A-J as in protocol setting)")
  }

  return(epoch)
}
DefaultAllowedVoltageDelta <- function(abf, epoch) abs(abf$Sections$EpochPerDAC$fEpochLevelInc[epoch]) * 0.05

#Calculate sampling window
GetWindow <- function(search_range, incr_step, win_size, win_pos) {
  init_ptr = range(search_range)[1]
  ret <- seq(from = init_ptr + win_pos * incr_step, by = 1, length.out = win_size * incr_step)
  return(ret)
}
#Check if sampling window is within search range
CheckWindow <- function(search_range, win) {
  min(win) >= min(search_range) && max(win) <= max(search_range)
}
#Calculate the range that all episodes of given epoch are within allowed range of set voltage
ExpectedEpochRange <- function(abf, epoch, voltage_chan_id, allowed_voltage_delta) {
  #figure out target voltage for each episode
  target_v <- seq(from = abf$Sections$EpochPerDAC$fEpochInitLevel[epoch],
                  by = abf$Sections$EpochPerDAC$fEpochLevelInc[epoch],
                  length.out = abf$NumOfEpisodes)
  #some episodes may have been removed
  target_v <- target_v[AvailEpisodeIdx(abf$ByChannel[[voltage_chan_id]])]

  allowed <- list()
  for (i in 1:length(target_v)) {
    tmp <- which(abs(abf$ByChannel[[voltage_chan_id]][, i] - target_v[i]) <= allowed_voltage_delta)
    allowed[[i]] <- min(tmp):max(tmp)
  }
  epoch_range <- Reduce(f = intersect, x = allowed)

  return(epoch_range)
}
#Calculate expected current for available episodes
ExpectedEpiCurrent <- function(abf, search_range, current_chan_id) {

  data <- abf$ByChannel[[current_chan_id]]
  n <- ncol(data)
  ret <- rep(0.0, n)
  for (i in 1:n) {
    h <- hist(data[search_range, i], plot = FALSE)
    ret[i] <- h$mids[which.max(h$counts)]
  }

  return(ret)
}
#Calculate stability of each episode in the given sampling window
SimpleStability <- function(df, win) {

  #This may not be a good idea to calculate stability
  n <- length(win)
  ptr1 <- win[1]
  ptr2 <- ptr1 + (n %/% 2) - 1
  ptr3 <- win[n]
  sum_l <- colSums(df[ptr1:ptr2, ])
  sum_r <- colSums(df[ptr2:ptr3, ])
  ret <- abs((sum_l - sum_r)/(sum_l + sum_r))

  return(ret)
}
SimpleSd <- function(df, win) {

  #We expect that current is stable, so standard deviation can be a good indicator
  #for stability.
  n <- nrow(df)
  data <- df[win, ]
  #Custom implementation for the missing colSds, may not be most effective
  ret <- colMeans(data * data) - colMeans(data)^2
  ret <- sqrt(ret * n / (n -1))

  return(ret)
}
