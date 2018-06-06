#' Select samples from data index
#'
#' @param dataindex : dataindex loaded from csv
#' @param oocyteNo : oocyte number
#' @param gene : gene name
#' @param buffer : buffer name
#'
#' @return Selected samples that satisfy conditions provided
#' @export
#'
#' @examples select.samples(loaded_index, c(5:10), "AtNRT1.1", "50mM Na")
select.samples = function(dataindex, oocyteNo, gene, buffer)
{

  dataindex %>%
    filter(!`Disregard?` %in% 'yes') %>%
    filter(!`DataType` %in% 'Gapfree')-> result
  if ( !missing(oocyteNo) )
  {
    result = filter(result, `OocyteNumber` %in% oocyteNo )
  }
  if ( !missing(gene) )
  {
    result = filter(result, `Gene` %in% gene)
  }
  if ( !missing(buffer))
  {
    result = filter(result, `Buffer` %in% buffer)
  }
  result = select(result, FileName, OocyteNumber, Gene, Buffer)

  return(result)
}

# Calculate fluctuation in the interval between sample_start and sample_end COLUMN-WISE
.fluctuation.integrate = function(data)
{
  m = mean(data)
  n = length(data)

  y = c()
  y[1]= data[1]
  for (i in 2:n)
  {
    y[i] = y[i-1] + (data[i] - m)
  }

  return(y)
}

.fluctuation.local = function(data)
{
  n = length(data)

  y = .fluctuation.integrate(data)

  x = 1:n
  linear = lm(data ~ x)
  b = linear$coefficients[[1]]
  slope = linear$coefficients[[2]]

  #Evaluate fluctuation within the whole local data set
  Fn = 0.0
  for (i in 1:n)
  {
    yn = b + slope * i
    delta = (y[i] - yn)^2.0
    Fn = Fn + delta
  }
  Fn = sqrt(Fn / n)

  return(Fn)
}

#' Guess the best sampling interval for obtaining I and V data
#'
#' @param sample_channels a list of channels contain I and V data, abf2data$ByChannel
#' @param interval_pts desired interval size
#' @param current_chan_id channel id for current, usually 1
#' @param voltage_chan_id channel id for voltage, usually 2
#' @param target_voltages TEVC protocal voltage settings
#' @param voltage_threshold maximum error from protocal voltage settings
#' @param restriction_div controls sampling area
#'
#' @return a sampling interval as vector of numeric
#' @export
#'
#' @examples
vclamp.best_interval = function(sample_channels, interval_pts, current_chan_id, voltage_chan_id, target_voltages, voltage_threshold, restriction_div)
{

  check.baseline = function(data, pts_start, pts_end, baseline, threshold)
  {
    # check every available episodes
    avail_epi = episode.avail(data)
    ok = TRUE
    for (i in 1:length(avail_epi))
    {
      interval_min = min(data[pts_start:pts_end, i])
      interval_max = max(data[pts_start:pts_end, i])
      #real_threshold = baseline[i] * threshold
      # some columns might have been removed, thus i does not necessarily correspond to baseline, instead we use idx from avail_epi
      idx = avail_epi[i]
      if ( abs(interval_min - baseline[idx]) > threshold || abs(interval_max - baseline[idx]) > threshold )
      {
        ok = FALSE
        break
      }
    }

    return(ok)
  }

  slope = function(data)
  {
    n = length(data)
    x = 1:n
    linear = lm(data ~ x)
    result = linear$coefficients[[2]]

    return(result)
  }

  total_pts = nrow(sample_channels[[1]])
  total_intv = total_pts %/% interval_pts
  start_intv = total_intv - (total_intv %/% restriction_div)

  min_voltage_slope = NA
  min_current_slope = NA
  min_voltage_fluc = NA
  min_current_fluc= NA
  best_start = NA
  best_end = NA

  for (i in total_intv:start_intv)
  {
    ptr_start = (i-1) * interval_pts + 1
    ptr_end = ptr_start + interval_pts - 1

    # check voltage channel baseline check
    if ( !check.baseline(sample_channels[[voltage_chan_id]], ptr_start, ptr_end, target_voltages, voltage_threshold))
      next

    # voltage channel reaches target voltage
    # now calculate fluctuations
    tmp = .0
    tmp_voltage = .0
    tmp_current = .0
    tmp_voltage_slope = .0
    tmp_current_slope = .0
    avail_epi = episode.avail(sample_channels[[voltage_chan_id]])
    for (j in 1:length(avail_epi))
    {
      tmp = abs(slope(sample_channels[[voltage_chan_id]][ptr_start:ptr_end, j]))
      if (tmp > tmp_voltage_slope)
        tmp_voltage_slope = tmp
      tmp = abs(slope(sample_channels[[current_chan_id]][ptr_start:ptr_end, j]))
      if (tmp > tmp_current_slope)
        tmp_current_slope = tmp
      tmp_voltage = tmp_voltage + .fluctuation.local(sample_channels[[voltage_chan_id]][ptr_start:ptr_end, j])
      # corresponding current channel should present
      tmp_current = tmp_current + .fluctuation.local(sample_channels[[current_chan_id]][ptr_start:ptr_end, j])
    }
    if (is.na(best_start))
    {
      min_current_slope = tmp_current_slope
      min_voltage_slope = tmp_voltage_slope
      min_current_fluc = tmp_current
      min_voltage_fluc = tmp_voltage
      best_start = ptr_start
      best_end = ptr_end
    }
    # Criteria: 1. At least one better slope. 2. Smaller fluctuations in both channels
    else if ( ((tmp_current_slope < min_current_slope) || (tmp_voltage_slope < min_voltage_slope)) && ((tmp_current < min_current_fluc) && (tmp_voltage < min_voltage_fluc)) )
    {
      min_current_fluc = tmp_current
      min_voltage_fluc = tmp_voltage
      min_current_slope = tmp_current_slope
      min_voltage_slope = tmp_voltage_slope
      best_start = ptr_start
      best_end = ptr_end
    }
  }

  return(c(best_start,best_end))
}

#' Returns a data.frame, the episodic averages (as columns) of channels (as rows) in the given interval of the sample
#'
#' @param sample_channels
#' @param interval
#'
#' @return a data frame as described
#' @export
#'
#' @examples
vclamp.sample_mean = function(sample_channels, interval)
{

  va = data.frame()
  lo = interval[1]
  hi = interval[2]

  # Get available episodes
  epi_names = colnames(sample_channels[[1]])
  n_epi = length(epi_names)
  for (i in 1:length(sample_channels))
  {
    for (j in 1:n_epi)
    {
      m = mean(sample_channels[[i]][lo:hi, j])
      va[i, j] = m
    }
  }
  colnames(va) <- epi_names

  return(va)
}

#' Plot sample mean
#'
#' @param df result data frame from vclamp.channel_mean
#' @param current_chan_id
#' @param voltage_chan_id
#'
#' @return
#' @export
#'
#' @examples
vclamp.plot_sample_mean = function(df, current_chan_id = 1, voltage_chan_id = 2)
{
  Current = unlist(df[current_chan_id, ])
  Voltage = unlist(df[voltage_chan_id, ])
  plot(x = Voltage, y = Current, type = "b")
}

#' Calculate channel mean
#'
#' @param sample_list a list of channels
#' @param intervals
#' @param chan_id
#'
#' @return a data.frame, the episodic averages (as columns) of a perticular channel (given by chan_id) of all samples (as rows)
#' @export
#'
#' @examples
vclamp.channel_mean = function(sample_list, intervals, chan_id)
{
  df = data.frame()
  for (i in 1:length(sample_list))
  {
    lo = intervals[i, "Start"]
    hi = intervals[i, "End"]
    epi_avail = episode.avail(sample_list[[i]][[chan_id]])
    for (j in 1:length(epi_avail))
    {
      if (is.na(lo)==TRUE){
        m = NA
      }
      else {
        m = mean(sample_list[[i]][[chan_id]][lo:hi, j])
      }
      # if an episode is removed, the corresponding data will be treated as NA
      df[i, epi_avail[j]] = m
    }
  }
  epi_names = episode.get_names(1:ncol(df))
  colnames(df) <- epi_names

  return(df)
}

#' Combines voltage and current means for each sample
#'
#' @param voltage_mean from vclamp.channel_mean
#' @param current_mean from vclamp.channel_man
#'
#' @return a data frame contins voltage mean and current mean for each sample
#' @export
#'
#' @examples
vclamp.individual_channel_mean = function(voltage_mean, current_mean)
{
  result = list()
  n = nrow(voltage_mean)
  for (i in 1:n)
  {
    df = data.frame()
    for (j in 1:ncol(voltage_mean))
    {
      df[j, 1] = voltagemeans[i, j]
      df[j, 2] = currentmeans[i, j]
    }
    colnames(df) <- c("Voltage", "Current")
    result[[i]] = df
  }

  return(result)
}

#' Combine current or voltage means
#'
#' @param IorVmeans a data frame with voltage or current mean
#'
#' @return
#' @export
#'
#' @examples
vclamp.individual_IorV_mean = function(IorVmeans)
{
  result = list()
  n = nrow(IorVmeans)
  for (i in 1:n)
  {
    df = data.frame()
    for (j in 1:ncol(IorVmeans))
    {
      df[j, 1] = IorVmeans[i, j]
    }
    colnames(df) <- "data"
    result[[i]] = df
  }

  return(result)
}

#' Combine voltage and current mean to a data frame
#'
#' @param voltage_mean from vclamp.channel_mean
#' @param current_mean from vclamp.channel_mean
#'
#' @return a data frame with Voltage, SEM Voltage, Current, SEM Current
#' @export
#'
#' @examples
vclamp.combine_channel_mean = function(voltage_mean, current_mean)
{
  df = data.frame()
  n = colSums(!is.na(voltage_mean))
  for (i in 1:ncol(voltage_mean))
  {
    df[i, 1] = mean(voltage_mean[, i], na.rm = TRUE)
    df[i, 2] = sd(voltage_mean[, i], na.rm = TRUE) / sqrt(n[[i]])
    df[i, 3] = mean(current_mean[, i], na.rm = TRUE)
    df[i, 4] = sd(current_mean[, i], na.rm = TRUE) / sqrt(n[[i]])
  }
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")

  return(df)
}

#' Get protocol voltage settings from abf data
#'
#' @param abfdata loaded from abf2.load
#'
#' @return protocol voltage settings for each episode as a vector of numeric
#' @export
#'
#' @examples
vclamp.voltage_setting = function(abfdata)
{
  init = abfdata$Sections$EpochPerDAC$fEpochInitLevel[2]
  incr = abfdata$Sections$EpochPerDAC$fEpochLevelInc[2]
  n = abfdata$NumOfEpisodes
  result = seq(from = init, by = incr, length.out = n)

  return(result)
}

# Plotting related function
#' Get xy_info used for vclamp.plot_channel
#'
#' @param abfdata
#' @param time_unit time unit for x axis, can be "ms", "s" or "ticks"
#'
#' @return
#' @export
#'
#' @examples
vclamp.plot_xy_info = function(abfdata, time_unit)
{
  result = list()
  #Channel names and units
  chan_name = abfdata$ChannelNameGuess
  chan_unit = abfdata$ChannelUnit
  #X axis
  if (missing(time_unit))
  {
    result$x = abfdata$X_ticks
    xmax = max(abfdata$X_ticks)
    xmin = min(abfdata$X_ticks)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / ticks"
  }
  else if (time_unit == "ms")
  {
    result$x = abfdata$X_ms
    xmax = max(abfdata$X_ms)
    xmin = min(abfdata$X_ms)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / ms"
  }
  else if (time_unit == "s")
  {
    result$x = abfdata$X_s
    xmax = max(abfdata$X_s)
    xmin = min(abfdata$X_s)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / s"
  }
  else
  {
    result$x = abfdata$X_ticks
    xmax = max(abfdata$X_ticks)
    xmin = min(abfdata$X_ticks)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / ticks"
  }
  result$ylabel = c()
  for (i in 1:length(chan_name))
    result$ylabel[i] = paste(chan_name[i], "/", chan_unit[i])

  return(result)
}

#' Plot channel
#'
#' @param sample_channels a list of channels
#' @param chan_id
#' @param xy_info
#' @param plottitle
#'
#' @return
#' @export
#'
#' @examples
vclamp.plot_channel = function(sample_channels, chan_id, xy_info, plottitle)
{
  data = sample_channels[[chan_id]]
  x = xy_info$x
  xlimit = xy_info$xlimit
  xlabel = xy_info$xlabel
  ymin = min(MinyAxis[,chan_id])
  ymax = max(MaxyAxis[,chan_id])
  ylimit = c(ymin, ymax)
  ylabel = xy_info$ylabel[[chan_id]]
  plot(x, data[, 1], type = "l", ylim = ylimit, xlim = xlimit, xlab = xlabel, ylab = ylabel)
  if (!missing(plottitle))
  {
    title(plottitle)
  }
  # plot available episodes only
  for (i in 2:ncol(data))
  {
    lines(x, data[, i])
  }
}

#' Plot sample channels with interval shown
#'
#' @param sample_channels a list of channels
#' @param interval a vector of numeric, can be obtained from vclamp.best_interval
#' @param xy_info can be obtained from vclamp.plot_xy_info
#'
#' @return
#' @export
#'
#' @examples
vclamp.plot_interval = function(sample_channels, interval, xy_info)
{
  lo = interval[1]
  hi = interval[2]
  #plot.new()
  par(mfrow=c(1,2))
  for (i in 1:length(sample_channels))
  {
    vclamp.plot_channel(sample_channels, i, xy_info)
    abline(v = lo, lty = 3)
    abline(v = hi, lty = 3)
  }
}

#' Calculate average current from a gapfree abfdata by intervals
#'
#' @param abfdata
#' @param n length of each interval
#' @param time_unit time unit used, can be "ms", "s" or "ticks"
#'
#' @return
#' @export
#'
#' @examples
gapfree.current_avg = function(abfdata, n, time_unit)
{
  if (missing(time_unit))
  {
    intv = abfdata$SampleInterval_s
  }
  else if (time_unit == "ms")
  {
    intv = abfdata$SampleInterval_ms
  }
  else
  {
    intv = abfdata$SampleInterval_s
  }
  l = length(abfdata$data[, "Current"])
  df.len = l %/% n + 1
  df = data.frame()
  for (i in 1:df.len)
  {
    p_start = (i - 1) * n + 1
    p_end = p_start + n - 1
    if (p_end > l)
      p_end = l
    df[i, 1] = intv * i * n
    df[i, 2] = mean(abfdata$data[, "Current"][p_start:p_end])
  }
  colnames(df) = c("Time", "Current")

  return(df)
}
