#' EpisodeName returns episode names from a list or vector of episode indices
#'
#' @param episode_idx a list or vector of episode indices
#'
#' @return a list or vector of episode names, the return type is determined by typeof episode_idx
#' @export
#'
#' @examples epi_name <- EpisodeName(c(1,3,4,5))
EpisodeName <- function(episode_idx) {
  if (typeof(episode_idx)  == "list") {
    return(lapply(episode_idx, function(x) paste("epi", as.character(x), sep = "")))
  } else {
    return(sapply(episode_idx, function(x) paste("epi", as.character(x), sep = ""), USE.NAMES = FALSE))
  }
}

#' Get episode names from episode indices
#'
#' @param episode_idx
#'
#' @return episode names as vector of characters
#' @export
#'
#' @examples episode.get_names(c(1,3,5,8))
episode.get_names <- function(episode_idx) {
  EpisodeName(episode_idx)
}

#' EpisodeIdx returns episode indices from a list or vector of episode names
#'
#' @param episode_names a list or vector of episode names
#'
#' @return a list or vector of episode names, the return type is determined by typeof episode_names
#' @export
#'
#' @examples epi_idx <- EpisodeIdx(c("epi3", "epi5", "epi6"))
EpisodeIdx <- function(episode_names) {
  if (typeof(episode_names) == "list") {
    return(lapply(episode_names, function(x) as.integer(substring(x, 4))))
  } else {
    return(sapply(episode_names, function(x) as.integer(substring(x, 4)), USE.NAMES = FALSE))
  }
}

#' Get episode indices from episode names
#'
#' @param episode_names
#'
#' @return episode indices as vector of numeric
#' @export
#'
#' @examples episode.get_idx(c("epi5", "epi4"))
episode.get_idx <- function(episode_names) {
  EpisodeIdx(episode_names)
}

#' AvailEpisodeIdx returns available episode indices
#'
#' @param ByChannel data.frame of a channel
#'
#' @return a vector of available episode indices
#' @export
#'
#' @examples idx <- AvailEpisodeIdx(abf$ByChannel[[1]])
AvailEpisodeIdx <- function(ByChannel) {
  EpisodeIdx(colnames(ByChannel))
}

#' Get available episodes from given channel
#'
#' @param df a channel
#'
#' @return availavle episode indices as vector of integer
#' @export
#'
#' @examples
episode.avail <- function(df) {
  AvailEpisodeIdx(df)
}

#' AvailEpisodeName returns available episode names
#'
#' @param ByChannel data.frame of a channel
#'
#' @return a vector of available episode names
#' @export
#'
#' @examples epi_names <- AvailEpisodeName(abf$ByChannel[[1]])
AvailEpisodeName <- function(ByChannel) {
  colnames(ByChannel)
}

#' RemoveEpisode removes episodes from a channel or a list of channel
#'
#' @param ByChannel a channel or a list of channels
#' @param episode_idx a list or vector of episode indices to be removed
#'
#' @return a channel or a list of channels with the desired episodes removed
#' @export
#'
#' @examples abf$ByChannel <- RemoveEpisode(abf$ByChannel, c(1,3))
RemoveEpisode <- function(ByChannel, episode_idx) {

  RemoveIn <- function(channel, epi) {
    epi_names <- EpisodeName(epi)
    return(channel[, !(names(channel) %in% epi_names)])
  }

  if (class(ByChannel) == "data.frame") {
    return(RemoveIn(ByChannel, episode_idx))
  } else {
    return(lapply(ByChannel, function(x) RemoveIn(x, episode_idx)))
  }
}

#' Remove episodes from multiple channels, returns a list of data.frame same size of channels
#'
#' @param channels a list of channels
#' @param episodes_to_remove a vector of episode indices
#'
#' @return a list of channels with the desired episodes removed
#' @export
#'
#' @examples
episode.remove_multi <- function(channels, episodes_to_remove) {
  return(RemoveEpisode(channels, episodes_to_remove))
}

#' Remove episodes from single channel, returns a data.frame
#'
#' @param channel a channel
#' @param episodes_to_remove a vector of episode indices
#'
#' @return a channel with the desired episodes removed
#' @export
#'
#' @examples
episode.remove_single <- function(channel, episodes_to_remove) {
  return(RemoveEpisode(channel, episodes_to_remove))
}

#' AbfAvailEpisodeIdx same as AvailEpisodeIdx but operates on an abf data
#'
#' @param abf data loaded from abf file
#'
#' @return a vector of available indices
#' @export
#'
#' @examples idx <- AbfAvailEpisodeIdx(abf)
AbfAvailEpisodeIdx <- function(abf) {
  AvailEpisodeIdx(abf$ByChannel[[1]])
}

#' AbfAvailEpisodeName same as AvailEpisodeName but operates on an abf data
#'
#' @param abf data loaded from abf file
#'
#' @return a vector of available names
#' @export
#'
#' @examples epi_names <- AbfAvailEpisodeName(abf)
AbfAvailEpisodeName <- function(abf) {
  AvailEpisodeName(abf$ByChannel[[1]])
}


#' AbfRemoveEpisode removes episodes from all channels of abf
#'
#' @param abf data loaded from abf file
#' @param episode_idx a list or vector of episode indices to be removed
#'
#' @return abf data with desired episodes removed
#' @export
#'
#' @examples abf <- AbfRemoveEpisode(abf, c(1,2,3,4,8))
AbfRemoveEpisode <- function(abf, episode_idx) {
  abf$ByChannel <- RemoveEpisode(abf$ByChannel, episode_idx)
  return(abf)
}
