EpisodeName <- function(episode_idx) {
  if (typeof(episode_idx)  == "list") {
    return(lapply(episode_idx, function(x) paste("epi", as.character(x), sep = "")))
  } else {
    return(sapply(episode_idx, function(x) paste("epi", as.character(x), sep = ""), USE.NAMES = FALSE))
  }
}
#backward compatibility
episode.get_names <- function(episode_idx) {
  EpisodeName(episode_idx)
}

EpisodeIdx <- function(episode_names) {
  if (typeof(episode_names) == "list") {
    return(lapply(episode_names, function(x) as.integer(substring(x, 4))))
  } else {
    return(sapply(episode_names, function(x) as.integer(substring(x, 4)), USE.NAMES = FALSE))
  }
}
#backward compatibility
episode.get_idx <- function(episode_names) {
  EpisodeIdx(episode_names)
}

AvailEpisodeIdx <- function(ByChannel) {
  EpisodeIdx(colnames(ByChannel))
}
#backward compatibility
episode.avail <- function(df) {
  AvailEpisodeIdx(df)
}

AvailEpisodeName <- function(ByChannel) {
  colnames(ByChannel)
}

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
#backward compatibility
episode.remove_multi <- function(channels, episodes_to_remove) {
  return(RemoveEpisode(channels, episodes_to_remove))
}
episode.remove_single <- function(channel, episodes_to_remove) {
  return(RemoveEpisode(channel, episodes_to_remove))
}
