
#===========================#
#           TRACKS
#===========================#

.getTrack <- function(score, track_no){ score %>% filter(track == track_no) }

.getTrackNames <- function(score){ 
  track_names <- score %>% filter(event == "Sequence/Track Name")
  track_names %>% select(track, parameterMetaSystem)
}

#===========================#
#           METER
#===========================#

.evaluateRhythm <- function(rhythms, beat_ticks, bar_beats){
  (rhythms / beat_ticks) * bar_beats
}

.getMeter <- function(score){
  meter <- score %>% filter(event == "Time Signature")
  meter <- meter$parameterMetaSystem %>% str_split(pattern = ",")
  meter[[1]]
}

.find_measure <- function(track, bar_ticks){
  track %>% mutate(bar = 1 + as.integer(time / bar_ticks))
}

#===========================#
#       MISCELLANEOUS
#===========================#

.getChannel <- function(score, channel){ score[which(score$channel == channel), ] }

#============================================#