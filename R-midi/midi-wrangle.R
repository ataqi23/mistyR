
#===========================#
#           VOICES
#===========================#

combine_voices <- function(voice1, voice2, voice_names, beat_ticks, bar_beats){
  # Generate the voice name suffixes
  voice_names <- paste(".",voice_names,sep = "")
  # Add the two voice dataframes together
  score_df <- full_join(voice1, voice2, by = "time", suffix = voice_names)
  # Wrangle the score (adding bars)
  score_df <- score_df %>% wrangle_score(beat_ticks, bar_beats)
  # Reorder the column names
  score_df <- score_df %>% select(time, bar, contains(voice_names))
  # Retrun the score data frame of these two voices
  return(score_df)
}

#===========================#
#           SCORES
#===========================#

wrangle_score <- function(score, beat_ticks, bar_beats, trim = T){
  # Get useful parameters
  bar_ticks <- bar_beats * beat_ticks
  # Evaluate the measures
  score <- score %>% .find_measure(bar_ticks)
  # Trim the score dataframe
  if(trim){ score <- score %>% .trimScore() }
  # Return the wrangled score
  return(score)
}

.trimScore <- function(score){
  # Trim by choosing a subset of variables
  unwanted_cols <- c("channel","track")
  # Choose columns
  score <- score %>% select(!contains(unwanted_cols))
}

#===========================#
#           TRACKS
#===========================#

wrangle_track <- function(track, beat_ticks, bar_beats){
  # Get useful parameters
  bar_ticks <- bar_beats * beat_ticks
  # Change some column names
  track <- .updateTrackCols(track)
  # Evaluate the rhythms
  track$rhythm <- track$rhythm %>% .evaluateRhythm(beat_ticks, bar_beats)
  # Return the wrangled track
  return(track)
}

.updateTrackCols <- function(track){
  # Update the column names for standardization
  track <- track %>% .renameColumn("length","rhythm")
  track <- track %>% .renameColumn("notename","note_name")
  track <- track %>% .renameColumn("note","note_value")
  return(track)
}

#===========================#
#       MISCELLANEOUS
#===========================#

.renameColumn <- function(data, old_name, new_name){
  colnames(data)[which(colnames(data) == old_name)] <- new_name
  data
}