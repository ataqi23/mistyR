
#============================================#
#               SCORE ANALYSIS
#============================================#

analyzeModality <- function(subscore){
  # Get the tonic and quality from the key signature
  key_sig <- relative_key(subscore)
  tonic <- str_to_upper(key_sig[[1]])
  quality <- str_to_lower(key_sig[[2]])
  # Get the number of bars
  bars <- levels(as.factor(as.numeric(subscore$measure_p)))
  num_bars <- length(bars)
  # Create a dataframe with a row for each bar
  modal <- data.frame(bar = 1:num_bars)
  modal <- cbind(modal, .modalRow())
  # Get the modal features of each bar
  for(i in 1:num_bars){
    # Get the measure
    measure <- getMeasures(measure = bars[i], score = subscore)
    # Obtain the notes and their sd freq
    measure_notes <- collapseNotes(measure)
    freqs <- SD_freq(measure_notes, tonic)
    modal[i,2:4] <- modality(freqs, quality)
  }
  return(modal)
}
#============================================#
.modalRow <- function(){
  data.frame(blues = NA, interchange = NA, borrowed_sub = NA)
}

#============================================#
#               MODAL ANALYSIS
#============================================#

# Get the frequency table of the scale degrees
SD_freq <- function(notes, tonic){
  table(map(notes, function(x,y){ scaleDegree(y,x) }, tonic) %>% unlist())
}
#============================================#
modality <- function(freqs, quality = NA){
  # Use the quality to study modality
  if(quality == "major"){ return (.modalityMajor(freqs)) }
  else if(quality == "minor"){ return (.modalityMinor(freqs)) }
}
#============================================#
.modalityMinor <- function(freqs){
  # Get the total number of notes
  total <- sum(freqs)
  # Create a data frame of features
  modality <- data.frame(blues = 0)
  # Blues
  modality$blues <- .countFreqs(c("3","#4","b5"), freqs)
  # Interchange
  modality$interchange <- .countFreqs(c("b2","6"), freqs)
  # Borrowed subdominant 
  modality$borrowed_sub <- .countFreqs(c("6"), freqs)
  # Return the features
  modality/total
}
#============================================#
.modalityMajor <- function(freqs){
  # Get the total number of notes
  total <- sum(freqs)
  # Create a data frame of features
  modality <- data.frame(blues = 0)
  # Blues
  modality$blues <- .countFreqs(c("b3","#4","b5","b6"), freqs)
  # Interchange
  modality$interchange <- .countFreqs(c("#4","b6","b7"),freqs)
  # Borrowed subdominant 
  modality$borrowed_sub <- .countFreqs(c("b6"), freqs)
  # Return the features
  modality/total
}
#============================================#
.countFreqs <- function(scale_degrees, freqs){
  count <- 0
  for(i in 1:length(scale_degrees)){
    freq <- freqs[scale_degrees[i]] %>% unname()
    if(!is.na(freq)){ count <- count + freq }
  }
  count
}




#============================================#
#               SCORE WRANGLING
#============================================#

# Remove unwanted columns from a score dataframe
getScoreNotes <- function(score){
  score %>% select(contains(c("key","measure","name")))
}
#============================================#
# Get a measure or range of measures of the score notes dataframe
getMeasures <- function(measure, score){
  score %>% filter(measure_p %in% c(measure)) #%>% select(!contains("measure_p"))
}
#============================================#
# Given a score (or subscore) dataframe, collapse the score into a vector of the notes in the note columns
collapseNotes <- function(score){
  score_notes <- score %>% select(!contains(c("key","measure_p")))
  score_notes <- map(score_notes, .cleanNoteColumn) %>% unlist() %>% unname()
  score_notes[which(!is.na(score_notes))]
}
#*******************************************#
# Helper function: .obtainNotes()
.cleanNoteColumn <- function(note_column){
  notes <- as.vector(note_column)
  rests_idx <- c(which(notes == "."), (which(notes == "rest")))
  notes[rests_idx] <- NA
  return(notes)
}
#============================================#



