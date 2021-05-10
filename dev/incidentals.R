
#===========================#
#         INCIDENTALS
#===========================#

`%INC%` <- function(tonic, incidental){
  if(incidental == "#"){
    return(.addIncidental(tonic, "sharp"))
  }
  else if(incidental == "b"){
    return(.addIncidental(tonic, "flat"))
  }
}

.addIncidental <- function(note, incidental){
  # Get note letter
  letter <- substring(note, first = 1, last = 1)
  # See if the note already has an incidental
  isFlat <- str_detect(note, pattern = "-") || str_detect(note, pattern = "b")
  isSharp <- str_detect(note, pattern = "#")
  # Find appropriate incidental
  if(incidental == "flat"){ 
    if(isFlat){ incidental <- "&" } 
    else if(isSharp){ incidental <- "" }
    else{ incidental <- "b" }
  }
  else if(incidental == "sharp"){
    if(isSharp){ incidental <- "x" } 
    else if(isFlat){ incidental <- "" }
    else { incidental <- "#" }
  }
  # Return note with added incidental
  return(paste(letter, incidental, sep = ""))
}

incidentalValue <- function(incidental){
  INCIDENTAL <- c("&", "-", "#", "x")
  VALUE <- c(-2,-1,1,2)
  VALUE[which(INCIDENTAL == incidental)]
}

# Return an extended hash table of the incidentals
.INC_HASH_EXT <- function(){
  # Create ordered vectors for all the incidentals covered
  INCS <- c("flat","sharp","2flat","2sharp")
  # Create ordered vectors for the incidental symbols and their name
  SYMBOLS <- c("-","b","#","&","bb","##","x","X")
  # Create a vector of counts for each incidental for robust maintainence
  COUNTS <- c(2,1,2,3)
  # Using the counts, create the correct ordered incidentals vector
  INCIDENTALS <- do.call("c",lapply(seq_along(COUNTS), function(i){rep(INCS[i], COUNTS[i])}))
  # Return the hash table
  data.frame(symbol = SYMBOLS, incidental = INCIDENTALS)
}
# Given a note, use the extended incidental hash table to detect incidental
.detectINC <- function(note){
  # Use str_detect to see if the note has an incidental and return match
  for(i in 1:nrow(INC_HASH)){
    if(str_detect(note, pattern = INC_HASH$symbol[i])){ return(INC_HASH$incidental[i]) }
  }
  # Otherwise, return "natural" or no incidental
  return("natural")
}

#===========================#
#         INCIDENTALS
#===========================#

.nearestWhite <- function(note){
  # Coerce the note into an enharmonic to remove "double" incidentals
  #note <- .findEnharmonic(note)
  # Detect note incidental
  incidental <- .detectINC(note)
  # Return same note if no incidental is found
  if(incidental == "natural"){ return(list(note, 0)) }
  # Find nearest white key
  if(incidental == "flat"){ # Do we care to && above %in% withFlats?
    above <- note %INC% "#"
    offset <- -1
    list(above, offset)
  } 
  else if(incidental == "sharp"){
    below <- note %INC% "b"
    offset <- 1
    list(below, offset)
  }
}

# Given a note, finds an enharmonic with less incidentals
.findEnharmonic <- function(note){
  # Detect note incidental
  incidental <- .detectINC(note)
  # Get note letter and its index in the alphabet
  note_letter <- substring(note,1,1)
  note_idx <- note_index(note_letter)
  # If the incidental is a flat/sharp and is a letter with none, return white key enharmonic
  # Ex/ {Fb and Cb} or {E# and B#}
  if(incidental == "flat" && !(note_letter %in% HAS_FLATS)){
    return( MUS_ALPH[note_idx - 1] )
  }
  else if(incidental == "sharp" && !(note_letter %in% HAS_SHARPS)){
    return( MUS_ALPH[note_idx + 1] )
  }
  # Now, handle cases of double incidentals
  # Ex/ {Fbb and Abb} or {E## and D##}
  if(incidental == "2flat"){
    # If the letter name has no flats, then we may simplify
    if(!(note_letter %in% HAS_FLATS)){ return ((.findEnharmonic(note_letter %INC% "b")) %INC% "b") }
    # Otherwise, just go to the letter below
  }
  else if(incidental == "2sharp"){
    # If the letter name has no flats, then we may simplify
    if(!(note_letter %in% HAS_FLATS)){ return ((.findEnharmonic(note_letter %INC% "b")) %INC% "b") }
    # Otherwise, just go to the letter below
  }
}

#===========================#
#       MODULAR MATH
#===========================#

`%STEPUP%` <- function(scale, note){
  idx <- note_index(note)
  scale[mod7(7 + idx) + 1]
}
`%STEPDOWN%` <- function(scale, note){
  idx <- note_index(note)
  idx <- mod7(-(8 - idx) - 1) + 1
  scale[idx]
}

mod7 <- function(x) { x %% 7 }

note_index <- function(note){which(MUS_ALPH == note)}

# Generates a wrapped range of indices
.IDX <- function(i, length, MAX){
  range <- i:(i + (length - 1))
  range <- mod7(range - 1)
  range + 1
}

# Generates a wrapped range of indices
.IDXR <- function(range){
  range <- mod7(range - 1)
  range + 1
}

# Find the number of black keys in between a tonic and a # of degrees
BLK_KEYS_BTWN <- function(tonic, degree){
  root <- which(MUS_ALPH == tonic)
  range <- .IDX(root, degree)
  SH_RANGE <- sum(ALPHABET[range,]$HAS_SH)
  FL_RANGE <- sum(ALPHABET[range,]$HAS_FL)
  max(SH_RANGE, FL_RANGE)
}
