
#============================================#
#            SIMPLE ENHARMONICS
#============================================#
.enharmonic <- function(note, above = TRUE, simplify = FALSE){
  # Simplify the enharmonic if prompted
  # if(simplify){ note <- .simpler_enharmonic(note) }
  # Get the current incidental and note letter
  note_letter <- .dropIncidental(note)
  note_inc <- .detectIncidental(note)
  # If the incidental is a black key, cycle enharmonics
  if(.isBlackNote(note)){ return(.cycle_enharmonic(note)) }
  # If it is natural, return a double enharmonic
  else if(note_inc == "natural"){ 
    # If prompted for letter above, return double flat
    if(above){ return(((MUS_ALPH %STEPUP% note) %INC% "b") %INC% "b")}
    # Otherwise, return letter below, double sharp
    else{ return(((MUS_ALPH %STEPDOWN% note) %INC% "#") %INC% "#")}
  }
  else if(note_inc == "2flat" || note_inc == "2sharp"){
    return(.simpler_enharmonic(note))
  }
  else{ return(.simpler_enharmonic(note)) }
}

# Given a black key, cycle between the two basic enharmonics
.cycle_enharmonic <- function(black_note){
  # Get the current incidental and note letter
  note_letter <- .dropIncidental(black_note)
  note_inc <- .detectIncidental(black_note)
  # If the incidental is a single sharp or flat, cycle enharmonics
  if(note_inc == "sharp"){ return((MUS_ALPH %STEPUP% note_letter) %INC% "b") }
  else if(note_inc == "flat"){ return((MUS_ALPH %STEPDOWN% note_letter) %INC% "#") }
}
#============================================#
#            COMPLEX ENHARMONICS
#============================================#
# Given a note, finds an enharmonic with less incidentals
.simpler_enharmonic <- function(note){
  # Detect note incidental
  incidental <- .detectIncidental(note)
  # Get note letter and its index in the alphabet
  note_letter <- .dropIncidental(note)
  note_idx <- note_index(note_letter)
  # If the incidental is a flat/sharp and is a letter with none, return white key enharmonic
  # Ex/ {Fb and Cb} or {E# and B#}
  if(incidental == "flat" && !(note_letter %in% HAS_FLATS)){
    return( MUS_ALPH %STEPDOWN% note_letter )
  }
  else if(incidental == "sharp" && !(note_letter %in% HAS_SHARPS)){
    return( MUS_ALPH %STEPUP% note_letter )
  }
  # Now, handle cases of double incidentals
  # Ex/ {Fbb and Abb} or {E## and D##}
  if(incidental == "2flat"){
    # If the letter name has no flats, then we may simplify
    if(!(note_letter %in% HAS_FLATS)){ return ((.findEnharmonic(note_letter %INC% "b")) %INC% "b") }
    # Otherwise, just go to the letter below
    return(MUS_ALPH %STEPDOWN% note_letter)
  }
  else if(incidental == "2sharp"){
    # If the letter name has no flats, then we may simplify
    if(!(note_letter %in% HAS_SHARPS)){ return ((.findEnharmonic(note_letter %INC% "#")) %INC% "#") }
    # Otherwise, just go to the letter above
    return(MUS_ALPH %STEPUP% note_letter)
  }
  #.enharmonic(note, simplify = FALSE)
}
#============================================#
#           ENHARMONICS DETECTION
#============================================#
.isBlackNote <- function(note){
  # Get the note incidental and note letter
  note_inc <- .detectIncidental(note)
  note_letter <- .dropIncidental(note)
  # Depending on incidental, check whether it is a black key
  if(note_inc == "flat"){ return(note_letter %in% HAS_FLATS) }
  else if(note_inc == "sharp"){ return(note_letter %in% HAS_SHARPS) }
  FALSE
}
