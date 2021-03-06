

#============================================#
#                 HASH TABLES
#============================================#
ALPH_HASH <- function(){
  # Create a hash table of the musical alphabet
  MUS_ALPH <- LETTERS[1:7]
  HAS_FL <- as.logical(c(1,1,0,1,1,0,1))
  HAS_SH <- as.logical(c(1,0,1,1,0,1,1))
  ALPHABET <- data.frame(LETTER = MUS_ALPH, HAS_FL, HAS_SH)
  # Return the hash table
  return(ALPHABET)
}
#============================================#
# Return an extended hash table of the incidentals
.INC_HASH_EXT <- function(){
  # Create ordered vectors for all the incidentals covered
  INCS <- c("flat","sharp","2flat","2sharp")
  # Create ordered vectors for the incidental symbols and their name
  SYMBOLS <- c("-","b","#","&","x","X") # Note: removed ## and bb due to regex confounding. Fix: match exact number.
  # Create a vector of counts for each incidental for robust maintainence
  COUNTS <- c(2,1,1,2)
  # Using the counts, create the correct ordered incidentals vector
  INCIDENTALS <- do.call("c",lapply(seq_along(COUNTS), function(i){rep(INCS[i], COUNTS[i])}))
  # Return the hash table
  data.frame(symbol = SYMBOLS, incidental = INCIDENTALS)
}

#============================================#
#              GLOBAL VARIABLES
#============================================#
# The musical alphabet
MUS_ALPH <- LETTERS[1:7]
# Alphabet hash table
ALPHABET <- ALPH_HASH()
# Get notes with corrosponding incidentals
HAS_FLATS <- ALPHABET[which(ALPHABET$HAS_FL),"LETTER"]
HAS_SHARPS <- ALPHABET[which(ALPHABET$HAS_SH),"LETTER"]
# Get an (extended) hash table of incidentals
INC_HASH <- .INC_HASH_EXT()
# Chromatic scale
CHROMATIC_C_SHPS <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
CHROMATIC_C_FLTS <- c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")
# Semitones by scale degree
SCALE_DEGREES <-   list(`1` = 0,
                        `b2`=1,
                        `2` = 2,
                        b3 = 3,
                        `3` = 4,
                        `4` = 5,
                        b5 = 6,
                        `5` = 7,
                        `#5` = 8,
                        `6` = 9,
                        b7 = 10,
                        `7` = 11)
# Diatonic scales by scale degrees
DIATONIC_SCALES <- list(major = c("2","3","4","5","6","7"))

