
#===========================#
#           SCALES
#===========================#

MUS_ALPH <- LETTERS[1:7]

ALPH_HASH <- function(){
  # Create a hash table of the musical alphabet
  MUS_ALPH <- LETTERS[1:7]
  HAS_FL <- as.logical(c(1,1,0,1,1,0,1))
  HAS_SH <- as.logical(c(1,0,1,1,0,1,1))
  ALPHABET <- data.frame(LETTER = MUS_ALPH, HAS_FL, HAS_SH)
  # Return the hash table
  return(ALPHABET)
}

ALPHABET <- ALPH_HASH()
