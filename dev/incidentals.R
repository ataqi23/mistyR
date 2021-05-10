
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

#===========================#
#       MODULAR MATH
#===========================#

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
