#============================================#
#          INTERVAL CONSTRUCTION
#============================================#
.buildInterval <- function(tonic, degree){
  # Get the number of semitones from the scale degree
  semitones <- SCALE_DEGREES
  
}

#============================================#
#            WHITE KEY INTERVALS
#============================================#
.simpleInterval <- function(){
  
}

#============================================#
#            SEMITONE ARITHMETIC
#============================================#

`%ST+%` <- function(note, semitones){
  if(semitones == 1){ return(note %INC% "#") }
  else if(semitones == 2){ return((note %INC% "#") %INC% "#")}
}
#============================================#
`%ST-%` <- function(note, semitones){
  if(semitones == 1){ return(note %INC% "b") }
  else if(semitones == 2){ return((note %INC% "b") %INC% "b")}
}


#============================================#
#            NEAREST WHITE KEY
#============================================#
# .nearestWhite <- function(note){
#   # Detect note incidental
#   incidental <- .detectIncidental(note)
#   if(incidental == "natural"){
#     # Return same note if no incidental is found
#     return(list(note, 0))
#   } 
#   else{
#     # Otherwise, try to find a simpler enharmonic before proceeding
#     note <- .simpler_enharmonic(note)
#   }
#   # Find nearest white key
#   if(incidental == "flat"){
#     above <- note %INC% "#"
#     offset <- -1
#     list(above, offset)
#   } 
#   else if(incidental == "sharp"){
#     below <- note %INC% "b"
#     offset <- 1
#     list(below, offset)
#   }
# }