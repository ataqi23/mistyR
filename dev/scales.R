


#============================================#
#              SCALE ARITHMETIC
#============================================#

`%STEPUP%` <- function(scale, note){
  idx <- note_index(note)
  scale[mod7(7 + idx) + 1]
}
#============================================#
`%STEPDOWN%` <- function(scale, note){
  idx <- note_index(note)
  idx <- mod7(-(8 - idx) - 1) + 1
  scale[idx]
}
#============================================#
`%STEP+%` <- function(note, steps){
  for(i in 1:steps){ note <- MUS_ALPH %STEPUP% note }
  note
}
#============================================#
`%STEP-%` <- function(note, steps){
  for(i in 1:steps){ note <- MUS_ALPH %STEPDOWN% note }
  note
}

#============================================#
#            LOW-LEVEL METHODS
#============================================#

# Arithmetic modulo 7
mod7 <- function(x) { x %% 7 }

# Convert a note into a note index in the musical alphabet
note_index <- function(note){ which(MUS_ALPH == note) }

# Generates a wrapped range of indices on the musical alphabet of a given a length
.ALPH_RANGE <- function(i, length){
  range <- i:(i + (length - 1))
  range <- mod7(range - 1)
  range + 1
}
# Rename broken alias for now
.IDX <- .ALPH_RANGE

# Wraps a static range of integers
.IDX_STATIC <- function(range){ mod7(range - 1) + 1 }
# Rename broken alias for now
.IDXR <- .IDX_STATIC

