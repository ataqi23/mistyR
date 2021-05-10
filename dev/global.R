
stepsByDegree <- list(`1` = 0,
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

# ==================
#  Global variables
# ==================
# Get notes with corrosponding incidentals
HAS_FLATS <- ALPHABET[which(ALPHABET$HAS_FL),"LETTER"]
HAS_SHARPS <- ALPHABET[which(ALPHABET$HAS_SH),"LETTER"]
# Get an (extended) hash table of incidentals
INC_HASH <- .INC_HASH_EXT()
# Chromatic scale
CHROMATIC_C_SHPS <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
CHROMATIC_C_FLTS <- c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")