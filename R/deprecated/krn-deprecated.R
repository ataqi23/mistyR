#========================================================##========================================================#
#                                                      HARMONY 
#========================================================##========================================================#


#========================================================#
#                 MELODIC INTERVALS
#========================================================#
#' tot_mel_int
#' How many melodic intervals each spline has
#' 
#' @param spline .krn file - grouped by spline number
#' @return how many melodic intervals each spline has
#' 
#' @examples 
#' 
#'
# NOTE ONLY CONSIDERING DIM AND MINOR - NO AGUMENTED
tot_mel_int <- function(spline){
  sum(is.na(spline))
}

#=========================================================
#' is_mel_int
#' Checks if two notes is a specified melodic interval
#' 
#' @param n1 one note
#' @param n2 second note
#' @param int what interval we are chekcing for
#' @return T or F 
#' 

is_mel_int <- function(n1,n2, int){ 
  ifelse(n1$V-n2$V == int,T,F)
}  

#=========================================================
#' top_line2
#' 
#' @param piece piece of krn 
#' @param col one instrument name name 
#' @return gives the top melodic line of a instrument
#'
top_line2 <- function(piece,inst){
  inst_cols <- grep(inst,colnames(piece),value = T)
  note_cols <- grep("_name\\d", inst_cols,value = T)
  n <- piece[,note_cols]
  if(length(note_cols)==1){
    x <- map_lgl(n,is.na)
    x <- data.frame(rep(F,length(x)),x)
  }else{
    x <- map_df(n,is.na)
    x <- cbind(rep(F,nrow(x)),x)
  }
  f <- function(i){max(which(i == F))-1}
  notes <- purrrlyr::by_row(x,f, .collate = "cols",
                            .labels = F)[[1]]
  notes
}
#=========================================================
#' voice_mel_ints
#' 
#' @param piece One instrument 
#' @param col name 
#' @return vector of counts for melodic intervals
#' 

mel_ints <- function(piece,col){
  mel <- top_line2(piece,col)
  mel <- as.numeric(as.vector(na.omit(mel)))
  mel_dif <- c()
  for(i in 1:length(mel)-1){
    mel_dif[i] <- abs(mel[i]-mel[i+1] %% 12)
    #min(max(mel[i],mel[i+1]) - min(mel[i],mel[i+1]),
    #                min(mel[i],mel[i+1]) + 12 - max(mel[i],mel[i+1]))
  }
  mel_dif <- mel_dif + 1 # Change indexing to start at 1
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  mel_fac <- factor(ints[mel_dif], levels = ints, ordered = T)
  m <- table(mel_fac)/sum(table(mel_fac))
  m
}

#=========================================================
#' consonances
#' 
#' @param piece One piece
#' @param col name of intstrument
#' @return vector of counts for consonances
#' 
consonances <- function(piece,col){
  mel <- mel_ints(piece,col)
  perfect <- sum(mel[c(1,6,8)])
  imperfect <- sum(mel[c(4,5,9,10)])
  dissonant <- sum(mel[c(2,3,7,11,12)])
  c(perfect,imperfect,dissonant)
}




#========================================================##========================================================#
#                                                       RHYTHM
#========================================================##========================================================#



#========================================================#
#                       DURATION
#========================================================#

#' Duration
#' 
#' @param piece
#' @return piece that has duration included
#' 

duration_df <- function(piece){
  cols <- grep("_name\\d|rhythm\\d", colnames(piece),value = T)
  for(j in 1:length(cols)){
    for(i in 2:nrow(piece)){
      if(!is.na(piece[i,cols[j]])){
        if(piece[i,cols[j]] == "." ){
          piece[i,cols[j]] <- piece[i-1,cols[j]]
        }
      }
    }
  }
  piece
}
#==============================================================
#' note_duration
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

note_duration <- function(piece){
  rhy_cols <- grep("measure|rhythm\\d", colnames(piece) ,value = T)
  rhy_df <- piece[,note_cols]
  onote_df <- note_df[,-1]
  rhy_no_na <- as.numeric(as.vector(na.omit()))
}

#========================================================#
#                       LENGTH
#========================================================#
#' Length
#' 
#' 

length_measures <- function(piece){
  measure_col <- grep("measure", colnames(piece),value = T)
  measure <-  piece[,measure_col] %>% as.numeric()
  max_m <- max(measure)
  max_m
}

#========================================================#
#                        METER
#========================================================#
#' Meter
#'
#' @param piece
#' @return Time signature of the piece
#'

meter <- function(piece){
  m <- piece[1,2]
  met <- regmatches(m, regexpr("M.{,2}",m))
  met <- sub(met, "", "M")
  met
}