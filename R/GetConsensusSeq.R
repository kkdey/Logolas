#' @title Function for obtaining consensus sequence of DNA sequence symbols 
#'        from a PWM matrix
#'
#' @description uses a special nomenclature (we call it the Logolas nomenclature)
#' to determine the consensus sequence of symbols based on the enrichment and
#' depletion of the symbols at each position. This approach is an alternative
#' to the getIUPAC() method used by the atSNP package.
#'
#' @param data The input data may be a vector of A, C, G and T sequences - 
#'             representing
#'             aligned  DNA or RNA sequences , or a matrix/ data frame with
#'             symbols of A, C, G and T along the rows of the matrix/data
#'             frame and the positions or sites of the aligned sequences along 
#'             the columns.
#'
#'
#' @return Returns the consensus sequence for the DNA/RNA sequence motif along 
#'        the positions using the Logolas nomenclature (highlighting both 
#'        enrichment and depletion).
#'
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' pwm=matrix(c(0.8,0.1,0.1,0,
#' 0.9,0.1,0,0,0.9,0.05,0.05,0,0.5,
#' 0.4,0,0.1,0.6,0.4,0,0,0.4,0.4,0.1,
#' 0.1,0.5,0,0.2,0.3,0.35,0.35,0.06,
#' 0.24,0.4,0.3,0.2,0.1,0.4,0.2,0.2,
#' 0.2,0.28,0.24,0.24,0.24,0.5,0.16,0.17,
#' 0.17,0.6,0.13,0.13,0.14,0.7,0.15,0.15,0),
#' nrow = 4,byrow = FALSE)
#' rownames(pwm)=c('A','C','G','T')
#' colnames(pwm)=1:ncol(pwm)
#' GetConsensusSeq(pwm)
#'
#' sequence <- c("CTATTGT", "CTCTTAT", "CTATTAA", "CTATTTA", "CTATTAT", 
#'               "CTTGAAT", "CTTAGAT", "CTATTAA", "CTATTTA", "CTATTAT")
#' GetConsensusSeq(sequence)
#'
#' @export

GetConsensusSeq=function(data){

  if(is.character(data)){

    if(length(data) == 1){
      stop("Just one character sequence provided, user needs to enter multiple
           such aligned sequences")
    }

    numchars <- sapply(data, function(x) return (nchar(x)))
    if(!(isTRUE(all.equal( max(numchars) ,min(numchars)) ))){
      stop("character sequences entered are not all of same length : 
           so cannot be aligned")
    }
    L <- length(data)
    pwm <- Biostrings::consensusMatrix(data)
    colnames(pwm) <- 1:dim(pwm)[2]
  }else{
    pwm <- data
  }

  bases=c('A','C','G','T')
  pwm2 <- pwm
  pwm <- pwm2[match(bases, rownames(pwm2)),]
  if(dim(pwm)[1] != dim(pwm2)[1]){
    stop("At least one of the bases A, C, G, T is missing in the row 
         name of the PWM matrix")
  }
  based=tolower(bases)
  ll=get_logo_heights(pwm, ic = FALSE, score = "log-odds")
  pp=c();for (i in 1:ncol(pwm))
      {pp=cbind(pp,ll$pos_ic[i]*ll$table_mat_pos_norm[,i])};
  nn=c();for(i in 1:ncol(pwm))
      {nn=cbind(nn,ll$neg_ic[i]*ll$table_mat_neg_norm[,i])};
  codes=c()

  for(i in 1:ncol(pwm)){
    #check if there is one emrichments

    if(sum(pp[,i]>=2)==1){
      #check if there is one depletion when there is one enrichment
      if(sum(nn[,i]>=3.9)==1){
        code=paste('(',bases[which.max(pwm[,i])],
                   based[which.min(pwm[,i])],')',sep = '')
      }else{code=bases[which.max(pwm[,i])]}
    }else if(sum(pp[,i]>=0.9)==2){ # if no enrichemnt, is there two?
      code=paste('(',bases[sort(pwm[,i],index.return=TRUE,
                                decreasing = TRUE)$ix[1]],bases[sort(pwm[,i],
                    index.return=TRUE,decreasing = TRUE)$ix[2]],')',sep = '')
    } else if(sum(nn[,i]>=2)==1){ # if no two, is there one depletion?
      code=based[which.min(pwm[,i])]
    }else {code='N'}
    codes[i]=code
  }
  return(paste(codes,collapse = ' '))
}
