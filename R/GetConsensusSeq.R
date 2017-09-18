#' @title Function for obtaining consensus sequence of DNA sequence
#' symbols from a PWM matrix
#'
#' @description uses a special nomenclature (we call it the Logolas nomenclature)
#' to determine the consensus sequence of symbols based on the enrichment and
#' depletion of the symbols at each position. This approach is an alternative
#' to the getIUPAC() method used by the atSNP package.
#'
#' @param pwm The PWM table for a DNA sequence motif comprising of four bases, A, C
#' G, T.
#'
#' @return Returns the consensus sequence for the DNA sequence motif along the positions
#' using the Logolas nomenclature (highlighting both enrichment and depletion).
#'
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' pwm=matrix(c(0.8,0.1,0.1,0,0.9,0.1,0,0,0.9,0.05,0.05,0,0.5,0.4,0,0.1,0.6,0.4,0,0,0.4,0.4,0.1,0.1,0.5,0,0.2,0.3,0.35,0.35,0.06,0.24,0.4,0.3,0.2,0.1,0.4,0.2,0.2,0.2,0.28,0.24,0.24,0.24,0.5,0.16,0.17,0.17,0.6,0.13,0.13,0.14,0.7,0.15,0.15,0),nrow = 4,byrow = F)
#' rownames(pwm)=c('A','C','G','T')
#' colnames(pwm)=1:ncol(pwm)
#' GetConsensusSeq(pwm)
#'
#' @export

GetConsensusSeq=function(pwm){
  bases=c('A','C','G','T')
  pwm2 <- pwm
  pwm <- pwm2[match(bases, rownames(pwm2)),]
  if(dim(pwm)[1] != dim(pwm2)[1]){
    stop("At least one of the bases A, C, G, T is missing in the row name of the PWM matrix")
  }
  based=tolower(bases)
  ll=get_logo_heights_log_odds(pwm,depletion_weight = 0)
  pp=c();for (i in 1:ncol(pwm)){pp=cbind(pp,ll$pos_ic[i]*ll$table_mat_pos_norm[,i])};
  nn=c();for(i in 1:ncol(pwm)){nn=cbind(nn,ll$neg_ic[i]*ll$table_mat_neg_norm[,i])};
  codes=c()

  for(i in 1:ncol(pwm)){
    #check if there is one emrichments

    if(sum(pp[,i]>=2)==1){
      #check if there is one depletion when there is one enrichment
      if(sum(nn[,i]>=3.9)==1){
        code=paste('(',bases[which.max(pwm[,i])],based[which.min(pwm[,i])],')',sep = '')
      }else{code=bases[which.max(pwm[,i])]}
    }else if(sum(pp[,i]>=0.9)==2){ # if no enrichemnt, is there two?
      code=paste('(',bases[sort(pwm[,i],index.return=T,decreasing = T)$ix[1]],bases[sort(pwm[,i],index.return=T,decreasing = T)$ix[2]],')',sep = '')
    } else if(sum(nn[,i]>=2)==1){ # if no two, is there one depletion?
      code=based[which.min(pwm[,i])]
    }else {code='N'}
    codes[i]=code
  }
  return(paste(codes,collapse = ' '))
}
