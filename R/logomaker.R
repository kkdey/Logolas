#' @title Create logo plots from aligned sequences or positional frequency
#' (weight) matrix
#'
#' @description Takes as input a vector of character sequences
#' (aligned to have the ) same length or a positional frequency or weight
#' matrix and plots the standard logo or the Enrichment Depletion (ED) Logo
#' plots.
#'
#' @param data The input data may be a vector of character sequences -
#'             representing aligned sequences of DNA, RNA or amino acids,
#'             or a matrix/ data frame with symbols of characters or strings
#'             of characters along the rows of the matrix/data frame and
#'             the positions or sites of the aligned sequences along the
#'             columns.
#'
#' @param type can either be "Logo" or "EDLogo" depending on if user wants to
#'                 plot the standard Logo or the Enrichment Depletion Logo.
#'
#' @param bg The background probability, which defaults to NULL, in which case
#'           equal probability is assigned to each symbol. The user can
#'           however specify a vector (equal to in length to the number of
#'           symbols) which specifies the background probability for each symbol
#'            and assumes this background probability to be the same across the
#'            columns (sites), or a matrix, whose each cell specifies the
#'            background probability of the symbols for each position.
#' @param n_data The number of sequences used to build the positional weight
#'               matrix (table).
#' @param n_bg The number of sequences used for the background probabilities.
#' @param tol The tolerance for the KL-divergence of the positional weight
#'            data and background probabilities.
#' @param shrink A Boolean indicating whether to use the \code{ash} shrinkage
#'               on the positional weights or not.
#' @param pseudocount A small pseudocount to be added mainly to bypass 0 entries.
#'                     Default is NULL. If \code{table} is a counts matrix,
#'                     the default changes to 0.5, if \code{table} is a
#'                     positional weight matrix, the default becomes 0.001 times
#'                     the minimum non-zero value of the table.
#'
#' @param color_type A list specifying the coloring scheme. Defaults to NULL,
#'                  for which, based on \code{color_seed}, a specific
#'                  coloring scheme is chosen. The list contains
#'                  two elements - \code{type} and \code{col}.The \code{type}
#'                  can be of three types - "per-row", "per-column" and
#'                  "per-symbol". The \code{col} element is a vector of colors,
#'                   of same length as number of rows in table for "per-row"
#'                   (assigning a color to each string), of same length
#'                  as number of columns in table for "per-column"
#'                  (assuming a color for each column), or a distinct color
#'                  for a distinct symbol in "per-symbol". For "per-symbol",
#'                  the length of the \code{color_profile$col} should be same
#'                  as library size of the logos, but if the vector of colors
#'                  provided is more or less, we can downsample or upsample
#'                  the colors as required. The colors are matched with the
#'                  symbols in the \code{total_chars}.
#'
#' @param colors Add description here.
#'
#' @param color_seed A seed for choosing among multiple available coloring
#'                   schemes in \code{color_profile}.
#'                   The default choice is 2030. But the user can use any seed
#'                   of her choice.
#'
#' @param return_heights Boolean. If TRUE, the function returns the stack
#'                      heights for the logo plot.For standard Logo
#'                      (type = "Logo"), it returns the information content.
#'                       For tyep = "EDLogo", it returns the total stack height
#'                       along positive and negative axis, as well as the
#'                       breakdown of the heights along different symbols
#'                       along the two axis. Defaults to FALSE.
#'
#' @param logo_control Control parameters for the logo plot. Check the
#'                     input arguments from the \code{plogomaker} and
#'                     \code{nlogomaker} functions.
#'
#'
#' @return Returns a standard or EDLogo plot of the sequence of the positional
#'         frequency matrix based on the \code{type} is equal to Logo or EDLogo.
#'
#' @examples
#' sequence <- c("CTATTGT", "CTCTTAT", "CTATTAA", "CTATTTA", "CTATTAT",
#'               "CTTGAAT", "CTTAGAT", "CTATTAA", "CTATTTA", "CTATTAT",
#'               "CTTTTAT", "CTATAGT", "CTATTTT", "CTTATAT", "CTATATT",
#'               "CTCATTT", "CTTATTT", "CAATAGT", "CATTTGA", "CTCTTAT",
#'               "CTATTAT", "CTTTTAT", "CTATAAT", "CTTAGGT",
#'               "CTATTGT", "CTCATGT", "CTATAGT", "CTCGTTA",
#'               "CTAGAAT", "CAATGGT")
#'
#' logomaker(sequence, type = "Logo")
#' logomaker (sequence, type = "EDLogo")
#'
#' library(ggseqlogo)
#' data(ggseqlogo_sample)
#'
#' sequence <- seqs_aa$AKT1
#' logomaker (sequence, type = "Logo")
#' logomaker (sequence, type = "EDLogo")
#'
#' data("seqlogo_example")
#' logomaker(seqlogo_example, type = "Logo", return_heights = TRUE)
#' logomaker(seqlogo_example, type = "EDLogo", return_heights = TRUE)
#'
#'
#' @import grid
#' @importFrom graphics par
#' @importFrom ashr ash
#' @importFrom  utils  modifyList
#' @importFrom stats rnorm
#' @import ggplot2
#' @import gridBase
#' @export

logomaker <- function(data,
                      type = c("Logo", "EDLogo"),
                      bg = NULL,
                      n_data = NULL, n_bg = NULL,
                      tol = 0, shrink = TRUE,
                      pseudocount = NULL,
                      color_type = NULL,
                      colors = NULL,
                      color_seed = NULL,
                      return_heights = FALSE,
                      logo_control = list()){

  ##################  Control Options for Logo/EDLogo plots  ###############

  if(!(type %in% c("Logo", "EDLogo"))){
    stop("type must be either 'Logo' for standard plot or 'EDLogo' for Enrichment Depletion plot")
  }

  if(type == "Logo"){
    logo_control_default <- list(ic=NULL,
                                 total_chars = c("A", "B", "C", "D", "E", "F",
                                                 "G", "H", "I", "J", "K", "L",
                                                 "M", "N", "O",
                                                 "P", "Q", "R", "S", "T", "U",
                                                 "V", "W", "X", "Y", "Z",
                                                 "zero", "one", "two",
                                                 "three", "four", "five", "six",
                                                 "seven", "eight", "nine",
                                                 "dot", "comma",
                                                 "dash", "colon", "semicolon",
                                                 "leftarrow", "rightarrow"),
                                 frame_width=NULL,  ic.scale=TRUE,
                                 xaxis=TRUE, yaxis=TRUE, xaxis_fontsize=15,
                                 xlab_fontsize=15, y_fontsize=15,
                                 main_fontsize=16, start=0.001,
                                 yscale_change=TRUE, pop_name = "",
                                 xlab = "position",ylab = "Information content",
                                 col_line_split="white", addlogos = NULL,
                                 addlogos_text = NULL, newpage = TRUE,
                                 control = list())
  }else if (type == "EDLogo"){
    logo_control_default <- list(ic = FALSE,
                                 score = "log",
                                 total_chars = c("A", "B", "C", "D", "E", "F",
                                                 "G", "H", "I", "J", "K", "L",
                                                 "M", "N", "O",
                                                 "P", "Q", "R", "S", "T", "U",
                                                 "V", "W", "X", "Y", "Z",
                                                 "zero", "one", "two",
                                                 "three", "four", "five", "six",
                                                 "seven", "eight", "nine",
                                                 "dot", "comma",
                                                 "dash", "colon", "semicolon",
                                                 "leftarrow", "rightarrow"),
                                 frame_width=NULL, yscale_change=TRUE,
                                 pop_name = "", addlogos = NULL,
                                 addlogos_text = NULL, newpage = TRUE,
                                 yrange = NULL, xaxis=TRUE,
                                 yaxis=TRUE, xaxis_fontsize=15,
                                 xlab_fontsize=15, y_fontsize=15,
                                 main_fontsize=16, start=0.001,
                                 xlab = "position", ylab = "Enrichment Score",
                                 col_line_split="white",
                                 control = list())
  }

  #################  Reading Data (sequences / matrix)  #####################

  if(is.character(data)){

    if(length(data) == 1){
      stop("Just one character sequence provided, user needs to enter
           multiple such aligned sequences")
    }

    numchars <- sapply(data, function(x) return (nchar(x)))
    if(!(isTRUE(all.equal( max(numchars) ,min(numchars)) ))){
      stop("character sequences entered are not all of same length : so
           cannot be aligned")
    }

    pfm <- Biostrings::consensusMatrix(data)
    colnames(pfm) <- 1:dim(pfm)[2]
  }else if(!is.character(data)){

    if(!is.matrix(data) & !is.data.frame(data)){
      stop("if not a character sequence, data must be either a matrix
           or a data frame")
    }

    if(is.null(rownames(data))){
      stop("row names of the  data matrix should be symbols to be plotted
           in the logo plot ")
    }

    if(is.null(colnames(data))){
      colnames(data) <- 1:dim(data)[2]
    }

    if(min(data[!is.na(data)]) < 0){
      stop("negative values in data matrix not permitted : try logo_pssm ()
           function for plotting position specific scores")
    }

    pfm <- data
  }


##########################  Reference probabilities  ##########################

  if (is.vector(bg)==TRUE){
    if(length(bg) != dim(pfm)[1]){
      stop("If background prob (bg) is a vector, the length of bg must
           equal the number of symbols for the logo plot")
    }else if(length(which(is.na(pfm))) > 0){
      stop("For NA in table, a vector bg is not allowed")
    }else{
      bgmat <- bg %*% t(rep(1, dim(pfm)[2]))
      bgmat_pre <- bgmat
    }
  }else if (is.matrix(bg)==TRUE){
    if(dim(bg)[1] != dim(pfm)[1] | dim(bg)[2] != dim(pfm)[2]){
      stop("If background prob (bg) is a matrix, its dimensions must
           match that of the table")
    }else{
      bgmat <- bg
      bgmat[which(is.na(pfm))] <- NA
      bgmat_pre <- bgmat
    }
  }else {
    message ("using a background with equal probability for all symbols")
    bgmat <- matrix(1/dim(pfm)[1], dim(pfm)[1], dim(pfm)[2])
    bgmat[which(is.na(pfm))] <- NA
    bgmat_pre <- bgmat
  }


  ####################  Modifying the Logo score  ########################

  numchars <- sapply(rownames(pfm), function(x) return (nchar(x)))
  if(max(numchars) == 1){
    logo_control_default$col_line_split <- "white"
  }else{
    logo_control_default$col_line_split <- "grey80"
  }
  logo_control_default$pop_name = ""
  logo_control_default$xlab = "position"


  if(type == "EDLogo"){

    if(!is.null(n_data) && n_data == Inf){shrink = FALSE}
    colsums_pfm <- colSums(pfm, na.rm = TRUE)

    if(shrink == TRUE){

      if(min(colsums_pfm) <= 1.01 && max(colsums_pfm) <= 1.01){
        message("The input is a positional weight matrix
                (entries are probabilities)")
        if(is.null(n_data)){n_data <- 10^3}
      }else if(max((pfm - floor(pfm)), na.rm = TRUE) == 0){
        message("Analyzing the positional frequency matrix")
        if(is.null(n_data)) {
          n_data <- max(colsums_pfm)
        }else{
          warning("user defined n_data used despite the matrix being counts,
                  to use the n_data from matrix, set n_data = NULL")
        }
      }
      if(is.null(n_bg)){n_bg <- n_data}else if(is.infinite(n_bg)){n_bg <- n_data}

      table_pre <- apply(pfm,2,normalize0)
      tab <- floor(n_data*table_pre)

      bgmat_pre <-  apply(bgmat_pre,2,normalize0)
      bgmat_counts <- floor(n_bg*bgmat_pre)

      llambda_mat <- matrix(NA, nrow(tab), ncol(tab))
      for(m in 1:ncol(tab)){
        successes <- tab[,m]
        failures <- bgmat_counts[,m]
        noNA_indices = which(!is.na(successes))

        ss <- successes[noNA_indices]
        ff <- failures[noNA_indices]
        tot <- ss+ff

        logit_vec <- array(0, length(ss))

        zero_ids <- which(ss == 0)
        full_ids <- which((tot - ss) == 0)
        normal_ids <- setdiff(1:length(ss), union(zero_ids, full_ids))
        if(length(zero_ids) > 0){
          logit_vec[zero_ids] <- log((ss[zero_ids] + 0.5)/ (ff[zero_ids] + 0.5), base=2) - 0.5
        }
        if(length(full_ids) > 0){
          logit_vec[full_ids] <- log((ss[full_ids] + 0.5)/ (ff[full_ids] + 0.5), base=2) + 0.5
        }
        if(length(normal_ids) > 0){
          logit_vec[normal_ids] <- log(ss[normal_ids]/ff[normal_ids], base=2)
        }
        Vthree <- (tot+1)/(tot) * (1/(ss+1) + 1/(ff+1))
        Vstar <- Vthree *  (1 - (2/tot) + Vthree/2)
        se_logit_vec <- sqrt(Vstar - 0.5*Vthree^2*(Vthree - (4/tot)))
        if(all(logit_vec == 0)){
          llambda_mat[noNA_indices, m] = logit_vec
        }else{
          fit <- ashr::ash(logit_vec, se_logit_vec, mode = "estimate",
                           mixcompdist = "normal")
          ash_logit_vec <- fit$result$PosteriorMean
          llambda_mat[noNA_indices, m] <- ash_logit_vec - log(n_data/n_bg, base=2)
        }
      }
      if(min(abs(llambda_mat), na.rm = TRUE) == 0){
           se <- 1e-15
          llambda_mat = llambda_mat + matrix(rnorm(nrow(llambda_mat)*ncol(llambda_mat),
                               0, se), nrow(llambda_mat), ncol(llambda_mat))
      }
    }

    if(shrink == FALSE){
        fl_pfm <- floor(pfm)
        diff <- pfm[!is.na(pfm)] - fl_pfm[!is.na(fl_pfm)]
        if(sum(abs(diff)) == 0){
          if(is.null(pseudocount)) {pseudocount <- 0.5}
          pfm <- pfm+pseudocount
        }else{
          if(is.null(pseudocount)) {
          pseudocount <- 0.5*min(min(abs(pfm[pfm > 0]), na.rm=TRUE),
                                min(abs(bgmat_pre[bgmat_pre > 0]), na.rm=TRUE))
          }
          pfm <- pfm + pseudocount
        }
    }

    if(tol == 0) {
      logo_control_default$score <- "log"
    }else if (tol > 0){
      logo_control_default$score <- "preclog"
    }else{
      stop("Tolerance must be a positive quantity")
    }

    logo_control <- modifyList(logo_control_default, logo_control)

    if(tol > 0){
      logo_control$score <- "preclog"
    }

  }

  if(type == "Logo"){
    fl_pfm <- floor(pfm)
    diff <- pfm[!is.na(pfm)] - fl_pfm[!is.na(fl_pfm)]
    if(sum(abs(diff)) == 0){
      if(is.null(pseudocount)) {pseudocount <- 0.5}
      pfm <- pfm+pseudocount
    }else{
      if(is.null(pseudocount)) {
      pseudocount <- 0.5*min(min(abs(pfm[pfm > 0]), na.rm=TRUE),
                             min(abs(bgmat_pre[bgmat_pre > 0]), na.rm=TRUE))
      }
      pfm <- pfm + pseudocount
    }

    logo_control <- modifyList(logo_control_default, logo_control)
  }

  pfm_scaled <- pfm

  ##############  Coloring choices for the logo plot ################

  if(is.null(color_type)){
    message("color_type not provided, so switching to per_row option for
            color_type")
    color_type = "per_row"
  }
  if(color_type == "per_row"){
    if(is.null(colors)){
      cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors,
                                 rownames(cols)))
      col_vector = col_vector[-c(4,5)]
      if(!is.null(color_seed)){
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = sample(col_vector, dim(pfm_scaled)[1],
                                             replace = FALSE))
      }else{
        color_profile <- list("type" = color_type,
                              "col" = col_vector[1:dim(pfm_scaled)[1]])
      }
    }else{
      if (length(colors) < dim(pfm_scaled)[1]){
        stop("For per_row color type, the colors vector must be as large
             as number of rows in the matrix for PFM/PWM input, or
             number of distinct characters in
             each aligned sequence for sequence data")
      }
      if(!is.null(color_seed)){
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = sample(colors, dim(pfm_scaled)[1],
                                             replace = FALSE))
      }else{
        color_profile <- list("type" = color_type,
                              "col" = colors[1:dim(pfm_scaled)[1]])
      }

      }
  }
  if(color_type == "per_symbol"){
    if(is.null(colors)){
      cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors,
                                 rownames(cols)))
      col_vector = col_vector[-c(4,5)]
      if(!is.null(color_seed)){
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = sample(col_vector,
                                             length(logo_control$total_chars),
                                             replace=FALSE))
      }else{
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = col_vector[1:length(logo_control$total_chars)])
      }
    }else{
      if (length(colors) < length(logo_control$total_chars)){
        stop("For per_symbol color type, the colors vector must be
             as large as number of
             symbols in total_chars argument in logo_control() :
             which is 50 by default ")
      }
      if(!is.null(color_seed)){
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = sample(colors,
                                             length(logo_control$total_chars), replace=FALSE))
      }else{
        color_profile <- list("type" = color_type,
                              "col" = colors[1:length(logo_control$total_chars)])
      }
      }
  }
  if(color_type == "per_column"){
    if(is.null(colors)){
      cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors,
                                 rownames(cols)))
      col_vector = col_vector[-c(4,5)]
      if(!is.null(color_seed)){
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = sample(col_vector, dim(pfm_scaled)[2],
                                             replace = FALSE))
      }else{
        color_profile <- list("type" = color_type,
                              "col" = col_vector[1:dim(pfm_scaled)[2]])
      }

    }else{
      if (length(colors) < dim(pfm_scaled)[2]){
        stop("For per_column color type, the colors vector must be as
             large as number of columns in the matrix for PFM/PWM input,
             or number of characters in each aligned
             sequence for sequence data")
      }
      if(!is.null(color_seed)){
        set.seed(color_seed)
        color_profile <- list("type" = color_type,
                              "col" = sample(colors, dim(pfm_scaled)[2],
                                             replace = FALSE))
      }else{
        color_profile <- list("type" = color_type,
                              "col" = colors[1:dim(pfm_scaled)[2]])
      }
      }
  }


  #################### Plotting the Logo/EDLogo plots  ####################

  if(type ==  "Logo"){
    out <- do.call(plogomaker, append (list(table = pfm_scaled,
                                            color_profile = color_profile,
                                            bg = bg),
                                       logo_control))
  }else if (type == "EDLogo"){
    if(shrink == FALSE){
      out <- do.call(nlogomaker, append (list(table = pfm_scaled,
                                              color_profile = color_profile,
                                              bg = bg,
                                              tol = tol),
                                         logo_control))
    }else{
      out <- do.call(nlogomaker, append (list(table = pfm_scaled,
                                              color_profile = color_profile,
                                              bg = bg,
                                              llambda = llambda_mat,
                                              tol = tol),
                                         logo_control))
    }
  }
  if(return_heights){
    return(out)
  }
}


normalize0 = function(x){return(x/sum(x[!is.na(x)]))}

