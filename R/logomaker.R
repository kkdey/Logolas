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
#' @param use_dash Boolean. If TRUE, performs adaptive scaling of the consensus
#'                 matrix from sequences or the positional frequency matrix
#'                 based on the number of underlying seqeunces. Step 
#'                 automatically skipped if the user provides a positional
#'                 weight matrix.
#'
#' @param bg The background probability, which defaults to NULL, in which case
#'           equal probability is assigned to each symbol. The user can 
#'           however specify a vector (equal to in length to the number of 
#'           symbols) which specifies the background probability for each symbol
#'            and assumes this background probability to be the same across the 
#'            columns (sites), or a matrix, whose each cell specifies the 
#'            background probability of the symbols for each position.
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
#' @param dash_control Control parameters for the adaptive scaling \code{dash}
#'                     function. Check the input arguments to the \code{dash}
#'                     function in this package.
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
#' @importFrom  utils  modifyList
#' @import ggplot2
#' @import gridBase
#' @export
logomaker <- function(data,
                      type = c("Logo", "EDLogo"),
                      use_dash = TRUE,
                      bg = NULL,
                      color_type = NULL,
                      colors = NULL,
                  #    color_seed = 2030,
                      color_seed = NULL,
                      return_heights = FALSE,
                      logo_control = list(),
                      dash_control = list()){

  dash_control_default <- list(concentration = NULL,
                               mode = NULL,
                               optmethod = "mixEM",
                               sample_weights = NULL,
                               verbose = FALSE,
                               bf = TRUE,
                               pi_init = NULL,
                               squarem_control = list(),
                               dash_control = list(),
                               reportcov = FALSE)

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

        if(type == "PSSM"){
          message("for character sequence data, switch to EDLogo type")
          type = "EDLogo"
        }

        L <- length(data)
        pfm <- Biostrings::consensusMatrix(data)
        colnames(pfm) <- 1:dim(pfm)[2]

        numchars <- sapply(rownames(pfm), function(x) return (nchar(x)))
        if(max(numchars) == 1){
          logo_control_default$col_line_split <- "white"
        }else{
          logo_control_default$col_line_split <- "grey80"
        }
        logo_control_default$pop_name = ""
        logo_control_default$xlab = "position"

        dash_control <- modifyList(dash_control_default, dash_control)
        logo_control <- modifyList(logo_control_default, logo_control)

        if(use_dash){
          pfm_scaled <- do.call(dash, append(list(comp_data = pfm),
                                             dash_control))$posmean
        }else{
          pfm_scaled <- pfm
        }
        
        pfm_scaled <- zero_augment(pfm_scaled)

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
        if(type ==  "Logo"){
          out <- do.call(plogomaker, append (list(table = pfm_scaled,
                                                  color_profile = color_profile,
                                                   bg = bg),
                                     logo_control))
        }else if (type == "EDLogo"){
          out <- do.call(nlogomaker, append (list(table = pfm_scaled,
                                                  color_profile = color_profile,
                                                   bg = bg),
                                      logo_control))
        }
   }
   

   if(!is.character(data)){

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

     numchars <- sapply(rownames(data), function(x) return (nchar(x)))
     if(max(numchars) == 1){
       logo_control_default$col_line_split <- "white"
     }else{
       logo_control_default$col_line_split <- "grey80"
     }
     logo_control_default$pop_name = ""
     logo_control_default$xlab = "position"


     if(all(data == floor(data))){
       datatype = "PFM"
     }else{
       datatype = "PWM"
     }

     if(datatype == "PWM"){
       use_dash = FALSE
     }

     dash_control <- modifyList(dash_control_default, dash_control)
     logo_control <- modifyList(logo_control_default, logo_control)


     if(use_dash){
       data_scaled <- do.call(dash, append(list(comp_data = data),
                                            dash_control))$posmean
     }else{
       data_scaled <- data
     }
     
     data_scaled <- zero_augment(data_scaled)
     
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
                                 "col" = sample(col_vector, dim(data_scaled)[1],
                                                replace = FALSE))
         }else{
           color_profile <- list("type" = color_type,
                                 "col" = col_vector[1:dim(data_scaled)[1]])
         }
       }else{
         if (length(colors) < dim(data_scaled)[1]){
           stop("For per_row color type, the colors vector must be as large
                as number of rows in the matrix for PFM/PWM input, or 
                number of distinct characters in
                each aligned sequence for sequence data")
         }
         if(!is.null(color_seed)){
           set.seed(color_seed)
           color_profile <- list("type" = color_type,
                                 "col" = sample(colors, dim(data_scaled)[1], 
                                                replace = FALSE))
         }else{
           color_profile <- list("type" = color_type,
                                 "col" = colors[1:dim(data_scaled)[1]])
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
                                                length(logo_control$total_chars), 
                                                replace=FALSE))
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
                                 "col" = sample(col_vector, dim(data_scaled)[2],
                                                replace = FALSE))
         }else{
           color_profile <- list("type" = color_type,
                                 "col" = col_vector[1:dim(data_scaled)[2]])
         }
         
       }else{
         if (length(colors) < dim(data_scaled)[2]){
           stop("For per_column color type, the colors vector must be as
                large as number of columns in the matrix for PFM/PWM input, 
                or number of characters in each aligned
                sequence for sequence data")
         }
         if(!is.null(color_seed)){
           set.seed(color_seed)
           color_profile <- list("type" = color_type,
                                 "col" = sample(colors, dim(data_scaled)[2], 
                                                replace = FALSE))
         }else{
           color_profile <- list("type" = color_type,
                                 "col" = colors[1:dim(data_scaled)[2]])
         }
         }
     }

    

     if(type == "Logo"){
       out <- do.call(plogomaker, append (list(table = data_scaled,
                                               color_profile = color_profile,
                                               bg = bg),
                                  logo_control))
     }else if (type == "EDLogo"){
       out <- do.call(nlogomaker, append (list(table = data_scaled,
                                               color_profile = color_profile,
                                               bg = bg),
                                   logo_control))
     }

   }
  if(return_heights){
    return(out)
  }
}
