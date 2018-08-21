#' @title Format conversion from FASTA, Genbank, CLustalW. PhyLIP formats to 
#' vector of aligned sequences for logomaker
#'
#' @description Takes as input a FASTA/ClustalW/PhyLIP files or accession numbers
#' from Genbank and converts the sequences into a vector of aligned sequences, 
#' ready to be fed into the \code{logomaker} function. 
#' (see examples in \code{logomaker} to see its input format, which is same as 
#' the output format for this function).
#' 
#' @param filename Name of the file with stored aligned sequences - may be of
#'                 the format .phylip (PHYLIP), .aln (CLUSTAL) or .fasta(FASTA).
#'                 May also be NULL if Genbank accession codes input
#'                 \code{accession} is non NULL
#' @param accession A vector of accession numbers for GenBank. Not null when
#'                  \code{filename} is NULL.
#' @param lower_base The base position from which to start recording the 
#'                   sequence. Defaults to 1.
#' @param upper_base The base position at which to end recording the sequence.
#'                   Defaults to Inf. That would comprise of the full sequence
#'                   if \code{lower_base = 1}
#'                   
#' @return Returns a vector of aligned sequences, that are ready to be fed as 
#'         input to the \code{logomaker} function
#'         
#' @examples 
#' 
#' library(seqinr)
#' out <- logomaker_format_input(filename = system.file("sequences/gopher.fasta", 
#'                               package = "seqinr"),
#'                               lower_base = 1, upper_base = 15)
#' out <- logomaker_format_input(filename = system.file("sequences/test.aln", 
#'                               package = "seqinr"),
#'                               lower_base = 1, upper_base = 15)
#' out <- logomaker_format_input(filename = system.file("sequences/test.phylip", 
#'                               package = "seqinr"),
#'                               lower_base = 1, upper_base = 15)
#' out <- logomaker_format_input(accession =  c("U15717", "U15718", "U15719",
#'                           "U15720", "U15721", "U15722", "U15723", "U15724"),
#'                            lower_base = 1, upper_base = 10)
#'
#'@import seqinr
#'@importFrom ape read.GenBank
#'@importFrom utils tail
#'@export
#'
logomaker_format_input <- function(filename = NULL,
                                   accession = NULL,
                                   lower_base = 1,
                                   upper_base = Inf){
  
  if(upper_base < lower_base){
    stop("upper_base must be greater than lower_base")
  }
  
  
  if(is.null(filename) & is.null(accession)){
    stop("both filename and accession are NULL")
  }
  
  if(!is.null(filename) & !is.null(accession)){
    stop("file name and accession both cannot be non NULL")
  }
  
  if(!is.null(filename)){
    if(utils::tail(strsplit(filename, "[.]")[[1]], 1) == "fasta"){
      cat("Reading the input as fasta")
      fa_file <- seqinr::read.fasta(file= filename)
      sequences <- c()
      
      max_base <- min(unlist(lapply(fa_file, function(x) return(length(x)))))
      if(lower_base > max_base) stop("lower_base must be smaller than ", 
                                     max_base, " for this file")
      if(upper_base > max_base) { upper_base = max_base}
      
      for(l in 1:length(fa_file)){
        sequences <- c(sequences, 
              toupper(paste0(as.character(fa_file[[l]])[lower_base:upper_base], 
                                      collapse = "")))
      }
      return(sequences)
    }
    
    if(utils::tail(strsplit(filename, "[.]")[[1]], 1) == "aln"){
      cat("Reading the input as clustal")
      clustal_file <- seqinr::read.alignment(file = filename, 
                                             format = "clustal")
      sequences <- c()
      
      max_base <- min(unlist(lapply(clustal_file$seq, function(x) 
                                    return(nchar(x)))))
      if(lower_base > max_base) stop("lower_base must be smaller than ", 
                                     max_base, " for this file")
      if(upper_base > max_base) { upper_base = max_base}
      
      for(l in 1:length(clustal_file$seq)){
        sequences <- c(sequences, toupper(substring(clustal_file$seq[[l]], 
                                                    lower_base, upper_base)))
      }
      return(sequences)
    }
    
    
    if(utils::tail(strsplit(filename, "[.]")[[1]], 1) == "phylip"){
      cat("Reading the input as PHYLIP")
      phylip_file <- seqinr::read.alignment(file = filename, format = "phylip")
      sequences <- c()
      
      max_base <- min(unlist(lapply(phylip_file$seq, function(x) 
                                    return(nchar(x)))))
      if(lower_base > max_base) stop("lower_base must be smaller than ", 
                                     max_base, " for this file")
      if(upper_base > max_base) { upper_base = max_base}
      
      for(l in 1:length(phylip_file$seq)){
        sequences <- c(sequences, toupper(substring(phylip_file$seq[[l]], 
                                                    lower_base, upper_base)))
      }
      return(sequences)
    }
  }
  
  if(!is.null(accession)){
    genbank_file <- ape::read.GenBank(accession, as.character = TRUE)
    sequences <- c()
    
    max_base <- min(unlist(lapply(genbank_file, function(x) return(length(x)))))
    if(lower_base > max_base) stop("lower_base must be smaller than ", 
                                   max_base, " for this file")
    if(upper_base > max_base) { upper_base = max_base}
    
    for(l in 1:length(genbank_file)){
      sequences <- c(sequences, 
        toupper(paste0(as.character(genbank_file[[l]])[lower_base:upper_base], 
                                    collapse = "")))
    }
    return(sequences)
  }
}