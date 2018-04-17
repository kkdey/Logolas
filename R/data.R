
#' An example of the position weight matrix taken from seqLogo package
#' A dataset of composition probabilities of A, C, G and T in 8 positions of the motif.
#' @format A matrix with 4 rows and 8 columns
#'
"seqlogo_example"

#' Compositional data of histone marks in different regions of genome along with background information
#' A list containing two matrices storing composition of 5 histone marks (H3K4ME1, H3K4ME2, H3K4ME3, H3AC, H4AC)
#' in 5 regions of the genome
#' @format A list with 2 matrices - each with 5 rows (histone marks) and 5 columns (regions of genome). One is the
#' actual matrix (mat), the other is the background matrix (bgmat)
#'
"histone_marks"


#' Position specific scoring matrix data
#' A dataset of position specific sores of various amino acids in 9 positions binding domain.
#' @format A matrix with 20 rows (aminoa acids) and 9 columns
#'
"pssm"

#' Compositional mutational signature data, with mismatch and flanking base frequencies reported
#' A dataset of compositional weights for mismatches in the "0"the position (center of signature) and
#' that of the bases A, C, G, T for two left flanking bases (-1, -2) and two right flanking bases (1, 2).
#' @format A matrix with 10 rows (4 bases and 6 types of mismatches) and 5 columns (center - 0, flanking bases
#' 1,2 on the left and flanking bases 1, 2 on the right of the center)
#'
"mutation_sig"

