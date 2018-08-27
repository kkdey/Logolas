#' EBF1_disc1 transcription factor position weight matrix
#' A dataset of composition probabilities of A, C, G and T in 10 positions of 
#' the motif.
#' @format A matrix with 4 rows and 10 columns
#'
"EBF1_disc1"

#' Consensus initiation contexts in seven different organisms. 
#' The consensus of initiation in different species was obtained from the Transterm
#' database. The data has been fetched from Ivanov et al 2012.
#' @format A list of matrices, each matrix with 4 rows and 15 columns
#'
"consensus_PWM_codons"

#' ELF1 initiation contexts in metazoa, fungi and plants. 
#' The data has been fetched from Ivanov et al 2012.
#' @format A list of 3 matrices, each matrix with 4 rows and 15 columns
#'
"ELF1"

#' Amino acid sequences from UniProt database that constitute N-glycosylation sites.
#' The data has been fetched from UniProt website.
#' @format A vector of sequences, with the glycosylation site at the center (usually N),
#' and 5 bases on either side of it.
#'
"N_Glycosyl_sequences"

#' An example of the position weight matrix taken from seqLogo package
#' A dataset of composition probabilities of A, C, G and T in 8 positions of 
#' the motif.
#' @format A matrix with 4 rows and 8 columns
#'
"seqlogo_example"

#' Compositional data of histone marks in different regions of genome along with
#'  background information
#' A list containing two matrices storing composition of 5 histone marks
#'  (H3K4ME1, H3K4ME2, H3K4ME3, H3AC, H4AC)
#' in 5 regions of the genome
#' @format A list with 2 matrices - each with 5 rows (histone marks) and 
#' 5 columns (regions of genome). One is the
#' actual matrix (mat), the other is the background matrix (bgmat)
#'
"histone_marks"


#' Position specific scoring matrix data
#' A dataset of position specific sores of various amino acids in 9 positions
#' binding domain.
#' @format A matrix with 20 rows (aminoa acids) and 9 columns
#'
"pssm"

#' Compositional mutational signature data, with mismatch and flanking base
#' frequencies reported
#' A dataset of compositional weights for mismatches in the "0"the position 
#' (center of signature) and
#' that of the bases A, C, G, T for two left flanking bases (-1, -2) and two 
#' right flanking bases (1, 2).
#' @format A matrix with 10 rows (4 bases and 6 types of mismatches) and 
#' 5 columns (center - 0, flanking bases
#' 1,2 on the left and flanking bases 1, 2 on the right of the center)
#'
"mutation_sig"


#' An example of the position weight matrix of the bases in the first 30 
#' positions from ends of sequenced
#' reads (together with the barcode)
#' A dataset of composition probabilities of A, C, G and T in 30 positions 
#' from the 5' end of reads.
#' @format A matrix with 4 rows and 30 columns
#'
"UMI"

#' Counts of the aRxiv field categories of 4 UChicago Statistics professors.
#' A dataset of compositional counts of 10 aRxiv field categories for 4 
#' professors' publications in aRxiv.
#' @format A matrix with 10 rows and 4 columns
#'
"aRxiv"


#' Proportional abundances of different bird species in the Himalayan mountains
#' A dataset of proportional composition of 140 bird species in 3 regions of 
#' the Himalayas.
#' @format A matrix with 140 rows and 3 columns
#'
"himalayan_fauna"
