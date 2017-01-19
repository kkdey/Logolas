

name <- "AC/lambda//theta/EF"

split_string <- strsplit(as.character(name), "")[[1]]
slash_index <- grep("/", split_string)

groups <- length(slash_index)/2;

sym <- array(0, groups)
sym_slash_index <- numeric()
sym_slash_top_index <- array(0, groups)
counter  <- 1

for(l in 1:groups){
    sym[l] <- paste0(as.character(split_string[(slash_index[counter]+1) : (slash_index[counter+1]-1)]),
               collapse="")
    sym_slash_index <- c(sym_slash_index, slash_index[counter] : slash_index[counter+1])
    sym_slash_top_index[l] <- slash_index[counter]
    counter <- counter + 2
}


split_string_alphanumeric <- split_string[-sym_slash_index]

inds_alphanumeric <- (1:length(split_string))[-sym_slash_index]

split_string_mod <- c(split_string_alphanumeric, sym)
split_string_inds <- c(inds_alphanumeric, sym_slash_top_index)

split_string <- split_string_mod[order(split_string_inds)]


