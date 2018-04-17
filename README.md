# Logolas

Logolas is an R package for Enrichment Depletion Logo plots with
string symbols, including adaptive scaling of position-weight matrices
(PWMs). Logolas can be considered an extension to the
[seqLogo package](https://doi.org/doi:10.18129/B9.bioc.seqLogo).

If you find a bug, please create an
[issue](https://github.com/kkdey/Logolas/issues).

This code has been tested in ...

<img src="utils/figures/misc2.png" alt="misc"
  height="400" width="700" align = "middle">

## License

Copyright (c) 2017-2018, Kushal Dey.

All source code and software in this repository are made available
under the terms of the [GNU General Public
License](http://www.gnu.org/licenses/gpl.html). See the
[LICENSE](LICENSE) file for the full text of the license.

## Citing this work

If you find that this R package is useful for your work, please cite
our paper:

> Dey, K.K., Xie, D. and Stephens, M., 2017. *A new sequence logo plot
to highlight enrichment and depletion.* bioRxiv
[doi:10.1101/226597](https://doi.org/10.1101/226597).

## Quick Start

The most recent version of Logolas available on
[Bioconductor](https://doi.org/doi:10.18129/B9.bioc.Logolas) can be
easily installed by running these two commands in R:

```R
source("https://bioconductor.org/biocLite.R")
biocLite("Logolas")
```

These commands will also install several other packages from CRAN and
Bioconductor that are needed to run Logolas.

Alternatively, you can to install the most up-to-date development
version. The easiest way to accomplish this is using the
[devtools](http://www.r-pkg.org/pkg/devtools) package, but you will
need to first install several packages from Bioconductor:

```R
source("https://bioconductor.org/biocLite.R")
biocLite(c("Biostrings","BiocStyle","Biobase","seqLogo","ggseqlogo"))
library(devtools)
install_github("kkdey/Logolas",build_vignettes = TRUE)
```

Once you have installed the package, load the package in R by entering

```R
library(Logolas)
```

To get an overview of the package, enter

```R
help(package = "Logolas")
```

Next, try creating a few plots using the `logomaker` function:

```R
sequence <- c("CTATTGT","CTCTTAT","CTATTAA","CTATTTA", "CTATTAT","CTTGAAT",
              "CTTAGAT","CTATTAA","CTATTTA","CTATTAT", "CTTTTAT","CTATAGT",
              "CTATTTT","CTTATAT","CTATATT","CTCATTT", "CTTATTT","CAATAGT",
              "CATTTGA","CTCTTAT","CTATTAT","CTTTTAT", "CTATAAT","CTTAGGT",
              "CTATTGT","CTCATGT","CTATAGT", "CTCGTTA","CTAGAAT","CAATGGT")
logomaker(sequence,type = "Logo")
```
             
<img src="utils/figures/fig1.png" alt="misc" height="200" width="400" align = "middle">

```R
logomaker(sequence, type = "EDLogo")
```

<img src="utils/figures/fig2.png" alt="misc" height="200" width="400" align = "middle">

```R
data(mutation_sig)
logomaker(mutation_sig, type = "EDLogo", color_type = "per_symbol",  color_seed = 2300)
```

<img src="utils/figures/fig3.png" alt="misc" height="200" width="400" align = "middle">

```R
data(histone_marks)
logomaker(histone_marks$mat, bg = histone_marks$bgmat, type = "EDLogo")
```

<img src="utils/figures/fig4.png" alt="misc" height="200"
width="400" align = "middle">

Finally, please walk through some more detailed examples in the
[vignette](vignettes/Logolas.Rmd):

```R
vignette("Logolas")
```

## Credits

This software was developed by [Kushal Dey](https://github.com/kkdey)
[Dongyue Xie](https://github.com/DongyueXie) and
[Matthew Stephens](http://stephenslab.uchicago.edu) at the University
of Chicago. For any questions or comments, please contact Kushal Dey
at [kkdey@uchicago.edu](kkdey@uchicago.edu).

The authors would like to acknowledge Oliver Bembom, the author of the
`seqLogo` fpackage or acting as an inspiration and giving us the base
through his awesome package, on which we developed this software. The
authors also thank Peter Carbonetto, Edward Wallace and John Blischak
for helpful discussions and feedback.
