# balancedSampling -- sampling according to the frequencies of qualitative variables

**Author:** Sylvain Loiseau<br/>
**License:** [BSD_3_clause](https://opensource.org/licenses/BSD-3-Clause)


# Installation

```{r}
devtools::install_github("sylvainloiseau/balancedSampling",  build_vignettes=TRUE)

```

This package helps partitioning a population into several samples into which each modality of one or several qualitative variables are equaly frequent; the randomization can be constrained further according to maximum allowed numbers of consecutive occurrences of the modalities.

