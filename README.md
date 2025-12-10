
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- # frontiermetrics <img src="man/figures/logo.png" align="right" width="150"/> -->

# frontiermetrics

Deforestation, especially in the tropics, is the leading driver of
biodiversity loss worldwide, contributing to climate change as well as
to the widespread degradation of nature’s contributions to people (Díaz
et al., 2019). Advances in satellite imagery and processing capabilities
have enabled major progress in identifying deforestation at increasingly
high spatial and temporal resolution, with rapid response time, and
across the globe.

Frontier metrics (Baumann et al., 2022; Buchadas et al., 2022) have
recently been developed to capture the spatio-temporal dynamics of
forest loss, and have been demonstrated to characterize advancing and
emerging frontiers over a period of 40 years for the South American Dry
Chaco (Baumann et al. 2022) and for the world’s tropical dry woodlands
globally over a 20-year period (Buchadas et al. 2020). Drawing on a more
detailed analysis of time series of forest loss, frontier metrics are
calculated through the aggregation of spatial data into larger units
that function as small landscapes composed of individual cells:

<div style="margin-top: 25px; margin-bottom: 25px;">

<img src="vignettes/img/frontier_definition.jpg" width="100%" style="display: block; margin: auto;" />

</div>

Built upon the frameworks proposed by Buchadas et al. (2022) and Baumann
et al. (2022), the R package `frontiermetrics` enables the calculation
of frontier metrics at any geographical extent.

You can install the R package from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("phuais/frontiermetrics", build_vignettes = TRUE)
```

## Getting started

Once installed, explore the introductory vignette:

``` r
vignette(topic = "intro", package = "frontiermetrics")
```

### References

Baumann, M., Gasparri, I., Buchadas, A., Oeser, J., Meyfroidt, P.,
Levers, C., … & Kuemmerle, T. (2022). Frontier metrics for a
process-based understanding of deforestation dynamics. Environmental
Research Letters, 17(9), 095010.

Buchadas, A., Baumann, M., Meyfroidt, P., & Kuemmerle, T. (2022).
Uncovering major types of deforestation frontiers across the world’s
tropical dry woodlands. Nature Sustainability, 5(7), 619-627.

Díaz, S., Settele, J., Brondízio, E. S., Ngo, H. T., Agard, J., Arneth,
A., … & Zayas, C. N. (2019). Pervasive human-driven decline of life on
Earth points to the need for transformative change. Science, 366(6471),
eaax3100.

Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S.
A., Tyukavina, A., … & Townshend, J. R. (2013). High-resolution global
maps of 21st-century forest cover change. science, 342(6160), 850-853.
