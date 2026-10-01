# limpidR

**Reproducible analysis of freshwater microplastic data**

Maintainer: **Chanikya Naidu** (<thefisherieschanikyaneeti@gmail.com>)

`limpidR` is the companion research-software toolkit for **LIMPID-India** (*Lake Inventory of Microplastic Pollution in Indian Freshwaters*). It is designed for transparent, reusable analysis of freshwater microplastic datasets rather than one-off thesis scripts.

## Scientific design principles

- Explicit units; no silent conversion.
- Missing values are never interpreted as zero.
- Composition profiles are checked for 0-100% bounds and closure.
- Relative hotspot maps do not imply geostatistical interpolation.
- Default predictive validation is grouped by lake to reduce spatial leakage.
- Depth plots require genuinely depth-resolved observations.
- Risk components require user-supplied reference/hazard assumptions; no universal polymer hazard weights are hard-coded.
- Environmental covariates can be filtered by model-readiness status rather than silently treated as primary measurements.

## Main workflow

```r
library(limpidR)

db <- load_limpid()
check_database(db)

summarise_abundance(db)
analyse_morphology(db)
analyse_size_distribution(db)
analyse_polymers(db)

plot_seasonality(db, lake = "Powai")
plot_polymer_profile(db)
plot_lake_map(db)
map_mp_hotspots(db)

mod <- model_mp_abundance(
  db,
  MP_Mean ~ Season_Global + Lake_Type,
  method = "lognormal_lm"
)

cross_validate_mp(
  db,
  MP_Mean ~ Season_Global + Lake_Type,
  group = "Lake_ID"
)
```

## Compositional analysis

```r
pol <- db$Polymer_Composition
pol_clr <- clr_transform(pol, c(
  "PE_pct", "PP_pct", "PET_PES_pct", "PA_Nylon_pct",
  "PS_EPS_pct", "PVC_pct", "OtherPolymer_pct"
))

d <- aitchison_distance(pol, c(
  "PE_pct", "PP_pct", "PET_PES_pct", "PA_Nylon_pct",
  "PS_EPS_pct", "PVC_pct", "OtherPolymer_pct"
))
```

## Transparent risk components

```r
risk <- calculate_risk(
  db,
  abundance_reference = 10,
  hazard_scores = c(
    PE = 1, PP = 1, PET_PES = 2, PA_Nylon = 2,
    PS_EPS = 3, PVC = 4, OtherPolymer = 2
  )
)
```

The values above are only an example of the **required explicit input format**. They are not package-endorsed hazard scores.

## Installing a source checkout

```r
install.packages(c("ggplot2", "testthat"))
# From the parent directory of the package source:
install.packages("limpidR", repos = NULL, type = "source")
```

During development, `devtools::load_all()` and `devtools::check()` are recommended.

## Data

The package bundles a deterministic **synthetic** CSV dataset that follows the LIMPID-India schema for examples and tests. It contains no observed or third-party measurements. The full LIMPID-India research database is distributed separately as a versioned data product; see `inst/extdata/DATASET-README.txt`.

## Status

Version **0.1.0** is a CRAN submission candidate maintained by **Chanikya Naidu**. Creator/maintainer metadata are complete. Before uploading to CRAN, build the source tarball with `R CMD build` and run `R CMD check --as-cran` plus appropriate multi-platform checks.
