# limpidR 0.1.0 — Scientific Build Report

## Purpose

`limpidR` is the reproducible analysis toolkit companion to LIMPID-India v1.0.0. It converts the database architecture into reusable scientific workflows for freshwater microplastic researchers.

## Implemented public API

### Data and QA
- `load_limpid()`
- `limpid_tables()`
- `check_database()`
- `validate_mp_data()`
- `clean_mp_data()`
- `convert_mp_units()`
- `provenance_summary()`

### Descriptive and compositional analysis
- `summarise_abundance()`
- `analyse_morphology()`
- `analyse_size_distribution()`
- `analyse_polymers()`
- `composition_closure()`
- `clr_transform()`
- `aitchison_distance()`

### Modelling
- `build_model_data()`
- `model_mp_abundance()`
- `predict_mp()`
- `cross_validate_mp()`

### Risk and spatial analysis
- `calculate_risk()`
- `classify_hotspots()`
- `map_mp_hotspots()`

### Plots and reproducibility
- `plot_seasonality()`
- `plot_morphology_profile()`
- `plot_polymer_profile()`
- `plot_depth_profile()`
- `plot_lake_map()`
- `limpid_session_info()`
- `limpid_citation()`

## Scientific safeguards

1. Percentage profiles are validated for bounds and closure.
2. CLR/Aitchison tools explicitly handle zeros through a user-visible pseudocount.
3. The default abundance model uses log1p-LM plus Duan smearing; Gamma(log) is available for strictly positive responses.
4. Cross-validation is grouped by lake by default instead of randomly splitting rows.
5. Hotspot maps are relative point classifications and explicitly do not claim interpolation.
6. Depth-profile plots reject event-level LIMPID data and require true depth-resolved rows.
7. Risk calculation has no built-in universal hazard weights or thresholds; assumptions must be supplied by the researcher.
8. Missing environmental values are not automatically imputed.

## Verification status

The source tree passes the included static audit, including exported-function/documentation matching and bundled-data structural checks. A true `R CMD build` / `R CMD check --as-cran` could not be executed because this runtime has no R installation and cannot reach OS package repositories.

Before CRAN submission, complete the items in `CRAN_SUBMISSION_CHECKLIST.md`.
