# limpidR 0.1.0 Static Build Audit

**Result:** PASS WITH RELEASE BLOCKERS

## Passes
- DESCRIPTION present
- NAMESPACE present
- LICENSE present
- all 28 exports have function definitions
- all 28 exports have Rd aliases
- R/ source is ASCII-only
- no triple-colon internal access
- no setwd usage
- no runtime download
- no q() usage
- lakes: 21 rows and unique Lake_ID
- events: 70 rows and unique Event_ID
- abundance: 70 rows and unique Event_ID
- morphology: 70 rows and unique Event_ID
- size: 70 rows and unique Event_ID
- polymer: 70 rows and unique Event_ID
- abundance: full Event_ID alignment
- morphology: full Event_ID alignment
- size: full Event_ID alignment
- polymer: full Event_ID alignment
- abundance non-negative
- morphology percentages valid and close within 0.2%
- size percentages valid and close within 0.2%
- polymer percentages valid and close within 0.2%
- Polymer_Composition.csv: rectangular 70x17
- Release_Data_Dictionary.csv: rectangular 269x8
- Enrichment_Provenance.csv: rectangular 143x17
- Method_Metadata.csv: rectangular 5x22
- Station_Master.csv: rectangular 5x12
- Environmental_Context.csv: rectangular 70x20
- Lake_Master.csv: rectangular 21x17
- WaterQuality_Event_Summary.csv: rectangular 2x14
- MP_Abundance.csv: rectangular 70x11
- Sampling_Events.csv: rectangular 70x18
- Size_Composition.csv: rectangular 70x10
- Morphology_Composition.csv: rectangular 70x11
- Source_References.csv: rectangular 28x7
- WaterQuality_External.csv: rectangular 16x22
- Catchment_Standard.csv: rectangular 21x21
- Model_Readiness.csv: rectangular 70x15
- Controlled_Vocab.csv: rectangular 23x3
- QAQC.csv: rectangular 70x14
- uncompressed source tree size: 327.5 KiB

## Issues
- None detected by the static audit.

## Release blockers / warnings
- Authors@R is intentionally placeholder; replace before CRAN submission.
- MIT copyright holder is intentionally placeholder; replace before CRAN submission.
- R CMD build/check was not executable in this runtime because R is absent and OS repositories are inaccessible.
- Bundled third-party-derived data require rights/licensing review before CRAN distribution; a code-only CRAN profile may be preferable if redistribution rights are uncertain.
- Package-name availability must be rechecked against current/past CRAN and current Bioconductor packages immediately before submission.
