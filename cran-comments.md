## limpidR 0.1.0

This is the first CRAN submission candidate for limpidR.

### Package/data design
- Bundled example data are synthetic and deterministic; the full LIMPID-India research database is not distributed inside the CRAN package.

### Checks completed in the ChatGPT build environment
- Static source-tree audit.
- Export/documentation alias audit.
- Bundled-data integrity audit.
- No non-ASCII bytes in R/ source files.

### Checks still required before CRAN submission
- Run R CMD build and R CMD check on an installed R environment.
- Run rhub::check_for_cran().
- Run macOS builder check.
- Verify package-name availability immediately before first submission.

The current build environment did not contain R and did not permit OS package downloads, so no claim of a completed R CMD check is made.

### Maintainer
Chanikya Naidu <thefisherieschanikyaneeti@gmail.com>
