# CRAN submission checklist

- [x] Creator metadata added: Chanikya Naidu.
- [x] Maintainer email added: thefisherieschanikyaneeti@gmail.com.
- [ ] Add ORCID in Authors@R if/when available (optional for submission).
- [ ] Confirm `limpidR` is still available on CRAN immediately before submission.
- [ ] Run `R CMD build limpidR`.
- [ ] Run `R CMD check --as-cran limpidR_0.1.0.tar.gz` with 0 errors/warnings and review all NOTEs.
- [ ] Run `rhub::check_for_cran()`.
- [ ] Run CRAN macOS builder check.
- [ ] Review examples/vignettes for execution time and internet independence.
- [x] Bundled CRAN example data are fully synthetic and contain no third-party observations.
- [ ] Add repository URL and BugReports URL once a public Git repository exists.
- [ ] Update citation with the LIMPID-India DOI after DOI assignment.
