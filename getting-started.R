## ----setup, message=FALSE--------------------------------------------------------------------
library(limpidR)

## --------------------------------------------------------------------------------------------
db <- load_limpid(quiet = TRUE)
check_database(db)

## --------------------------------------------------------------------------------------------
head(summarise_abundance(db))
head(analyse_morphology(db))
head(analyse_size_distribution(db))
head(analyse_polymers(db))

## --------------------------------------------------------------------------------------------
cv <- cross_validate_mp(
  db,
  MP_Mean ~ Season_Global + Lake_Type,
  method = "lognormal_lm",
  group = "Lake_ID"
)
cv$overall

## --------------------------------------------------------------------------------------------
pol <- db$Polymer_Composition
cols <- c("PE_pct", "PP_pct", "PET_PES_pct", "PA_Nylon_pct",
          "PS_EPS_pct", "PVC_pct", "OtherPolymer_pct")
head(clr_transform(pol, cols))
aitchison_distance(pol, cols)

