library(limpidR)

db <- load_limpid()
print(check_database(db))

print(head(summarise_abundance(db)))
print(head(analyse_morphology(db)))
print(head(analyse_polymers(db)))

m <- model_mp_abundance(db, MP_Mean ~ Season_Global + Lake_Type)
print(m)
print(cross_validate_mp(db, MP_Mean ~ Season_Global + Lake_Type, group = "Lake_ID")$overall)

plot_seasonality(db, lake = "Powai")
plot_lake_map(db)
