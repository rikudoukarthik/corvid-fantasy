
# for sort order of species
# ebd_tax <- ebirdtaxonomy(key = myebirdtoken) %>%
#   rename(ENGLISH.NAME = comName) %>%
#   mutate(SORT = 1:n()) %>%
#   dplyr::select(ENGLISH.NAME, SORT)
# write_csv(ebd_tax, "eBirdTaxonomy.csv")
ebd_tax <- read_csv("eBirdTaxonomy.csv")
