vertices <- naab_unique |>
  mutate(yob = year(dob)) |>
  filter(yob < 2014)

adj <- naab[(naab$reg_id %in% vertices$reg_id), c("reg_id", "sire_id")]

adj <- adj |>
  mutate(reg_id = factor(reg_id),
         sire_id = factor(sire_id))


nodes <- union(levels(adj$reg_id), levels(adj$sire_id))
edges <- adj |>
  rename(from = sire_id,
         to = reg_id) |>
  mutate(from = as.character(from),
         to = as.character(to))
edges <- edges[!duplicated(edges),]

naab_ped <- naab[naab$reg_id %in% unique(naab_aiss$reg_id),]
naab_ped <- naab_ped[!duplicated(naab_ped$reg_id),]

library(ggroups)
naab_ped <- naab_ped |>
  select(c(reg_id, sire_id, dam_id)) |>
  data.frame()

#pedcheck(naab_ped)
naab_ped <- naab_ped |> 
  dplyr::select(c(reg_id, sire_id, dam_id)) 

naab_ped[is.na(naab_ped)] <- 0
naab_ped <- renum(naab_ped)

Amat <- buildA(naab_ped$newped)
Amat <- mat2tab(Amat)

Amat <- Amat |>
  mutate(ID1 = as.integer(ID1),
         ID2 = as.integer(ID2))

Amat_1 <- left_join(Amat, naab_ped$xrf, by = c("ID1" = "newID"))
Amat_1 <- left_join(Amat_1, naab_ped$xrf, by = c("ID2" = "newID"))

edges_1 <- Amat_1 |>
  dplyr::select(c(ID.x, ID.y, val)) |>
  rename(from = ID.x,
         to = ID.y) |>
  relocate(val, .after = "to")

edges_1 <- edges_1 |> 
  filter(val != 1) |>
  mutate(from = factor(from),
         to = factor(to))

rm(Amat_1, edges, vertices, adj)