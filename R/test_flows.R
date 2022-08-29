library(here)
library(data.table)
library(readxl)
library(sf)
library(ggplot2)

path_to_aldo <- "../../ALDO/Outil ALDO_2021_12.xlsx"
path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")

# functional_area <- st_read(here("data", "arep", "functional_area.gpkg"))
# functional_area <- st_transform(functional_area, 3035)

clc <- fread(here("data", "arep", "buildings", "clc_2000_2018.csv"))
clc <- melt(clc, id.vars = c("region", "code", "name", "area"), variable.name = "year", value.name = "clc_category", encoding = "UTF-8")
clc[, year := ifelse(year == "code_00", 2000, 2020)]
clc[, clc_category := as.character(clc_category)]

aldo_clc <- as.data.table(read_excel(path_to_aldo_clc))

# Stocks
# Soil carbon content
aldo_soil_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/Outil ALDO_2021_12.xlsx", sheet = "Ref_Sols")
aldo_soil_co2 <- as.data.table(aldo_soil_co2)
aldo_soil_co2 <- aldo_soil_co2[EPCI_Siren == "245701404"]
aldo_soil_co2 <- aldo_soil_co2[, 9:18]
aldo_soil_co2 <- melt(aldo_soil_co2, variable.name = "aldo_soil_category", value.name = "soil_carbon_content")

# Biomass outside forests carbon content
aldo_non_forest_biomass_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/Outil ALDO_2021_12.xlsx", sheet = "Ref_Biom_HorsF")
aldo_non_forest_biomass_co2 <- as.data.table(aldo_non_forest_biomass_co2)
aldo_non_forest_biomass_co2 <- aldo_non_forest_biomass_co2[siren == "245701404"]
aldo_non_forest_biomass_co2 <- aldo_non_forest_biomass_co2[, 4:13]
aldo_non_forest_biomass_co2 <- melt(aldo_non_forest_biomass_co2, variable.name = "aldo_biomass_category", value.name = "biomass_carbon_content")

# Forest biomass carbon content
aldo_forest_biomass_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/Outil ALDO_2021_12.xlsx", sheet = "Ref_Biom_foret")
aldo_forest_biomass_co2 <- as.data.table(aldo_forest_biomass_co2)
aldo_forest_biomass_co2 <- aldo_forest_biomass_co2[SIREN_EPCI == "245701404"]
aldo_forest_biomass_co2 <- unique(aldo_forest_biomass_co2[, c(3, 6)])
setnames(aldo_forest_biomass_co2, c("aldo_biomass_category", "biomass_carbon_content"))
aldo_forest_biomass_co2[, aldo_biomass_category := tolower(aldo_biomass_category)]

aldo_biomass_co2 <- rbindlist(list(aldo_non_forest_biomass_co2, aldo_forest_biomass_co2))
aldo_biomass_co2 <- aldo_biomass_co2[!is.na(biomass_carbon_content)]

# Aldo categories
aldo <- unique(aldo_clc[, list(aldo_soil_category, aldo_biomass_category)])
aldo <- merge(aldo, aldo_soil_co2, by = c("aldo_soil_category"), all.x = TRUE)
aldo <- merge(aldo, aldo_biomass_co2, by = c("aldo_biomass_category"), all.x = TRUE)

aldo[is.na(soil_carbon_content), soil_carbon_content := 0]
aldo[is.na(biomass_carbon_content), biomass_carbon_content := 0]
aldo[, litter_carbon_content := ifelse(aldo_soil_category == "forêts", 9.0, 0)]


clc_co2 <- merge(clc, aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)], by = "clc_category", all.x = TRUE)
clc_co2 <- merge(clc_co2, aldo, by = c("aldo_soil_category", "aldo_biomass_category"), all.x = TRUE)


clc_co2[, soil_carbon := area/1e4*soil_carbon_content]
clc_co2[, litter_carbon := area/1e4*litter_carbon_content]
clc_co2[, biomass_carbon := area/1e4*biomass_carbon_content]

clc_co2[, c("soil_carbon_content", "litter_carbon_content", "biomass_carbon_content") := NULL]

clc_co2 <- melt(
  clc_co2,
  id.vars = c(
    "aldo_soil_category", "aldo_biomass_category", "clc_category",
    "region", "code", "name", "year", "area")
)


clc_co2[, list(sum(value)), by = list(variable, year)][order(year)]
clc_co2[, list(sum(value)/1e6), by = year]


# Flows
# Soil



aldo_soil_flow_co2 <- read_excel(path_to_aldo, sheet = "Ref_Sols")
aldo_soil_flow_co2 <- as.data.table(aldo_soil_flow_co2)
aldo_soil_flow_co2 <- aldo_soil_flow_co2[EPCI_Siren == "245701404"]
aldo_soil_flow_co2 <- aldo_soil_flow_co2[, 19:47]
aldo_soil_flow_co2 <- melt(aldo_soil_flow_co2, variable.name = "change", value.name = "soil_carbon_flow")

aldo_soil_flow_co2[, from := tstrsplit(change, "_")[[2]]]
aldo_soil_flow_co2[, to := tstrsplit(change, "_")[[3]]]

aldo_soil_flow_co2[to == "art", to := paste0(to, "_", gsub(" ", "", tstrsplit(change, "_")[[4]]))]

aldo_soil_flow_co2[from == "prairies", from := "prai"]
aldo_soil_flow_co2[, from := gsub(" ", "", from)]

aldo_soil_flow_co2 <- merge(
  aldo_soil_flow_co2,
  unique(aldo_clc[, list(aldo_soil_category_short, aldo_soil_category)]),
  by.x = "from",
  by.y = "aldo_soil_category_short",
  all.x = TRUE
)

aldo_soil_flow_co2 <- merge(
  aldo_soil_flow_co2,
  unique(aldo_clc[, list(aldo_soil_category_short, aldo_soil_category)]),
  by.x = "to",
  by.y = "aldo_soil_category_short",
  suffixes = c("_from", "_to"),
  all.x = TRUE
)

aldo_soil_flow_co2 <- aldo_soil_flow_co2[, list(aldo_soil_category_from, aldo_soil_category_to, soil_carbon_flow)]


# Complete matrix
flow_completion_rules <- as.data.table(read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/matrix_completion.xlsx", sheet = "soil"))

complete_flow_matrix <- CJ(
  aldo_soil_category_from = unique(aldo_clc$aldo_soil_category),
  aldo_soil_category_to = unique(aldo_clc$aldo_soil_category)
)

complete_flow_matrix <- merge(
  complete_flow_matrix,
  aldo_soil_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

complete_flow_matrix <- merge(
  complete_flow_matrix,
  flow_completion_rules,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all = TRUE
)

complete_flow_matrix <- merge(
  complete_flow_matrix,
  aldo_soil_flow_co2,
  by.x = c("aldo_soil_category_from_copy", "aldo_soil_category_to_copy"),
  by.y = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

complete_flow_matrix[, soil_carbon_flow := ifelse(is.na(soil_carbon_flow.x), soil_carbon_flow.y, soil_carbon_flow.x)]

complete_flow_matrix <- complete_flow_matrix[,
                                             list(
                                               soil_carbon_flow = sum(soil_carbon_flow)
                                             ),
                                             by = list(aldo_soil_category_from, aldo_soil_category_to)
]

complete_flow_matrix[is.na(soil_carbon_flow), soil_carbon_flow := 0]

aldo_soil_flow_co2 <- copy(complete_flow_matrix)


# Biomass
aldo_non_forest_biomass_flow_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/Outil ALDO_2021_12.xlsx", sheet = "Ref_Biom_HorsF")
aldo_non_forest_biomass_flow_co2 <- as.data.table(aldo_non_forest_biomass_flow_co2)
aldo_non_forest_biomass_flow_co2 <- aldo_non_forest_biomass_flow_co2[siren == "245701404"]
aldo_non_forest_biomass_flow_co2 <- aldo_non_forest_biomass_flow_co2[, 14:91]
aldo_non_forest_biomass_flow_co2 <- melt(aldo_non_forest_biomass_flow_co2, variable.name = "change", value.name = "biomass_carbon_flow")

aldo_non_forest_biomass_flow_co2[, from := tstrsplit(change, " vers ")[[1]]]
aldo_non_forest_biomass_flow_co2[, to := tstrsplit(change, " vers ")[[2]]]

aldo_non_forest_biomass_flow_co2[, from := tolower(from)]
aldo_non_forest_biomass_flow_co2[, to := tolower(to)]
aldo_non_forest_biomass_flow_co2[, to := gsub(" \\(tc/ha/an\\)", "", to)]

aldo_non_forest_biomass_flow_co2[, from := gsub("sosl", "sols", from)]
aldo_non_forest_biomass_flow_co2[, to := gsub("sosl", "sols", to)]

aldo_non_forest_biomass_flow_co2[from == "prairies arborées", from := "prairies zones arborées"]
aldo_non_forest_biomass_flow_co2[from == "prairies arbustives", from := "prairies zones arbustives"]
aldo_non_forest_biomass_flow_co2[from == "prairies herbagées", from := "prairies zones herbacées"]
aldo_non_forest_biomass_flow_co2[from == "sols artificiels arborés", from := "sols artificiels arborés et buissonants"]

aldo_non_forest_biomass_flow_co2[to == "prairies arborées", to := "prairies zones arborées"]
aldo_non_forest_biomass_flow_co2[to == "prairies arbustives", to := "prairies zones arbustives"]
aldo_non_forest_biomass_flow_co2[to == "prairies herbagées", to := "prairies zones herbacées"]
aldo_non_forest_biomass_flow_co2[to == "sols artificiels arborés", to := "sols artificiels arborés et buissonants"]
aldo_non_forest_biomass_flow_co2[to == "sols aritificiels arbustifs", to := "sols artificiels arbustifs"]

aldo_non_forest_biomass_flow_co2 <- aldo_non_forest_biomass_flow_co2[, list(from, to, biomass_carbon_flow)]
setnames(aldo_non_forest_biomass_flow_co2, c("aldo_biomass_category_from", "aldo_biomass_category_to", "biomass_carbon_flow"))


# Complete matrix
flow_completion_rules <- as.data.table(read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/matrix_completion.xlsx", sheet = "biomass"))

complete_flow_matrix <- CJ(
  aldo_biomass_category_from = unique(aldo_clc$aldo_biomass_category),
  aldo_biomass_category_to = unique(aldo_clc$aldo_biomass_category)
)

# complete_flow_matrix <- complete_flow_matrix[!aldo_biomass_category_from %in% c("feuillu", "mixte", "conifere", "peupleraies")]
# complete_flow_matrix <- complete_flow_matrix[!aldo_biomass_category_to %in% c("feuillu", "mixte", "conifere", "peupleraies")]

complete_flow_matrix <- merge(
  complete_flow_matrix,
  aldo_non_forest_biomass_flow_co2,
  by = c("aldo_biomass_category_from", "aldo_biomass_category_to"),
  all.x = TRUE
)

complete_flow_matrix <- merge(
  complete_flow_matrix,
  flow_completion_rules,
  by = c("aldo_biomass_category_from", "aldo_biomass_category_to"),
  all = TRUE
)

complete_flow_matrix <- merge(
  complete_flow_matrix,
  aldo_non_forest_biomass_flow_co2,
  by.x = c("aldo_biomass_category_from_copy", "aldo_biomass_category_to_copy"),
  by.y = c("aldo_biomass_category_from", "aldo_biomass_category_to"),
  all.x = TRUE
)

complete_flow_matrix[, biomass_carbon_flow := ifelse(is.na(biomass_carbon_flow.x), biomass_carbon_flow.y, biomass_carbon_flow.x)]

complete_flow_matrix <- complete_flow_matrix[,
                                             list(
                                               biomass_carbon_flow = sum(biomass_carbon_flow)
                                             ),
                                             by = list(aldo_biomass_category_from, aldo_biomass_category_to)
]

complete_flow_matrix[is.na(biomass_carbon_flow), biomass_carbon_flow := 0]

aldo_non_forest_biomass_flow_co2 <- copy(complete_flow_matrix)


# Forest biomass carbon flow
aldo_forest_biomass_flow_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/Outil ALDO_2021_12.xlsx", sheet = "Ref_Biom_foret")
aldo_forest_biomass_flow_co2 <- as.data.table(aldo_forest_biomass_flow_co2)
aldo_forest_biomass_flow_co2 <- aldo_forest_biomass_flow_co2[SIREN_EPCI == "245701404"]
aldo_forest_biomass_flow_co2 <- unique(aldo_forest_biomass_flow_co2[, c(3, 8, 12, 11)])

setnames(aldo_forest_biomass_flow_co2, c("aldo_biomass_category", "wood_products_volume_flow", "forest_biomass_carbon_flow", "forest_biomass_volume_flow"))
aldo_forest_biomass_flow_co2[, aldo_biomass_category := tolower(aldo_biomass_category)]
aldo_forest_biomass_flow_co2[, wood_products_carbon_flow := wood_products_volume_flow*forest_biomass_carbon_flow/forest_biomass_volume_flow]

aldo_forest_biomass_flow_co2[, c("wood_products_volume_flow", "forest_biomass_volume_flow") := NULL]

# aldo_non_forest_biomass_flow_co2[aldo_biomass_category_from %in% c("feuillu", "mixte", "conifere", "peupleraies"), biomass_carbon_flow := 0]
#
# aldo_non_forest_biomass_flow_co2 <- merge(
#   aldo_non_forest_biomass_flow_co2,
#   aldo_forest_biomass_flow_co2,
#   by.x = c("aldo_biomass_category_to"),
#   by.y = c("aldo_biomass_category"),
#   all.x = TRUE
# )
#
# aldo_non_forest_biomass_flow_co2[, biomass_carbon_flow := ifelse(is.na(biomass_carbon_flow.y), biomass_carbon_flow.x, biomass_carbon_flow.y)]
# aldo_non_forest_biomass_flow_co2[, c("biomass_carbon_flow.x", "biomass_carbon_flow.y") := NULL]


aldo_biomass_flow_co2 <- copy(aldo_non_forest_biomass_flow_co2)
aldo_biomass_flow_co2[is.na(biomass_carbon_flow), biomass_carbon_flow := 0]


# Litter carbon flow
aldo_litter_flow_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/litter_carbon_flows.xlsx")
aldo_litter_flow_co2 <- as.data.table(aldo_litter_flow_co2)
aldo_litter_flow_co2 <- melt(aldo_litter_flow_co2, variable.name = "aldo_soil_category_to", value.name = "litter_carbon_flow")
aldo_litter_flow_co2[is.na(litter_carbon_flow), litter_carbon_flow := 0]





# Immediate / progressive carbon flow
# Soil
immediate_soil_flow <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/immediate_carbon_emissions.xlsx", sheet = "soil")
immediate_soil_flow <- as.data.table(immediate_soil_flow)
immediate_soil_flow <- melt(immediate_soil_flow, id.vars = "aldo_soil_category_from", variable.name = "aldo_soil_category_to", value.name = "immediate")
immediate_soil_flow[is.na(immediate), immediate := FALSE]

# Non forest biomass
immediate_biomass_flow <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/immediate_carbon_emissions.xlsx", sheet = "biomass")
immediate_biomass_flow <- as.data.table(immediate_biomass_flow)
immediate_biomass_flow <- melt(immediate_biomass_flow, id.vars = "aldo_biomass_category_from", variable.name = "aldo_biomass_category_to", value.name = "immediate")
immediate_biomass_flow[is.na(immediate), immediate := FALSE]

# Add forest biomass
# immmediate_forest_biomass_flow <- aldo_forest_biomass_flow_co2[, list(from, to)]
# setnames(immmediate_forest_biomass_flow, c("aldo_biomass_category_from", "aldo_biomass_category_to"))
# immmediate_forest_biomass_flow[, immediate := FALSE]
# immediate_biomass_flow <- rbindlist(list(immediate_biomass_flow, immmediate_forest_biomass_flow))

# Litter
immediate_litter_flow <- copy(immediate_biomass_flow)
immediate_litter_flow[, immediate := FALSE]
setnames(immediate_litter_flow, c("aldo_soil_category_from", "aldo_soil_category_to", "immediate"))


# Adjust the flows taking into account the progressive / immediate nature of carbon flows
aldo_soil_flow_co2 <- merge(aldo_soil_flow_co2, immediate_soil_flow, by = c("aldo_soil_category_from", "aldo_soil_category_to"), all.x = TRUE)
aldo_soil_flow_co2[, soil_carbon_flow := ifelse(immediate == TRUE, soil_carbon_flow, soil_carbon_flow*20)]
aldo_soil_flow_co2[, immediate := NULL]

aldo_biomass_flow_co2 <- merge(aldo_biomass_flow_co2, immediate_biomass_flow, by = c("aldo_biomass_category_from", "aldo_biomass_category_to"), all.x = TRUE)
# aldo_biomass_flow_co2[aldo_biomass_category_from %in% c("conifere", "feuillu", "mixte") & aldo_biomass_category_to %in% c("conifere", "feuillu", "mixte"), immediate := TRUE]
# aldo_biomass_flow_co2[is.na(immediate), immediate := TRUE]

aldo_biomass_flow_co2[, biomass_carbon_flow := ifelse(immediate == TRUE, biomass_carbon_flow, biomass_carbon_flow*20)]
aldo_biomass_flow_co2[, immediate := NULL]





agricultural_practices <- as.data.table(read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/agricultural_practices.xlsx"))
agricultural_practices <- agricultural_practices[,
                                                 list(additional_soil_carbon_flow = sum(additional_soil_carbon_flow*deployment_share),
                                                      additional_biomass_carbon_flow = sum(additional_biomass_carbon_flow*deployment_share)),
                                                 by = clc_category]



