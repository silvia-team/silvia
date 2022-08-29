library(here)
library(data.table)
library(readxl)
library(sf)
library(ggplot2)

functional_area <- st_read(here("data", "arep", "functional_area.gpkg"))

clc <- fread(here("data", "arep", "buildings", "clc_2000_2018.csv"))
clc <- melt(clc, id.vars = c("region", "code", "name", "area"), variable.name = "year", value.name = "clc_category", encoding = "UTF-8")
clc[, year := ifelse(year == "code_00", 2000, 2020)]
clc[, clc_category := as.character(clc_category)]

aldo_clc <- as.data.table(read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/aldo_clc.xlsx"))

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
aldo_soil_flow_co2 <- read_excel("C:/Users/pouchaif/Documents/docs/Carbone/Aldo/Outil ALDO_2021_12.xlsx", sheet = "Ref_Sols")
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

aldo_forest_biomass_flow_co2 <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
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




# --------------------
# 2000 - 2020

clc <- fread(here("data", "arep", "buildings", "clc_2000_2018.csv"), encoding = "UTF-8")
clc[, code_90 := as.character(code_00)]
clc[, code_18 := as.character(code_18)]

clc[, year := 2020]
clc[, scenario := "HIST"]
clc[, clc_proj := code_18]

clc_co2 <- merge(
  clc,
  aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)],
  by.x = "code_90",
  by.y = "clc_category",
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)],
  by.x = "code_18",
  by.y = "clc_category",
  all.x = TRUE,
  suffixes = c("_from", "_to")
)

clc_co2 <- merge(
  clc_co2,
  aldo_soil_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_litter_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_biomass_flow_co2,
  by = c("aldo_biomass_category_from", "aldo_biomass_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_forest_biomass_flow_co2,
  by.x = "aldo_biomass_category_to",
  by.y = "aldo_biomass_category",
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  agricultural_practices[, list(clc_category, additional_soil_carbon_flow, additional_biomass_carbon_flow)],
  by.x = "code_18",
  by.y = "clc_category",
  all.x = TRUE
)

clc_co2[, n2o_co2e_flow := ifelse(soil_carbon_flow + litter_carbon_flow < 0.0, (soil_carbon_flow + litter_carbon_flow)/15*0.01*44/25+(soil_carbon_flow + litter_carbon_flow)/15*0.3*0.0075*44/28, 0.0)]
clc_co2[, n2o_co2e_flow := n2o_co2e_flow*298]

clc_co2[, forest_biomass_carbon_flow := ifelse(is.na(forest_biomass_carbon_flow), 0, forest_biomass_carbon_flow)]
clc_co2[, wood_products_carbon_flow := ifelse(is.na(wood_products_carbon_flow), 0, wood_products_carbon_flow)]
clc_co2[is.na(biomass_carbon_flow), biomass_carbon_flow := 0]

clc_co2[, additional_soil_carbon_flow := 0*ifelse(is.na(additional_soil_carbon_flow), 0, additional_soil_carbon_flow)]
clc_co2[, additional_biomass_carbon_flow := 0*ifelse(is.na(additional_biomass_carbon_flow), 0, additional_biomass_carbon_flow)]

clc_co2[, area_change := area]
clc_co2[code_90 != code_18, area_change := area/18]

# Flows to date
clc_co2[, wood_products_co2_flow := wood_products_carbon_flow*area/1e4*44/12]
clc_co2[, wood_products_co2_carbon_flow := -wood_products_carbon_flow]

# Additional carbon in 2030 with a 0 - 100% linear deployment between 2020 and 2050 and a 20 year period before saturation
clc_co2[, additional_soil_co2_flow := 0]
clc_co2[, additional_biomass_co2_flow := 0]

clc_co2[year == 2030, additional_soil_co2_flow := 1/3*20*additional_soil_carbon_flow*area/1e4*44/12]
clc_co2[year == 2030, additional_biomass_co2_flow := 1/3*20*additional_biomass_carbon_flow*area/1e4*44/12]

clc_co2[year == 2050, additional_soil_co2_flow := 1/3*20*additional_soil_carbon_flow*area/1e4*44/12]
clc_co2[year == 2050, additional_biomass_co2_flow := 1/3*20*additional_biomass_carbon_flow*area/1e4*44/12]

clc_co2[, forest_biomass_co2_flow := forest_biomass_carbon_flow*area/1e4*44/12]

# LUC flows
clc_co2[, biomass_co2_flow := biomass_carbon_flow*area_change/1e4*44/12]
clc_co2[, litter_co2_flow := litter_carbon_flow*area_change/1e4*44/12]
clc_co2[, soil_co2_flow := soil_carbon_flow*area_change/1e4*44/12]

clc_co2[, total_co2_flow := soil_co2_flow + litter_co2_flow + biomass_co2_flow + forest_biomass_co2_flow + n2o_co2e_flow + 1.0*(additional_soil_co2_flow + additional_biomass_co2_flow)]

clc_co2[, sum(total_co2_flow), by = list(year, aldo_soil_category_to)]
clc_co2[, sum(total_co2_flow), by = list(region, year)]
clc_co2[, sum(total_co2_flow)/(st_area(functional_area)/1e4), by = year]


clc_co2_00_20 <- copy(clc_co2)








# -------------------------------------------------------
# BAU
clc <- fread(here("data", "arep", "buildings", "clc_proj.csv"), encoding = "UTF-8")
clc <- clc[scenario == "BAU"]

clc[, code_90 := as.character(code_18)]
clc[, code_18 := as.character(clc_proj)]

clc_co2 <- merge(
  clc,
  aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)],
  by.x = "code_90",
  by.y = "clc_category",
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)],
  by.x = "code_18",
  by.y = "clc_category",
  all.x = TRUE,
  suffixes = c("_from", "_to")
)

clc_co2 <- merge(
  clc_co2,
  aldo_soil_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_litter_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_biomass_flow_co2,
  by = c("aldo_biomass_category_from", "aldo_biomass_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_forest_biomass_flow_co2,
  by.x = "aldo_biomass_category_to",
  by.y = "aldo_biomass_category",
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  agricultural_practices[, list(clc_category, additional_soil_carbon_flow, additional_biomass_carbon_flow)],
  by.x = "code_18",
  by.y = "clc_category",
  all.x = TRUE
)

clc_co2[, n2o_co2e_flow := ifelse(soil_carbon_flow + litter_carbon_flow < 0.0, (soil_carbon_flow + litter_carbon_flow)/15*0.01*44/25+(soil_carbon_flow + litter_carbon_flow)/15*0.3*0.0075*44/28, 0.0)]
clc_co2[, n2o_co2e_flow := n2o_co2e_flow*298]

clc_co2[, forest_biomass_carbon_flow := ifelse(is.na(forest_biomass_carbon_flow), 0, forest_biomass_carbon_flow)]
clc_co2[, wood_products_carbon_flow := ifelse(is.na(wood_products_carbon_flow), 0, wood_products_carbon_flow)]
clc_co2[is.na(biomass_carbon_flow), biomass_carbon_flow := 0]

clc_co2[, additional_soil_carbon_flow := 0*ifelse(is.na(additional_soil_carbon_flow), 0, additional_soil_carbon_flow)]
clc_co2[, additional_biomass_carbon_flow := 0*ifelse(is.na(additional_biomass_carbon_flow), 0, additional_biomass_carbon_flow)]

clc_co2[, area_change := area]
clc_co2[code_90 != code_18, area_change := area/ifelse(year == 2030, 10, 30)]

# Flows to date
clc_co2[, wood_products_co2_flow := wood_products_carbon_flow*area/1e4*44/12]
clc_co2[, wood_products_co2_carbon_flow := -wood_products_carbon_flow]

# Additional carbon in 2030 with a 0 - 100% linear deployment between 2020 and 2050 and a 20 year period before saturation
clc_co2[year == 2030, additional_soil_co2_flow := 1/3*20*additional_soil_carbon_flow*area/1e4*44/12]
clc_co2[year == 2030, additional_biomass_co2_flow := 1/3*20*additional_biomass_carbon_flow*area/1e4*44/12]

clc_co2[year == 2050, additional_soil_co2_flow := 1/3*20*additional_soil_carbon_flow*area/1e4*44/12]
clc_co2[year == 2050, additional_biomass_co2_flow := 1/3*20*additional_biomass_carbon_flow*area/1e4*44/12]

clc_co2[, forest_biomass_co2_flow := forest_biomass_carbon_flow*area/1e4*44/12]

# LUC flows
clc_co2[, biomass_co2_flow := biomass_carbon_flow*area_change/1e4*44/12]
clc_co2[, litter_co2_flow := litter_carbon_flow*area_change/1e4*44/12]
clc_co2[, soil_co2_flow := soil_carbon_flow*area_change/1e4*44/12]

clc_co2[, total_co2_flow := soil_co2_flow + litter_co2_flow + biomass_co2_flow + forest_biomass_co2_flow + n2o_co2e_flow + 1.0*(additional_soil_co2_flow + additional_biomass_co2_flow)]

clc_co2[, sum(total_co2_flow), by = list(year, aldo_soil_category_to)]
clc_co2[, sum(total_co2_flow), by = list(region, year)]
clc_co2[, sum(total_co2_flow)/(st_area(functional_area)/1e4), by = year]

clc_co2_proj_bau <- copy(clc_co2)





# -------------------------------------------------------
# LIT
clc <- fread(here("data", "arep", "buildings", "clc_proj.csv"), encoding = "UTF-8")
clc <- clc[scenario == "LIT"]

clc[, code_90 := as.character(code_18)]
clc[, code_18 := as.character(clc_proj)]

clc_co2 <- merge(
  clc,
  aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)],
  by.x = "code_90",
  by.y = "clc_category",
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_clc[, list(aldo_soil_category,	aldo_biomass_category,	clc_category)],
  by.x = "code_18",
  by.y = "clc_category",
  all.x = TRUE,
  suffixes = c("_from", "_to")
)

clc_co2 <- merge(
  clc_co2,
  aldo_soil_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_litter_flow_co2,
  by = c("aldo_soil_category_from", "aldo_soil_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_biomass_flow_co2,
  by = c("aldo_biomass_category_from", "aldo_biomass_category_to"),
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  aldo_forest_biomass_flow_co2,
  by.x = "aldo_biomass_category_to",
  by.y = "aldo_biomass_category",
  all.x = TRUE
)

clc_co2 <- merge(
  clc_co2,
  agricultural_practices[, list(clc_category, additional_soil_carbon_flow, additional_biomass_carbon_flow)],
  by.x = "code_18",
  by.y = "clc_category",
  all.x = TRUE
)

clc_co2[, n2o_co2e_flow := ifelse(soil_carbon_flow + litter_carbon_flow < 0.0, (soil_carbon_flow + litter_carbon_flow)/15*0.01*44/25+(soil_carbon_flow + litter_carbon_flow)/15*0.3*0.0075*44/28, 0.0)]
clc_co2[, n2o_co2e_flow := n2o_co2e_flow*298]

clc_co2[, forest_biomass_carbon_flow := ifelse(is.na(forest_biomass_carbon_flow), 0, forest_biomass_carbon_flow)]
clc_co2[, wood_products_carbon_flow := ifelse(is.na(wood_products_carbon_flow), 0, wood_products_carbon_flow)]
clc_co2[is.na(biomass_carbon_flow), biomass_carbon_flow := 0]

clc_co2[, additional_soil_carbon_flow := ifelse(is.na(additional_soil_carbon_flow), 0, additional_soil_carbon_flow)]
clc_co2[, additional_biomass_carbon_flow := ifelse(is.na(additional_biomass_carbon_flow), 0, additional_biomass_carbon_flow)]

clc_co2[, area_change := area]
clc_co2[code_90 != code_18, area_change := area/ifelse(year == 2030, 10, 30)]

# Flows to date
clc_co2[, wood_products_co2_flow := wood_products_carbon_flow*area/1e4*44/12]
clc_co2[, wood_products_co2_carbon_flow := -wood_products_carbon_flow]

# Additional carbon in 2030 with a 0 - 100% linear deployment between 2020 and 2050 and a 20 year period before saturation
clc_co2[year == 2030, additional_soil_co2_flow := 1/3*20*additional_soil_carbon_flow*area/1e4*44/12]
clc_co2[year == 2030, additional_biomass_co2_flow := 1/3*20*additional_biomass_carbon_flow*area/1e4*44/12]

clc_co2[year == 2050, additional_soil_co2_flow := 2/3*20*additional_soil_carbon_flow*area/1e4*44/12]
clc_co2[year == 2050, additional_biomass_co2_flow := 2/3*20*additional_biomass_carbon_flow*area/1e4*44/12]

clc_co2[, forest_biomass_co2_flow := forest_biomass_carbon_flow*area/1e4*44/12]

# LUC flows
clc_co2[, biomass_co2_flow := biomass_carbon_flow*area_change/1e4*44/12]
clc_co2[, litter_co2_flow := litter_carbon_flow*area_change/1e4*44/12]
clc_co2[, soil_co2_flow := soil_carbon_flow*area_change/1e4*44/12]

clc_co2[, total_co2_flow := soil_co2_flow + litter_co2_flow + biomass_co2_flow + forest_biomass_co2_flow + n2o_co2e_flow + 1.0*(additional_soil_co2_flow + additional_biomass_co2_flow)]


clc_co2[, sum(total_co2_flow), by = list(year, aldo_soil_category_to)][order(year)]
clc_co2[, sum(total_co2_flow), by = list(region, year)]
clc_co2[, sum(total_co2_flow)/(st_area(functional_area)/1e4), by = year]


clc_co2_proj_lit <- copy(clc_co2)


# --------------------------
# Concatenate all results
clc_co2 <- rbindlist(list(clc_co2_00_20, clc_co2_proj_bau, clc_co2_proj_lit), use.names = TRUE)

trajectories_by_city <- clc_co2[,
                        list(
                          sequestrations= sum(total_co2_flow[total_co2_flow > 0]),
                          emissions = sum(-total_co2_flow[total_co2_flow < 0])
                        ),
                        by = list(region, code, scenario, year)
]

fwrite(trajectories_by_city, here("data", "arep", "buildings", "land_use_biomass_emissions_trajectories.csv"), sep = ";")



trajectories <- clc_co2[,
  list(
    sequestrations= sum(total_co2_flow[total_co2_flow > 0]) + sum(wood_products_co2_flow),
    emissions = sum(-total_co2_flow[total_co2_flow < 0]) + sum(wood_products_co2_flow)
  ),
  by = list(scenario, year)
]

p <- ggplot(trajectories)
p <- p + geom_point(aes(x = year, y = emissions, col = scenario))
p <- p + geom_point(aes(x = year, y = -sequestrations, col = scenario))
p


trajectories <- clc_co2[,
                        list(
                          sequestrations= sum(total_co2_flow[total_co2_flow > 0]) + sum(wood_products_co2_flow),
                          emissions = sum(-total_co2_flow[total_co2_flow < 0]) + sum(wood_products_co2_flow)
                        ),
                        by = list(region, scenario, year)
]

p <- ggplot(trajectories[region == "LUX"])
p <- p + geom_point(aes(x = year, y = emissions, col = scenario))
p <- p + geom_point(aes(x = year, y = -sequestrations, col = scenario))
p



# Population
population <- fread(here("data", "arep", "population", "population_count_cities.csv"))
population[, code := as.character(code)]
population <- population[, list(population = sum(population)), by = list(region, year)]

trajectories <- merge(trajectories, population, c("region", "year"))
trajectories[, emissions_per_person := emissions/population]
trajectories[, sequestrations_per_person := sequestrations/population]

trajectories <- trajectories[order(region, year)]
fwrite(trajectories, here("data", "arep", "buildings", "trajectories.csv"), sep = ";")




# Only forests
trajectories <- clc_co2[,
  list(
    sequestrations= sum(total_co2_flow[total_co2_flow > 0]),
    emissions = sum(-total_co2_flow[total_co2_flow < 0])
  ),
  by = list(region, scenario, aldo_soil_category_to, year)
]

trajectories <- merge(trajectories, population, c("region", "year"))
trajectories[, emissions_per_person := emissions/population]
trajectories[, sequestrations_per_person := sequestrations/population]

trajectories <- trajectories[order(region, year)]
fwrite(trajectories, here("data", "arep", "buildings", "trajectories_by_land_use.csv"), sep = ";")



trajectories_by_land_use <- clc_co2[,
                                list(
                                  sequestrations= sum(total_co2_flow[total_co2_flow > 0]),
                                  emissions = sum(-total_co2_flow[total_co2_flow < 0])
                                ),
                                by = list(aldo_soil_category_to, scenario, year)
]

trajectories_by_land_use <- trajectories_by_land_use[emissions > 0 | sequestrations > 0]




trajectories <- clc_co2[,
  list(
    sequestrations= sum(total_co2_flow[total_co2_flow > 0]),
    emissions = sum(-total_co2_flow[total_co2_flow < 0])
  ),
  by = list(scenario, year)
]

population <- population[, list(population = sum(population)), by = list(year)]

trajectories <- merge(trajectories, population, c("year"))
trajectories[, emissions_per_person := emissions/population]
trajectories[, sequestrations_per_person := sequestrations/population]



trajectories <- clc_co2[,
        list(
          sequestrations= sum(total_co2_flow[total_co2_flow > 0]),
          emissions = sum(-total_co2_flow[total_co2_flow < 0])
        ),
        by = list(scenario, aldo_soil_category_to, year)
]

trajectories <- merge(trajectories, population, c("year"))
trajectories[, emissions_per_person := emissions/population]
trajectories[, sequestrations_per_person := sequestrations/population]

trajectories[year == 2050]
