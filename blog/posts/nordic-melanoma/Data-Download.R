## ---- Packages ----
library(jsonlite)
library(data.table)
library(purrr)

## ---- Setup URL ----
design_map <- list(
  sex = c(Male = 1, Female = 2),
  type = c(Incidence = 0, Mortality = 1),
  cancer = c(Melanoma = 290),
  country = c(
    Denmark = 208, Finland = 246, Iceland = 352,
    Norway = 578, Sweden = 752
  )
)
label_values <- function(data, label_map, var) {
  label_vec <- label_map[[var]]
  label <- `names<-`(names(label_vec), label_vec)
  data[[var]] <- data[, label[as.character(get(var))]]
  return(data)
}

design <- CJ(
  sex = 1:2,
  type = 0:1,
  cancer = 290,
  country = c(208, 246, 352, 578, 752)
)
design[, url := glue::glue_data(.SD, 
  "https://gco.iarc.fr/gateway_prod/api/nordcan/v2/92/data/population/{type}/{sex}/({country})/({cancer})/?ages_group=5_17&year_start=1980&year_end=2020&year_grouped=0"
)]

design <- design %>% 
  label_values(design_map, "sex") %>% 
  label_values(design_map, "type") %>% 
  label_values(design_map, "cancer") %>% 
  label_values(design_map, "country")

design[, data := map(url, ~read_json(.x) %>% pluck("dataset"))]

saveRDS(design, file = "Data/NordCan-JSON.Rds")

# https://nordcan.iarc.fr/en/dataviz/tables?cancers=290&sexes=1&populations=208_234_246_352_578_752&age_start=5&years_available=1943_2020&group_cancers=0&multiple_cancers=0&group_populations=0&mode=population&types=0&years=1980_2020&group_years=1 

# https://gco.iarc.fr/gateway_prod/api/nordcan/v2/92//data/population/1<type>/1<sex>/(752<country>)/(290<cancer_type>)/?ages_group=5_17&year_start=1980&year_end=2020&year_grouped=0

# <type>: inc: 1, mort: 0
# <sex>: Male: 1, Female: 2
# <cancer_type>: Melanoma: 290
# <country>: 
#   Denmark: 208, Finland: 246, Iceland: 352, Norway: 578, Sweden: 752