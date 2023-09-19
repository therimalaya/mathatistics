## ---- Load Packages ----
library(data.table)
library(purrr)

## ---- Read JSON Data ----
data <- readRDS("Data/NordCan-JSON.Rds")

data[, overview := map(data, function(dta) {
  out <- data.table(
    year = map_int(dta, "year"),
    asr_w = map_dbl(dta, "asr"),
    asr_e = map_dbl(dta, "asr_e"),
    asr_n = map_dbl(dta, "asr_n"),
    crude_rate = map_dbl(dta, "crude_rate"),
    count = map_dbl(dta, "total"),
    population = map_dbl(dta, "total_pop"),
    cum_risk = map(dta, "cum_risk") %>% 
      unlist()
  )
  if ("cum_risk" %in% names(out)) {
    out[, cum_risk := as.numeric(cum_risk)]
  }
  return(out)
})]

data[, by_age := map(data, function(dta) {
  year <- map_int(dta, "year")
  count_df <- map_df(dta, "ages") %>% 
    as.data.table() %>%
    cbind(year = year) %>% 
    melt.data.table(
      id.vars = "year",
      variable.name = "age_group",
      value.name = "count"
    )
  pop_df <- map_df(dta, "populations") %>% 
    as.data.table() %>%
    cbind(year = year) %>% 
    melt.data.table(
      id.vars = "year",
      variable.name = "age_group",
      value.name = "population"
    )
  asr_df <- map_df(dta, "age_specific_rate") %>% 
    as.data.table() %>%
    cbind(year = year) %>% 
    melt.data.table(
      id.vars = "year",
      variable.name = "age_group",
      value.name = "asr"
    )
  out <- reduce(
    list(count_df, pop_df, asr_df),
    merge.data.table,
    by = c("year", "age_group")
  )
  age_lbl <- paste(
    seq(0, 85, 5),
    seq(0, 85, 5) + 4,
    sep = "-"
  )
  age_lbl[length(age_lbl)] <- "85+"
  names(age_lbl) <- 1:18
  
  out[, age_group := age_lbl[age_group]]
  out[, year := as.integer(year)]
  out[, count := as.integer(count)]
  out[, population := as.integer(population)]
  out[, asr := as.numeric(asr)]
  return(out[])
})]

adj_rates <- data[, map_df(overview, as.data.table), 
  by = .(sex, type, cancer, country)]

by_age <- data[, map_df(by_age, as.data.table),
  by = .(sex, type, cancer, country)]

saveRDS(adj_rates, file = "Data/Adj-Rates.Rds")
saveRDS(by_age, file = "Data/Rate-ByAge.Rds")