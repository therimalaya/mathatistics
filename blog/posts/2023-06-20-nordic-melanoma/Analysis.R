## ---- Source files ----
source("Functions.R")

## ---- Load packages ----
pkgs <- c(
  "data.table", "purrr", "haven", 
  "ggplot2", "segmented", "gghighlight"
)
for (pkg in pkgs) require(pkg, character.only = TRUE)

## ---- Plot defaults ----
ggplot2::theme_set(
  ggthemes::theme_few() +
  theme(
    panel.grid = element_line(color = "#f0f0f0")
  )
)

## ---- Paths ----
data_path <- "Data"
out_path <- "Results"
plot_path <- "Plots"
table_path <- "Tables"

## ---- Prepare data ----
data <- readRDS(file.path(data_path, "Adj-Rates.Rds"))
data <- data[country != "Iceland"]

## ---- Fit a poisson model for each country ----
countries <- data[, unique(country)]
names(countries) <- countries
mdls <- map(countries, ~get_fitted(data, .x))
mdl_fit <- modify_depth(mdls, 2, "model")
fitted <- modify_depth(mdls, 2, "df") %>% map_df(rbindlist)
apc <- modify_depth(mdls, 2, "apc") %>% map_df(rbindlist)
aapc <- modify_depth(mdls, 2, "aapc") %>% map_df(rbindlist)

## ---- Tidy-model fit ----
tidy_fit <- modify_depth(mdl_fit, 2, "glm") %>% 
  modify_depth(
    .depth = 2, .f = broom::tidy, 
    exponentiate = TRUE, conf.int = TRUE
  ) %>% 
  map_df(rbindlist, idcol = "strata", .id = "country") %>% 
  .[term == "year"] %>% 
  tidyr::separate(
    "strata", into = c("sex", "type"),
    sep = "\\."
  ) %>% as.data.table()
tidy_fit[, country := factor(
   country, 
   level = c("Norway", "Sweden", "Denmark", "Finland")
)]


saveRDS(tidy_fit, file = file.path(out_path, "Tidy-Fit.Rds"))
saveRDS(mdl_fit, file = file.path(out_path, "Fitted-Models.Rds"))
saveRDS(fitted, file = file.path(out_path, "Fitted-DF.Rds"))
saveRDS(list(apc, aapc), file = file.path(out_path, "APC.Rds"))

## ---- Model summary plot ----
plt_model_summary <- ggplot(
  data = tidy_fit, 
  aes(
    x = estimate, 
    y = country, 
    color = sex
  )
) +
  facet_grid(rows = vars(type)) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    shape = 21,
    fill = "whitesmoke",
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey") +
  scale_color_brewer(palette = "Set1") +
  expand_limits(x = 1) +
  ggthemes::theme_few() +
  theme(
    panel.grid = element_line(color = "#f0f0f0"),
    legend.position = "bottom",
    legend.justification = "left"
  ) +
  labs(
    x = "Percentage change in count",
    y = NULL
  )

ggsave(
  plt_model_summary,
  filename = file.path(plot_path, "model-summary-plot.svg"),
  width = 5,
  height = 5,
  device = svglite::svglite
)

## ---- Get plot and save them ----
plots <- map(
  c("Norway", "Denmark", "Sweden", "Finland"),
  function(land) {
    plot <- get_plot(fitted, land, aapc)
    ggsave(
      file.path(plot_path, paste0(land, ".svg")),
      width = 6, height = 5,
      device = svglite::svglite,
      scale = 1.2
    )
    return(plot)
  }
)

## ---- Plot: incidence and mortality ----
fitted_long <- fitted %>%
  melt.data.table(
    id.vars = 1:5,
    measure.vars = c("fit1", "sub1"),
    variable.name = "Model",
    value.name = "fitted"
  )
fitted_long[, Model := fifelse(Model == "fit1", "GLM", "Segmented")]
cols <- RColorBrewer::brewer.pal(fitted[, uniqueN(country)], "Set1") 
names(cols) <- fitted[, unique(country)]

plt <- ggplot(
  fitted, 
  aes(
    x = year, 
    y = crude_rate,
    group = country
  )) +
  facet_grid(
    cols = vars(sex),
    rows = vars(type),
    scales = "free_y"
  ) + 
  scale_color_manual(
    breaks = names(cols),
    values = cols
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing.y = unit(0, "mm")
  ) +
  labs(
    x = "Year of diagnosis",
    y = "Crude rate per 100,000 person-years",
    color = "Country",
    linetype = "Model"
  )
  
hlt_nor_fin_line <- plt + 
  geom_line(
    color = "grey",
    alpha = 0.5
  ) +
  geom_point(
    shape = 21,
    alpha = 0.3,
    fill = "whitesmoke"
  ) +
  geom_line(
    data = fitted[country %in% c("Norway", "Finland")],
    aes(y = crude_rate, color = country)
  )

ggsave(
  hlt_nor_fin_line,
  filename = file.path(plot_path, "Line-Nor-Fin.svg"),
  device = svglite::svglite,
  width = 6,
  height = 5,
  scale = 1.2
)

hlt_nor_fin_glm <- plt + 
  geom_line(
    color = "grey",
    alpha = 0.4
  ) +
  geom_point(
    shape = 21,
    alpha = 0.1,
    fill = "whitesmoke"
  ) +
  geom_point(
    data = fitted[country %in% c("Norway", "Finland")],
    aes(color = country),
    shape = 21,
    alpha = 0.4,
    fill = "whitesmoke"
  ) +
  geom_line(
    data = fitted_long[
      country %in% c("Norway", "Finland") &
      Model == "GLM"
    ],
    aes(y = fitted, color = country),
    linewidth = 0.75
  )

ggsave(
  hlt_nor_fin_glm,
  filename = file.path(plot_path, "GLM-Nor-Fin.svg"),
  device = svglite::svglite,
  width = 6,
  height = 5,
  scale = 1.2
)

hlt_nor_fin_sgmt <- plt + 
  geom_line(
    color = "grey",
    alpha = 0.4
  ) +
  geom_point(
    shape = 21,
    alpha = 0.1,
    fill = "whitesmoke"
  ) +
  geom_point(
    data = fitted[country %in% c("Norway", "Finland")],
    aes(color = country),
    shape = 21,
    alpha = 0.4,
    fill = "whitesmoke"
  ) +
  geom_line(
    data = fitted_long[
      country %in% c("Norway", "Finland") &
      Model == "GLM"
    ],
    alpha = 0.2,
    aes(y = fitted, color = country),
    linewidth = 0.75
  ) +
  geom_line(
    data = fitted_long[
      country %in% c("Norway", "Finland") &
      Model == "Segmented"
    ],
    aes(y = fitted, color = country),
    linetype = 6,
    linewidth = 0.75
  )

ggsave(
  hlt_nor_fin_sgmt,
  filename = file.path(plot_path, "Sgmt-Nor-Fin.svg"),
  device = svglite::svglite,
  width = 6,
  height = 5,
  scale = 1.2
)

hlt_all_glm <- plt + 
  geom_line(
    color = "grey",
    alpha = 0.4
  ) +
  geom_point(
    data = fitted,
    aes(color = country),
    shape = 21,
    alpha = 0.4,
    fill = "whitesmoke"
  ) +
  geom_line(
    data = fitted_long[
      Model == "GLM"
    ],
    aes(y = fitted, color = country),
    linewidth = 0.75
  )

ggsave(
  hlt_all_glm,
  filename = file.path(plot_path, "GLM-all.svg"),
  device = svglite::svglite,
  width = 6,
  height = 5,
  scale = 1.2
)

hlt_all_sgmt <- plt + 
  geom_line(
    color = "grey",
    alpha = 0.4
  ) +
  geom_point(
    data = fitted,
    aes(color = country),
    shape = 21,
    alpha = 0.4,
    fill = "whitesmoke"
  ) +
  geom_line(
    data = fitted_long[
      Model == "Segmented"
    ],
    aes(y = fitted, color = country),
    linewidth = 0.75,
    linetype = 6
  )

ggsave(
  hlt_all_sgmt,
  filename = file.path(plot_path, "Sgmt-all.svg"),
  device = svglite::svglite,
  width = 6,
  height = 5,
  scale = 1.2
)


