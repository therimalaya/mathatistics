get_fitted <- function(data, land) {
  dta <- subset(
    data,
    subset = country == land
  )

  dta %>%
    split(by = c("sex", "type")) %>%
    imap(function(.dta, .name) {
      out <- copy(.dta)
      mdl_1 <- glm(
        count ~ year + offset(log(population)),
        family = poisson(link = "log"),
        data = out
      )
      # mdl_2 <- lm(
      #   log(crude_rate) ~ year,
      #   data = out
      # )

      sub_1 <- segmented(mdl_1, npsi = 2)
      if (!("segmented" %in% class(sub_1))) {
        sub_1 <- segmented(mdl_1, npsi = 1)
      }
      # sub_2 <- segmented(mdl_2, npsi = 2)

      if (("segmented" %in% class(sub_1))) {
        apc <- slope(sub_1, APC = TRUE)[[1]] %>%
          as.data.table(keep.rownames = "segment")

        psi <- unname(c(1980, sub_1$psi[, "Est."], 2020))
        psi_range <- first(paste(
          round(psi),
          shift(round(psi), -1),
          sep = "-"
        ), -1)
        apc[, psi := psi_range]
        apc <- cbind(
          .dta[, .(sex, type, cancer, country)] %>% unique(),
          apc
        )
        setnames(apc, 6:8, c("estimate", "lower", "upper"))
        apc[, label := glue::glue_data(
          .SD, "{estimate} ({lower}, {upper})",
          .transformer = function(d, e) round(get(d, e), 2)
        )]

        aapc <- aapc(sub_1) %>%
          t() %>%
          as.data.table() %>%
          setnames(c("estimate", "std_err", "lower", "upper"))

        aapc[, psi := "1980-2020"]
        aapc <- cbind(
          .dta[, .(sex, type, cancer, country)] %>% unique(),
          aapc
        )
        aapc[, label := glue::glue_data(
          .SD, "{estimate} ({lower}, {upper})",
          .transformer = function(d, e) {
              round(get(d, e) * 100, 2)
            }
        )]
      } else {
        apc <- NULL
        aapc <- NULL
      }


      new_data <- CJ(year = 1980:2020, population = 1e5)
      out[, fit1 := predict(
        mdl_1,
        newdata = new_data,
        type = "response"
      )]
      # out[, fit2 := predict(
      #   mdl_2, newdata = new_data,
      # ) %>% exp()]
      out[, sub1 := predict(
        sub_1,
        newdata = new_data,
        type = "response"
      )]
      # out[, sub2 := predict(
      #   sub_2, newdata = new_data
      # ) %>% exp()]
      return(list(
        model = list(glm = mdl_1, segmented = sub_1), 
        df = out, apc = apc, aapc = aapc
      ))
    })
}

get_plot <- function(data, land, aapc = NULL) {
  data <- copy(data)[country == land]
  land <- data[, unique(country)]
  plt <- ggplot(data, aes(year, crude_rate)) +
    geom_point(shape = 21, fill = "whitesmoke") +
    geom_line(aes(
      y = fit1, color = type, linetype = "GLM"
    )) +
    geom_line(aes(
      y = sub1, color = type, linetype = "Segmented"
    )) +
    scale_y_continuous(
      breaks = scales::breaks_width(10),
      limits = c(0, 78)
    ) +
    scale_color_brewer(
      palette = "Set1"
    ) +
    facet_grid(cols = vars(sex)) +
    theme(
      legend.position = "bottom"
    ) +
    labs(
      title = paste0("Country: ", land),
      x = "Year of diagnosis",
      y = "Incidence per 100,000 person-years",
      color = "Measure",
      linetype = "Model"
    )
    if (!is.null(aapc)) {
      aapc <- copy(aapc)[country == land]
      plt <- plt + ggrepel::geom_text_repel(
        data = aapc,
        aes(
          x = 1980, y = 70, color = type,
          label = paste(type, label, sep = ": ")
        ), seed = 123,
        hjust = 0, size = rel(3),
        direction = "y",
        min.segment.length = Inf,
        show.legend = FALSE
      ) + annotate(
        geom = "text", x = 1980, y = Inf,
        vjust = 1.2, hjust = 0,
        label = "Average annual percentage change"
      )
    }
    return(plt)
}
