#' @title Graficar la distribución de casos por grupos de edad
#' @export
plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  plot <- ggplot2::ggplot(report_data,
                          ggplot2::aes_string(x =
                                                factor(report_data[[var_x]],
                                                       levels =
                                                      category_labels),
                                              y = var_y,
                                              fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
      }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 11),
                   axis.title = ggplot2::element_text(face = "bold"),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  return(plot)
}

#' @title Graficar la distribución de casos de la vigilancia
#' @export
plot_distribution_surveillance <- function(report_data,
                                           var_x = "casos",
                                           var_y = "porcentaje",
                                           var_fill = "etiqueta") {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(9)
  plot <- ggplot2::ggplot(report_data, ggplot2::aes_string(x = var_x,
                                                           y = var_y,
                                                           fill = var_fill)) + 
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                labels = scales::percent_format()) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  return(plot)
}

#' @title Graficar la distribución de casos por semana epidemiológica
#' @export
plot_distribution_epiweek <- function(report_data,
                                      var_x = "semanaepidemiologicavegeneral",
                                      var_y = "casos",
                                      var_fill = "etiqueta",
                                      influenza = FALSE,
                                      positives = NULL) {
  if (!influenza) {
    colors <- c("Adenovirus" = "#AC6DAD",
                "Rinovirus" = "#FCB814",
                "Bocavirus" = "#D49392",
                "Parainfluenza" = "#64439B",
                "Influenza b" = "#B94846",
                "H3N2" = "#19AFE5",
                "Metapneumovirus" = "#87C762",
                "VSR" = "#2274BB",
                "H1N1" = "#F4802D",
                "H1N1 2009" = "#9DB2D0")
  } else {
    colors <- c("H3N2" = "#145765",
                "H1N1 2009" = "#F4802D",
                "A no subtipificado" = "#EEEA3D",
                "H1N1" = "#19AFE5",
                "Influenza B" = "#B94846")
  }
  plot_epiweek <- ggplot2::ggplot(report_data) +
    ggplot2::geom_col(ggplot2::aes_string(x = var_x,
                                          y = var_y,
                                          fill = var_fill), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("\nSemana epidemiologica\n") +
    ggplot2::ylab("Numero de casos\n") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, 2)) +
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  if (!is.null(positives)) {
    plot_epiweek <- plot_epiweek +
      ggplot2::geom_line(data = positives,
                         ggplot2::aes_string(x = var_x,
                                             y = "porcentaje"),
                         stat = "identity",
                         linetype = "dashed",
                         color = "black",
                         size = 0.8,
                         group = 1) +
      ggplot2::scale_y_continuous(sec.axis =
                                    ggplot2::sec_axis(~. * 0.0055,
                                                      labels =
                                                    scales::percent_format(
                                                        ),
                                                      breaks = seq(0,
                                                                   1,
                                                                   0.1)),
                                  limits = c(0, 100)) +
      ggplot2::theme(text = ggplot2::element_text(size = 14,
                                                  family = "Montserrat"))
  }
  return(plot_epiweek)
}

#' @title Graficar la distribución acumulada de los casos
#' @export
plot_cumulative_proportion <- function(data_proportion) {
  cases <- data_proportion$casos
  labels <- paste0(data_proportion$porcentaje, "%")
  colors <- c("Adenovirus" = "#AC6DAD",
              "Rinovirus" = "#FCB814",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza b" = "#B94846",
              "H3N1" = "#145765",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#F4802D",
              "H1N1 2009" = "#9DB2D0")
  if (any(cases == 0)) {
    index <- which(cases == 0)
    cases <- cases[-index]
    labels <- labels[-index]
  }
  par(family = "Montserrat")
  plotrix::pie3D(cases,
                 mar = rep(1, 4),
                 col = colors,
                 labels = labels,
                 explode = 0.2,
                 border = "white",
                 labelcex = 1.6,
                 radius = 0.6,
                 start = 0.8)
  legend("bottom",
         legend = data_proportion$etiqueta,

         fill = colors, 

         xpd = TRUE,
         ncol = 3)
}

#' @title Graficar la distribución de casos de Tosferina
#' @export
plot_results_tosferina <- function(report_data,
                                   column = "semana_epidemiologica",
                                   label_x = "Semana epidemiologica",
                                   positives = NULL,
                                   show_values = FALSE) {
  colors <- c("Positivo para Bordetella" = "#F4802D",
              "Negativo para Bordetella" = "#145765")
  plot <- ggplot2::ggplot()
  if (column == "grupo_edad") {
    config_path <- system.file("extdata", "config.yml", package = "labrep")
    category_labels <-
      config::get(file = config_path,
                  "tosferina_data")$age_groups$labels
    plot <- plot +
      ggplot2::geom_bar(data = report_data,
                        ggplot2::aes(x = factor(grupo_edad,
                                                levels =
                                                  category_labels),
                                     y = casos,
                                     fill = interpretacion_del_resultado),
                        alpha = 0.9,
                        stat = "identity",
                        width = 0.5,
                        position = ggplot2::position_dodge())
  } else {
    plot <- plot +
      ggplot2::geom_bar(data = report_data,
                        ggplot2::aes_string(x = column,
                                            y = "casos",
                                            fill =
                                              "interpretacion_del_resultado"),
                        alpha = 0.9,
                        stat = "identity",
                        width = 0.5,
                        position = ggplot2::position_dodge())
  }
  max_val_pos <- max(positives[["porcentaje"]])
  max_val_report <- max(report_data[["casos"]])
  if (!is.null(positives)) {
    if (column == "grupo_edad") {
      plot <- plot +
        ggplot2::geom_line(data = positives,
                           ggplot2::aes(x = factor(grupo_edad,
                                                   levels = category_labels),
                                        y = (porcentaje * max_val_report)
                                            / max_val_pos),
                           stat = "identity",
                           color = "#F99D00",
                           size = 0.8,
                           group = 1)
    } else {
      plot <- plot +
        ggplot2::geom_line(data = positives,
                           ggplot2::aes_string(x = column,
                                               y = "(porcentaje * max_val_pos)
                                               / max_val_report"),
                           stat = "identity",
                           color = "#F99D00",
                           size = 0.8,
                           group = 1)
    }
    plot <- plot +
      ggplot2::scale_y_continuous(name = "Numero de muestras analizadas\n",
                                  sec.axis =
                                    ggplot2::sec_axis(trans = ~ . * max_val_report /
                                                        max_val_pos,
                                                      name = "Porcentaje"))
  }
  plot <- plot +
    ggplot2::theme_classic() +
    ggplot2::xlab(label_x) +
    ggplot2::ylab("Numero de muestras analizadas\n") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Interpretación del resultado")
  return(plot)
}

#' @title Graficar la tabla con la distribución de casos por semana
#' epidemiológica
#' @export
plot_table_vrs_epiweek <- function(data_epiweek,
                                   col_epiweek = "Semana",
                                   epiweek) {
  data_table <- data_epiweek %>%
    dplyr::filter(!!dplyr::sym(col_epiweek) <= epiweek)
  table_epiweek <-
    knitr::kable(data_table,
                 col.names = c("Semana Epidemiologica", "% Positivos"),
                 align = "c",
                 longtable = TRUE,
                 caption = "Positividad de virus respiratorios por semana 
                 epidemiológica, Bogotá 2024 \n ") %>%
    kableExtra::row_spec(0, bold = TRUE,
                         color = "white", background = "#145765") %>%
    kableExtra::row_spec(seq(2, nrow(data_table), by = 2),
                         background = "#D4EFFB") %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(2, border_right = TRUE) %>%
    kableExtra::kable_styling()
  return(table_epiweek)
}

#' @title Graficar la tabla con la distribución de casos de Tosferna por semana
#' epidemiológica
#' @export
plot_table_epiweek_tosferina <- function(data_epiweek,
                                         distribution_epiweek,
                                         col_epiweek = "SE",
                                         epiweek) {
  data_table <- data_epiweek %>%
    dplyr::filter(!!dplyr::sym(col_epiweek) <= epiweek)
  table_epiweek <-
    knitr::kable(data_table,
                 longtable = TRUE,
                 col.names = c("SE", "% Positivos"),
                 align = "c",
                 caption = "Positividad de tosferina por semana epidemiológica,
                 Bogotá 2024") %>%
    kableExtra::row_spec(0, bold = TRUE,
                         color = "white", background = "#145765") %>%
    kableExtra::row_spec(seq(2, nrow(data_table), by = 2),
                         background = "#D4EFFB") %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(2, border_right = TRUE) %>%
    kableExtra::kable_styling()
  return(table_epiweek)
}



#' @title Generar gráfico de evolución epidemiológica histórica
#'
#' @description
#' Crea un gráfico combinado con barras apiladas y una línea de tendencia, 
#' representando la evolución de virus respiratorios y la positividad epidemiológica 
#' en función del tiempo.
#'
#' @param dataset_epiTime Dataset con datos epidemiológicos, que debe incluir las columnas 
#' `ano`, `periodo_epidemiologico`, y los distintos tipos de virus.
#' @param periodo_epi Período epidemiológico a destacar en el gráfico (valor entre 1 y 13).
#'
#' @return Un objeto `ggplot2` con el gráfico de evolución epidemiológica.
#' @export
plot_historic_epi_time <- function(dataset_epiTime, periodo_epi ) {
  
  # Ensure the epidemiological period is within valid range
  periodo_epi <- pmax(1, pmin(periodo_epi, 13))
  
  #get stacked bars and line datasets
  historic_epi_times <- get_historic_epi_times(dataset_epi_times = dataset_epiTime)
  stacked_data <- historic_epi_times$stacked_data
  line_data <- historic_epi_times$line_data
  
  #get texts of the axis from config.yml
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  config_path <- "C:/Users/willi/Documents/feat-storage-data/inst/extdata/config.yml"
  text_axis_labels <-  config::get(file = config_path,"respiratory_viruses_historic_data")$legends
  y_axis1_name <- text_axis_labels$y_1_axis_name
  x_axis_name <- text_axis_labels$x_axis_name
  
  #get plot theme parameters
  colores <- get_color_periodo_epidemiologico()
  plot_parameters <- get_axis_config_periodo_epidemiologico()
  plot_text_years_labels <- get_text_labels_periodo_epidemiologico(dataset_epiTime=dataset_epiTime)
  annotate_x_pos <- 0.6727 * periodo_epi + 17.8273

  # Generate the plot
  ggplot2::ggplot() +
    # Stacked bar chart
    ggplot2::geom_bar(
      data = stacked_data,
      ggplot2::aes(x = YearWeek, y = Cases, fill = Virus_Type),
      stat = "identity",
      width = plot_parameters$bar_width
    ) +
    # Line chart for positivity rate
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        x = YearWeek,
        y = Percent_Positivity * plot_parameters$scaling_factor,
        group = 1
      ),
      color = colores$color_linea,
      linewidth = plot_parameters$line_width
    ) +
    # Y-axis and secondary axis
    ggplot2::scale_y_continuous(
      name = y_axis1_name,
      limits = c(-500, plot_parameters$y_axis1_max_value),
      breaks = seq(0, plot_parameters$y_axis1_max_value, by = 100),
      sec.axis = ggplot2::sec_axis(~ . / plot_parameters$scaling_factor,
                                   breaks = seq(0, plot_parameters$y_axis2_max_value, by = 10),
                                   labels = scales::number_format(accuracy = 0.1))
    ) +
    # X-axis
    ggplot2::scale_x_discrete(labels = dataset_epiTime$periodo_epidemiologico) +
    # Custom fill colors
    ggplot2::scale_fill_manual(values = c(
      "a_h1n1_pdm09" = colores$color_a_h1n1_pdm09,
      "a_no_subtipificado" = colores$color_a_no_subtipificado,
      "a_h3" = colores$color_a_h3,
      "influenza_b" = colores$color_influenza_b,
      "parainfluenza" = colores$color_parainfluenza,
      "vsr" = colores$color_vsr,
      "adenovirus" = colores$color_adenovirus,
      "metapneumovirus" = colores$color_metapneumovirus,
      "rinovirus" = colores$color_rinovirus,
      "bocavirus" = colores$color_bocavirus,
      "otros_virus" = colores$color_otros_virus
    ),
    labels = c(
      "a_h1n1_pdm09" = "H1N1 2009",
      "a_no_subtipificado" = "A no subtipificado",
      "a_h3" = "H3N2",
      "influenza_b" = "Influenza B",
      "parainfluenza" = "Parainfluenza",
      "vsr" = "VSR",
      "adenovirus" = "Adenovirus",
      "metapneumovirus" = "Metapneumovirus",
      "rinovirus" = "Rinovirus",
      "bocavirus" = "Bocavirus",
      "otros_virus" = "Otros Virus"
    )) +
    ggplot2::labs(x = x_axis_name, fill = NULL, color = NULL) +
    # Themes and styling
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 7, margin = ggplot2::margin(t = -255, b = -5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, b = -10), size = 8, face = "bold", color = colores$color_axis_titles),
      axis.title.y = ggplot2::element_text(hjust = 0.7,margin = ggplot2::margin(r = 10), 
                                           size = 8, face = "bold", color = colores$color_axis_titles),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.height = ggplot2::unit(0.02, "lines"),
      legend.text = ggplot2::element_text(size = 7)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 3,
        byrow = TRUE
      ),
      color = "none"
    )+
    # Vertical lines and annotations
    ggplot2::geom_segment(ggplot2::aes(x = 13.5, xend = 13.5, y = -25, yend = 700), color = colores$color_vertical_lines, linewidth = 0.65) +
    ggplot2::geom_segment(ggplot2::aes(x = 26.5, xend = 26.5, y = -25, yend = 700), color = colores$color_vertical_lines, linewidth = 0.65) +
    ggplot2::annotate("text", x = c(7, 20, 26.5 + floor(periodo_epi / 2)), y = -35, label = plot_text_years_labels, size = 2.4, fontface = "bold") +
    ggplot2::annotate("segment", x = annotate_x_pos-0.3, xend = annotate_x_pos + 0.9, y = -145, yend = -145, color = colores$color_linea, linewidth = 0.7) +
    ggplot2::annotate("text", x = annotate_x_pos + 1.3, y = -145, label = "% de positividad", hjust = 0, color = "black", size = 2.5)
  
}