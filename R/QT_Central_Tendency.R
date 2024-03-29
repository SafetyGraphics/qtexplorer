#' QT central tendency
#'
#' @param data ECG data structured as one record per person per visit per measurement. See details for column requirements.
#' @param settings named list of settings with the parameters specified below.
#'
#' @details The settings object provides details the columns in the data set.
#'
#' \itemize{
#'  \item{"id_col"}{ID column}
#'  \item{"value_col"}{Value column}
#'  \item{"measure_col"}{Measure column}
#'  \item{"measure_values"}{Measure values}
#'  \item{"visit_col"}{Visit column}
#'  \item{"visitn_col"}{Visit number column (numeric)}
#'  \item{"baseline_flag_col}{Baseline flag column}
#'  \item{"baseline_flag_values}{Baseline flag value}
#' }
#'
#'
#' @return returns a chart object
#'
#' @importFrom plotly ggplotly
#' @import rlang
#' @importFrom rlang .data
#' @import dplyr
#'
#' @export




QT_Central_Tendency <- function(data, settings) {


    # TODO: handle cross-over TQT study, VISIT-TPT scenario
    # TODO: add mean profile plot

    # choose between observed or change values

    if (settings$plot_what == "Observed") {
        value_var <- settings$value_col
        value_var_label <- paste0("Observed values: ", settings$measure_values)
        href_lines <- c(450, 480, 500)
    } else if (settings$plot_what == "Change") {
        value_var <- "CHG" # assuming change variable is named CHG, check CDISC standard
        value_var_label <- paste0("Change from baseline: ", settings$measure_values)
        href_lines <- c(30, 60)
    }


    data2 <- data %>%
        filter(.data[[settings$measure_col]] %in% settings$measure_values) %>%
        group_by(.data[[settings$group_col]], .data[[settings$visit_col]]) %>%
        summarise(
            Mean = mean(.data[[value_var]]),
            sd = sd(.data[[value_var]]),
            n = n(),
            se = sd / sqrt(n)
        )

    pd <- position_dodge(.3) # Save the dodge spec because we use it repeatedly

    fig0 <- ggplot(data2, aes(
        x = .data[[settings$visit_col]], y = Mean,
        colour = .data[[settings$group_col]], group = .data[[settings$group_col]]
    )) +
        geom_errorbar(
            aes(ymin = Mean - se, ymax = Mean + se),
            width = .2,
            size = 0.25,
            colour = "black",
            position = pd
        ) +
        ylab(value_var_label) +
        geom_hline(yintercept = href_lines, linetype = 2, col = "red") +
        geom_line(position = pd) +
        geom_point(position = pd, size = 2.5) +
        theme_bw()

    fig <- fig0 %>% plotly::ggplotly()


    return(fig)
}