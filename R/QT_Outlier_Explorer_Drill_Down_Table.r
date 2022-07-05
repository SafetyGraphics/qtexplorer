#' QT Outlier Explorer Drill Down Table
#'
#' @param data ECG data structured as one record per person per visit per measurement. See details for column requirements.
#' @param settings named list of settings with the parameters specified below.
#'
#' @details The settings object provides details the columns in the data set.
#'
#' \itemize{
#'  \item{"id_col"}{ID column}
#'  \item{"value_col"}{Value column}
#'  \item{"baseline_col"}{Baseline Value column}
#'  \item{"change_col"}{Change from Baseline Value column}
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
#' @import plotly
#' @import rlang
#' @importFrom rlang .data
#' @import dplyr
#'
#' @export

QT_Outlier_Explorer_Drill_Down_Table <- function(data, settings)
{

    #get info about subject when clicked on in click variable
    click = event_data("plotly_click", source="QT_outlier_Explorer_click")
	#display nothing unless plot has been clicked
	req(click)
	
	#get demographic information from subject that was clicked on
	DM_data <- data %>%
                   filter(.data[[settings$id_col]] %in% click$key[1]) %>% 
				   mutate_if(is.numeric, as.character)
	
	return(DM_data)

}