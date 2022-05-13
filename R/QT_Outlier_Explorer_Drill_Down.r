#' QT Outlier Explorer Drill Down Plot
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

QT_Outlier_Explorer_Drill_Down <- function(data, settings)
{

    if (settings$plot_what == "Observed") {
        value_var <- settings$value_col
    } else if (settings$plot_what == "Change") {
        value_var <- "CHG" # assuming change variable is named CHG, check CDISC standard
    }	
	
    #get info about subject when clicked on in click variable
    click = event_data("plotly_click", source="QT_outlier_Explorer_click")
	#display nothing unless plot has been clicked
	req(click)
	
	#get ECG information from subject that was clicked on (for all parameters)
	ECG_results <- data %>%
                   filter(.data[[settings$id_col]] %in% click$key[1]) %>%
				   mutate(Y_VAR = .data[[value_var]]) %>%
				   mutate(TITLE = paste0(.data[[settings$measure_col]], " (", .data[[settings$unit_col]], ")"), SPLIT = paste0(.data[[settings$measure_col]]))
				   
	#find the number of parameters to be presented and determine optimal plot height
	Height <- ECG_results %>%
	            distinct(.data[[settings$measure_col]], .keep_all = FALSE) %>% 
				count()*250
				   
	#Rename the column Height_n
	names(Height) <- paste("Height", names(Height), sep="_")
	#Join height to main data to be used in plot
	ECG_results2 <- ECG_results %>%
				   right_join(Height, by = character())

    #Derive Y axis titles based on selected variables	
	if(settings$plot_what == "Observed"){Y_Title = "Observed Values"}
	  else if(settings$plot_what == "Change"){Y_Title = "Change from Baseline"}	

	#create the individual line plots for each parameter
    fig <- ECG_results2 %>%
           split(.$SPLIT) %>%
           lapply(function(d) 
              plot_ly(d,
                x         = ~.data[[settings$visit_col]],
                y         = ~Y_VAR,		
                type      = 'scatter',
                mode      = 'lines+markers'
			  ) %>%
			  layout (
			     showlegend = FALSE,
			  	 xaxis      = list(title = 'Time Point', 
				                   ticktext = ~paste0(sprintf("%02d", .data[[settings$visitn_col]]), " - ", .data[[settings$visit_col]]), 
								   tickvals = ~.data[[settings$visit_col]],
				                   zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff',
								   type = "category"
								   ),
	  	         yaxis      = list(title = Y_Title),
				 plot_bgcolor='#e5ecf6',
				 height     = ~Height_n
			  )%>%
		     add_annotations(
                text = ~.data[["TITLE"]],
                x = 0.5,
                y = 1.0,
                yref = "paper",
                xref = "paper",
                xanchor = "middle",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 15)
	         )
	       ) %>%
		   subplot(nrows = NROW(.), shareX=TRUE, shareY=FALSE, titleY=TRUE, titleX=TRUE)	

	 return(fig)

}