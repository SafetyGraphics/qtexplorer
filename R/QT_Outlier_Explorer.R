#' QT Outlier Explorer
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
#' @import plotly
#' @import rlang
#' @importFrom rlang .data
#' @import dplyr
#'
#' @export




QT_Outlier_Explorer <- function(data, settings)
{
    
    # horizontal reference line
    hline <- function(y = 0, color = "blue") {
        list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = y,
            y1 = y,
            line = list(color = color, width= 2, dash = 'dash')
        )
    }
    
    # choose between observed or change values

    if (settings$plot_what == "Observed") {
        value_var <- settings$value_col
    } else if (settings$plot_what == "Change") {
        value_var <- "CHG" # assuming change variable is named CHG, check CDISC standard
    }	
	
    data_filtered <- data %>%
        filter(.data[[settings$measure_col]] %in% settings$measure_values) 
	   
	#Derive columns to be presented on x and y axis based on user choices
    data1 <- data_filtered %>% 
        mutate(X_VAR = .data[[settings$Outlier_X_var]], 
		       Y_VAR = .data[[value_var]]
			   )
	
    #Derive X axis title based on selected variables	
	if(settings$Outlier_X_var == settings$value_col){X_Title = "Observed Values"}
	  else if(settings$Outlier_X_var == settings$base_col){X_Title = "Baseline"}
    
    #Derive Y axis title based on selected variables	
	if(settings$plot_what == "Observed"){Y_Title = "Observed Values"}
	  else if(settings$plot_what == "Change"){Y_Title = "Change from Baseline"}	
	
    fig <- data1 %>%
    plot_ly(
        x         = ~X_VAR,
        y         = ~Y_VAR,
        size      = 10,
        color     = ~.data[[settings$group_col]],
        frame     = ~paste0(sprintf("%02d", .data[[settings$visitn_col]]), " - ", .data[[settings$visit_col]] ),
        text      = ~paste0(.data[[settings$measure_col]], "<br>Time point: ", .data[[settings$visit_col]], "<br>Treatment: ",
                            .data[[settings$group_col]], "<br>Baseline:", X_VAR, "<br>Change: ", Y_VAR),
        hoverinfo = "text",
        type      = 'scatter',
        mode      = 'markers',
		key       = ~.data[[settings$id_col]],		
		source    = 'QT_outlier_Explorer_click'
    ) %>%
    animation_slider(
        currentvalue = list(prefix = "Time Point: ")
    ) %>%
    layout(
	  margin = list(b=200),
	  xaxis  = list(title = X_Title),
	  yaxis  = list(title = Y_Title),
	  shapes = 
        list(
            hline(30), 
            hline(60),
            hline(450), 
            hline(480), 
            hline(500)
            )
        )

return(fig)    
}
