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
	
	#define reference lines based on Y axis variable
    if (settings$plot_what == "Observed") {
	    reflines <- list()
	    if (any(settings$RefLines %in% "QTc Interval > 450")){
		   reflines[[1]] <- hline(y=450)
		   }
	    if (any(settings$RefLines %in% "QTc Interval > 480")){
		   reflines[[2]] <- hline(y=480)
		   }
	    if (any(settings$RefLines %in% "QTc Interval > 500")){
		   reflines[[3]] <- hline(y=500)
		   }		

	} else if (settings$plot_what == "Change") {
	    reflines <- list()
	    if (any(settings$RefLines %in% "QTc Change from Baseline > 30")){
		   reflines[[1]] <- hline(y=30)
		   }
	    if (any(settings$RefLines %in% "QTc Change from Baseline > 60")){
		   reflines[[2]] <- hline(y=60)
		   }
		if (any(settings$RefLines %in% "QTc Change from Baseline x=y Line")){
        reflines[[3]] <-  list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 0,
            y1 = 1,
			yref="paper",
            line = list(color = "red", width= 2, dash = 'dash')
        )
		   }	
		   
		if (any(settings$RefLines %in% "QTc Change from Baseline > 450 Diagonal Line")){
        reflines[[4]] <-  list(
            type = "line",
            x0 = 450,
            x1 = 0,
            y0 = 0,
            y1 = 450,
            line = list(color = "orange", width= 2, dash = 'dash')
        )
		  }
		  
		if (any(settings$RefLines %in% "QTc Change from Baseline > 480 Diagonal Line")){
        reflines[[5]] <-  list(
            type = "line",
            x0 = 480,
            x1 = 0,
            y0 = 0,
            y1 = 480,
            line = list(color = "orange", width= 2, dash = 'dash')
        )
		  }

		if (any(settings$RefLines %in% "QTc Change from Baseline > 500 Diagonal Line")){
        reflines[[6]] <-  list(
            type = "line",
            x0 = 500,
            x1 = 0,
            y0 = 0,
            y1 = 500,
            line = list(color = "orange", width= 2, dash = 'dash')
        )
		  }		   		   
		   		   
    }	
	
    fig <- data1 %>%
    plot_ly(
        x         = ~X_VAR,
        y         = ~Y_VAR,
        size      = 10,
        color     = ~.data[[settings$group_col]],
        frame     = ~paste0(sprintf("%02d", .data[[settings$visitn_col]]), " - ", .data[[settings$visit_col]] ),
        text      = ~paste0(.data[[settings$measure_col]], "<br>Time point: ", .data[[settings$visit_col]], "<br>Treatment: ",
                            .data[[settings$group_col]], "<br>X-Value:", X_VAR, "<br>Y-Value: ", Y_VAR),
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
	  shapes = reflines
	)

	
return(fig)    
}
