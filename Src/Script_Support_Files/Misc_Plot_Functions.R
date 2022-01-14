### Depth profile plot function

plot_depth_profile <- function(data_in){
  data_in +
    geom_point() +
    scale_y_reverse() +
    labs(y = "Depth (m)") +
    theme_bw()
}



### Bivariate plot function

plot_bivariate <- function(data_in, x_log = "N", y_log = "N", alpha_val = 1){
  fig_made <- data_in +
    geom_point(alpha = alpha_val, size = 3, shape = 21) +
    theme_bw()
  
  if(tolower(x_log) == "y"){
    fig_made <- fig_made + 
      scale_x_log10()
  }
  
  if(tolower(y_log) == "y"){
    fig_made <- fig_made + 
      scale_y_log10()
  }
  
  fig_made
}