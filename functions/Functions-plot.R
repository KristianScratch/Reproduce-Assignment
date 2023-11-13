## ---------------------------
##
## Script name: Functions_for_Palmer_Penguins.r
##
## Purpose of script: 
##      Functions for plotting and saving the figires used (Historgam and Bar) 
##
## Author: Anon
##
## Date Created: 2023-11-11
##
##----------------------------

#A function to plot a histogram of a penguin species coloured by sex
plot_histogram <- function(data, x, facet_by, title_as, x_axis_lab, y_axis_lab) {
  ggplot(data= data,
         aes_string(x= x,
                    fill= facet_by))+
    geom_histogram(binwidth = 100) +
    scale_fill_manual(values = c("MALE" = "lightblue", "FEMALE" = "lightpink"))+
    facet_wrap(facet_by, ncol = 1) +
    labs(title = title_as,
         x = x_axis_lab,
         y = y_axis_lab) +
    theme_linedraw()
}

#Function to save figure

save_figure <- function(Figure_to_save,
                        filename, width, height, res, scaling){
  agg_png(filename, 
          width   =  width, 
          height  =  height, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  Figure_to_save
  print(Figure_to_save)
  dev.off()
}

#Function to plot a bar graph for body mass between sexes, with confidence intervals 

plot_bar_BodyMass_Sex <- function(data) {
  ggplot(data = data,
         aes(x = sex, y = body_mass_g), color = sex) +
    stat_summary(fun = "mean", geom = "bar", fill = c("lightpink", "lightblue"), show.legend = FALSE) +
    geom_jitter(aes(color = sex), alpha= 0.3, position = position_jitter(width= 0.2, seed= 0), show.legend = FALSE) +
    scale_colour_manual(values = c("MALE" = "blue", "FEMALE" = "#e75480")) +
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.55, alpha= 0.8) +
    coord_cartesian(ylim = c(4000, 6000)) +
    labs(title = "Bar Graph of Body Mass by Sex in Gentoo",
         x = "Gentoo Sex",
         y = "Body Mass (g)")+
    theme_bw()
}

#Function to print mean, SE and confidence intervals for a given sex

calculate_stats <- function(data, sex_selected) {
  filter(Gentoo_only, sex == sex_selected) %>%
    summarise(mean = mean(body_mass_g), SE = sd(body_mass_g) / sqrt(n()), CI = SE*2) %>%
    print()
}



