###################################################
### Functions for "The Ocean of Possible Truth" ###
###################################################

#### Plots ####

### Congruence plots

congruence_plot <- function(data_set, x_var, y_var, treatment, line_type, 
                            x_lab, y_lab, treat_lab, treat1_lab, treat2_lab,
                            y_min = -2.5, y_max = 2.5, y_breaks = c(-2, -1, 0, 1, 2), 
                            x_min = -2, x_max = 2) {
  ggplot(data_set, 
         aes(x = x_var, 
             y = y_var, 
             group = treatment)) +
    geom_smooth(aes(linetype = factor(treatment)), 
                method = line_type, se = FALSE, fullrange = FALSE,
                colour = "#181818", size = 0.5) + 
    labs(y = x_lab, 
         x = y_lab,
         linetype = treat_lab) +
    scale_y_continuous(breaks = y_breaks,
                       limits = c(y_min, y_max)) +
    scale_x_continuous(breaks = c(-2, -1, 0, 1, 2),
                       limits = c(x_min, x_max)) + 
    theme(
      panel.border = element_rect(colour = "#DCDCDC", fill = NA, size = 1.5),
      panel.background = element_blank(),
      panel.grid.major = element_line(size = .5, colour = "#EEEEEE"),
      # panel.grid.minor = element_line(size = .3, colour = "#EEEEEE"),
      plot.margin = unit(c(0.8,0,0,0), "cm")) +
    scale_linetype_discrete(labels = c(treat1_lab, treat2_lab)) +
    geom_rug(sides="b", alpha = 0.1, size = 1.2, position = "jitter")
}

### Source effect plots

source_plot <- function(data_set, y_lab, y_breaks, y_lim_low, y_lim_high) {
  ggplot(data_set, aes(x = variable,
                          y = mean, 
                          group = Treatment, 
                          shape = Treatment)) +
  geom_point(position = position_dodge(0.6)) + 
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                width = .2,
                position = position_dodge(0.6)) +
  labs(y = y_lab, x = "") +
  scale_y_continuous(breaks = y_breaks,
                     limits = c(y_lim_low, y_lim_high)) +
  theme(
    panel.border = element_rect(colour = "#DCDCDC", fill = NA, size =1.5),
    panel.background = element_blank(), 
    panel.grid.major.y = element_line(size=.5, color = "#EEEEEE") ,
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 30,
                               hjust = 1,
                               vjust = 1, 
                               margin = margin(0, 0, 0.1, 0,"cm")),
    axis.title.y = element_text(margin = margin(0, 0.3, 0, 0,"cm")),
    axis.ticks.x = element_blank(),
    legend.key = element_blank()) +
  scale_color_manual(values = c('#181818','#181818'))
}

#### Congruence margins plots

h1a_marginsplot <- function(margins_data, x_lab, y_lab = NULL, 
                            x_lim_low, x_lim_high, rug_data,
                            y_lim_low = -1.2, y_lim_high = 1.2) {
  ggplot(margins_data, aes(x = at, y = delta)) +
    geom_point() + 
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                  width=.05) + 
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
    labs(y = y_lab, x = x_lab) +
    xlim(x_lim_low, x_lim_high) +
    scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
                       limits = c(y_lim_low, y_lim_high)) +
    theme(
      panel.border = element_rect(colour = "#DCDCDC", fill = NA, size =1.5),
      panel.background = element_blank(), 
      plot.margin = unit(c(1.5,0.5,0.5,0.5), "lines"),
      panel.grid.major = element_line(size=.5, color = "#EEEEEE"),
      legend.key = element_blank()) +
    scale_color_manual(values = c('#181818','#181818')) +
    geom_rug(inherit.aes = FALSE, 
             aes(x = attitude, y = y),
             data = rug_data, 
             sides="b", alpha = 0.1, size = 1.2, position = "jitter")
}

#### Attitude histograms

attitude_histogram <- function(attitude, dt) {
  ggplot(dt, aes(x = attitude)) + 
    geom_histogram(colour = "white", fill="grey", bins = 11) + 
    geom_vline(aes(xintercept = mean(attitude, na.rm = TRUE)),
               color="black", linetype="dashed", size=0.5) +
    theme(panel.border = element_rect(colour = "#DCDCDC", fill = NA, size =1.5),
          panel.background = element_blank(), 
          panel.grid.major = element_line(size=.5, color = "#EEEEEE"),
          panel.grid.minor = element_line(size = .3, colour = "#EEEEEE"),
          legend.key = element_blank()) +
    ylim(0, (max(table(attitude)) + 15)) + 
    scale_x_continuous(breaks = c(-5, 0, 5))
} 

attitude_histogram_fac <- function(attitude, dt) {
  ggplot(dt, aes(x = attitude)) + 
    geom_histogram(colour = "white", fill="grey", bins = 20) + 
    geom_vline(aes(xintercept = mean(attitude, na.rm = TRUE)),
               color="black", linetype="dashed", size=0.5) +
    theme(panel.border = element_rect(colour = "#DCDCDC", fill = NA, size =1.5),
          panel.background = element_blank(), 
          panel.grid.major = element_line(size=.5, color = "#EEEEEE"),
          panel.grid.minor = element_line(size = .3, colour = "#EEEEEE"),
          legend.key = element_blank()) +
    ylim(0, (max(table(attitude)) + 15)) + 
    scale_x_continuous(breaks = c(-3:3))
} 


#### Full models renaming
# 
# `renameh1<-` <- function(model_names, model_list) {
#   for(i in model_names) {
#     names(model_list[[i]]$coefficients) <- c("Constant",
#                                              "Report (0 = left-wing)",
#                                              "Attitude",
#                                              "Report * Attitude")
#   }
# }

#### Values ####

# attitude_percentile <- function(data_set, attitude, percentile) {
#   attitudes <- data_set$attitude[is.na(data_set$attitude)]
#   value <- summarize(quants = quantile(attitudes, probs = percentile))
#   return(as.vector(value[[1]]))
# }
# 
# attitude_percentile(data_1, topic4_attitudes_factor, 0.95)

#### Tables ####

### Full model tables

full_models_table <- function(models, table_label, table_title, dep_vars) {
  stargazer(models,
            type = "latex",
            out = "./table-4.tex",
            table.placement = "!ht",
            omit.stat = c("LL","ser","f","adj.rsq"),
            label = table_label, 
            single.row = FALSE,
            no.space = FALSE,
            digits = 2,
            align = TRUE,
            font.size = "scriptsize",
            column.sep.width = "-14pt",
            star.cutoffs = c(0.1, 0.05, 0.01),
            covariate.labels = c("Report (0 = left-wing)",
                               "Attitude",
                               "Report * Attitude",
                               "Source (0 = unknown)",
                               "Trust",
                               "Source * Trust",
                               "Report * Source",
                               "Attitude * Source",
                               "Report * Attitude * Source",
                               "Constant"),
            order = c(2, 3, 4, 8, 9, 10, 5, 6, 7, 1),
            model.names = FALSE, 
            model.numbers = FALSE,
            title = table_title,
            dep.var.caption = "",
            column.labels = c("H1a", "H2a", "H3a", "H4a",
                              "H1a", "H2a", "H3a", "H4a",
                              "H1a", "H2a", "H3a", "H4a",
                              "H1a", "H2a", "H3a", "H4a"),
            dep.var.labels = dep_vars,
            header = FALSE)
}

### Population sample tables

population_sample_table <- function(data_set, caption_text, fn_text, 
                                    column_width = NULL, columns_coll = NULL) {
  kable(data_set, 
        caption = caption_text, 
        format = "latex", booktabs = T, longtable = T, escape = F) %>%
    kable_styling(full_width = T,
                  latex_options = c("scale_down", "HOLD_position"),
                  font_size = 10) %>%
    add_header_above(c(" " = 1, "Population" = 2, 
                       "Sample Study 1" = 2, "Sample Study 2" = 2)) %>%
    footnote(alphabet = fn_text, threeparttable = T) %>%
    column_spec(1, width = column_width) %>%
    collapse_rows(columns = columns_coll, valign = "middle", latex_hline = "none")
}
