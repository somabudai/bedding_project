library(RMariaDB)
library(ggplot2)
library(dplyr)
library("writexl")
library(RColorBrewer)
library(ggpubr)


# ELEMENTS REPRESENTED IN MULTIPLE LOGS

elements_multiple_log <- beds_table_sandgs_lam_features_elements %>% select(log_ID, element_ID, element_general_type.x, element_thickness_type) %>%
  filter(element_thickness_type %in% element_thickness_type_use) %>%
  rename('element_general_type' = 'element_general_type.x') %>%
  distinct(log_ID, element_ID, element_general_type) %>% filter(element_general_type %in% selected_elements) %>%
  group_by(element_ID, element_general_type) %>% summarise(n = n()) %>%
  filter(n > 1)
elements_multiple_log_list <- elements_multiple_log %>% pull(element_ID) %>% as.list()

# VERTICAL TRENDS IN DIFFERENT LOGS

# THICKENING-THINNING

multiple_log_vertical_bed_thickness <- log_vertical_bed_thickness %>% 
  select(log_ID, log_element, element_ID, element_general_type, sys_gs_category, log_trend) %>% 
  rename('log_trend_thickness' = 'log_trend') %>% filter(element_ID %in% elements_multiple_log_list)

# FINING-COARSENING

multiple_log_vertical_bed_coarseness <- log_vertical_bed_coarseness %>% 
  select(log_ID, log_element, element_ID, element_general_type, sys_gs_category, log_trend) %>%
  rename('log_trend_gs' = 'log_trend') %>% filter(element_ID %in% elements_multiple_log_list)

# BED THICKNESS CHANGES IN DIFFERENT LOGS

multiple_log_element_bed_thickness <- beds_table_sandgs_lam_features_elements %>% 
  select(log_ID, element_ID, bed_thickness, code, element_general_type.x, element_thickness_type) %>%
  rename('element_general_type' = 'element_general_type.x') %>%
  group_by(element_ID, log_ID) %>% summarise(avg_bed_thck = round(mean(bed_thickness),2),
                                             min_bed_thck = min(bed_thickness),
                                             max_bed_thck = max(bed_thickness),
                                             n = n()) %>%
  mutate(log_element = paste(log_ID,element_ID, sep = '_'))

element_bed_thickness <- beds_table_sandgs_lam_features_elements %>% 
  select(log_ID, element_ID, bed_thickness, code, element_general_type.x, element_thickness_type) %>%
  rename('element_general_type' = 'element_general_type.x') %>%
  group_by(element_ID) %>% summarise(element_avg_bed_thck = round(mean(bed_thickness),2),
                                     element_min_bed_thck = min(bed_thickness),
                                     element_max_bed_thck = max(bed_thickness),
                                     element_sd_bed_thck = round(sd(bed_thickness),2),
                                     element_n = n()) %>%
  mutate(coeffvar_element_bed_thck = round(element_sd_bed_thck/element_avg_bed_thck,2) )
# ELEMENT THICKNESS IN LOGS
element_thickness_logs <- dbGetQuery(con, "SELECT d15_facies.1d_data_ID AS log_ID, d13_element.element_ID,
 d13_element.thickness_type AS element_thickness_type, SUM(d15_facies.thickness) AS log_element_thck
FROM d15_facies
JOIN d13_element ON d13_element.element_ID = d15_facies.parent_element_ID
GROUP BY element_ID, log_ID") %>% mutate(log_element = paste(log_ID,element_ID, sep = '_'))


# LATERAL TRENDS

lateral_trends <- left_join(multiple_log_vertical_bed_thickness, multiple_log_vertical_bed_coarseness, by=c('log_element', 'log_ID', 'element_ID','element_general_type', 'sys_gs_category')) %>%
  left_join(multiple_log_element_bed_thickness, by=c('log_element', 'log_ID', 'element_ID')) %>% left_join(element_thickness_logs, by=c('log_element', 'log_ID', 'element_ID')) %>%
  left_join(element_bed_thickness, by = 'element_ID') %>%
  arrange(element_ID) %>%
  group_by(element_ID) %>% mutate(log_order = 1:n()) %>% filter(log_order != 1 | lead(log_order) != 1)


lateral_changes_plot <- function(data,element_type, system_category, legend = c('none','bottom'), title) {
  input_data <- data %>% filter(element_general_type == {{element_type}}) %>%
    filter(sys_gs_category == {{system_category}})
  
  avg_bed_thck_max <- max(input_data$avg_bed_thck)
  max_log_nr <- max(input_data$log_order)
  


plot <- ggplot(input_data) + 
  geom_hline(yintercept = seq(0,avg_bed_thck_max, 0.25), alpha = 0.3, lwd=0.11)+
  geom_line(aes(x=log_order, y = avg_bed_thck), lwd = 0.11)+
  geom_point(aes(x=log_order, y = avg_bed_thck, shape = log_trend_thickness, fill = log_trend_gs, size = log_element_thck), stroke=0.11)+
  facet_grid(element_ID ~ .)+
  scale_y_continuous(limits = c(0,avg_bed_thck_max), breaks = seq(0,avg_bed_thck_max, by = 0.5))+
  scale_x_continuous(limits = c(1,max_log_nr), breaks = seq(1,max_log_nr, by = 1))+
  scale_shape_manual(values = c('no trend' = 21,
                                'thickening' = 22,
                                'thinning' = 23))+
  scale_fill_manual(values = c('coarsening' = '#F15A29',
                               'fining' = '#FBB040',
                               'no trend' = '#009444'))+
  labs(
    title = title,
    y= 'Mean bed thickness',
    x = 'Log')+
  theme_classic()+
  theme(strip.text.y = element_text(size=7, color='black', angle = 0))+
  theme(strip.background = element_rect(colour="black", fill="white", 
                                        size=0.23, linetype="solid"))+
  theme(legend.position= legend,
        legend.text = element_text(size = 7),
        legend.title = element_text(face = 'bold', size = 7),
        legend.key.height = unit(3.5, 'mm'),
        legend.key.width = unit(5, 'mm'),
        axis.text.x = element_text(color = "black", size = 9),
        axis.title.x=element_text(color = "black", size = 9),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 7),
        axis.title.y = element_text(color = "black", size = 9),
        plot.title = element_text(color = 'black', size = 10),
  )+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5, override.aes=list(shape=21)))+
  guides(shape = guide_legend(title.position="top", title.hjust = 0.5))+
  guides(size = guide_legend(title.position="top", title.hjust = 0.5))
return(plot)

}
#SANDY
sandy_terminal_lateral_plot <- lateral_changes_plot(lateral_trends,'terminal deposit','sandy system','none','Sandy terminal deposits')
sandy_terminal_lateral_plot
ggsave('plots/sandy_terminal_lateral_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

sandy_channel_lateral_plot <- lateral_changes_plot(lateral_trends,'channel','sandy system','bottom','Sandy channel deposits')
sandy_channel_lateral_plot
ggsave('plots/sandy_channel_lateral_plot.pdf', device = 'pdf', width = 90, height = 60, units = 'mm')

# sandy_levee_lateral_plot <- lateral_changes_plot(lateral_trends,'levee','sandy system','sandy terminal')
# sandy_levee_lateral_plot
# ggsave('plots/lateral_changes_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

sandy_lat_changes_combined_plot <- ggarrange(sandy_terminal_lateral_plot, 
                                             sandy_channel_lateral_plot,
                                             nrow = 1,
                                             ncol = 2,
                                             heights = c(1,0.5),
                                             labels = c('A','B'),
                                                 common.legend = TRUE, legend = 'bottom')
sandy_lat_changes_combined_plot
ggsave('plots/sandy_lat_changes_combined_plot.pdf', device = 'pdf', width = 180, height = 120, units = 'mm')
ggsave('plots/sandy_lat_changes_combined_plot.jpeg', device = 'jpeg', width = 180, height = 120, units = 'mm')
#GS
gs_terminal_lateral_plot <- lateral_changes_plot(lateral_trends,'terminal deposit','gravelly-sand system','GS terminal')
gs_terminal_lateral_plot
ggsave('plots/lateral_changes_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

gs_channel_lateral_plot <- lateral_changes_plot(lateral_trends,'channel','gravelly-sand system','GS terminal')
gs_channel_lateral_plot
ggsave('plots/lateral_changes_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

# sandy_levee_lateral_plot <- lateral_changes_plot(lateral_trends,'levee','sandy system','sandy terminal')
# sandy_levee_lateral_plot
# ggsave('plots/lateral_changes_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')
