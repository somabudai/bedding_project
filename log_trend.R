library(RMariaDB)
library(ggplot2)
library(dplyr)
library("writexl")
library(RColorBrewer)
library(ggpubr)

element_thickness_type_use <- c('true (maximum)','true (not maximum)','apparent')

vertical_change_barplot <- function (data, type=c('thickness','grain_size'), system_type, title) {
  
  if (type == 'thickness') {
    colors = c('thickening' = '#F09B36',
               'thinning' = '#5BC9E1',
               'no trend' = '#009444')
    order = c('thickening', 'thinning', 'no trend')
  }
  if (type == 'grain_size') {
    colors = c('coarsening' = '#F15A29',
               'fining' = '#FBB040',
               'no trend' = '#009444')
    order = c('coarsening', 'fining', 'no trend')
  }
  
  input_data <- data %>% filter(sys_gs_category == {{system_type}})
  
  plot <- ggplot(input_data, aes(x=factor(element_general_type, level = selected_elements), y=n_percent, fill = factor(log_trend, level = order)))+
    geom_bar(position = 'stack', stat ='identity', width = 0.7, colour = 'black', lwd=0.23)+
    geom_text(data = input_data, aes(x = element_general_type, y=104 , label=n_sum, fill = NULL),  size = 7/.pt)+
    scale_fill_manual(values = colors)+
    labs(
      title = title,
      x='',
      y= 'percentage of logs',
      fill = 'Vertical trend'
    )+
    theme_classic()+
    scale_y_continuous(limits = c(0,107),expand = c(0,0))+
    theme_classic()+
    theme(legend.position= 'bottom',
          legend.text = element_text(size = 7),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(3.5, 'mm'),
          legend.key.width = unit(5, 'mm'),
          axis.text.x = element_text(color = "black", size = 9),
          axis.title.x=element_blank(),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.text.y = element_text(color = "black", size = 9),
          axis.title.y = element_text(color = "black", size = 9),
          plot.title = element_text(color = 'black', size = 10),
    )+
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
  
  return(plot)
}



### VERTICAL THICKNESS CHANGE-----------------------
log_vertical_bed_thickness <- beds_table_sandgs_lam_features_elements %>% filter(element_thickness_type %in% element_thickness_type_use) %>%
  mutate(log_element = paste(log_ID,element_ID, sep = '_')) %>%
  group_by(log_element) %>% mutate(n_beds = n()) %>% ungroup() %>% filter(n_beds >= 3) %>%
  select(artificial_bed_id, log_ID, element_ID, log_element, bed_thickness, code, element_general_type.x, sys_gs_category) %>%
  group_by(log_element) %>% mutate(log_sum_bed_thck = sum(bed_thickness)) %>% ungroup() %>%
  mutate(bed_thck_difference = case_when(lead(log_element) == log_element ~ lead(bed_thickness)-bed_thickness, lead(log_element) != log_element ~ 500)) %>%
  filter(bed_thck_difference != 500) %>% 
  group_by(log_element) %>% mutate(pos = sum(bed_thck_difference >0), neg = sum(bed_thck_difference <0)) %>% ungroup() %>%
  group_by(log_element) %>% mutate(bed_thck_difference_sum = round(sum(bed_thck_difference),3)) %>% ungroup() %>%
  mutate(log_trend_value = bed_thck_difference_sum/log_sum_bed_thck) %>%
  distinct(log_element, .keep_all = TRUE) %>% rename('element_general_type' = 'element_general_type.x') %>%
  filter(element_general_type %in% selected_elements) %>%
  mutate(log_trend_margin = case_when(log_trend_value >= 0.03 ~ 'thickening', log_trend_value <= -0.03 ~ 'thinning', TRUE ~ 'no trend')) %>%
  mutate(log_trend = case_when(log_trend_value > 0 ~ 'thickening', log_trend_value < 0 ~ 'thinning', TRUE ~ 'no trend'))

vertical_change_summary <- log_vertical_bed_thickness %>% group_by(element_general_type, sys_gs_category, log_trend) %>%
  summarise(n =n()) %>% ungroup() %>% group_by(element_general_type, sys_gs_category) %>% mutate(n_sum = sum(n)) %>% ungroup() %>%
  mutate(n_percent = round(n/n_sum,4)*100)

#SANDY
vert_thickness_change_sandy_plot <- vertical_change_barplot(vertical_change_summary,'thickness','sandy system','Sandy systems')
vert_thickness_change_sandy_plot
ggsave('plots/vert_thickness_change_sandy_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/vert_thickness_change_sandy_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')

#GS
vert_thickness_change_gs_plot <- vertical_change_barplot(vertical_change_summary,'thickness','gravelly-sand system','Gravelly-sand systems')
vert_thickness_change_gs_plot
ggsave('plots/vert_thickness_change_gs_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/vert_thickness_change_gs_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')

#COMBINED THICKNESS PLOT

vert_thickness_change_plot <- ggarrange(vert_thickness_change_sandy_plot, 
                                        vert_thickness_change_gs_plot , 
                                        labels = c('A','B'),
                                        font.label = list(size = 10, face = "bold", color ="black"),
                                        common.legend = TRUE, legend = 'bottom')


vert_thickness_change_plot <- annotate_figure(vert_thickness_change_plot,
                top = text_grob('Vertical thickness change', face = 'bold', size = 10))

vert_thickness_change_plot

ggsave('plots/vert_thickness_change_plot.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
ggsave('plots/vert_thickness_change_plot.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')




ggplot(log_vertical_bed_thickness, (aes(x=element_general_type, y = log_trend_value, color = sys_gs_category))) +
geom_point(position=position_jitterdodge(jitter.width = 0.2), size = 2)+
  annotate(geom= 'text', x=0.5, y= 0.3, label='thickening upward', angle = 90)+
  annotate(geom= 'text', x=0.5, y= -0.3, label='thinning upward', angle = 90)+
  coord_cartesian(ylim = c(-0.75,0.75))+
  scale_color_manual(values = c('gravelly-sand system' = '#5f2e1f',
                               'sandy system' = '#ffb800'))+
  geom_hline(yintercept = 0, alpha = 0.3, lwd=0.23)+
  theme_classic()+
  theme(axis.line = element_line(colour = 'black', size = 0.23),
        axis.ticks = element_line(colour = 'black', size = 0.23))

### VERTICAL GRAIN SIZE CHANGE-----------------------

grain_size_phi <- data.frame(grain_size = c("_G", "very coarse", "coarse", "medium", "fine", "very fine"),
                 phi_value = c("6", "5", "4", "3", "2", "1")
)

beds_facies_with_phi <- beds %>% select(facies_ID, artificial_bed_id, facies_type, grain_size_sand) %>%
  mutate(grain_size = case_when(facies_type == '_G' ~ facies_type, TRUE ~ grain_size_sand)) %>% 
  left_join(grain_size_phi, by = 'grain_size') %>% filter(!is.na(phi_value)) %>%
  group_by(artificial_bed_id) %>% summarise(phi_max = max(phi_value)) %>% mutate_at('phi_max', as.numeric)

log_vertical_bed_coarseness <- beds_table_sandgs_lam_features_elements %>% 
  filter(element_thickness_type %in% element_thickness_type_use) %>%
  mutate(log_element = paste(log_ID,element_ID, sep = '_')) %>%
  group_by(log_element) %>% mutate(n_beds = n()) %>% ungroup() %>% filter(n_beds >= 3) %>%
  select(artificial_bed_id, log_ID, element_ID, log_element, bed_thickness, code, element_general_type.x, sys_gs_category) %>%
  left_join(beds_facies_with_phi, by = 'artificial_bed_id') %>%
  mutate(phi_max_difference = case_when(lead(log_element) == log_element ~ lead(phi_max)-phi_max, lead(log_element) != log_element ~ 500)) %>%
  filter(phi_max_difference != 500) %>% 
  group_by(log_element) %>% mutate(pos = sum(phi_max_difference >0), neg = sum(phi_max_difference <0)) %>% ungroup() %>%
  group_by(log_element) %>% mutate(phi_max_difference_sum = round(sum(phi_max_difference),3)) %>% ungroup() %>%
  distinct(log_element, .keep_all = TRUE) %>% rename('element_general_type' = 'element_general_type.x') %>%
  filter(element_general_type %in% selected_elements) %>%
  mutate(log_trend = case_when(phi_max_difference_sum > 0 ~ 'coarsening', phi_max_difference_sum < 0 ~ 'fining', TRUE ~ 'no trend'))

vertical_gs_change_summary <- log_vertical_bed_coarseness %>% group_by(element_general_type, sys_gs_category, log_trend) %>%
  summarise(n =n()) %>% ungroup() %>% group_by(element_general_type, sys_gs_category) %>% mutate(n_sum = sum(n)) %>% ungroup() %>%
  mutate(n_percent = round(n/n_sum,4)*100)

#SANDY
vert_gs_change_sandy_plot <- vertical_change_barplot(vertical_gs_change_summary,'grain_size','sandy system','Sandy systems')
vert_gs_change_sandy_plot
ggsave('plots/vert_gs_change_sandy_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/vert_gs_change_sandy_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')

#GS
vert_gs_change_gs_plot <- vertical_change_barplot(vertical_gs_change_summary,'grain_size','gravelly-sand system','Gravelly-sand systems')
vert_gs_change_gs_plot
ggsave('plots/vert_gs_change_gs_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/vert_gs_change_gs_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')

#COMBINED GRAIN SIZE PLOT

vert_gs_change_plot <- ggarrange(vert_gs_change_sandy_plot, 
                                        vert_gs_change_gs_plot , 
                                        labels = c('C','D'),
                                        font.label = list(size = 10, face = "bold", color ="black"),
                                        common.legend = TRUE, legend = 'bottom')


vert_gs_change_plot <- annotate_figure(vert_gs_change_plot,
                top = text_grob('Vertical grain size change', face = 'bold', size = 10))
vert_gs_change_plot

ggsave('plots/vert_gs_change_plot.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
ggsave('plots/vert_gs_change_plot.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')
#COMBINED THICKNESS AND GS PLOT
vert_change_combined_plot <- ggarrange(vert_thickness_change_plot, 
                                 vert_gs_change_plot,
                                 ncol = 1,
                                 nrow =2,
                                 common.legend = FALSE)

vert_change_combined_plot

ggsave('plots/vert_change_combined_plot.pdf', width = 160, height = 130, units = 'mm', device = 'pdf')
ggsave('plots/vert_change_combined_plot.jpeg', width = 160, height = 130, units = 'mm', device = 'jpeg')




