library(RMariaDB)
library(ggplot2)
library(dplyr)
library(writexl)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "",
  password = "",
  host = "",
  port = ,
  dbname = ""
)

age_global_events <- read.csv('climate_spreadsheet.csv')
#age id 136 was both but it was reclassified as greenhouse

#
#Systems
#

facies_data_db_system <- dbGetQuery(con, "SELECT d15_facies.facies_ID,d15_facies.1d_data_ID as log_ID, d15_facies.1d_data_order AS data_order, d15_facies.trans_from_below, d15_facies.trans_from_below_event_b,
d15_facies.facies_type, d15_facies.grain_size_sand, d15_facies.grain_size_gravel, d15_facies.grading, d15_facies.general_structure, d15_facies.lamination_type,
d15_facies.thickness,  d15_facies.thickness_type,
d13_element.element_ID, d13_element.original_interpretation AS element_interpret, 
d13_element.general_type AS element_general_type, 
d07_subset.subset_ID ,d07_subset.data_type, d07_subset.facies_suit_proportions_type,
d06_case_study.case_study_ID, d06_case_study.name AS cs_name, ifnull(d06_case_study.age_ID_from, d06_case_study.age_ID_to) AS cs_age_from, ifnull(d06_case_study.age_ID_to, d06_case_study.age_ID_from) AS cs_age_to,	
d04_system.system_ID ,d04_system.name AS sys_name, ifnull(d04_system.age_ID_from,d04_system.age_ID_to) AS sys_age_from, ifnull(d04_system.age_ID_to,d04_system.age_ID_from) AS sys_age_to, 
d04_system.palaeo_lat_range, d04_system.palaeo_latitude, d04_system.palaeo_lat_hemisphere
FROM d15_facies
JOIN d13_element ON d13_element.element_ID = d15_facies.parent_element_ID
JOIN d07_subset ON d07_subset.subset_ID = d13_element.subset_ID
JOIN d06_case_study ON d06_case_study.case_study_ID = d07_subset.case_study_ID
JOIN d04_system ON d04_system.system_ID = d06_case_study.system_ID
WHERE d15_facies.thickness IS NOT NULL
ORDER BY d15_facies.1d_data_ID, d15_facies.1d_data_order ASC")
facies_data_systems <- left_join(facies_data_db_system, age_global_events, by = "sys_age_to")

#
#option 1 everything in one column
#
# 
# system_gravel_sand_ratio <- facies_data_systems %>% select(system_ID, sys_name, facies_type, thickness) %>%
#   filter(facies_type %in% c('_G','_S','gS','sG','(g)S', 'mS')) %>% 
#   mutate(facies_type = case_when(facies_type %in% c('_G','sG') ~ 'G', facies_type %in% c('_S','mS', 'gS', '(g)S') ~ 'S')) %>%
#   group_by(system_ID) %>% mutate(sys_thickness = sum(thickness)) %>% ungroup() %>%
#   group_by(system_ID, facies_type) %>% mutate(facies_thickness = sum(thickness)) %>% ungroup() %>%
#   distinct(system_ID, facies_type, .keep_all = TRUE) %>%
#   mutate(facies_fraction = round((facies_thickness/sys_thickness),2)) %>%
#   filter(facies_type == 'S')
# 


#
#option separate columns
#




system_facies_data <- facies_data_systems %>% select(system_ID, sys_name, facies_type, thickness) %>%
  filter(facies_type %in% c('_G','_S','gS','sG','(g)S', 'mS')) %>% 
  mutate(facies_type = case_when(facies_type %in% c('_G','sG') ~ 'G', facies_type %in% c('_S','mS') ~ 'S', facies_type %in% c('gS', '(g)S') ~ 'gS'))
system_facies_ratio <- system_facies_data %>% group_by(system_ID) %>% mutate(sys_thickness = sum(thickness)) %>% ungroup() %>% distinct(system_ID, sys_name, sys_thickness)
 #G
system_gravel_content <- system_facies_data %>% filter(facies_type == 'G') %>% group_by(system_ID, sys_name) %>% 
  mutate(gravel_content = sum(thickness)) %>% ungroup() %>% 
  distinct(system_ID,gravel_content) 
system_facies_ratio <- left_join(system_facies_ratio, system_gravel_content, by = 'system_ID')
  #gS
system_gs_content <- system_facies_data %>% filter(facies_type == 'gS') %>% group_by(system_ID, sys_name) %>% 
  mutate(gs_content = sum(thickness)) %>% ungroup() %>% 
  distinct(system_ID,gs_content) 
system_facies_ratio <- left_join(system_facies_ratio, system_gs_content, by = 'system_ID')
  #S
system_sand_content <- system_facies_data %>% filter(facies_type == 'S') %>% group_by(system_ID, sys_name) %>% 
  mutate(sand_content = sum(thickness)) %>% ungroup() %>% 
  distinct(system_ID,sand_content) 
system_facies_ratio <- left_join(system_facies_ratio, system_sand_content, by = 'system_ID') 
system_facies_ratio <- system_facies_ratio %>% replace(is.na(.), 0)
# calculate fractions
system_facies_ratio <- system_facies_ratio %>% mutate(gravel_fraction = round(gravel_content/sys_thickness,2)) %>% 
  mutate(gs_fraction = round(gs_content/sys_thickness,2)) %>% mutate(sand_fraction = round(sand_content/sys_thickness,2)) %>%
  mutate(combined_sand_fraction = 1-gravel_fraction)

#
#Plots to examine gravel content of systems
#

# ggplot(system_facies_ratio, aes(x = factor(0), y = combined_sand_fraction))+geom_boxplot(width = 0.5)+
#   geom_violin(alpha = 0.5)+
#   geom_point(position = position_dodge2(width = 0.5))+
#   annotate('rect', ymin = 0.5, ymax = 1, xmin = 0, xmax =Inf ,fill = 'yellow', alpha = 0.3)+
#   labs(x='',
#        y='',
#        title = 'Sand fraction of the studied systems')+
#   theme_bw()

#SYSTEM COARSENESS
system_coarseness_plot <- ggplot(system_facies_ratio, aes(x = combined_sand_fraction))+geom_histogram(binwidth = 0.1, boundary = 0, color = "black", fill = "grey50",lwd=0.24)+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,1, by = 0.1))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x='sand/sand+gravel',
    y = 'number of systems'
  )+
  theme_classic()+
  theme(legend.position= 'none',
        axis.text.x = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))
  
system_coarseness_plot
ggsave('plots/system_coarseness_plot.pdf', width = 90, height = 90, units = 'mm', device = 'pdf')
#
# add categories
#

system_facies_ratio <- system_facies_ratio %>% mutate(sys_gs_category = case_when(combined_sand_fraction >= 0.94 ~ 'sandy system', combined_sand_fraction < 0.94 & combined_sand_fraction >= 0.47 ~ 'gravelly-sand system', combined_sand_fraction < 0.47 ~ 'gravelly system' ))
sys_gs_category <- system_facies_ratio %>% select(system_ID,sys_gs_category,combined_sand_fraction)

#
#number of systems per sys gs type per element
#

sys_count_sysgs_element <- beds_table_sandgs_lam_features %>% filter(element_general_type %in% c('terminal deposit','channel','levee')) %>%
  select(sys_gs_category, element_general_type, sys_name) %>%
  group_by(sys_gs_category, element_general_type) %>% distinct(sys_name, .keep_all = TRUE)


#
# count of beds per sys_gs and element
#

sys_gs_bed_count <- beds_table_sandgs_lam_features %>% select(artificial_bed_id,code, element_general_type, sys_gs_category, climate) %>%
  filter(element_general_type %in% c('levee','channel','terminal deposit')) %>%
  group_by(sys_gs_category,element_general_type) %>% summarise(n = n())
