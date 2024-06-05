library(RMariaDB)
library(ggplot2)
library(dplyr)


source('config.R')

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "",
  password = "",
  host = "",
  port = ,
  dbname = ""
)

age_global_events <- read.csv('climate_spreadsheet.csv')

element_ng_query <- dbGetQuery(con, "SELECT * FROM 
(SELECT SUM(d15_facies.thickness) AS facies_thickness, d13_element.element_ID, d13_element.width, d13_element.width_type, 
d13_element.`length`,d13_element.length_endpoints, d13_element.length_sinuous, d13_element.length_type, 
d13_element.thickness, d13_element.thickness_type,
d13_element.original_interpretation,d13_element.general_type,d06_case_study.name AS case_study_name,
d04_system.name AS system_name, d04_system.age_ID_from AS sys_age_from, d04_system.age_ID_to AS sys_age_to, d04_system.palaeo_lat_range, d04_system.system_ID
FROM d15_facies
JOIN d13_element ON d13_element.element_ID = d15_facies.parent_element_ID
JOIN d07_subset ON d07_subset.subset_ID = d13_element.subset_ID
JOIN d06_case_study ON d06_case_study.case_study_ID = d07_subset.case_study_ID
JOIN d04_system ON d04_system.system_ID = d06_case_study.system_ID
GROUP BY d13_element.element_ID) total
LEFT JOIN
(SELECT SUM(d15_facies.thickness) AS mud_thickness, d13_element.element_ID AS mud_element
FROM d15_facies
JOIN d13_element ON d13_element.element_ID = d15_facies.parent_element_ID
WHERE d15_facies.facies_type IN ('M', '_M', 'C','Z')
GROUP BY d13_element.element_ID) mud
ON total.element_ID = mud.mud_element")

element_ng_all <- left_join(element_ng_query, age_global_events, by = "sys_age_to")

element_ng_all <- element_ng_all %>% mutate(mud_thickness = case_when(is.na(mud_thickness) ~ 0, TRUE ~ mud_thickness)) %>%
  mutate(not_mud_thickness = facies_thickness - mud_thickness) %>% mutate(NG = round(not_mud_thickness/facies_thickness,2)) %>%
  filter(system_name != 'Lower Atoka system')

element_ng_filtered_thck_type <- element_ng_all %>% filter(thickness_type %in% selected_element_thickness_type)

element_ng_thck_element_type <- element_ng_filtered_thck_type %>% filter(general_type %in% selected_elements)

#PLOT
climate_ng_n <- element_ng_thck_element_type %>% group_by(climate, general_type) %>% summarise(n = n(), mean = mean(NG))
  # paste(levels(as.factor(element_ng_thck_element_type$climate)), "\nN=",table(element_ng_thck_element_type$climate),sep="")
  # 


element_ng_climate_plot <- ggplot(element_ng_thck_element_type, aes(x=factor(general_type, level = selected_elements), y = NG, fill = climate))+
  geom_boxplot(outlier.shape = 4, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  geom_text(data = climate_ng_n, aes(x=factor(general_type, level = selected_elements), label = n), y = 1.02 , position = position_dodge2(0.9), size = 7/.pt)+
  scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                               'icehouse' = icehouse_color,
                               'uncertain' = uncertain_color
  ))+
  theme_classic()+
  theme(
  axis.text.x = element_text(color = "black", size = 9),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    plot.title = element_text(color = 'black', size = 10),
    legend.position = 'bottom')+
  labs(
    y= 'Sand fraction',
    fill = 'Climate')
element_ng_climate_plot
ggsave('plots/element_ng_climate_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/element_ng_climate_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')
