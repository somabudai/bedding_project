source('config.R')


# CALCULATE OVERLYING STACKED FACIES THICKNESS

mud_facies <- c('M','Z','C','_M','gM','(g)M','sM')


total_facies_log <- facies_data %>% select(log_ID, data_order, facies_ID, facies_type, thickness, element_general_type, sys_gs_category, climate) %>% mutate(log_order = paste(log_ID,data_order, sep = '_')) %>%
  filter(!is.na(data_order)) %>% distinct(log_order, .keep_all = TRUE) %>% 
  mutate(simple_facies = case_when(facies_type %in% mud_facies ~ 'M', TRUE ~ facies_type))


facies_bedid <- beds %>% select(facies_ID, artificial_bed_id)
total_facies_log <- left_join(total_facies_log, facies_bedid, by = 'facies_ID')
bedid_code <- beds_table_sandgs_lam_features %>% select(artificial_bed_id, code)
total_facies_log<- left_join(total_facies_log, bedid_code, by = 'artificial_bed_id')
total_facies_log <- total_facies_log %>% mutate(code_facies = case_when(is.na(code) ~ simple_facies, !is.na(code)~ code))
total_facies_log <- total_facies_log %>% mutate(boundary = case_when(data_order != lag(data_order)+1 | artificial_bed_id != lag(artificial_bed_id) | code_facies != lag(code_facies) ~ 1, TRUE ~ 0)) %>%
  mutate(unit_number = cumsum(boundary)) %>% group_by(unit_number) %>% 
  mutate(group_thickness = sum(thickness)) %>% ungroup()

underlying_units <- total_facies_log %>% select(log_ID, data_order, code_facies, unit_number, artificial_bed_id,  sys_gs_category, element_general_type, climate) %>% mutate(log_order = paste(log_ID,data_order, sep = '_')) %>%
  filter(!is.na(data_order)) %>% distinct(log_order, .keep_all = TRUE)

overlying_units <- total_facies_log %>% select(log_ID, data_order, code_facies, unit_number, artificial_bed_id) %>% 
  mutate(lag_data_order = lag(data_order)) %>% filter(data_order != 1) %>%
  rename('log_ID2' = 'log_ID', 'data_order2' = 'data_order', 'code_facies2' = 'code_facies', 
         'unit_number2' ='unit_number', 'artificial_bed_id2'='artificial_bed_id') %>% 
  mutate(log_order = paste(log_ID2,lag_data_order, sep = '_')) %>% filter(!is.na(data_order2))%>% distinct(log_order, .keep_all = TRUE)

unit_transition <- left_join(underlying_units,overlying_units, by = 'log_order') %>% filter(!is.na(log_ID2)) %>% 
  select(log_ID, code_facies, unit_number, artificial_bed_id, code_facies2, unit_number2, artificial_bed_id2, sys_gs_category, element_general_type, climate) %>% filter(unit_number != unit_number2)

unit_thickness <- total_facies_log %>% select(unit_number, group_thickness) %>% distinct(unit_number, .keep_all = TRUE)
unit_thickness2 <- total_facies_log %>% select(unit_number, group_thickness) %>% distinct(unit_number, .keep_all = TRUE) %>% 
  rename('unit_number2' = 'unit_number', 'group_thickness2' = 'group_thickness')

unit_transition <- left_join(unit_transition,unit_thickness, by = 'unit_number')
unit_transition <- left_join(unit_transition,unit_thickness2, by = 'unit_number2')

unit_transition_distinct_bed <- unit_transition %>% filter(!is.na(artificial_bed_id))

# FILTER OVERLYING STACKED MUD THICKNESS

overlying_mud_thickness <- unit_transition %>% filter(!is.na(artificial_bed_id)) %>% filter(code_facies2 == 'M') %>% 
  select(artificial_bed_id, group_thickness2) %>% rename('overlying_mud_thck' = 'group_thickness2')

# ADD IT TO TRANSITION SUMMARY DF

bed_under_over <- left_join(bed_under_over, overlying_mud_thickness, by = 'artificial_bed_id') %>%
  mutate(overlying_mud_thickness = case_when(is.na(overlying_mud_thickness) ~ 'no mud', TRUE ~ overlying_mud_thickness))



# two_systems_comparison_overlying_thck <- unit_transition %>% filter(code_facies != 'G-N-ns-sl') %>%  filter(code_facies2 == 'M') %>%
#   filter(element_general_type %in% selected_elements) %>%
#   group_by(sys_gs_category, element_general_type) %>% summarise(mean = mean(group_thickness2))

#bed type overlying mud system gs comparison
# sandy_top_beds_count_thck_5 
#       gs_top_beds_count_thck_5 

#SANDY

overlying_mud_thickness_sandy <- unit_transition %>% filter(code_facies %in% sandy_top_beds_count_thck_5) %>% filter(code_facies2 == 'M') %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(sys_gs_category == 'sandy system')

overlying_mud_thickness_sandy_sum <- overlying_mud_thickness_sandy %>% group_by(element_general_type, code_facies) %>% summarise(n=n(), mean = mean(group_thickness2))

overlying_mud_thickness_sandy_plot <- ggplot(overlying_mud_thickness_sandy, aes(x=code_facies,y=group_thickness2, 
                                                                               fill=factor(element_general_type, level=selected_elements)))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  geom_text(data = overlying_mud_thickness_sandy_sum, aes(x=code_facies, label = n), y=2.2, size = 7/.pt, angle = 90, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  coord_cartesian(ylim = c(0,2.2))+
  theme_classic()+
  theme(legend.position = 'bottom')+
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(face = 'bold', size = 7),
    axis.text.x = element_text(color = "black", size = 9, angle = 45, vjust = 1, hjust = 1),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    plot.title = element_text(color = 'black', size = 10),
    legend.position = 'bottom',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    title = 'Sandy systems',
    y= 'Bed thickness',
    fill = 'Element')
overlying_mud_thickness_sandy_plot

#GS

overlying_mud_thickness_gs <- unit_transition %>% filter(code_facies %in% gs_top_beds_count_thck_5) %>% filter(code_facies2 == 'M') %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(sys_gs_category == 'gravelly-sand system') %>%
  group_by(element_general_type, code_facies) %>%
  mutate(n = n()) %>% ungroup() %>% filter(n>=5) %>%
  filter(code_facies != 'G-N-ns-sl')

overlying_mud_thickness_gs_GNnssl <- unit_transition %>% filter(code_facies %in% gs_top_beds_count_thck_5) %>% filter(code_facies2 == 'M') %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(sys_gs_category == 'gravelly-sand system') %>%
  filter(code_facies == 'G-N-ns-sl')

overlying_mud_thickness_gs_sum <- unit_transition %>% filter(code_facies %in% gs_top_beds_count_thck_5) %>% filter(code_facies2 == 'M') %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(sys_gs_category == 'gravelly-sand system') %>% group_by(element_general_type, code_facies) %>% summarise(n=n(), mean = mean(group_thickness2))%>%
  filter(n>=5) %>%
  filter(code_facies != 'G-N-ns-sl')

overlying_mud_thickness_gs_plot <- ggplot(overlying_mud_thickness_gs, aes(x=code_facies,y=group_thickness2, 
                                                                                fill=factor(element_general_type, level=selected_elements)))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  geom_text(data = overlying_mud_thickness_gs_sum, aes(x=code_facies, label = n), y=0.6, size = 7/.pt, angle = 90, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  coord_cartesian(ylim = c(0,0.7))+
  theme_classic()+
  theme(legend.position = 'bottom')+
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(face = 'bold', size = 7),
    axis.text.x = element_text(color = "black", size = 9, angle = 45, vjust = 1, hjust = 1),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    plot.title = element_text(color = 'black', size = 10),
    legend.position = 'bottom',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    title = 'Gravelly-sandy systems',
    y= 'Bed thickness (m)',
    fill = 'Element')
overlying_mud_thickness_gs_plot


overlying_mud_thickness_combined <- ggarrange(overlying_mud_thickness_sandy_plot, 
                                              overlying_mud_thickness_gs_plot,
                                                 widths = c(0.8,1),
                                                 labels = c('A','B'),
                                                 font.label = (size = 10),
                                                 common.legend = TRUE, legend = 'bottom')
overlying_mud_thickness_combined
ggsave('plots/overlying_mud_thickness_combined.pdf', width = 190, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/overlying_mud_thickness_combined.jpeg', width = 190, height = 70, units = 'mm', device = 'jpeg')

# CLIMATE

overlying_mud_thck_sandy <- bed_under_over %>% filter(!is.na(overlying_mud_thck))  %>%
  filter(code %in% sandy_top_beds_count_thck_5) %>% filter(element_general_type %in% selected_elements) %>%  
  filter(sys_gs_category == 'sandy system') %>% filter(climate %in% c('icehouse','greenhouse'))

overlying_mud_thck_sandy_summary <- overlying_mud_thck_sandy %>% group_by(climate, element_general_type, code) %>% summarise(n = n(), mean_overl_mud_thck = mean(overlying_mud_thck))

climate_overl_mud_thickness_comparison_plot <- function (data, sum_data, selected_element, ymax ,title) {
  
  data <- data %>% filter(element_general_type == selected_element)
  sum_data <- sum_data %>% filter(element_general_type == selected_element)
  plot <-  ggplot(data, aes(x=code,y=overlying_mud_thck, fill= climate))+
    geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
    stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
    geom_text(data = sum_data, aes(x=code, label = n), y = ymax+0.01 , position = position_dodge2(0.9), size = 7/.pt)+
    coord_cartesian(ylim = c(0,ymax))+
    scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                                 'icehouse' = icehouse_color
    ))+
    theme_classic()+
    theme(legend.position = 'bottom')+
    theme(
      legend.text = element_text(size = 7),
      legend.title = element_text(face = 'bold', size = 7),
      axis.text.x = element_text(color = "black", size = 9),
      axis.title.x=element_blank(),
      axis.line = element_line(colour = 'black', size = 0.24),
      axis.ticks = element_line(colour = 'black', size = 0.24),
      axis.text.y = element_text(color = "black", size = 9),
      axis.title.y = element_text(color = "black", size = 9),
      plot.title = element_text(color = 'black', size = 10),
      legend.position = 'bottom',
      legend.key.height = unit(3.5, 'mm'),
      legend.key.width = unit(5, 'mm'))+
    labs(
      title = title,
      y= 'Bed thickness',
      fill = 'Climate')
  return(plot)
} 

#SANDY

overl_mud_thck_sandy_terminal_plot <- climate_overl_mud_thickness_comparison_plot(overlying_mud_thck_sandy, overlying_mud_thck_sandy_summary,'terminal deposit',1.5, 'Terminal deposits')
overl_mud_thck_sandy_terminal_plot
overl_mud_thck_sandy_channel_plot <- climate_overl_mud_thickness_comparison_plot(overlying_mud_thck_sandy, overlying_mud_thck_sandy_summary,'channel',1.5, 'Channel deposits')
overl_mud_thck_sandy_channel_plot

overlying_mud_thickness_climate_sandy <- unit_transition %>% 
  filter(code_facies %in% sandy_top_beds_count_thck_5) %>%
  filter(code_facies2 == 'M') %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(sys_gs_category == 'sandy system') %>%
  group_by(climate, element_general_type, code_facies) %>%
  mutate(n = n()) %>% ungroup()

overlying_mud_thickness_climate_sandy_sum <- overlying_mud_thickness_climate_sandy %>% group_by(climate,element_general_type, code_facies) %>% summarise(n=n(), mean = mean(group_thickness2))

overlying_mud_thickness_climate_sandy_plot <- ggplot(overlying_mud_thickness_climate_sandy, aes(x=code_facies,y=group_thickness2, 
                                                                                fill=climate))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  geom_text(data = overlying_mud_thickness_climate_sandy_sum, aes(x=code_facies, label = n), y=2.2, size = 7/.pt, angle = 90, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                               'icehouse' = icehouse_color
  ))+
  facet_wrap(~ element_general_type, ncol = 1)+
  coord_cartesian(ylim = c(0,2.2))+
  theme_classic()+
  theme(legend.position = 'bottom')+
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(face = 'bold', size = 7),
    axis.text.x = element_text(color = "black", size = 9, angle = 45, vjust = 1, hjust = 1),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    plot.title = element_text(color = 'black', size = 10),
    legend.position = 'bottom',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    title = 'Sandy systems',
    y= 'Bed thickness',
    fill = 'Element')
overlying_mud_thickness_climate_sandy_plot


### OLD CODE ------------

ggplot(overlying_mud_thickness, aes(x=code_facies, y=group_thickness2, fill = sys_gs_category)) +
  geom_boxplot(outlier.shape = NA, lwd=0.23, position = position_dodge(.9))+
  stat_summary(fun.y = 'mean', size = 0.3, shape = 15, position = position_dodge(.9))+
  coord_cartesian(ylim = c(0,1.5))+
  theme_classic()

#
#sandy system
#
overlying_mud_comp_sandy_system <- unit_transition %>% filter(sys_gs_category == 'sandy system') %>%
  filter(code_facies %in% sandy_system_top_beds) %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(code_facies2 == 'M')
overlying_mud_comp_sandy_system_summ <- overlying_mud_comp_sandy_system %>% group_by(code_facies, element_general_type) %>% summarise(mean = mean(group_thickness2), n = n())

overlying_mud_thck_sandy_sys_plot <- ggplot(overlying_mud_comp_sandy_system, aes(x=factor(code_facies, level = sandy_system_top_beds),y=group_thickness2, fill=factor(element_general_type, level=selected_elements)))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, position = position_dodge(.9))+
  stat_summary(fun.y = 'mean', size = 0.3, shape = 15, position = position_dodge(.9))+
  coord_cartesian(ylim = c(0,2))+
  theme_classic()+
  theme(legend.position = 'bottom')+
  ggtitle('Overlying mud thickness, sandy systems')+
  theme(axis.line = element_line(colour = 'black', size = 0.23),
        axis.ticks = element_line(colour = 'black', size = 0.23),
        axis.title.x=element_blank())+
  labs(
    y= 'Mud thickness',
    fill = 'Element')
overlying_mud_thck_sandy_sys_plot

#
#gravelly sandy system
#
overlying_mud_comp_sandy_system <- unit_transition %>% filter(sys_gs_category == 'gravelly-sand system') %>%
  filter(code_facies %in% gs_system_top_beds) %>%
  filter(element_general_type %in% selected_elements) %>%
  filter(code_facies2 == 'M')


overlying_mud_thck_gs_sys_plot <- ggplot(overlying_mud_comp_sandy_system, aes(x=factor(code_facies, level = gs_system_top_beds),y=group_thickness2, fill=factor(element_general_type, level=selected_elements)))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, position = position_dodge(.9))+
  stat_summary(fun.y = 'mean', size = 0.3, shape = 15, position = position_dodge(.9))+
  coord_cartesian(ylim = c(0,2))+
  theme_classic()+
  theme(legend.position = 'bottom')+
  ggtitle('Overlying mud thickness, gravelly-sandy systems')+
  theme(axis.line = element_line(colour = 'black', size = 0.23),
        axis.ticks = element_line(colour = 'black', size = 0.23),
        axis.title.x=element_blank())+
  labs(
    y= 'Mud thickness',
    fill = 'Element')
overlying_mud_thck_gs_sys_plot

