library(ggplot2)
library(dplyr)

#
#icehouse greenhouse examples
#

climate_bed_numbers <- beds_table_features %>% group_by(climate) %>% summarise(n = n()) %>% filter(climate != 'both')

climate_bed_numbers_plot <- ggplot(climate_bed_numbers, aes(x=climate, y=n))+geom_bar(stat="identity")+
  ggtitle("Bed count by climate")+
  scale_y_continuous(breaks = seq(0, 10000, by = 1000))+
  theme_light()
climate_bed_numbers_plot

climate_bed_numbers_elements <- beds_table_features %>% group_by(element_general_type, climate) %>% summarise(n = n())%>% filter(climate != 'both')


climate_bed_numbers_elements_plot <- ggplot(climate_bed_numbers_elements, aes(x=element_general_type, y=n, fill = climate))+geom_bar(stat="identity", position = "dodge")+
  ggtitle("Bed count by element and climate")+
  scale_y_continuous(breaks = seq(0, 6100, by = 500))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))
climate_bed_numbers_elements_plot

period_bed_numbers <- beds_table_features %>% group_by(period) %>% summarise(n = n())
period_bed_numbers_plot <- ggplot(period_bed_numbers, aes(x=period, y=n))+geom_bar(stat="identity")+
  ggtitle("Bed count by period")+
  scale_y_continuous(breaks = seq(0, 10000, by = 1000))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))
period_bed_numbers_plot

period_bed_number_element <- beds_table_features %>% group_by(element_general_type,period) %>% summarise(n = n())

case_study_bed_numbers <- beds_table_features %>% group_by(case_study_ID,period) %>% summarise(n = n())

#
#case studies that are missing bed boundaries
#

case_studies_fix <- facies_data %>% filter(is.na(trans_from_below_event_b)) %>% filter(facies_type %in% c('_G','_S','gS','sG','(g)S')) %>%
  filter(thickness_type %in% c('true (maximum)','true (not maximum)','apparent')) %>% group_by(system_ID, case_study_ID, period, cs_name, sys_name) %>% summarise(n = n())
