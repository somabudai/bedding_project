#
#bed type presence in systems
#
system_facies_data <- facies_data %>% select(sys_name) %>% distinct(sys_name)
sys_number <- beds_table_sandgs_lam_features %>% select(sys_name) %>% distinct(sys_name) %>% nrow()
sys_number_allbeds <- beds_table_features %>% select(sys_name) %>% distinct(sys_name)

bed_type_sys_presence <- beds_table_sandgs_lam_features %>% select(gs_category, code2, code, sys_name) %>%
  group_by(gs_category, code2, code) %>% distinct(sys_name, .keep_all = TRUE) %>% summarize(n = n()) %>% mutate(percent_of_systems = round(n/sys_number,2))


bed_type_sys_presence_plot <- ggplot(bed_type_sys_presence, aes(x=code2,y=factor(gs_category, level = gs_order), fill = n))+
  geom_tile(color = 'black', lwd = 0.25)+
  geom_vline(xintercept = seq(1.5,nrow(bed_type_sys_presence)-0.5, 1), alpha = 0.3)+
  geom_hline(yintercept = seq(1.5, 5.5, 1), alpha = 0.3)+
  geom_text(aes(label = n), angle = 90, colour = 'white', size = 3.5)+
  ggtitle(paste('Number of systems a bed represented in, total system number=', sys_number, sep=''))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust= 0.5),axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = 'black', size = 0.23),
        axis.ticks = element_line(colour = 'black', size = 0.23))
bed_type_sys_presence_plot 
