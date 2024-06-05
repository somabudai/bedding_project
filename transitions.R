library("viridis") 
mud_facies <- c('M','Z','C','_M','gM','(g)M','sM')
sand_facies <- c('_S','mS')
gsand_facies <- c('gS','(g)S')
gravel_facies <- c('_G','mG','sG')

### CREATE TRANSITION TABLE-----------------------

#underlying facies
facies1 <- facies_data %>% select(log_ID, data_order, facies_ID, facies_type) %>% mutate(log_order = paste(log_ID,data_order, sep = '_')) %>%
  filter(!is.na(data_order)) %>% distinct(log_order, .keep_all = TRUE)
#overlying facies
facies2 <- facies_data %>% select(log_ID, data_order,facies_ID, facies_type) %>% mutate(lag_data_order = lag(data_order)) %>%
  filter(data_order != 1) %>%
  rename('log_ID2' = 'log_ID', 'data_order2' = 'data_order', 'facies_ID2' = 'facies_ID', 
          'data_order_lag' ='lag_data_order' ,'facies_type2' = 'facies_type') %>% 
  mutate(log_order = paste(log_ID2,data_order_lag, sep = '_')) %>% filter(!is.na(data_order2))%>% distinct(log_order, .keep_all = TRUE)
#transition table
transition <- left_join(facies1,facies2, by = 'log_order') %>% filter(!is.na(log_ID2))
#
#add bed bed types to the transition table
#

#underlying bed
facies_bedid <- beds %>% select(facies_ID, artificial_bed_id)
transition <- left_join(transition, facies_bedid, by = 'facies_ID')
bedid_code <- beds_table_sandgs_lam %>% select(artificial_bed_id, code)
transition <- left_join(transition, bedid_code, by = 'artificial_bed_id')
transition  <- transition  %>% rename('bed1' = 'artificial_bed_id', 'code1' = 'code') %>% mutate(bed1 = case_when(is.na(bed1) ~ 0, TRUE ~ bed1))

#overlying bed
facies_bedid2 <- beds %>% select(facies_ID, artificial_bed_id) %>% rename('facies_ID2' = 'facies_ID')
transition <- left_join(transition, facies_bedid2, by = 'facies_ID2')
bedid_code2 <- beds_table_sandgs_lam %>% select(artificial_bed_id, code)
transition <- left_join(transition, bedid_code, by = 'artificial_bed_id')
transition  <- transition  %>% rename('bed2' = 'artificial_bed_id', 'code2' = 'code') %>% mutate(bed2 = case_when(is.na(bed2) ~ 0, TRUE ~ bed2 ))

#transition table with bed codes only the one facies below and above is present
transition <- transition %>% mutate(facies_under = case_when(is.na(code1) ~ facies_type, !is.na(code1) ~ code1 )) %>%
  mutate(facies_over = case_when(is.na(code2) ~ facies_type2, !is.na(code2) ~ code2 )) %>% filter(bed1 != bed2)

#
# under over only gs
#

underlying_facies <- transition %>% select(bed2, facies_type, code1) %>% filter(bed2 != 0) %>% rename('artificial_bed_id' = 'bed2', 'underlying_ft' = 'facies_type', 'underlying_bt' = 'code1')
overlying_facies <- transition %>% select(bed1, facies_type2, code2) %>% filter(bed1 != 0) %>% rename('artificial_bed_id' = 'bed1', 'overlying_ft' = 'facies_type2', 'overlying_bt' = 'code2')

bed_under_over <- full_join(underlying_facies,overlying_facies, by = 'artificial_bed_id' ) %>% replace(is.na(.), 'unknown')
           
bed_under_over <- left_join(beds_table_sandgs_lam_features, bed_under_over, by = 'artificial_bed_id') %>% mutate(underlying_ft = case_when(underlying_ft %in% mud_facies ~ 'M', underlying_ft %in% sand_facies ~ 'S',
                                                                                                                                           underlying_ft %in% gsand_facies ~ 'gS', underlying_ft %in% gravel_facies ~ 'G',
                                                                                                                                           TRUE ~ underlying_ft)) %>%
  mutate(overlying_ft = case_when(overlying_ft %in% mud_facies ~ 'M', overlying_ft %in% sand_facies ~ 'S',
                                  overlying_ft %in% gsand_facies ~ 'gS', overlying_ft %in% gravel_facies ~ 'G',
                                  TRUE ~ overlying_ft))


### FACIES TRANSITION PLOTS-----------------------


#      list of beds that are above 5% based on either count or thickness 
#       sandy_top_beds_count_thck_5 
#       gs_top_beds_count_thck_5 
#       common_top_beds_count_thck_5 

calculate_facies_transition_count_allbedtype <- function(data,input_col,col1,val1,col2,val2,col3,val3) {
  input_data <- data %>%
    filter({{input_col}} != 'unknown') %>%
    filter({{input_col}} != 'non clastic') %>%
    filter({{col1}} == {{val1}}) %>% 
    filter({{col2}} == {{val2}}) %>% 
    filter({{col3}} == {{val3}})
  
  output <- input_data %>% select({{input_col}}) %>% group_by({{input_col}}) %>%
    summarise(n=n()) %>% ungroup() %>% mutate(sum_n = sum(n)) %>% 
    mutate(percent = round(n*100/sum_n,2))
  return(output)
}


#calculate under/overlying porportions per bed type
calculate_facies_transition_count <- function(data,input_col,selected_beds,col1,val1,col2,val2,col3,val3) {
  input_data <- data %>%
    filter(code %in% {{selected_beds}}) %>%
    filter({{input_col}} != 'unknown') %>%
    filter({{input_col}} != 'non clastic') %>%
    filter({{col1}} == {{val1}}) %>% 
    filter({{col2}} == {{val2}}) %>% 
    filter({{col3}} == {{val3}})
  
  output <- input_data %>% select(code,{{input_col}}) %>% group_by(code,{{input_col}}) %>%
    summarise(n=n()) %>% ungroup() %>% group_by(code) %>% mutate(sum_n = sum(n)) %>% 
    ungroup() %>% mutate(percent = round(n*100/sum_n,2))
  return(output)
}

facies_transition_plot <- function(data,x,y,fill, selected_beds, title) {
  gs_fill_order = c('G','gS','S','heterolithic','M')
  
  #add colors
  # data <- data %>% mutate(color = case_when({{fill}} == 'G' ~ 'darkorange', {{fill}} == 'gS' ~ 'gold3', {{fill}} == 'S' ~ 'gold1',
  #                                           {{fill}} == 'heterolithic' ~ 'grey80', {{fill}} == 'M' ~ 'grey60'))
  # costum_colors = c('darkorange','gold3','gold1','grey80','grey60')
  
  counts <- data %>% select({{x}},sum_n) %>% distinct(code, .keep_all = TRUE)
  
  transition_plot <- ggplot(data, aes(fill = factor({{fill}}, level = gs_fill_order), x=factor({{x}}, level = {{selected_beds}}), y={{y}}))+
    geom_bar(position = 'stack', stat ='identity', colour = 'black', lwd=0.23)+
    geom_text(aes(x = code, y=103 , label=sum_n, fill = NULL), data = counts, size = 7/.pt)+
    scale_fill_manual(values = c('G' = 'darkorange',
                                 'gS' = 'gold3',
                                 'S' = 'gold1',
                                 'heterolithic' = 'grey80',
                                 'M' = 'grey60'), drop = FALSE)+
    labs(
      x='',
      y= 'overlying facies porportions',
      title = title,
      fill = 'Facies'
    )+
    scale_y_continuous(limits = c(0,105),expand = c(0,0))+
    scale_x_discrete(drop = FALSE)+
    theme_classic()+
    theme(legend.position= 'bottom',
          legend.text = element_text(size = 7),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(3, 'mm'),
          axis.text.x = element_text(color = "black", size = 7, angle = 45, vjust = 1, hjust = 1),
          axis.title.x= element_text(color = "black", size = 9),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.title.y= element_text(color = "black", size = 9),
          axis.text.y = element_text(color = "black", size = 7),
          plot.title = element_text(color = 'black', size = 10))
  
  return(transition_plot)
  

}

#
#all systems
#
underlying_facies_count <- calculate_facies_transition_count(bed_under_over,underlying_ft,common_top_beds_count_thck_5)
underlying_facies_plot <- facies_transition_plot(underlying_facies_count,code,percent,underlying_ft,common_top_beds_count_thck_5,'Underlying facies percentage (count)')
underlying_facies_plot
overlying_facies_count <- calculate_facies_transition_count(bed_under_over,overlying_ft,common_top_beds_count_thck_5)
overlying_facies_plot <- facies_transition_plot(overlying_facies_count,code,percent,overlying_ft,common_top_beds_count_thck_5,'Overlying facies percentage (count)')
overlying_facies_plot

#
#sandy systems
#
  
    #terminal deposits
    
    underlying_fc_sandy_terminal<- calculate_facies_transition_count(bed_under_over,underlying_ft, sandy_top_beds_count_thck_5, element_general_type,'terminal deposit',sys_gs_category,'sandy system') 
    underlying_facies_sandy_terminal_plot <- facies_transition_plot(underlying_fc_sandy_terminal,code,percent,underlying_ft,sandy_top_beds_count_thck_5,'Underlying facies percentage: sandy terminal deposits (count)')
    underlying_facies_sandy_terminal_plot
    ggsave('sandy_term_under.pdf',underlying_facies_sandy_terminal_plot,device='pdf')
    overlying_fc_sandy_terminal <- calculate_facies_transition_count(bed_under_over,overlying_ft,sandy_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'sandy system')
    overlying_fc_sandy_terminal_allbedtype <- calculate_facies_transition_count_allbedtype(bed_under_over,overlying_ft,element_general_type,'terminal deposit',sys_gs_category,'sandy system')
    overlying_facies_sandy_terminal_plot <- facies_transition_plot(overlying_fc_sandy_terminal,code,percent,overlying_ft,sandy_top_beds_count_thck_5,'Terminal deposits (count)')
    overlying_facies_sandy_terminal_plot
    ggsave('sandy_term_over.pdf',overlying_facies_sandy_terminal_plot,device='pdf')
    
    
    #channel deposits
    underlying_fc_sandy_channel<- calculate_facies_transition_count(bed_under_over,underlying_ft,sandy_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'sandy system')

    underlying_facies_sandy_channel_plot <- facies_transition_plot(underlying_fc_sandy_channel,code,percent,underlying_ft,sandy_top_beds_count_thck_5,'Underlying facies percentage: sandy channel deposits (count)')
    underlying_facies_sandy_channel_plot
    ggsave('sandy_ch_under.pdf',underlying_facies_sandy_channel_plot,device='pdf')
    overlying_fc_sandy_channel <- calculate_facies_transition_count(bed_under_over,overlying_ft,sandy_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'sandy system')
    overlying_fc_sandy_channel_allbedtype <- calculate_facies_transition_count_allbedtype(bed_under_over,overlying_ft,element_general_type,'channel',sys_gs_category,'sandy system')
    overlying_facies_sandy_channel_plot <- facies_transition_plot(overlying_fc_sandy_channel,code,percent,overlying_ft,sandy_top_beds_count_thck_5,'Channel deposits (count)')
    overlying_facies_sandy_channel_plot
    ggsave('sandy_ch_over.pdf',overlying_facies_sandy_channel_plot,device='pdf')
    
    #levee deposits
    underlying_fc_sandy_levee<- calculate_facies_transition_count(bed_under_over,underlying_ft,sandy_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'sandy system')

    underlying_facies_sandy_levee_plot <- facies_transition_plot(underlying_fc_sandy_levee,code,percent,underlying_ft,sandy_top_beds_count_thck_5,'Underlying facies percentage: sandy levee deposits (count)')
    underlying_facies_sandy_levee_plot
    ggsave('sandy_levee_under.pdf',underlying_facies_sandy_levee_plot,device='pdf')
    overlying_fc_sandy_levee <- calculate_facies_transition_count(bed_under_over,overlying_ft,sandy_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'sandy system') 

    overlying_facies_sandy_levee_plot <- facies_transition_plot(overlying_fc_sandy_levee,code,percent,overlying_ft,sandy_top_beds_count_thck_5,'Levee deposits (count)')
    overlying_facies_sandy_levee_plot
    ggsave('sandy_levee_over.pdf',overlying_facies_sandy_levee_plot,device='pdf')

    
#OVERLYIG COMBINED    
    
    overlying_facies_sandy_combined_plot <- ggarrange(overlying_facies_sandy_terminal_plot, 
                                                      overlying_facies_sandy_channel_plot , 
                                                      overlying_facies_sandy_levee_plot,
                                                        labels = c('A','C','E'),
                                                        font.label = list(size = 10, face = "bold", color ="black"),
                                                        nrow = 3,
                                                        ncol = 1,
                                                        common.legend = TRUE, legend = 'bottom')
    
    
    overlying_facies_sandy_combined_plot <- annotate_figure(overlying_facies_sandy_combined_plot,
                                                              top = text_grob('Sandy Systems', face = 'bold', size = 10))
    
    overlying_facies_sandy_combined_plot   
    
#
# gravelly sandy systems
#

    #terminal deposits
    underlying_fc_gs_terminal<- calculate_facies_transition_count(bed_under_over,underlying_ft,gs_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'gravelly-sand system') 

    underlying_facies_gs_terminal_plot <- facies_transition_plot(underlying_fc_gs_terminal,code,percent,underlying_ft,gs_top_beds_count_thck_5,'Underlying facies percentage: gs terminal deposits (count)')
    underlying_facies_gs_terminal_plot
    ggsave('gs_term_under.pdf',underlying_facies_gs_terminal_plot,device='pdf')
    overlying_fc_gs_terminal <- calculate_facies_transition_count(bed_under_over,overlying_ft,gs_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'gravelly-sand system') 
    overlying_fc_gs_terminal_allbedtype <- calculate_facies_transition_count_allbedtype(bed_under_over,overlying_ft,element_general_type,'terminal deposit',sys_gs_category,'gravelly-sand system')
    overlying_facies_gs_terminal_plot <- facies_transition_plot(overlying_fc_gs_terminal,code,percent,overlying_ft,gs_top_beds_count_thck_5,'Terminal deposits (count)')
    overlying_facies_gs_terminal_plot
    ggsave('gs_term_over.pdf',overlying_facies_gs_terminal_plot,device='pdf')
    
    
    #channel deposits
    underlying_fc_gs_channel<- calculate_facies_transition_count(bed_under_over,underlying_ft,gs_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'gravelly-sand system')

    underlying_facies_gs_channel_plot <- facies_transition_plot(underlying_fc_gs_channel,code,percent,underlying_ft,gs_top_beds_count_thck_5,'Underlying facies percentage: gs channel deposits (count)')
    underlying_facies_gs_channel_plot
    ggsave('gs_ch_under.pdf',underlying_facies_gs_channel_plot,device='pdf')
    overlying_fc_gs_channel <- calculate_facies_transition_count(bed_under_over,overlying_ft,gs_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'gravelly-sand system') 

    overlying_facies_gs_channel_plot <- facies_transition_plot(overlying_fc_gs_channel,code,percent,overlying_ft,gs_top_beds_count_thck_5,'Channel deposits (count)')
    overlying_facies_gs_channel_plot
    ggsave('gs_ch_over.pdf',underlying_facies_gs_channel_plot,device='pdf')
    
    #levee deposits
    underlying_fc_gs_levee<- calculate_facies_transition_count(bed_under_over,underlying_ft,gs_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'gravelly-sand system')

    underlying_facies_gs_levee_plot <- facies_transition_plot(underlying_fc_gs_levee,code,percent,underlying_ft,gs_top_beds_count_thck_5,'Underlying facies percentage: gs levee deposits (count)')
    underlying_facies_gs_levee_plot
    ggsave('gs_levee_under.pdf',underlying_facies_gs_levee_plot,device='pdf')
    overlying_fc_gs_levee <- calculate_facies_transition_count(bed_under_over,overlying_ft,gs_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'gravelly-sand system')
    overlying_facies_gs_levee_plot <- facies_transition_plot(overlying_fc_gs_levee,code,percent,overlying_ft,gs_top_beds_count_thck_5,'Levee deposits (count)')
    overlying_facies_gs_levee_plot
    ggsave('gs_levee_over.pdf',overlying_facies_gs_levee_plot,device='pdf')


#OVERLYING COMBINED GS
    
    overlying_facies_gs_combined_plot <- ggarrange(overlying_facies_gs_terminal_plot, 
                                                     overlying_facies_gs_channel_plot, 
                                                     overlying_facies_gs_levee_plot,
                                                     labels = c('B','D','F'),
                                                     font.label = list(size = 10, face = "bold", color ="black"),
                                                     nrow = 3,
                                                     ncol = 1,
                                                     common.legend = TRUE, legend = 'bottom')
    
    
    overlying_facies_gs_combined_plot <- annotate_figure(overlying_facies_gs_combined_plot,
                                                           top = text_grob('Gravelly-sandy system', face = 'bold', size = 10))
    
    overlying_facies_gs_combined_plot
  
    
#OVERLYING COMBINED PLOT
    overlying_facies_combined_plot <- ggarrange(overlying_facies_sandy_combined_plot, 
                                                  overlying_facies_gs_combined_plot, 
                                                  nrow = 1,
                                                  ncol = 2,
                                                  widths = c(0.8,1),
                                                  common.legend = TRUE, legend = 'bottom')
    overlying_facies_combined_plot
    ggsave('plots/overlying_facies_combined_plot.pdf', device = 'pdf', width = 190, height = 210, units = 'mm')
    ggsave('plots/overlying_facies_combined_plot.jpeg', device = 'jpeg', width = 190, height = 210, units = 'mm')
    
    
#CLIMATE
    underlying_fc_sandy_terminal_greenhouse<- calculate_facies_transition_count(bed_under_over,underlying_ft, sandy_top_beds_count_thck_5, element_general_type,'terminal deposit',sys_gs_category,'sandy system',climate,'greenhouse') 
    underlying_facies_sandy_terminal_greenhouse_plot <- facies_transition_plot(underlying_fc_sandy_terminal,code,percent,underlying_ft,sandy_top_beds_count_thck_5,'Underlying facies percentage: sandy terminal deposits (count)')
    underlying_facies_sandy_terminal_plot
    overlying_fc_sandy_terminal_greenhouse <- calculate_facies_transition_count(bed_under_over,overlying_ft,sandy_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'sandy system',climate,'icehouse')
    
### OVERLYING FACIES OR BED TYPE -----------------------
#

under_over_bed_type <- transition 

transition_count <- under_over_bed_type %>% group_by(facies_under,facies_over) %>% summarise(n = n())

### BEDS DIRECT TRANSITION PLOTS-----------------------


#      list of beds that are above 5% based on either count or thickness 
#       sandy_top_beds_count_thck_5 
#       gs_top_beds_count_thck_5 
#       common_top_beds_count_thck_5 


calculate_bed_direct_transition_count <- function(data,input_col, selected_beds, col1,val1,col2,val2,col3,val3) {
  input_data <- data %>% 
    mutate(underlying_bt = case_when(underlying_ft == 'unknown' ~ 'no trans', underlying_bt == 'unknown' ~ 'not bedded', !underlying_bt %in% {{selected_beds}} ~ 'other bed type',  underlying_bt %in% {{selected_beds}} ~ underlying_bt)) %>%
    mutate(overlying_bt = case_when(overlying_ft == 'unknown' ~ 'no trans', overlying_bt == 'unknown' & overlying_ft == 'M' ~ 'M', overlying_bt == 'unknown' ~ 'not bedded', !overlying_bt %in% {{selected_beds}} ~ 'other bed type', overlying_bt %in% {{selected_beds}} ~ overlying_bt)) %>%
    filter(code %in% {{selected_beds}}) %>%
    filter(overlying_bt != 'non clastic') %>%
    filter({{input_col}} != 'no trans') %>%
    filter({{col1}} == {{val1}}) %>% 
    filter({{col2}} == {{val2}}) %>% 
    filter({{col3}} == {{val3}})

  output <- input_data %>% select(code,{{input_col}}) %>% group_by(code,{{input_col}}) %>%
    summarise(n=n()) %>% ungroup() %>% group_by(code) %>% mutate(sum_n = sum(n)) %>%
    ungroup() %>% mutate(percent = round(n*100/sum_n,2))
  return(output)
}

direct_bed_transition_plot <- function(input_data,selected_beds,fill_col,title) {
  
  fill_factor = common_top_beds_count_thck_5 %>% append('other bed type') %>% append('not bedded')
  colors <- brewer.pal(12, 'Paired')
  colors[13] <- "grey60"
  colors[14] <- "grey80"
  
  counts <- input_data %>% select(code,sum_n) %>% distinct(code,.keep_all = TRUE)
  
  plot <- ggplot(input_data, aes(x=factor(code, level = {{selected_beds}}), y=percent,fill=factor({{fill_col}}, level = fill_factor)))+
    geom_bar(position = position_stack(reverse = TRUE), stat ='identity', width = 0.7, colour = 'black', lwd=0.23)+
    geom_text(data = counts, aes(x = code, y=103 , label=sum_n, fill = NULL), size = 7/.pt)+
    scale_y_continuous(limits = c(0,110),expand = c(0,0))+
    scale_x_discrete(drop = FALSE)+
    scale_fill_manual(values = colors, drop = FALSE)+
    theme_classic()+
    labs(
      x='',
      y= 'overlying bed type proportion',
      fill = 'Bed type',
      title = title
    )+
    theme(legend.position= 'bottom',
          legend.text = element_text(size = 7),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(3.5, 'mm'),
          legend.key.width = unit(5, 'mm'),
          axis.text.x = element_text(color = "black", size = 7, angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.text.y = element_text(color = "black", size = 9),
          axis.title.y = element_text(color = "black", size = 9),
          plot.title = element_text(color = 'black', size = 10),
    )+
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 3))
  return(plot)
}
#SANDY SYSTEMS
overlying_direct_bed_sandy_terminal<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'sandy system')
overlyin_direct_bed_sandy_terminal_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_terminal,sandy_top_beds_count_thck_5,overlying_bt,'Terminal deposits')
overlyin_direct_bed_sandy_terminal_plot

overlying_direct_bed_sandy_channel<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'sandy system')
overlyin_direct_bed_sandy_channel_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_channel,sandy_top_beds_count_thck_5,overlying_bt,'Channel deposits')
overlyin_direct_bed_sandy_channel_plot

overlying_direct_bed_sandy_levee<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'sandy system')
overlyin_direct_bed_sandy_levee_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_levee,sandy_top_beds_count_thck_5,overlying_bt,'Levee deposits')
overlyin_direct_bed_sandy_levee_plot

#COMBINED SANDY PLOT
overlying_bed_sandy_combined_plot <- ggarrange(overlyin_direct_bed_sandy_terminal_plot, 
                                               overlyin_direct_bed_sandy_channel_plot, 
                                               overlyin_direct_bed_sandy_levee_plot,
                                                    labels = c('A','C','E'),
                                                    font.label = list(size = 10, face = "bold", color ="black"),
                                                    nrow = 3,
                                                    ncol = 1,
                                                    common.legend = TRUE, legend = 'bottom')


overlying_bed_sandy_combined_plot <- annotate_figure(overlying_bed_sandy_combined_plot,
                                                          top = text_grob('Sandy Systems', face = 'bold', size = 10))

overlying_bed_sandy_combined_plot



#GRAVELLY SANDY SYSTEMS
overlying_direct_bed_gs_terminal<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,gs_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'gravelly-sand system')
overlyin_direct_bed_gs_terminal_plot <- direct_bed_transition_plot(overlying_direct_bed_gs_terminal,gs_top_beds_count_thck_5,overlying_bt,'Terminal deposits')
overlyin_direct_bed_gs_terminal_plot

overlying_direct_bed_gs_channel<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,gs_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'gravelly-sand system')
overlyin_direct_bed_gs_channel_plot <- direct_bed_transition_plot(overlying_direct_bed_gs_channel,gs_top_beds_count_thck_5,overlying_bt,'Channel deposits')
overlyin_direct_bed_gs_channel_plot

overlying_direct_bed_gs_levee<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,gs_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'gravelly-sand system')
overlyin_direct_bed_gs_levee_plot <- direct_bed_transition_plot(overlying_direct_bed_gs_levee,gs_top_beds_count_thck_5,overlying_bt,'Levee deposits')
overlyin_direct_bed_gs_levee_plot

#COMBINED GSS PLOT
overlying_bed_gs_combined_plot <- ggarrange(overlyin_direct_bed_gs_terminal_plot, 
                                               overlyin_direct_bed_gs_channel_plot, 
                                               overlyin_direct_bed_gs_levee_plot,
                                               labels = c('B','D','F'),
                                               font.label = list(size = 10, face = "bold", color ="black"),
                                               nrow = 3,
                                               ncol = 1,
                                               common.legend = TRUE, legend = 'bottom')


overlying_bed_gs_combined_plot <- annotate_figure(overlying_bed_gs_combined_plot ,
                                                     top = text_grob('Gravelly-sandy systems', face = 'bold', size = 10))

overlying_bed_gs_combined_plot

#COMBINED OVERLYING BED PLOT


overlying_bed_combined_plot <- ggarrange(overlying_bed_sandy_combined_plot, 
                                         overlying_bed_gs_combined_plot, 
                                              widths = c(0.7,1),
                                              nrow = 1,
                                              ncol = 2,
                                              common.legend = TRUE, legend = 'bottom')
overlying_bed_combined_plot
ggsave('plots/overlying_bed_combined_plot.pdf', device = 'pdf', width = 190, height = 210, units = 'mm')
ggsave('plots/overlying_bed_combined_plot.jpeg', device = 'jpeg', width = 190, height = 210, units = 'mm')


#CLIMATE
#SANDY SYSTEMS GREEENHOUSE
overlying_direct_bed_sandy_terminal_greenhouse<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'sandy system', climate, 'greenhouse')
overlyin_direct_bed_sandy_terminal_greenhouse_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_terminal_greenhouse,sandy_top_beds_count_thck_5,overlying_bt,'Terminal deposits - greenhouse')
overlyin_direct_bed_sandy_terminal_greenhouse_plot

overlying_direct_bed_sandy_channel_greenhouse<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'sandy system', climate, 'greenhouse')
overlyin_direct_bed_sandy_channel_greenhouse_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_channel_greenhouse,sandy_top_beds_count_thck_5,overlying_bt,'Channel deposits - greenhouse')
overlyin_direct_bed_sandy_channel_greenhouse_plot

overlying_direct_bed_sandy_levee_greenhouse<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'sandy system', climate, 'greenhouse')
overlyin_direct_bed_sandy_levee_greenhouse_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_levee_greenhouse,sandy_top_beds_count_thck_5,overlying_bt,'Levee deposits - greenhouse')
overlyin_direct_bed_sandy_levee_greenhouse_plot

overlying_direct_bed_sandy_greenhouse_plot <- ggarrange(overlyin_direct_bed_sandy_terminal_greenhouse_plot, 
                                                        overlyin_direct_bed_sandy_channel_greenhouse_plot, 
                                                        overlyin_direct_bed_sandy_levee_greenhouse_plot,
                                               labels = c('A','C','E'),
                                               font.label = list(size = 10, face = "bold", color ="black"),
                                               nrow = 3,
                                               ncol = 1,
                                               common.legend = TRUE, legend = 'bottom')


overlying_direct_bed_sandy_greenhouse_plot <- annotate_figure(overlying_direct_bed_sandy_greenhouse_plot,
                                                     top = text_grob('Greenhouse', face = 'bold', size = 10))

overlying_direct_bed_sandy_greenhouse_plot

#SANDY SYSTEMS ICEHOUSE
overlying_direct_bed_sandy_terminal_icehouse<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'terminal deposit',sys_gs_category,'sandy system', climate, 'icehouse')
overlyin_direct_bed_sandy_terminal_icehouse_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_terminal_icehouse,sandy_top_beds_count_thck_5,overlying_bt,'Terminal deposits - icehouse')
overlyin_direct_bed_sandy_terminal_icehouse_plot

overlying_direct_bed_sandy_channel_icehouse<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'channel',sys_gs_category,'sandy system', climate, 'icehouse')
overlyin_direct_bed_sandy_channel_icehouse_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_channel_icehouse,sandy_top_beds_count_thck_5,overlying_bt,'Channel deposits - icehouse')
overlyin_direct_bed_sandy_channel_icehouse_plot

overlying_direct_bed_sandy_levee_icehouse<- calculate_bed_direct_transition_count(bed_under_over,overlying_bt,sandy_top_beds_count_thck_5,element_general_type,'levee',sys_gs_category,'sandy system', climate, 'icehouse')
overlyin_direct_bed_sandy_levee_icehouse_plot <- direct_bed_transition_plot(overlying_direct_bed_sandy_levee_icehouse,sandy_top_beds_count_thck_5,overlying_bt,'Levee deposits - icehouse')
overlyin_direct_bed_sandy_levee_icehouse_plot

overlying_direct_bed_sandy_icehouse_plot <- ggarrange(overlyin_direct_bed_sandy_terminal_icehouse_plot, 
                                                        overlyin_direct_bed_sandy_channel_icehouse_plot, 
                                                        overlyin_direct_bed_sandy_levee_icehouse_plot,
                                                        labels = c('A','C','E'),
                                                        font.label = list(size = 10, face = "bold", color ="black"),
                                                        nrow = 3,
                                                        ncol = 1,
                                                        common.legend = TRUE, legend = 'bottom')


overlying_direct_bed_sandy_icehouse_plot <- annotate_figure(overlying_direct_bed_sandy_icehouse_plot,
                                                              top = text_grob('Icehouse', face = 'bold', size = 10))

overlying_direct_bed_sandy_icehouse_plot

overlying_bed_combined_sandy_climate_plot <- ggarrange(overlying_direct_bed_sandy_greenhouse_plot, 
                                         overlying_direct_bed_sandy_icehouse_plot, 
                                         widths = c(1,1),
                                         nrow = 1,
                                         ncol = 2,
                                         common.legend = TRUE, legend = 'bottom')
overlying_bed_combined_sandy_climate_plot
ggsave('plots/overlying_bed_combined_sandy_climate_plot.pdf', device = 'pdf', width = 190, height = 210, units = 'mm')
ggsave('plots/overlying_bed_combined_sandy_climate_plot.jpeg', device = 'jpeg', width = 190, height = 210, units = 'mm')

### BEDS INDIRECT TRANSITION PLOTS-----------------------

#      list of beds that are above 5% based on either count or thickness 
#       sandy_top_beds_count_thck_5 
#       gs_top_beds_count_thck_5 
#       common_top_beds_count_thck_5 

bed_indirect_transition <- beds_table_sandgs_lam_features_elements %>% mutate(log_element = paste(log_ID,element_ID, sep = '_')) %>%
  mutate(underlying_bt = case_when(log_element != lag(log_element) ~ 'no trans', TRUE ~ lag(code))) %>%
  mutate(overlying_bt = case_when(log_element != lead(log_element) ~ 'no trans', TRUE ~ lead(code))) %>%
  filter(!is.na(underlying_bt) & underlying_bt != 'no trans') %>%
  filter(!is.na(overlying_bt) & overlying_bt != 'no trans') %>%
  rename('element_general_type' = 'element_general_type.x') %>%
  select(code, overlying_bt, element_general_type, sys_gs_category) 

calculate_indirect_transition_count <- function(data,input_col,selected_beds,col1,val1,col2,val2,col3,val3) {
  input_data <- data %>%
    filter(code %in% {{selected_beds}}) %>%
    filter({{input_col}} %in% {{selected_beds}}) %>%
    filter({{col1}} == {{val1}}) %>% 
    filter({{col2}} == {{val2}}) %>% 
    filter({{col3}} == {{val3}})
  
  output <- input_data %>% select(code,{{input_col}}) %>% group_by(code,{{input_col}}) %>%
    summarise(n=n()) %>% ungroup() %>% group_by(code) %>% mutate(sum_n = sum(n)) %>% 
    ungroup() %>% mutate(percent = round(n*100/sum_n,2))
  return(output)
}

indirect_transition_plot <- function(input_data, title) {
  # library("viridis") 
  # library(wesanderson)
  # pal <- wes_palette("Zissou1", 100, type = "continuous")
  
  plot <- ggplot(input_data, aes(x = code, y = overlying_bt, fill = percent)) +
    geom_tile(color = 'black', lwd = 0.11)+
    geom_text(aes(label = percent), colour = 'black', size = 7/.pt)+
    labs(
      title = title,
      y= 'Overlying bed type',
      x = 'Bed type')+
    scale_fill_gradient(limits = c(0,100), low = 'white', high = 'red')+
    theme_classic()+
    theme(legend.position= 'bottom',
          legend.text = element_text(size = 7, vjust = -2),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(3, 'mm'),
          axis.text.x = element_text(color = "black", size = 7, angle = 45, vjust = 1, hjust = 1),
          axis.title.x= element_text(color = "black", size = 9),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.title.y= element_text(color = "black", size = 9),
          axis.text.y = element_text(color = "black", size = 7),
          plot.title = element_text(color = 'black', size = 10))
  
  
  
  return(plot)
}

# SANDY
indirect_overlying_sandy_terminal<- calculate_indirect_transition_count(bed_indirect_transition,overlying_bt, sandy_top_beds_count_thck_5, element_general_type,'terminal deposit',sys_gs_category,'sandy system') 
indirect_overlying_sandy_terminal_plot <- indirect_transition_plot(indirect_overlying_sandy_terminal, 'Terminal deposits')
indirect_overlying_sandy_terminal_plot 
ggsave('plots/indirect_overlying_sandy_terminal_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

indirect_overlying_sandy_channel<- calculate_indirect_transition_count(bed_indirect_transition,overlying_bt, sandy_top_beds_count_thck_5, element_general_type,'channel',sys_gs_category,'sandy system') 
indirect_overlying_sandy_channel_plot <- indirect_transition_plot(indirect_overlying_sandy_channel, 'Channel deposits')
indirect_overlying_sandy_channel_plot 
ggsave('plots/indirect_overlying_sandy_channel.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

indirect_overlying_sandy_levee<- calculate_indirect_transition_count(bed_indirect_transition,overlying_bt, sandy_top_beds_count_thck_5, element_general_type,'levee',sys_gs_category,'sandy system') 
indirect_overlying_sandy_levee_plot <- indirect_transition_plot(indirect_overlying_sandy_levee, 'Levee deposits')
indirect_overlying_sandy_levee_plot 
ggsave('plots/indirect_overlying_sandy_channel.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')


indirect_overlying_sandy_combined_plot <- ggarrange(indirect_overlying_sandy_terminal_plot , 
                                                    indirect_overlying_sandy_channel_plot , 
                                                    indirect_overlying_sandy_levee_plot,
                                        labels = c('A','C','E'),
                                        font.label = list(size = 10, face = "bold", color ="black"),
                                        nrow = 3,
                                        ncol = 1,
                                        common.legend = TRUE, legend = 'bottom')


indirect_overlying_sandy_combined_plot <- annotate_figure(indirect_overlying_sandy_combined_plot,
                                              top = text_grob('Sandy Systems', face = 'bold', size = 10))

indirect_overlying_sandy_combined_plot

# GS
indirect_overlying_gs_terminal<- calculate_indirect_transition_count(bed_indirect_transition,overlying_bt, gs_top_beds_count_thck_5, element_general_type,'terminal deposit',sys_gs_category,'gravelly-sand system') 
indirect_overlying_gs_terminal_plot <- indirect_transition_plot(indirect_overlying_gs_terminal, 'Terminal deposits')
indirect_overlying_gs_terminal_plot 
ggsave('plots/indirect_overlying_gs_terminal_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

indirect_overlying_gs_channel<- calculate_indirect_transition_count(bed_indirect_transition,overlying_bt, gs_top_beds_count_thck_5, element_general_type,'channel',sys_gs_category,'gravelly-sand system') 
indirect_overlying_gs_channel_plot <- indirect_transition_plot(indirect_overlying_gs_channel, 'Channel deposits')
indirect_overlying_gs_channel_plot 
ggsave('plots/indirect_overlying_gs_channel.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

indirect_overlying_gs_levee<- calculate_indirect_transition_count(bed_indirect_transition,overlying_bt, gs_top_beds_count_thck_5, element_general_type,'levee',sys_gs_category,'gravelly-sand system') 
indirect_overlying_gs_levee_plot <- indirect_transition_plot(indirect_overlying_gs_levee, 'Levee deposits')
indirect_overlying_gs_levee_plot 
ggsave('plots/indirect_overlying_gs_channel.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')


indirect_overlying_gs_combined_plot <- ggarrange(indirect_overlying_gs_terminal_plot , 
                                                    indirect_overlying_gs_channel_plot , 
                                                    indirect_overlying_gs_levee_plot,
                                                    labels = c('B','D','F'),
                                                    font.label = list(size = 10, face = "bold", color ="black"),
                                                    nrow = 3,
                                                    ncol = 1,
                                                    common.legend = TRUE, legend = 'bottom')


indirect_overlying_gs_combined_plot <- annotate_figure(indirect_overlying_gs_combined_plot,
                                                          top = text_grob('Gravelly-sandy system', face = 'bold', size = 10))

indirect_overlying_gs_combined_plot

#COMBINED SANDY GS PLOT

indirect_overlying_combined_plot <- ggarrange(indirect_overlying_sandy_combined_plot , 
                                              indirect_overlying_gs_combined_plot , 
                                              widths = c(0.7,1),
                                                 nrow = 1,
                                                 ncol = 2,
                                                 common.legend = TRUE, legend = 'bottom')
indirect_overlying_combined_plot
ggsave('plots/indirect_overlying_combined_plot.pdf', device = 'pdf', width = 190, height = 210, units = 'mm')
ggsave('plots/indirect_overlying_combined_plot.jpeg', device = 'jpeg', width = 190, height = 210, units = 'mm')


