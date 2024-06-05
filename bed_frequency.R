library(RMariaDB)
library(ggplot2)
library(dplyr)
library("writexl")
library(RColorBrewer)
library(ggpubr)
source('config.R')

### HEAT MAPS FOR BED FREQUENCY -----------------

#
#Functions
#

bed_frequency_calc <- function(data,col1,val1,col2,val2,col3,val3) {
  all_bed_type <- beds_table_sandgs_lam  %>% 
    select(code, gs_category, code2) %>% distinct(code, .keep_all = TRUE)
  
  input_data <- data %>% 
    filter({{col1}} == {{val1}}) %>% 
    filter({{col2}} == {{val2}}) %>% 
    filter({{col3}} == {{val3}}) %>% 
    filter(!is.na(gs_trend)) %>% 
    group_by(gs_category,code2) %>% summarise(n = n()) %>% mutate(code = paste(gs_category,code2, sep = '-'))%>%
    ungroup()%>%
    mutate(percent = round(n*100/sum(n),2))
  
  input_data <- left_join(all_bed_type, input_data, by = c('code','gs_category','code2')) %>% 
    mutate(n = case_when(is.na(n)~ 0, TRUE ~ n )) %>% 
    mutate(percent = case_when(is.na(percent) ~ 0, TRUE ~ percent) ) 
  return(input_data)
}
bed_frequency_calc_thck <- function(data,col1,val1,col2,val2,col3,val3) {
  all_bed_type <- beds_table_sandgs_lam   %>% 
    select(code, gs_category, code2) %>% distinct(code, .keep_all = TRUE)
  
  input_data <- data %>% 
    filter({{col1}} == {{val1}}) %>% 
    filter({{col2}} == {{val2}}) %>% 
    filter({{col3}} == {{val3}}) %>% 
    group_by(gs_category,code2) %>% summarise(sum_bed_thickness = sum(bed_thickness)) %>% mutate(code = paste(gs_category,code2, sep = '-')) %>%
    ungroup() %>% 
    mutate(total_bed_thickness = sum(sum_bed_thickness)) %>%
    mutate(percent = round(sum_bed_thickness*100/total_bed_thickness,2))

  input_data <- left_join(all_bed_type, input_data, by = c('code','gs_category','code2')) %>%
    mutate(sum_bed_thickness = case_when(is.na(sum_bed_thickness)~ 0, TRUE ~ sum_bed_thickness )) %>%
    mutate(percent = case_when(is.na(percent) ~ 0, TRUE ~ percent))
  return(input_data)
}
bed_frequency_plot_log <- function(data,fill,title) {
  gs_order <- c('G','sG', 'gS', 'S', 'SM') 
  
  sample_number <- sum({{data}}$n)
  
  distinct_code2 <-  beds_table_sandgs_lam  %>% 
    select(code, gs_category, code2) %>% distinct(code2)
  
  bed_frequency_plot <- ggplot(data, aes(x=code2,y=factor(gs_category, level = gs_order), fill = {{fill}}))+
    geom_tile(color = 'black', lwd = 0.11)+
    geom_vline(xintercept = seq(1.5,nrow(distinct_code2)+0.5, 1), alpha = 0.3, lwd=0.11)+
    geom_hline(yintercept = seq(1.5, 5.5, 1), alpha = 0.3, lwd=0.11)+
    scale_fill_gradient(trans = 'log10')+
    geom_text(aes(label = {{fill}}), angle = 90, colour = 'white', size = 7/.pt)+
    ggtitle(paste(title, '  ' ,'n=', sample_number, sep=''))+
    theme_classic()+
    theme(legend.position= 'bottom',
          legend.text = element_text(size = 7, vjust = -2),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(3, 'mm'),
          axis.text.x = element_text(color = "black", size = 7, angle = 90,vjust = 0.5),
          axis.title.x=element_blank(),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.title.y=element_blank(),
          axis.text.y = element_text(color = "black", size = 9),
          plot.title = element_text(color = 'black', size = 10))
  return(bed_frequency_plot)
}
bed_frequency_thickness_plot <- function(data,fill,title) {
  gs_order <- c('G','sG', 'gS', 'S', 'SM') 
  
  sample_number <- sum({{data}}$sum_bed_thickness)
  
  distinct_code2 <-  beds_table_sandgs_lam  %>% mutate(code2 = paste(gs_trend,sand_trend,lam_category, sep = '-')) %>% 
    select(code, gs_category, code2) %>% distinct(code2)
  
  bed_frequency_plot <- ggplot(data, aes(x=code2,y=factor(gs_category, level = gs_order), fill = {{fill}}))+
    geom_tile(color = 'black', lwd = 0.11)+
    geom_vline(xintercept = seq(1.5,nrow(distinct_code2)+0.5, 1), alpha = 0.3, lwd=0.11)+
    geom_hline(yintercept = seq(1.5, 5.5, 1), alpha = 0.3, lwd=0.11)+
    scale_fill_gradient(trans = 'log10')+
    geom_text(aes(label = {{fill}}), angle = 90, colour = 'white', size = 7/.pt)+
    ggtitle(paste(title, '  ' ,'n=', round(sample_number,1), ' m', sep=''))+
    theme_classic()+
    theme(legend.position= 'bottom',
          legend.text = element_text(size = 7),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(3, 'mm'),
          axis.text.x = element_text(color = "black", size = 7, angle = 90,vjust = 0.5),
          axis.title.x=element_blank(),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.title.y=element_blank(),
          axis.text.y = element_text(color = "black", size = 9),
          plot.title = element_text(color = 'black', size = 10))
  return(bed_frequency_plot)
  
}
bed_frequency_diff_calc <- function(data1,data2) {

  data1 <- data1 %>% rename('percent1' = percent, 'n1' = n)
  data2 <- data2 %>% rename('percent2' = percent, 'n2' = n)
 
  difference_data <- full_join(data1, data2, by = c('code','gs_category','code2')) %>% mutate(difference = round(percent1-percent2,2))
  return(difference_data)
}
bed_frequency_diff_plot <- function(data,fill,title) {
  gs_order <- c('G','sG', 'gS', 'S', 'SM') 
  
  data1_n <- sum({{data}}$n1)
  data2_n <- sum({{data}}$n2)
  
  min_diff <- min({{data}}$difference)
  max_diff <- max({{data}}$difference)
  if (abs(min_diff) >= max_diff){
    limit1 = min_diff
    limit2 = abs(min_diff)
  } else { 
    limit1 = max_diff*(-1)
    limit2 = max_diff}
  
  bed_frequency_diff_plot <- ggplot(data, aes(x=code2,y=factor(gs_category, level = gs_order), fill = {{fill}}))+geom_tile(lwd=0.23)+
    geom_text(aes(label = {{fill}}), angle = 90, colour = 'white')+
    geom_vline(xintercept = seq(1.5,nrow(data)-0.5, 1), alpha = 0.3, lwd=0.23)+
    geom_hline(yintercept = seq(1.5, 5.5, 1), alpha = 0.3, lwd=0.23)+
    scale_fill_gradientn(colors = c('blue','lightgrey','red'),limits = c(limit1,limit2))+
    ggtitle(paste(title,'  ', 'n1=',data1_n,' n2=',data2_n, sep=''))+
    theme_classic()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
                          axis.line = element_line(colour = 'black', size = 0.23),
                          axis.ticks = element_line(colour = 'black', size = 0.23))
  return(bed_frequency_diff_plot)
}



bed_counts <- beds_table_sandgs_lam_features %>% group_by(climate,element_general_type) %>% summarise(n = n())
 
### 
  #count
bed_type_frequence <- bed_frequency_calc(beds_table_sandgs_lam)%>% filter(code2 != 'NA-ns-sl' )
bed_type_frequency_plot <- bed_frequency_plot_log(bed_type_frequence,percent,'Total dataset')
bed_type_frequency_plot 
ggsave('plots/bed_type_frequency_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

  #thickness
bed_type_frequence_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam)%>% filter(code2 != 'NA-ns-sl' )
bed_type_frequence_thickness_plot <- bed_frequency_thickness_plot(bed_type_frequence_thickness,percent,'Total dataset - thickness')
bed_type_frequence_thickness_plot
ggsave('plots/bed_type_frequence_thickness_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

element_bed_frequency_heat_combined <- ggarrange(bed_type_frequency_plot, 
                                                 bed_type_frequence_thickness_plot, labels = c('A','B'),
                                        common.legend = FALSE, legend = 'bottom')
element_bed_frequency_heat_combined
ggsave('plots/element_bed_frequency_heat_combined.pdf', width = 180, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/element_bed_frequency_heat_combined.jpeg', width = 180, height = 100, units = 'mm', device = 'jpeg')
#bed frequency per systems

bed_freq_system <- beds_table_sandgs_lam_features %>% group_by(system_ID, sys_name, code) %>% summarise(n = n()) %>%
  ungroup() %>% group_by(system_ID, sys_name) %>% mutate(sum_n = sum(n)) %>% ungroup() %>% mutate(n_percent = round(n/sum_n,2))


#
#sandy systems
#
  #COUNT
  #terminal deposit
  bed_type_sandy_terminal <- bed_frequency_calc(beds_table_sandgs_lam_features,sys_gs_category,'sandy system',element_general_type,'terminal deposit')
  bed_type_sandy_terminal_plot <- bed_frequency_plot_log(bed_type_sandy_terminal,percent,'Sandy systems - terminal deposit')
  bed_type_sandy_terminal_plot
  
    #climate difference
    bed_type_sandy_terminal_icehouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse',sys_gs_category,'sandy system',element_general_type,'terminal deposit')
    bed_type_sandy_terminal_greenhouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse',sys_gs_category,'sandy system',element_general_type,'terminal deposit')
    bed_type_sandy_terminal_climate_diff <-  bed_frequency_diff_calc(bed_type_sandy_terminal_greenhouse,bed_type_sandy_terminal_icehouse)
    sandy_terminal_cdiff_plot <- bed_frequency_diff_plot(bed_type_sandy_terminal_climate_diff,difference,'Greenhouse and icehouse sandy terminal d. difference')
    sandy_terminal_cdiff_plot
  #channel
  bed_type_sandy_channel <- bed_frequency_calc(beds_table_sandgs_lam_features,sys_gs_category,'sandy system',element_general_type,'channel')
  bed_type_sandy_channel_plot <- bed_frequency_plot_log(bed_type_sandy_channel,percent,'Sandy systems - channel deposit')
  bed_type_sandy_channel_plot
  
    #climate difference
    bed_type_sandy_channel_icehouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse',sys_gs_category,'sandy system',element_general_type,'channel')
    bed_type_sandy_channel_greenhouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse',sys_gs_category,'sandy system',element_general_type,'channel')
    bed_type_sandy_channel_climate_diff <-  bed_frequency_diff_calc(bed_type_sandy_channel_greenhouse,bed_type_sandy_channel_icehouse)
    sandy_channel_cdiff_plot <- bed_frequency_diff_plot(bed_type_sandy_channel_climate_diff,difference,'Greenhouse and icehouse sandy channels difference')
    sandy_channel_cdiff_plot
  #levee
  bed_type_sandy_levee <- bed_frequency_calc(beds_table_sandgs_lam_features,sys_gs_category,'sandy system',element_general_type,'levee')
  bed_type_sandy_levee_plot <- bed_frequency_plot_log(bed_type_sandy_levee,percent,'Sandy systems - levee deposit')
  bed_type_sandy_levee_plot
  
    #climate difference
    bed_type_sandy_levee_icehouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse',sys_gs_category,'sandy system',element_general_type,'levee')
    bed_type_sandy_levee_greenhouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse',sys_gs_category,'sandy system',element_general_type,'levee')
    bed_type_sandy_levee_climate_diff <-  bed_frequency_diff_calc(bed_type_sandy_levee_greenhouse,bed_type_sandy_levee_icehouse)
    sandy_levee_cdiff_plot <- bed_frequency_diff_plot(bed_type_sandy_levee_climate_diff,difference,'Greenhouse and icehouse sandy levee difference')
    sandy_levee_cdiff_plot
  
    #differences between elements

  sandy_terminal_channel_diff <- bed_frequency_diff_calc(bed_type_sandy_terminal, bed_type_sandy_channel)
  sandy_terminal_channel_diff_plot <- bed_frequency_diff_plot(sandy_terminal_channel_diff,difference,'Sandy terminal d. and channel difference')
  sandy_terminal_channel_diff_plot

#THICKNESS
  #terminal deposits
  bed_type_sandy_terminal_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam_features,sys_gs_category,'sandy system',element_general_type,'terminal deposit')
  bed_type_sandy_terminal_thickness_plot <- bed_frequency_thickness_plot(bed_type_sandy_terminal_thickness,percent,'Sandy systems - terminal deposit (thickness)')
  bed_type_sandy_terminal_thickness_plot
  
  #channel
  bed_type_sandy_channel_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam_features,sys_gs_category,'sandy system',element_general_type,'channel')
  bed_type_sandy_channel_thickness_plot <- bed_frequency_thickness_plot(bed_type_sandy_channel_thickness,percent,'Sandy systems - channel deposit (thickness)')
  bed_type_sandy_channel_thickness_plot
  
  #levee
  bed_type_sandy_levee_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam_features,sys_gs_category,'sandy system',element_general_type,'levee')
  bed_type_sandy_levee_thickness_plot  <- bed_frequency_thickness_plot(bed_type_sandy_levee_thickness,percent,'Sandy systems - levee deposit (thickness)')
  bed_type_sandy_levee_thickness_plot 
  
  
#
#gravelly-sand systems
#
  #COUNT
  #terminal deposit
  bed_type_gs_terminal <- bed_frequency_calc(beds_table_sandgs_lam_features,sys_gs_category,'gravelly-sand system',element_general_type,'terminal deposit')
  bed_type_gs_terminal_plot <- bed_frequency_plot_log(bed_type_gs_terminal,percent,'Gravelly-sand systems - terminal deposit')
  bed_type_gs_terminal_plot
 
    #climate difference
    bed_type_gs_terminal_icehouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse',sys_gs_category,'gravelly-sand system',element_general_type,'terminal deposit')
    bed_type_gs_terminal_greenhouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse',sys_gs_category,'gravelly-sand system',element_general_type,'terminal deposit')
    bed_type_gs_terminal_climate_diff <-  bed_frequency_diff_calc(bed_type_gs_terminal_greenhouse,bed_type_gs_terminal_icehouse)
    gs_terminal_terminal_cdiff_plot <- bed_frequency_diff_plot(bed_type_gs_terminal_climate_diff ,difference,'Greenhouse and icehouse gs terminal d. difference')
    gs_terminal_terminal_cdiff_plot
    
  #channel
  bed_type_gs_channel <- bed_frequency_calc(beds_table_sandgs_lam_features,sys_gs_category,'gravelly-sand system',element_general_type,'channel')
  bed_type_gs_channel_plot <- bed_frequency_plot_log(bed_type_gs_channel,percent,'Gravelly-sand systems - channel deposit')
  bed_type_gs_channel_plot
  
    #climate difference
    bed_type_gs_channel_icehouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse',sys_gs_category,'gravelly-sand system',element_general_type,'channel')
    bed_type_gs_channel_greenhouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse',sys_gs_category,'gravelly-sand system',element_general_type,'channel')
    bed_type_gs_channel_climate_diff <-  bed_frequency_diff_calc(bed_type_gs_channel_greenhouse,bed_type_gs_channel_icehouse)
    gs_channel_cdiff_plot <- bed_frequency_diff_plot(bed_type_gs_channel_climate_diff,difference,'Greenhouse and icehouse gs channels difference')
    gs_channel_cdiff_plot
    
  #levee
  bed_type_gs_levee <- bed_frequency_calc(beds_table_sandgs_lam_features,sys_gs_category,'gravelly-sand system',element_general_type,'levee')
  bed_type_gs_levee_plot <- bed_frequency_plot_log(bed_type_gs_levee,percent,'Gravelly-sand systems - levee deposit')
  bed_type_gs_levee_plot
  
    #climate difference
    bed_type_gs_levee_icehouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse',sys_gs_category,'gravelly-sand system',element_general_type,'levee')
    bed_type_gs_levee_greenhouse <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse',sys_gs_category,'gravelly-sand system',element_general_type,'levee')
    bed_type_gs_levee_climate_diff <-  bed_frequency_diff_calc(bed_type_gs_levee_greenhouse,bed_type_gs_levee_icehouse)
    gs_levee_cdiff_plot <- bed_frequency_diff_plot(bed_type_gs_levee_climate_diff,difference,'Greenhouse and icehouse gslevee difference')
    gs_levee_cdiff_plot

    #THICKNESS
    #terminal deposits
    bed_type_gs_terminal_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam_features,sys_gs_category,'gravelly-sand system',element_general_type,'terminal deposit')
    bed_type_gs_terminal_thickness_plot <- bed_frequency_thickness_plot(bed_type_gs_terminal_thickness,percent,'Gravelly-sandy systems - terminal deposit (thickness)')
    bed_type_gs_terminal_thickness_plot
    
    #channel
    bed_type_gs_channel_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam_features,sys_gs_category,'gravelly-sand system',element_general_type,'channel')
    bed_type_gs_channel_thickness_plot <- bed_frequency_thickness_plot(bed_type_gs_channel_thickness,percent,'Gravelly-sandy systems - channel deposit (thickness)')
    bed_type_gs_channel_thickness_plot
    
    #levee
    bed_type_gs_levee_thickness <- bed_frequency_calc_thck(beds_table_sandgs_lam_features,sys_gs_category,'gravelly-sand system',element_general_type,'levee')
    bed_type_gs_levee_thickness_plot  <- bed_frequency_thickness_plot(bed_type_gs_levee_thickness,percent,'Gravelly-sandy - levee deposit (thickness)')
    bed_type_gs_levee_thickness_plot
    


### ANALYZING MOST COMMON BEDS FOR EACH ELEMENT -----------------------

  #DETERMINING MOST COMMON BEDS >5%
    
  determine_frequent_bed_types_stat <- function(input_data, system_type, elements) {
      
      top_beds_stat <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
        filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
        group_by(element_general_type,code) %>% summarise(n = n()) %>%
        group_by(element_general_type) %>%
        mutate(sum_n = sum(n)) %>%
        mutate(percent = round(n*100/sum_n,2)) %>%
        ungroup()
      
      return(top_beds_stat)}
  determine_frequent_bed_types_thck_stat <- function(input_data, system_type, elements) {
      
      top_beds_stat <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
        filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
        group_by(element_general_type,code) %>% summarise(sum_bed_thickness = sum(bed_thickness)) %>%
        group_by(element_general_type) %>%
        mutate(sum_n = round(sum(sum_bed_thickness),2)) %>%
        mutate(percent = round(sum_bed_thickness*100/sum_n,2)) %>%
        ungroup()
      
      return(top_beds_stat)}
  determine_frequent_bed_types_list <- function(input_data, system_type, elements, percent) {
      
      top_beds <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
      filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
      group_by(element_general_type,code) %>% summarise(n = n()) %>%
      group_by(element_general_type) %>%
      mutate(sum_n = sum(n)) %>%
      mutate(percent = round(n*100/sum_n,2))%>%
      filter(percent >= {{percent}}) %>%
      ungroup() %>%
      distinct(code) %>% pull(code) %>% as.list()
      
      return(top_beds)}
  determine_frequent_bed_types_thck_list <- function(input_data, system_type, elements, percent) {
    
    top_beds <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
      filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
      group_by(element_general_type,code) %>% summarise(sum_bed_thickness = sum(bed_thickness)) %>%
      group_by(element_general_type) %>%
      mutate(sum_n = round(sum(sum_bed_thickness),2)) %>%
      mutate(percent = round(sum_bed_thickness*100/sum_n,2)) %>%
      filter(percent >= {{percent}}) %>%
      ungroup() %>%
      distinct(code) %>% pull(code) %>% as.list()
    
    return(top_beds)}
  
  sandy_system_top_beds_stat <- determine_frequent_bed_types_stat (beds_table_sandgs_lam_features, 'sandy system', selected_elements)
  gs_system_top_beds_stat <- determine_frequent_bed_types_stat (beds_table_sandgs_lam_features, 'gravelly-sand system', selected_elements)
  
  sandy_system_top_beds_thck_stat <- determine_frequent_bed_types_thck_stat (beds_table_sandgs_lam_features, 'sandy system', selected_elements)
  gs_system_top_beds_thck_stat <- determine_frequent_bed_types_thck_stat (beds_table_sandgs_lam_features, 'gravelly-sand system', selected_elements)
  
    #CREATE LIST FOR MOST COMMON BED TYPES
    #COUNT
      #SANDY SYSTEMS
      sandy_system_top_beds_5 <- determine_frequent_bed_types_list (beds_table_sandgs_lam_features, 'sandy system', selected_elements, 5)
      
      #GRAVELLY SANDY SYSTEMS
      gs_system_top_beds_5 <- determine_frequent_bed_types_list (beds_table_sandgs_lam_features, 'gravelly-sand system', selected_elements, 5)
      
      #BOTH SYSTEMS
      common_top_beds_5 <- union(sandy_system_top_beds_5, gs_system_top_beds_5) #beds that are present in either s or gs systems
      intersect_top_beds_5 <- intersect(sandy_system_top_beds_5, gs_system_top_beds_5) #beds that are present in both s or gs systems
      
    #THICKNESS
      #SANDY SYSTEMS
      sandy_system_top_beds_5_thck <- determine_frequent_bed_types_thck_list (beds_table_sandgs_lam_features, 'sandy system', selected_elements,5)
      #GRAVELLY SANDY SYSTEMS
      gs_system_top_beds_5_thck <- determine_frequent_bed_types_thck_list (beds_table_sandgs_lam_features, 'gravelly-sand system', selected_elements,5)
      #BOTH SYSTEMS
      common_top_beds_thck_5 <- union(sandy_system_top_beds_5_thck, gs_system_top_beds_5_thck)
      intersect_top_beds_thck_5 <- intersect(sandy_system_top_beds_5_thck, gs_system_top_beds_5_thck)
      
    #BEDS THAT ABOVE 5% FOR EITHER THICKNESS OR COUNT
      sandy_top_beds_count_thck_5 <- union(sandy_system_top_beds_5, sandy_system_top_beds_5_thck)
      gs_top_beds_count_thck_5 <- union(gs_system_top_beds_5, gs_system_top_beds_5_thck)
      common_top_beds_count_thck_5 <- union(common_top_beds_5, common_top_beds_thck_5)
      intersect_top_beds_count_thck_5 <- union(intersect_top_beds_5, intersect_top_beds_thck_5)
      
  #BED FREQUENCY OF ELEMENTS PER MOST COMMON BED TYPE CONTAINS OTHER AS CATEGORY
    #COUNT FUNCTION
        frequent_bed_types_count <- function(input_data, system_type, elements, bed_list) {
          
          frequent_beds_stat <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
            filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
            mutate(code = case_when(code %in% {{bed_list}} ~ code, TRUE ~ 'other')) %>%
            group_by(element_general_type,code) %>% summarise(n = n()) %>%
            group_by(element_general_type) %>%
            mutate(sum_n = sum(n)) %>%
            mutate(percent = round(n*100/sum_n,2)) %>%
            ungroup()
          
          return(frequent_beds_stat)}
    #THICKNESS  FUNCTION
        frequent_bed_types_thck <- function(input_data, system_type, elements, bed_list) {
          
          frequent_beds_stat <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
            filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
            mutate(code = case_when(code %in% {{bed_list}} ~ code, TRUE ~ 'other')) %>%
            group_by(element_general_type,code) %>% summarise(sum_bed_thickness = sum(bed_thickness)) %>%
            group_by(element_general_type) %>%
            mutate(sum_n = round(sum(sum_bed_thickness),2)) %>%
            mutate(percent = round(sum_bed_thickness*100/sum_n,2)) %>%
            ungroup()
          
          return(frequent_beds_stat)}     
        
        
      #COUNT 
        #SANDY SYSTEMS
        element_bed_proportions_sandy <- frequent_bed_types_count(beds_table_sandgs_lam_features, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
        #GRAVELLY SANDY SYSTEMS
        element_bed_proportions_gs <- frequent_bed_types_count(beds_table_sandgs_lam_features, 'gravelly-sand system', selected_elements, gs_top_beds_count_thck_5)
        #BOTH SYSTEMS TYPES need this to create common color scheme for figures
        element_bed_proportions_both <- beds_table_sandgs_lam_features %>% 
          filter(element_general_type %in% selected_elements) %>% filter(!is.na(gs_trend)) %>% 
          mutate(code = case_when(code %in% common_top_beds_count_thck_5 ~ code, TRUE ~ 'other')) %>%
          group_by(element_general_type,code) %>% summarise(n = n()) %>%
          group_by(element_general_type) %>%
          mutate(percent = round(n*100/sum(n),2)) %>%
          ungroup()
        
        
       #THICKNESS
        #SANDY SYSTEMS
        element_bed_proportions_sandy_thck <- frequent_bed_types_thck(beds_table_sandgs_lam_features, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
        #GRAVELLY SANDY SYSTEMS
        element_bed_proportions_gs_thck <- frequent_bed_types_thck(beds_table_sandgs_lam_features, 'gravelly-sand system', selected_elements, gs_top_beds_count_thck_5)
  
        
  #STACKED BARCHARTS TO SHOW BED FREQUEUNCY IN ELEMENTS
      #creates fill order based on bed frequency in terminal deposits
      element_bed_proportions_order <- element_bed_proportions_both %>% filter(element_general_type == 'terminal deposit') %>%
        filter(code != 'other') %>%
        arrange(desc(percent)) %>% pull(code) %>% as.list() %>% append('other') 
      #PLOT FUNCTION
      bed_frequency_elements_barchart <- function(input_data, title) { 
        
        colors <- brewer.pal(12, 'Paired')
        colors[13] <- "grey10"
        
        counts <- input_data %>% select(element_general_type,sum_n) %>% distinct(element_general_type, .keep_all = TRUE)
        
        plot <- ggplot(input_data, aes(x=factor(element_general_type, level = selected_elements), y=percent,fill= factor(code, level = element_bed_proportions_order)))+
        geom_bar(position = position_stack(reverse = TRUE), stat ='identity', width = 0.7, colour = 'black', lwd=0.23)+
        geom_text(data = counts, aes(x = element_general_type, y=103 , label=sum_n, fill = NULL), size = 7/.pt)+
        scale_y_continuous(limits = c(0,110),expand = c(0,0))+
        scale_fill_manual(values = colors, drop = FALSE)+
        theme_classic()+
        labs(
          x='',
          y= 'percent',
          fill = 'Bed type',
          title = title
        )+
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
          guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) #change nrow for combined plots
     return(plot)}
      #COUNT
      element_bed_prop_sandy_count_plot <- bed_frequency_elements_barchart(element_bed_proportions_sandy, 'Sandy systems')
      element_bed_prop_sandy_count_plot
      ggsave('plots/element_bed_prop_sandy_count_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
      ggsave('plots/element_bed_prop_sandy_count_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')
      
      element_bed_prop_gs_count_plot <- bed_frequency_elements_barchart(element_bed_proportions_gs, 'Gravelly-sandy systems')
      element_bed_prop_gs_count_plot
      ggsave('plots/element_bed_prop_gs_count_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
      ggsave('plots/element_bed_prop_gs_count_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')
      
      element_bed_count_combined <- ggarrange(element_bed_prop_sandy_count_plot, 
                                              element_bed_prop_gs_count_plot, labels = c('A','B'),
                                              common.legend = TRUE, legend = 'bottom')
      element_bed_count_combined
      ggsave('plots/element_bed_count_combined.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
      ggsave('plots/element_bed_count_combined.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')
     
       #THICKNESS
      element_bed_prop_sandy_thck_plot <- bed_frequency_elements_barchart(element_bed_proportions_sandy_thck, 'Sandy systems - thickness')
      element_bed_prop_sandy_thck_plot
      ggsave('plots/element_bed_prop_sandy_thck_plot.pdf', width = 90, height = 90, units = 'mm', device = 'pdf')
      ggsave('plots/element_bed_prop_sandy_thck_plot.jpeg', width = 90, height = 90, units = 'mm', device = 'jpeg')
      
      element_bed_prop_gs_thck_plot <- bed_frequency_elements_barchart(element_bed_proportions_gs_thck, 'Gravelly-sandy systems - thickness')
      element_bed_prop_gs_thck_plot
      ggsave('plots/element_bed_prop_gs_thck_plot.pdf', width = 90, height = 90, units = 'mm', device = 'pdf')
      ggsave('plots/element_bed_prop_gs_thck_plot.jpeg', width = 90, height = 90, units = 'mm', device = 'jpeg')
      
      #COMBINED PLOT
      element_bed_count_thickness_combined <- ggarrange(element_bed_prop_sandy_count_plot, 
                                              element_bed_prop_gs_count_plot,
                                              element_bed_prop_sandy_thck_plot,
                                              element_bed_prop_gs_thck_plot,
                                              labels = c('A','B', 'C', 'D'),
                                              font.label = (size = 10),
                                              common.legend = TRUE, legend = 'bottom')
      element_bed_count_thickness_combined
      ggsave('plots/element_bed_count_thickness_combined.pdf', width = 160, height = 130, units = 'mm', device = 'pdf')
      ggsave('plots/element_bed_count_thickness_combined.jpeg', width = 160, height = 130, units = 'mm', device = 'jpeg')
    
### COMPARE BED FREQUNECY BETWEEN ELEMENTS -----------------------
#uses variables from previous section inncluding:
#      list of beds that are above 5% based on either count or thickness 
#       sandy_top_beds_count_thck_5 
#       gs_top_beds_count_thck_5 
#       common_top_beds_count_thck_5 
#     dataframes containing element count and thickness proportions for each element
#       element_bed_proportions_sandy
#       element_bed_proportions_gs
#       element_bed_proportions_sandy_thck
#       element_bed_proportions_gs_thck 
# element_bed_proportions_order: order of bed types based on their frequency in terminal deposits
      
  #PLOT FUNCTION
      bed_frequency_element_comparison_chart <- function(input_data, title) { 
        
        input_data <- input_data %>% filter(code != 'other')
        
        plot <- ggplot(input_data, aes(x=factor(code, level = element_bed_proportions_order), y=percent, fill=factor(element_general_type, level=selected_elements))) +
          geom_bar(position = position_dodge(preserve = 'single'), stat = 'identity',color = 'black', lwd = 0.23)+
          scale_y_continuous(limits = c(0,80), breaks = seq(0,80,15),expand = c(0,0))+
          theme_classic()+
          labs(
            y= 'Frequency %',
            fill = 'Element',
            title = title
          )+
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
                legend.position = c(0.8,0.9),
                legend.key.height = unit(3.5, 'mm'),
                legend.key.width = unit(5, 'mm'))
        return(plot)}
      
      #BARCHARTS TO SHOW DIFFERENCES IN BED FREQUEUNCY AMONG ELEMENTS
      
      #COUNT
      bed_element_comp_sandy_count_plot <- bed_frequency_element_comparison_chart(element_bed_proportions_sandy, 'Sandy systems')
      bed_element_comp_sandy_count_plot
      
      bed_element_comp_gs_count_plot <- bed_frequency_element_comparison_chart(element_bed_proportions_gs, 'Gravelly-sandy systems')
      bed_element_comp_gs_count_plot
      
      #THICKNESS
      
      bed_element_comp_sandy_thck_plot <- bed_frequency_element_comparison_chart(element_bed_proportions_sandy_thck, 'Sandy systems - thickness')
      bed_element_comp_sandy_count_plot
      
      bed_element_comp_gs_thck_plot <- bed_frequency_element_comparison_chart(element_bed_proportions_gs_thck, 'Gravelly-sandy systems - thickness')
      bed_element_comp_gs_thck_plot
    
      #COMBINED PLOT
      bed_element_comp_count_thickness_combined <- ggarrange(bed_element_comp_sandy_count_plot , 
                                                             bed_element_comp_gs_count_plot ,
                                                             bed_element_comp_sandy_thck_plot,
                                                             bed_element_comp_gs_thck_plot,
                                                             widths = c(0.8,1,0.8,1),
                                                        labels = c('A','B', 'C', 'D'),
                                                        font.label = (size = 10),
                                                        common.legend = TRUE, legend = 'bottom')
      bed_element_comp_count_thickness_combined
      ggsave('plots/bed_element_comp_count_thickness_combined.pdf', width = 190, height = 140, units = 'mm', device = 'pdf')
      ggsave('plots/bed_element_comp_count_thickness_combined.jpeg', width = 190, height = 140, units = 'mm', device = 'jpeg')
    
  
    
# # 
# #bed type frequency changes with system gs  
# #  
# most_frequent_types_terminal <-  bed_frequency_calc(beds_table_sandgs_lam_features,element_general_type,'terminal deposit') %>%
#   filter(percent > 1)
# most_frequent_types_terminal_list <- most_frequent_types_terminal %>% pull(code) %>% as.list()
# bed_frequency_change_terminal <- beds_table_sandgs_lam_features %>% group_by(system_ID, sys_name, combined_sand_fraction, code) %>% 
#   summarise(n=n()) %>% ungroup() %>% group_by(system_ID) %>% mutate(sys_bed_nr = sum(n)) %>% ungroup() %>%
#   mutate(percent = round(n*100/sys_bed_nr,2)) %>%
#   filter(code %in% most_frequent_types_terminal_list)
# ggplot(bed_frequency_change_terminal, aes(x=combined_sand_fraction, y= percent)) + 
#   geom_line(aes(color=code)) + geom_point(aes(color=code))+ geom_smooth(method = 'lm', se = FALSE, alpha = 0.5)+
#   facet_grid(rows = vars(code), scales ='free_y')+
#   labs(
#     x='system sand fraction',
#     y= 'bed fraction',
#     title = 'bed type percentages vs. system coarseness'
#   )+
#   theme_classic()
# ggplot(bed_frequency_change_terminal)+geom_boxplot(aes(x=code,y=percent))
#   
# #
# #select beds based on climate, element, etc
# #
# 
# beds_table_sandgs_features <- left_join(beds_table_sandgs,beds_features, by = 'artificial_bed_id') 
# bed_climate_summary <- beds_table_sandgs_features %>% group_by(climate) %>% summarise(n=n())
# #
# #total bed frequency
# #
# 
# #
# #generate plot
# #
# icehouse_bed_frequency <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'icehouse') %>% filter(code2 != 'NA-ns-sl' )
# icehouse_bed_frequency_plot_log <- bed_frequency_plot(icehouse_bed_frequency,percent,'icehouse')
# icehouse_bed_frequency_plot
# greenhouse_bed_frequency <- bed_frequency_calc(beds_table_sandgs_lam_features,climate,'greenhouse')
# greenhouse_bed_frequency_plot_log <- bed_frequency_plot(greenhouse_bed_frequency,percent,'greenhouse')
# greenhouse_bed_frequency_plot
# 
# icehouse_greenhouse_diff <- full_join(icehouse_bed_frequency,greenhouse_bed_frequency, by = c('code','gs_category','code2') ) %>% 
#   rename('icehouse_percent' = 'percent.x') %>% rename('greenhouse_percent' = 'percent.y') %>%
#   rename('icehouse_n' = 'n.x') %>% rename('greenhouse_n' = 'n.y') %>%
#   mutate(icehouse_percent = case_when(is.na(icehouse_percent) ~ 0, TRUE ~ icehouse_percent)) %>%
#   mutate(greenhouse_percent = case_when(is.na(greenhouse_percent) ~ 0, TRUE ~ greenhouse_percent)) %>%
#   mutate(icehouse_n = case_when(is.na(icehouse_n) ~ 0, TRUE ~ icehouse_n)) %>%
#   mutate(greenhouse_n = case_when(is.na(greenhouse_n) ~ 0, TRUE ~ greenhouse_n)) %>%
#   mutate(diff = icehouse_percent - greenhouse_percent)
# 
# icehouse_greenhouse_diff_plot <- bed_frequency_diff_plot(icehouse_greenhouse_diff,diff,'Differences in bed frequency')
# icehouse_greenhouse_diff_plot 
# 
# #number
# ggplot(gs_gs_trend, aes(x=code2,y=factor(gs_category, level = gs_order), fill = n))+geom_tile()+
#   scale_fill_gradient(trans = 'log10')+
#   theme_classic()+theme(axis.text.x = element_text(angle = 90))
# #percent
# ggplot(gs_gs_trend, aes(x=code2,y=gs_category, fill = percent))+geom_tile()+
#   scale_fill_gradient(trans = 'log10')+
#   theme_classic()+theme(axis.text.x = element_text(angle = 90))

  


### COMPARE BED FREQUENCY CLIMATE --------------
      
      #BED FREQUENCY OF ELEMENTS OF DIFFERENT CLIMATE PER MOST COMMON BED TYPE CONTAINS OTHER AS CATEGORY
      #COUNT FUNCTION
      frequent_bed_types_count_climate <- function(input_data, system_type, elements, bed_list) {
        
        frequent_beds_stat <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
          filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
          mutate(code = case_when(code %in% {{bed_list}} ~ code, TRUE ~ 'other')) %>% #remove this line if you want every bed
          group_by(element_general_type, climate, code) %>% summarise(n = n()) %>% ungroup() %>%
          group_by(element_general_type, climate) %>%
          mutate(sum_n = sum(n)) %>%
          mutate(percent = round(n*100/sum_n,2)) %>%
          ungroup()
        
        return(frequent_beds_stat)}
      #THICKNESS  FUNCTION
      frequent_bed_types_thck_climate <- function(input_data, system_type, elements, bed_list) {
        
        frequent_beds_stat <- input_data %>% filter(sys_gs_category == {{system_type}}) %>% 
          filter(element_general_type %in% {{elements}}) %>% filter(!is.na(gs_trend)) %>% 
          mutate(code = case_when(code %in% {{bed_list}} ~ code, TRUE ~ 'other')) %>%
          group_by(element_general_type, climate, code)  %>% summarise(sum_bed_thickness = sum(bed_thickness)) %>%
          group_by(element_general_type, climate)%>%
          mutate(sum_n = round(sum(sum_bed_thickness),2)) %>%
          mutate(percent = round(sum_bed_thickness*100/sum_n,2)) %>%
          ungroup()
        
        return(frequent_beds_stat)}     

      #BARCHART FUNCTION
      bed_frequency_element_climate_comparison_chart <- function(input_data, title) { 
        
        input_data <- input_data %>% filter(code != 'other')
        
        plot <- ggplot(input_data, aes(x=factor(code, level = element_bed_proportions_order), y=percent, fill= climate)) +
          geom_bar(width = 0.9, position = position_dodge(0.9,preserve = 'single'), stat = 'identity',color = 'black', lwd = 0.23)+
          scale_y_continuous(limits = c(0,90), breaks = seq(0,85,15),expand = c(0,0))+
          geom_text(data = input_data, aes(x = code, y=85 , label=n, fill = NULL), position = position_dodge2(0.9), size = 7/.pt, angle = 90)+
          scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                                       'icehouse' = icehouse_color
          ))+
          theme_classic()+
          labs(
            y= 'Frequency %',
            fill = 'Climate',
            title = title
          )+
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
            legend.key.width = unit(5, 'mm'))
        return(plot)}    
      #COUNT 
      #SANDY SYSTEMS
      element_bed_proportions_climate_sandy <- frequent_bed_types_count_climate(beds_table_sandgs_lam_features, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
      element_bed_proportions_climate_sandy_terminal <- frequent_bed_types_count_climate(beds_table_sandgs_lam_features, 'sandy system', selected_elements, sandy_top_beds_count_thck_5) %>% filter(element_general_type == 'terminal deposit')    
      element_bed_proportions_climate_sandy_channel <- frequent_bed_types_count_climate(beds_table_sandgs_lam_features, 'sandy system', selected_elements, sandy_top_beds_count_thck_5) %>% filter(element_general_type == 'channel')
      element_bed_proportions_climate_sandy_levee <- frequent_bed_types_count_climate(beds_table_sandgs_lam_features, 'sandy system', selected_elements, sandy_top_beds_count_thck_5) %>% filter(element_general_type == 'levee')
      
      
      bed_element_climate_comp_sandy_count_terminal_plot <- bed_frequency_element_climate_comparison_chart(element_bed_proportions_climate_sandy_terminal, 'Terminal deposits')
      bed_element_climate_comp_sandy_count_terminal_plot
      bed_element_climate_comp_sandy_count_channel_plot <- bed_frequency_element_climate_comparison_chart(element_bed_proportions_climate_sandy_channel, 'Channel deposits')
      bed_element_climate_comp_sandy_count_channel_plot
      bed_element_climate_comp_sandy_count_levee_plot <- bed_frequency_element_climate_comparison_chart(element_bed_proportions_climate_sandy_levee, 'Levee deposits')
      bed_element_climate_comp_sandy_count_levee_plot
      
      bed_element_climate_comp_sandy_count_plot <- ggarrange(bed_element_climate_comp_sandy_count_terminal_plot, 
                                                             bed_element_climate_comp_sandy_count_channel_plot,
                                                             bed_element_climate_comp_sandy_count_levee_plot,
                                                              ncol=1,
                                                              nrow=3,
                                                              font.label = (size = 10),
                                                              common.legend = TRUE, legend = 'bottom')
      bed_element_climate_comp_sandy_count_plot
      ggsave('plots/bed_element_climate_comp_sandy_count_plot.pdf', width = 90, height = 180, units = 'mm', device = 'pdf')
      ggsave('plots/bed_element_climate_comp_sandy_count_plot.jpeg', width = 90, height = 180, units = 'mm', device = 'jpeg')
