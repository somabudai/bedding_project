library(RMariaDB)
library(ggplot2)
library(dplyr)
library("writexl")
library(RColorBrewer)
library(ggpubr)

bed_id_type <- beds_table_sandgs_lam_features %>% select(artificial_bed_id, gs_category, code, code2, sys_gs_category, element_general_type)
lamination_data <- beds %>% 
  select(artificial_bed_id,facies_ID, facies_type, grain_size_sand, general_structure, lamination_type, grading, thickness) %>%
  left_join(bed_id_type, by = 'artificial_bed_id') %>%
  filter(!is.na(lamination_type))

### LAMINATION TYPE PER ELEMENT -----------------------

lamination_frequency_elements_calc <- function(input_data, system_type, elements, bed_list) {

lamination_frequence_count <- lamination_data %>% 
  filter(sys_gs_category == {{system_type}}) %>% 
  filter(element_general_type %in% {{elements}}) %>%
  filter(code %in% {{bed_list}}) %>%
  group_by(sys_gs_category, element_general_type, lamination_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(sys_gs_category, element_general_type) %>%
  mutate(sum_n = sum(n)) %>% ungroup() %>% mutate(percent = round((n/sum_n)*100,2))

return(lamination_frequence_count)

}

lamination_frequency_elements_thickness_calc <- function(input_data, system_type, elements, bed_list) {
  
  lamination_frequence_count <- lamination_data %>% 
    filter(sys_gs_category == {{system_type}}) %>% 
    filter(element_general_type %in% {{elements}}) %>%
    filter(code %in% {{bed_list}}) %>%
    group_by(sys_gs_category, element_general_type, lamination_type) %>%
    summarise(sum_facies_thickness = sum(thickness)) %>% ungroup() %>%
    group_by(element_general_type) %>%
    mutate(sum_n = round(sum(sum_facies_thickness),2)) %>% ungroup() %>% mutate(percent = round((sum_facies_thickness/sum_n)*100,2))
  
  return(lamination_frequence_count)
  
}

# PLOT FUNCTION

lamination_frequency_elements_barchart <- function(input_data, title) { 
  
  counts <- input_data %>% select(element_general_type,sum_n) %>% distinct(element_general_type, .keep_all = TRUE)
  
  lamination_order <- c('non planar parallel', 'planar parallel', 'small-scale cross-stratification', 'wavy', 'large-scale cross-stratification')
  
  plot <- ggplot(input_data, aes(x=factor(element_general_type, level = selected_elements), y=percent,fill= factor(lamination_type, level = lamination_order)))+
    geom_bar(position = position_stack(reverse = TRUE), stat ='identity', width = 0.7, colour = 'black', lwd=0.23)+
    geom_text(data = counts, aes(x = element_general_type, y=104 , label=sum_n, fill = NULL), size = 7/.pt)+
    scale_y_continuous(limits = c(0,110),expand = c(0,0))+
    scale_fill_brewer(palette = 'Paired', drop = FALSE)+
    theme_classic()+
    labs(
      x='',
      y= 'percent',
      fill = 'lamination type',
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

  #SANDY
  lamination_frequency_sandy_elements <- lamination_frequency_elements_calc(lamination_data, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
  lamination_frequency_sandy_elements_plot <- lamination_frequency_elements_barchart(lamination_frequency_sandy_elements, 'Sandy systems')
  lamination_frequency_sandy_elements_plot
  
  #GS
  lamination_frequency_gs_elements <- lamination_frequency_elements_calc(lamination_data, 'gravelly-sand system', selected_elements, gs_top_beds_count_thck_5)
  lamination_frequency_gs_elements_plot <- lamination_frequency_elements_barchart(lamination_frequency_gs_elements, 'Gravelly-sandy systems')
  lamination_frequency_gs_elements_plot
  
  #SANDY THICKNESS
  lamination_frequency_sandy_thickness_elements <- lamination_frequency_elements_thickness_calc(lamination_data, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
  lamination_frequency_sandy_elements_thickness_plot <- lamination_frequency_elements_barchart(lamination_frequency_sandy_thickness_elements, 'Sandy systems-thickness')
  lamination_frequency_sandy_elements_thickness_plot
  
  #GS THICKNESS
  lamination_frequency_gs_thickness_elements <- lamination_frequency_elements_thickness_calc(lamination_data, 'gravelly-sand system', selected_elements, gs_top_beds_count_thck_5)
  lamination_frequency_gs_elements_thickness_plot <- lamination_frequency_elements_barchart(lamination_frequency_gs_thickness_elements, 'Gravelly-sandy systems - thickness')
  lamination_frequency_gs_elements_thickness_plot
  
  #COMBINED PLOT
      #COUNT
      lamination_frequence_elements_count_plot <- ggarrange(lamination_frequency_sandy_elements_plot, 
                                              lamination_frequency_gs_elements_plot , 
                                              labels = c('A','B'),
                                              font.label = list(size = 10, face = "bold", color ="black"),
                                              common.legend = TRUE, legend = 'none')
      
      
      lamination_frequence_elements_count_plot <- annotate_figure(lamination_frequence_elements_count_plot,
                                                    top = text_grob('Lamination frequency based on number of occurrence', face = 'bold', size = 10))
      
      lamination_frequence_elements_count_plot
      
      ggsave('plots/lamination_frequence_elements_count_plot.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
      ggsave('plots/lamination_frequence_elements_count_plot.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')
    
      #THICKNESS    
      lamination_frequence_elements_thickness_plot <- ggarrange(lamination_frequency_sandy_elements_thickness_plot, 
                                                            lamination_frequency_gs_elements_thickness_plot, 
                                                            labels = c('C','D'),
                                                            font.label = list(size = 10, face = "bold", color ="black"),
                                                            common.legend = TRUE, legend = 'bottom')
    
  
      lamination_frequence_elements_thickness_plot <- annotate_figure(lamination_frequence_elements_thickness_plot,
                                                                  top = text_grob('Lamination frequency based on thickness', face = 'bold', size = 10))
      
      lamination_frequence_elements_thickness_plot
      
      ggsave('plots/lamination_frequence_elements_count_plot.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
      ggsave('plots/lamination_frequence_elements_count_plot.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')
    
    #COUNT THICKNESS COMBINED
    
    lamination_frequence_elements_combined_plot <- ggarrange(lamination_frequence_elements_count_plot, 
                                           lamination_frequence_elements_thickness_plot,
                                           ncol = 1,
                                           nrow =2,
                                           heights = c(0.8,1),
                                           common.legend = TRUE, legend = 'bottom')
    
    lamination_frequence_elements_combined_plot
    
    ggsave('plots/lamination_frequence_elements_combined_plot.pdf', width = 160, height = 130, units = 'mm', device = 'pdf')
    ggsave('plots/lamination_frequence_elements_combined_plot.jpeg', width = 160, height = 130, units = 'mm', device = 'jpeg')

### LAMINATION TYPE PER BED TYPE AND ELEMENT -----------------------    
    
    lamination_frequency_bedtypes_elements_calc <- function(input_data, system_type, element, bed_list) {
      
      lamination_frequence_count <- lamination_data %>% 
        filter(sys_gs_category == {{system_type}}) %>% 
        filter(element_general_type == {{element}}) %>%
        filter(code %in% {{bed_list}}) %>%
        group_by(sys_gs_category, element_general_type, code, lamination_type) %>%
        summarise(n = n()) %>% ungroup() %>%
        group_by(sys_gs_category, element_general_type, code) %>%
        mutate(sum_n = sum(n)) %>% ungroup() %>% mutate(percent = round((n/sum_n)*100,2))
      
      return(lamination_frequence_count)
      
    }
    
    lamination_frequency_bedtypes_elements_thickness_calc <- function(input_data, system_type, elements, bed_list) {
      
      lamination_frequence_count <- lamination_data %>% 
        filter(sys_gs_category == {{system_type}}) %>% 
        filter(element_general_type %in% {{elements}}) %>%
        filter(code %in% {{bed_list}}) %>%
        group_by(sys_gs_category, element_general_type, code, lamination_type) %>%
        summarise(sum_facies_thickness = sum(thickness)) %>% ungroup() %>%
        group_by(element_general_type, code) %>%
        mutate(sum_n = round(sum(sum_facies_thickness),2)) %>% ungroup() %>% mutate(percent = round((sum_facies_thickness/sum_n)*100,2))
      
      return(lamination_frequence_count)
      
    }
    
    lamination_frequency_bedtypes_elements_barchart <- function(input_data, title) { 
      
  input_data <- input_data %>% mutate(element_general_type = case_when(element_general_type == 'terminal deposit' ~ 'T',
                                                                       element_general_type == 'channel' ~ 'C',
                                                                       element_general_type == 'levee' ~ 'L'))
      
  counts <- input_data %>% select(element_general_type,code,sum_n) %>% distinct(element_general_type, code, .keep_all = TRUE)
  
  lamination_order <- c('non planar parallel', 'planar parallel', 'small-scale cross-stratification', 'wavy', 'large-scale cross-stratification')
  
  plot <- ggplot(input_data, aes(x=factor(element_general_type, level = c('T','C','L')), y=percent,fill= factor(lamination_type, level = lamination_order)))+
    geom_bar(position = position_stack(reverse = TRUE), stat ='identity', width = 0.7, colour = 'black', lwd=0.23)+
    geom_text(data = counts, aes(x = element_general_type, y=104 , label=sum_n, fill = NULL), size = 7/.pt)+
    scale_y_continuous(limits = c(0,110),expand = c(0,0))+
    scale_fill_brewer(palette = 'Paired', drop = FALSE)+
    theme_classic()+
    labs(
      x='',
      y= 'percent',
      fill = 'lamination type',
      title = title
    )+
    facet_grid(~ code)+
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
  return(plot)
  }

  #COUNT
    lamination_frequency_sandy_bedtypes_elements <- lamination_frequency_bedtypes_elements_calc(lamination_data, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
    lamination_frequency_sandy_bedtypes_elements_plot <- lamination_frequency_bedtypes_elements_barchart(lamination_frequency_sandy_bedtypes_elements, 'Sandy systems')
    lamination_frequency_sandy_bedtypes_elements_plot
    
    lamination_frequency_gs_bedtypes_elements <- lamination_frequency_bedtypes_elements_calc(lamination_data, 'gravelly-sand system', selected_elements, gs_top_beds_count_thck_5)
    lamination_frequency_gs_bedtypes_elements_plot <- lamination_frequency_bedtypes_elements_barchart(lamination_frequency_gs_bedtypes_elements, 'Gravelly-sandy systems')
    lamination_frequency_gs_bedtypes_elements_plot
    
    #THICKNESS
    lamination_frequency_sandy_bedtypes_elements_thickness <- lamination_frequency_bedtypes_elements_thickness_calc(lamination_data, 'sandy system', selected_elements, sandy_top_beds_count_thck_5)
    lamination_frequency_sandy_bedtypes_elements_thickness_plot <- lamination_frequency_bedtypes_elements_barchart(lamination_frequency_sandy_bedtypes_elements_thickness, 'Sandy systems - thickness')
    lamination_frequency_sandy_bedtypes_elements_thickness_plot
    
    lamination_frequency_gs_bedtypes_elements_thickness <- lamination_frequency_bedtypes_elements_thickness_calc(lamination_data, 'gravelly-sand system', selected_elements, gs_top_beds_count_thck_5)
    lamination_frequency_gs_bedtypes_elements_thickness_plot <- lamination_frequency_bedtypes_elements_barchart(lamination_frequency_gs_bedtypes_elements_thickness, 'Gravelly-sandy systems - thickness')
    lamination_frequency_sandy_bedtypes_elements_thickness_plot
    
    #COMBINED PLOT
    #COUNT
    lamination_frequence_bedtypes_elements_count_plot <- ggarrange(lamination_frequency_sandy_bedtypes_elements_plot, 
                                                          lamination_frequency_gs_bedtypes_elements_plot, 
                                                          labels = c('A','B'),
                                                          font.label = list(size = 10, face = "bold", color ="black"),
                                                          common.legend = TRUE, legend = 'none')
    
    
    lamination_frequence_bedtypes_elements_count_plot <- annotate_figure(lamination_frequence_bedtypes_elements_count_plot,
                                                                top = text_grob('Lamination frequency based on number of occurrence', face = 'bold', size = 10))
    
    lamination_frequence_bedtypes_elements_count_plot
    
    ggsave('plots/lamination_frequence_bedtypes_elements_count_plot.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
    ggsave('plots/lamination_frequence_bedtypes_elements_count_plot.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')
    
    #THICKNESS    
    lamination_frequence_bedtypes_elements_thickness_plot <- ggarrange(lamination_frequency_sandy_bedtypes_elements_thickness_plot, 
                                                              lamination_frequency_gs_bedtypes_elements_thickness_plot, 
                                                              labels = c('C','D'),
                                                              font.label = list(size = 10, face = "bold", color ="black"),
                                                              common.legend = TRUE, legend = 'bottom')
    
    
    lamination_frequence_bedtypes_elements_thickness_plot <- annotate_figure(lamination_frequence_bedtypes_elements_thickness_plot,
                                                                    top = text_grob('Lamination frequency based on thickness', face = 'bold', size = 10))
    
    lamination_frequence_bedtypes_elements_thickness_plot
    
    ggsave('plots/lamination_frequence_elements_count_plot.pdf', width = 160, height = 80, units = 'mm', device = 'pdf')
    ggsave('plots/lamination_frequence_elements_count_plot.jpeg', width = 160, height = 80, units = 'mm', device = 'jpeg')
    
    #COUNT THICKNESS COMBINED
    
    lamination_frequence_bedtypes_elements_combined_plot <- ggarrange(lamination_frequence_bedtypes_elements_count_plot, 
                                                             lamination_frequence_bedtypes_elements_thickness_plot,
                                                             ncol = 1,
                                                             nrow =2,
                                                             heights = c(0.8,1),
                                                             common.legend = TRUE, legend = 'bottom')
    
    lamination_frequence_bedtypes_elements_combined_plot
    
    ggsave('plots/lamination_frequence_bedtypes_elements_combined_plot.pdf', width = 160, height = 140, units = 'mm', device = 'pdf')
    ggsave('plots/lamination_frequence_bedtypes_elements_combined_plot.jpeg', width = 160, height = 140, units = 'mm', device = 'jpeg')
          
#LAMINATION TYPE FREQUENCE FOR LAMINATED BED TYPES

lamination_frequence_count_bedtype <- lamination_data %>% filter(element_general_type %in% selected_elements) %>%
  filter(code %in% sandy_system_top_beds | code %in% gs_system_top_beds) %>%
  group_by(code, lamination_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(code) %>%
  mutate(sum_n = sum(n)) %>% ungroup() %>% mutate(percent = round((n/sum_n)*100,2))

lamination_frequence_count_bedtype_plot <- ggplot(lamination_frequence_count_bedtype, aes(x="", y=percent, fill = lamination_type))+
  geom_bar(stat='identity', width = 1, color = 'black')+
  ggtitle('Lamination frequence within bed types (count)')+
  coord_polar("y", start = 0)+
  theme_void()+
  theme(legend.position = 'bottom',
        legend.title = element_blank())+
  facet_wrap(~ code)
lamination_frequence_count_bedtype_plot 

### NUNMBER OF DISTINCT LAMINATION PER BED -----------------------

lam_number_bedtype <- lamination_data %>% distinct(artificial_bed_id, lamination_type, .keep_all = TRUE) %>%
  

#PRESENCE OF NORMALLY GRADED FACIES


# LAMINATION TYPE FREQUENCE SYS GS TYPE AND ELEMENET TYPE
  #COUNT

    
    lamination_frequence_count_plot <- ggplot(lamination_frequence_count, aes(x="", y=percent, fill = lamination_type))+
      geom_bar(position = 'stack', stat='identity', width = 1, color = 'black')+
      ggtitle('Lamination frequence (count)')+
      theme_classic()+
      theme(legend.position = 'bottom',
            legend.title = element_blank())+
      facet_wrap(~ sys_gs_category + element_general_type)
    lamination_frequence_count_plot
  #THICKNESS
    lamination_frequence_thck <- lamination_data %>% filter(element_general_type %in% selected_elements) %>%
      group_by(sys_gs_category, element_general_type, lamination_type) %>%
      summarise(thck = sum(thickness)) %>% ungroup() %>%
      group_by(sys_gs_category, element_general_type) %>%
      mutate(sum_thck = sum(thck)) %>% ungroup() %>% mutate(percent = round((thck/sum_thck)*100,2))
    
    lamination_frequence_thck_plot <- ggplot(lamination_frequence_thck, aes(x="", y=percent, fill = lamination_type))+
      geom_bar(stat='identity', width = 1, color = 'black')+
      ggtitle('Lamination frequence (thickness)')+
      coord_polar("y", start = 0)+
      theme_void()+
      theme(legend.position = 'bottom',
            legend.title = element_blank())+
      facet_wrap(~ sys_gs_category + element_general_type)
    lamination_frequence_thck_plot
    
#PORPORTION OF BEDS CONTAINING MULTIPLE LAMINATED FACIES
    #LAMINATION ORDER
      