library(ggridges)
library(ggpubr)
library(goft)
source('config.R')
#
#bed thickness per system and case studies
#

avg_bed_thickness_system <- beds_table_sandgs_lam_features %>% select(bed_thickness, sys_name) %>% group_by(sys_name) %>%
  summarise(mean = mean(bed_thickness),
            max = max(bed_thickness),
            min = min(bed_thickness),
            n = n())

### COMPARE SANDY AND GS SYTEMS-----------------------
  ### BED THICKNESS HEATMAP -----------------------

gs_order <- c('G','sG', 'gS', 'S', 'SM')

#CALCULATE AVERAGE BED THICKNESS FOR SANDY AND GS SYSTEMS

#GET ALL BED TYPES SO MISSING TYPES CAN BE SHOWN ON THE PLOTS
all_bed_type <- beds_table_sandgs_lam  %>% mutate(code2 = paste(gs_trend,sand_trend,lam_category, sep = '-')) %>% 
  select(code, gs_category, code2) %>% distinct(code, .keep_all = TRUE)

  #SANDY
  bed_thickness_average_sandy <- beds_table_sandgs_lam_features %>% filter(sys_gs_category == 'sandy system')  %>% mutate(code2 = paste(gs_trend,sand_trend,lam_category, sep = '-')) %>% 
    select(code, gs_category, code2, bed_thickness) %>% group_by(code, gs_category, code2) %>% summarise(n = n(), avg_bed_thck = round(mean(bed_thickness),2)) %>% 
    ungroup() %>% mutate(sum_n = sum(n)) %>% mutate(percent = round(n/sum_n*100,2)) %>% 
    mutate(frequency = as.character(avg_bed_thck)) %>%
    mutate(frequency = case_when( n<5 ~ paste(frequency, '*'), TRUE ~ frequency))
  
  bed_thickness_average_sandy <- left_join(all_bed_type, bed_thickness_average_sandy, by = c('code','gs_category','code2')) %>%  
    mutate(frequency = case_when(is.na(n) ~ 'NA' ,TRUE ~ frequency))
  #GS
  bed_thickness_average_gs <- beds_table_sandgs_lam_features %>% filter(sys_gs_category == 'gravelly-sand system')  %>% mutate(code2 = paste(gs_trend,sand_trend,lam_category, sep = '-')) %>% 
    select(code, gs_category, code2, bed_thickness) %>% group_by(code, gs_category, code2) %>% summarise(n = n(), avg_bed_thck = round(mean(bed_thickness),2)) %>% 
    ungroup() %>% mutate(sum_n = sum(n)) %>% mutate(percent = round(n/sum_n*100,2)) %>% 
    mutate(frequency = as.character(avg_bed_thck)) %>%
    mutate(frequency = case_when( n<5 ~ paste(frequency, '*'), TRUE ~ frequency)) 
  
  bed_thickness_average_gs <- left_join(all_bed_type, bed_thickness_average_gs, by = c('code','gs_category','code2')) %>%  
    mutate(frequency = case_when(is.na(n) ~ 'NA' ,TRUE ~ frequency))
#FILTER BEDS THAT HAVE A NUMBER ABOVE 5
  bed_thickness_average_sandy_above5 <- bed_thickness_average_sandy %>% filter(n>=5)
  bed_thickness_average_gs_above5 <- bed_thickness_average_gs %>% filter(n>=5) 
#PLOTS
  #SANDY
    bed_thickness_avg_sandy_plot <- ggplot() + 
      geom_tile(data = bed_thickness_average_sandy, aes(x=code2,y=factor(gs_category, level = gs_order), fill = avg_bed_thck), alpha = 0.5, 
                color = 'black', lwd = 0.23)+
      geom_tile(data = bed_thickness_average_sandy_above5, aes(x=code2,y=factor(gs_category, level = gs_order), fill = avg_bed_thck), 
                color = 'black', lwd = 0.23)+
      scale_fill_gradient(trans = 'log10')+
      geom_text(data = bed_thickness_average_sandy, aes(x=code2,y=factor(gs_category, level = gs_order), label = frequency), 
                angle = 90, colour = 'white', size = 7/.pt)+
      annotate('text', x = 'C-n-sl', y = 'G', label = '* less than 5 observed beds', fontface = 'italic', size = 7/.pt)+
      theme_classic()+
      ggtitle('Average bed thickness - Sandy systems')+
      labs(
        fill= 'average bed thickness',
      )+
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

    bed_thickness_avg_sandy_plot
    ggsave('plots/bed_thickness_avg_sandy_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')
#GS
  bed_thickness_avg_gs_plot <- ggplot() + 
    geom_tile(data = bed_thickness_average_gs, aes(x=code2,y=factor(gs_category, level = gs_order), fill = avg_bed_thck), alpha = 0.5, 
              color = 'black', lwd = 0.23)+
    geom_tile(data = bed_thickness_average_gs_above5, aes(x=code2,y=factor(gs_category, level = gs_order), fill = avg_bed_thck), 
              color = 'black', lwd = 0.23)+
    scale_fill_gradient(trans = 'log10')+
    geom_text(data = bed_thickness_average_gs, aes(x=code2,y=factor(gs_category, level = gs_order), label = frequency), angle = 90, 
              colour = 'white', size = 7/.pt)+
    annotate('text', x = 'C-n-sl', y = 'G', label = '* less than 5 observed beds', fontface = 'italic', size = 7/.pt)+
    theme_classic()+
    ggtitle('Average bed thickness - Gravelly-sandy systems')+
    labs(
      fill= 'average bed thickness',
    )+
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
  
  bed_thickness_avg_gs_plot
  ggsave('plots/bed_thickness_avg_gs_plot.pdf', device = 'pdf', width = 90, height = 100, units = 'mm')

#COMBINED PLOT
  bed_thickness_avg_combined_plot <- ggarrange(bed_thickness_avg_sandy_plot, 
                                          bed_thickness_avg_gs_plot, labels = c('A','B'),
                                          common.legend = FALSE, legend = 'bottom')
  bed_thickness_avg_combined_plot
  ggsave('plots/bed_thickness_avg_combined_plot.pdf', width = 180, height = 100, units = 'mm', device = 'pdf')
  ggsave('plots/bed_thickness_avg_combined_plot.jpeg', width = 180, height = 100, units = 'mm', device = 'jpeg')
  
  ### SYSTEM DIFFERENCE BOXPLOTS -----------------------
  selected_elements <- c('terminal deposit', 'channel', 'levee')
  sandy_vs_gs_bed_thickness_diff <- beds_table_sandgs_lam_features %>% select(bed_thickness, code, element_general_type, method, sys_gs_category) %>%
    filter(sys_gs_category %in% c('sandy system', 'gravelly-sand system')) %>% filter(element_general_type %in% selected_elements)
  
  #t-test differences
  sandy_bed_thickness <- sandy_vs_gs_bed_thickness_diff %>% filter(sys_gs_category == 'sandy system') %>% select(bed_thickness)
  gs_bed_thickness <- sandy_vs_gs_bed_thickness_diff %>% filter(sys_gs_category == 'gravelly-sand system') %>% select(bed_thickness)
  t_test_res <- t.test(sandy_bed_thickness, gs_bed_thickness, var.equal = TRUE)
  t_test_res
  #GENERAL DIFFERENCE IN SYSTEM BED THICKNESS
      sandy_vs_gs_bed_thickness_diff_plot <- ggplot(sandy_vs_gs_bed_thickness_diff, aes(x=factor(sys_gs_category, level = c('sandy system', 'gravelly-sand system')), y=bed_thickness, fill = sys_gs_category)) + 
        coord_cartesian(expand = TRUE, ylim = c(0,1.5))+
        geom_boxplot(outlier.shape = NA, width = 0.6, colour = 'black', lwd=0.23)+
        stat_summary(fun.y=mean, geom="point", shape=15, size = 2)+
        scale_fill_manual(values = c('gravelly-sand system' = 'darkorange',
                                     'sandy system' = 'gold1'))+
        labs(
          y= 'bed thickness (m)',
          title = 'Differences in systems',
          fill = 'average'
        )+
        theme_classic()+
        theme(legend.position= 'none',
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
        guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1))
      sandy_vs_gs_bed_thickness_diff_plot
      ggsave('plots/sandy_vs_gs_bed_thickness_diff_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
      ggsave('plots/sandy_vs_gs_bed_thickness_diff_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')
  #GENERAL DIFFERENCE IN SYSTEM ELEMENT BED THICKNESS
  sandy_vs_gs_bed_element_thickness_diff_plot <- ggplot(sandy_vs_gs_bed_thickness_diff, aes(x=factor(sys_gs_category, level = c('sandy system', 'gravelly-sand system')), y=bed_thickness, fill = factor(element_general_type, level = selected_elements))) + 
    coord_cartesian(expand = TRUE, ylim = c(0,1.5))+
    geom_boxplot(outlier.shape = NA, width = 0.6, colour = 'black', lwd=0.23)+
    stat_summary(fun.y=mean, geom="point", shape=15, size = 2, position = position_dodge(width= 0.6))+
    scale_fill_brewer(palette = 'Dark2')+
    labs(
      y= 'bed thickness (m)',
      title = 'Differences in systems and elements',
      fill = 'element type'
    )+
    theme_classic()+
    theme(legend.position= c(0.15,0.9),
          legend.background = element_rect(fill = alpha("lightblue",0)),
          legend.text = element_text(size = 7),
          legend.title = element_text(face = 'bold', size = 7),
          legend.key.height = unit(6, 'mm'),
          legend.key.width = unit(4, 'mm'),
          axis.text.x = element_text(color = "black", size = 9),
          axis.title.x=element_blank(),
          axis.line = element_line(colour = 'black', size = 0.24),
          axis.ticks = element_line(colour = 'black', size = 0.24),
          axis.text.y = element_text(color = "black", size = 9),
          axis.title.y = element_text(color = "black", size = 9),
          plot.title = element_text(color = 'black', size = 10),
    )+
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 3))
  sandy_vs_gs_bed_element_thickness_diff_plot
  ggsave('plots/sandy_vs_gs_bed_element_thickness_diff_plot.pdf', width = 120, height = 100, units = 'mm', device = 'pdf')
  ggsave('plots/sandy_vs_gs_bed_element_thickness_diff_plot.jpeg', width = 120, height = 100, units = 'mm', device = 'jpeg')
  
  #COMBINED PLOT
  sandy_vs_gs_bed_differences_plot <- ggarrange(sandy_vs_gs_bed_thickness_diff_plot, 
                                                    sandy_vs_gs_bed_element_thickness_diff_plot,
                                                    labels = c('A','B'),
                                                    widths = c(0.6,1),
                                                    font.label = (size = 10))
  sandy_vs_gs_bed_differences_plot
  ggsave('plots/sandy_vs_gs_bed_differences_plot.pdf', width = 190, height = 100, units = 'mm', device = 'pdf')
  ggsave('plots/sandy_vs_gs_bed_differences_plot.jpeg', width = 190, height = 100, units = 'mm', device = 'jpeg')
  
  outcrop_vs_core_system_diff_plot <- ggplot(sandy_vs_gs_bed_thickness_diff, aes(x=sys_gs_category, y=bed_thickness, fill = method)) + 
    coord_cartesian(expand = TRUE, ylim = c(0,1.5))+
    geom_boxplot(outlier.shape = NA)+
    theme_classic()
  outcrop_vs_core_system_diff_plot
### COMPARE BEDS WITHIN SYSTEMS TYPES-----------------------
  
  #      list of beds that are above 5% based on either count or thickness 
  #       sandy_top_beds_count_thck_5 
  #       gs_top_beds_count_thck_5 
  #       common_top_beds_count_thck_5 
  
  #boxplots, sandy and gs separately, x bed type, fill element type


beds_sand_sandgs_lam_sum <- beds_table_sandgs_lam_features %>% group_by(code, sys_gs_category, element_general_type) %>% summarise(n=n())

selected_elements <- c('terminal deposit', 'channel', 'levee')

#SANDY SYSTEMS

bed_thickness_comparison_sandy_sys <- sandy_vs_gs_bed_thickness_diff %>%
  filter(code %in% sandy_top_beds_count_thck_5) %>% filter(element_general_type %in% selected_elements) %>% 
  filter(sys_gs_category == 'sandy system')

bed_thickness_comparison_sandy_sys_stat <- bed_thickness_comparison_sandy_sys %>% group_by(code, element_general_type) %>%
  summarise(n = n(), min = min(bed_thickness), mean = mean(bed_thickness), max = max(bed_thickness))

#create order factor based on bed type average thickness
sandy_bed_order_avgthck <- bed_thickness_comparison_sandy_sys  %>% group_by(code) %>% summarise(mean_thck = mean(bed_thickness)) %>%
  arrange(mean_thck) %>% pull(code) %>% as.list()


bed_thickness_sandy_sys_plot <- ggplot(bed_thickness_comparison_sandy_sys, aes(x=factor(code, level = sandy_bed_order_avgthck),y=bed_thickness, 
                                                                               fill=factor(element_general_type, level=selected_elements)))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  coord_cartesian(ylim = c(0,2))+
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
bed_thickness_sandy_sys_plot

ggsave('plots/bed_thickness_sandy_sys_plot.pdf', width = 100, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/bed_thickness_sandy_sys_plot.jpeg', width = 100, height = 70, units = 'mm', device = 'jpeg')

#gravelly sandy systems


bed_thickness_comparison_gs_sys <- sandy_vs_gs_bed_thickness_diff %>%
  filter(code %in% gs_top_beds_count_thck_5) %>% filter(element_general_type %in% selected_elements) %>% 
  filter(sys_gs_category == 'gravelly-sand system')

bed_thickness_comparison_gs_sys_stat <- bed_thickness_comparison_gs_sys %>% group_by(code, element_general_type) %>%
  summarise(n = n(), min = min(bed_thickness), mean = mean(bed_thickness), max = max(bed_thickness))

#create order factor based on bed type average thickness
gs_bed_order_avgthck <- bed_thickness_comparison_gs_sys  %>% group_by(code) %>% summarise(mean_thck = mean(bed_thickness)) %>%
  arrange(mean_thck) %>% pull(code) %>% as.list()


bed_thickness_gs_sys_plot <- ggplot(bed_thickness_comparison_gs_sys, aes(x=factor(code, level = gs_bed_order_avgthck),y=bed_thickness, 
                                                                               fill=factor(element_general_type, level=selected_elements)))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  coord_cartesian(ylim = c(0,3))+
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
    y= 'Bed thickness',
    fill = 'Element')
bed_thickness_gs_sys_plot

ggsave('plots/bed_thickness_gs_plot.pdf', width = 100, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/bed_thickness_gs_plot.jpeg', width = 100, height = 70, units = 'mm', device = 'jpeg')

bed_thickness_element_comp_combined <- ggarrange(bed_thickness_sandy_sys_plot, 
                                                 bed_thickness_gs_sys_plot,
                                                       widths = c(0.8,1),
                                                       labels = c('A','B'),
                                                       font.label = (size = 10),
                                                       common.legend = TRUE, legend = 'bottom')
bed_thickness_element_comp_combined
ggsave('plots/bed_thickness_element_comp_combined.pdf', width = 190, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/bed_thickness_element_comp_combined.jpeg', width = 190, height = 70, units = 'mm', device = 'jpeg')

### COMPARE BED TYPE THICKNESS OF ELEMENTS OF DIFFERENT SYS TYPE -----------------------
#      list of beds that are above 5% based on either count or thickness 
#       sandy_top_beds_count_thck_5 
#       gs_top_beds_count_thck_5 
#       common_top_beds_count_thck_5 

terminal_deposits_thickness_ranges <- sandy_vs_gs_bed_thickness_diff %>% filter(element_general_type == 'terminal deposit')
channel_thickness_ranges <- sandy_vs_gs_bed_thickness_diff %>% filter(element_general_type == 'channel') 
levee_thickness_ranges <- sandy_vs_gs_bed_thickness_diff %>% filter(element_general_type == 'levee')
system_bed_thickness_comparions_data_counts <- sandy_vs_gs_bed_thickness_diff %>% group_by(sys_gs_category, element_general_type, code) %>%
  summarise(n=n())


system_bed_thickness_comparions_plot <- function(data, bed_list, ymax ,title) {
  
  gs_bed_order_avgthck <- bed_thickness_comparison_gs_sys  %>% group_by(code) %>% summarise(mean_thck = mean(bed_thickness)) %>%
    arrange(mean_thck) %>% pull(code) %>% as.list()
  
  data <- data %>% filter(code %in% {{bed_list}}) #i can change this to count sys type for each code if it is not 2 i can exclude it
  
  data_count <- data %>% group_by(sys_gs_category, code) %>%
    summarise(n=n())
  
  plot <- ggplot(data, aes(x=factor(code, level = gs_bed_order_avgthck),y=bed_thickness, 
                                                         fill=factor(sys_gs_category, level=c('sandy system','gravelly-sand system'))))+
    geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
    stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
    geom_text(data = data_count, aes(x=factor(code, level = gs_bed_order_avgthck), label = n), y = ymax-0.1, position = position_dodge2(0.9), size = 7/.pt)+
    coord_cartesian(ylim = c(0,ymax))+
    scale_fill_manual(values = c('gravelly-sand system' = 'darkorange',
                                 'sandy system' = 'gold1'))+
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
      title = title,
      y= 'Bed thickness',
      fill = 'Element')
  return(plot)
}

terminal_deposits_thickness_ranges_plot <- system_bed_thickness_comparions_plot(terminal_deposits_thickness_ranges,intersect_top_beds_count_thck_5,3,'Terminal deposits')
terminal_deposits_thickness_ranges_plot

channel_deposits_thickness_ranges_plot <- system_bed_thickness_comparions_plot(channel_thickness_ranges,intersect_top_beds_count_thck_5,3,'Channel deposits')
channel_deposits_thickness_ranges_plot

levee_deposits_thickness_ranges_plot <- system_bed_thickness_comparions_plot(levee_thickness_ranges,intersect_top_beds_count_thck_5,1.5,'Levee deposits')
levee_deposits_thickness_ranges_plot

system_bed_thickness_comparions_plot <- ggarrange(terminal_deposits_thickness_ranges_plot, 
                                                  channel_deposits_thickness_ranges_plot,
                                                  levee_deposits_thickness_ranges_plot,
                                                  ncol=1,
                                                  nrow=3,
                                                font.label = (size = 10),
                                                common.legend = TRUE, legend = 'bottom')
system_bed_thickness_comparions_plot
ggsave('plots/system_bed_thickness_comparions_plot.pdf', width = 90, height = 180, units = 'mm', device = 'pdf')
ggsave('plots/system_bed_thickness_comparions_plot.jpeg', width = 90, height = 180, units = 'mm', device = 'jpeg')





### CLIMATE BED THICKNESS COMPARISON ---------

#CLIMATE AND ELEMENT
bed_thickness_general_climate_sandy_summary <- beds_table_sandgs_lam_features %>% select(code,bed_thickness,element_general_type,climate, sys_gs_category, sys_name) %>%
  filter(element_general_type %in% selected_elements) %>% filter(climate %in% c('icehouse','greenhouse')) %>%  filter(sys_gs_category == 'sandy system') %>%
  group_by(element_general_type, climate) %>% summarise(mean_bed_thck = mean(bed_thickness), min_bed_thck = min(bed_thickness), n =n ())

#CODE CLIMATE AND ELEMENT
bed_thickness_climate_sandy <- beds_table_sandgs_lam_features %>% select(code,bed_thickness,element_general_type,climate, sys_gs_category, sys_name) %>%
  filter(code %in% sandy_top_beds_count_thck_5) %>% filter(element_general_type %in% selected_elements) %>%  filter(sys_gs_category == 'sandy system') %>% filter(climate %in% c('icehouse','greenhouse'))


bed_thickness_climate_sandy_summary <- bed_thickness_climate_sandy %>% group_by(element_general_type, climate,  code) %>% summarise(mean_bed_thck = mean(bed_thickness), n =n ())

climate_bed_thickness_comparison_plot <- function (data, sum_data, selected_element, ymax ,title) {
  
  input_data <- data %>% filter(element_general_type %in% selected_element)
  input_data_sum <- sum_data %>% filter(element_general_type %in% selected_element)
  
  plot <-  ggplot(input_data, aes(x=factor(code, level = element_bed_proportions_order),y=bed_thickness, fill= climate))+
    geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
    stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
    geom_text(data = input_data_sum, aes(x=code, label = n), y = ymax+0.01 , position = position_dodge2(0.9), size = 7/.pt)+
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

bed_thickness_climate_sandy_terminal_plot <- climate_bed_thickness_comparison_plot(bed_thickness_climate_sandy, bed_thickness_climate_sandy_summary,'terminal deposit',1.5, 'Terminal deposits')
bed_thickness_climate_sandy_terminal_plot
bed_thickness_climate_sandy_channel_plot <- climate_bed_thickness_comparison_plot(bed_thickness_climate_sandy, bed_thickness_climate_sandy_summary,'channel',2, 'Channel deposits')
bed_thickness_climate_sandy_channel_plot
bed_thickness_climate_sandy_levee_plot <- climate_bed_thickness_comparison_plot(bed_thickness_climate_sandy, bed_thickness_climate_sandy_summary,'levee',0.5, 'Levee deposits')
bed_thickness_climate_sandy_levee_plot

bed_thickness_elements_climate_sandy_plot <- ggarrange(bed_thickness_climate_sandy_terminal_plot, 
                                              bed_thickness_climate_sandy_channel_plot,
                                              bed_thickness_climate_sandy_levee_plot,
                                              ncol=1,
                                              nrow=3,
                                              font.label = (size = 10),
                                              common.legend = TRUE, legend = 'bottom')
bed_thickness_elements_climate_sandy_plot
ggsave('plots/bed_thickness_elements_climate_sandy_plot.pdf', width = 90, height = 180, units = 'mm', device = 'pdf')
ggsave('plots/bed_thickness_elements_climate_sandy_plot.jpeg', width = 90, height = 180, units = 'mm', device = 'jpeg')

# 
# 
# bed_thickness_climate_sandy$bed_thickness_ln <- log(bed_thickness_climate_sandy$bed_thickness)
# 
# qqnorm(bed_thickness_climate_sandy$bed_thickness_ln)
# lnorm_test(bed_thickness_climate_sandy$bed_thickness)
# ggqqplot(bed_thickness_climate_sandy$bed_thickness_ln)
# shapiro.test(bed_thickness_climate_sandy$bed_thickness_ln)
# hist(bed_thickness_climate_sandy$bed_thickness_ln)
# hist(bed_thickness_climate_sandy$bed_thickness)
# 
# fitdistr(bed_thickness_climate_sandy$bed_thickness, 'lognormal')
# mean(bed_thickness_climate_sandy$bed_thickness)
# 


bed_thickness_elements_climate_sandy_summary <- bed_thickness_climate_sandy %>% group_by(element_general_type, climate, sys_name) %>% summarise(mean_bed_thck = mean(bed_thickness), n =n ())

bed_thickness_climate_sandy_plot <- ggplot(bed_thickness_climate_sandy, aes(x=factor(element_general_type, level = selected_elements),
                                                                            y=bed_thickness, 
                 fill= climate))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  geom_text(data = bed_thickness_elements_climate_sandy_summary, aes(x=element_general_type, label = n), y = 2 , position = position_dodge2(0.9), size = 7/.pt)+
  coord_cartesian(ylim = c(0,2))+
  scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                               'icehouse' = icehouse_color
  ))+
  theme_classic()+
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
    legend.position = 'none',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    y= 'Bed thickness',
    fill = 'Climate')
bed_thickness_climate_sandy_plot

ggsave('plots/bed_thickness_climate_sandy_plot.pdf', width = 90, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/bed_thickness_climate_sandy_plot.jpeg', width = 90, height = 70, units = 'mm', device = 'jpeg')


bed_thickness_climate_sandy_violin_plot <- ggplot(bed_thickness_climate_sandy, aes(x=factor(element_general_type, level = selected_elements),
                                                                                   y=bed_thickness, 
                                                                            fill= climate))+
  geom_violin( lwd=0.23, width = 0.9)+
  # geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'), alpha = 0.3)+
  scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                               'icehouse' = icehouse_color
  ))+
  scale_y_continuous(trans='log10', breaks = c(0.001,0.01,0.1,1,10))+
  geom_text(data = bed_thickness_elements_climate_sandy_summary, aes(x=element_general_type, label = n), y = 10 , position = position_dodge2(0.9), size = 7/.pt)+
  theme_classic()+
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
    legend.position = 'none',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    y= 'Bed thickness',
    fill = 'Climate')
bed_thickness_climate_sandy_violin_plot

ggsave('plots/bed_thickness_climate_sandy_violin_plot.pdf', width = 90, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/bed_thickness_climate_sandy_violin_plot.jpeg', width = 90, height = 70, units = 'mm', device = 'jpeg')

# geom_text(data = data_count, aes(x=factor(code, level = gs_bed_order_avgthck), label = n), y = ymax-0.1, position = position_dodge2(0.9), size = 7/.pt)+
#terminal deposit

# sandy_terminal_climate_bed_sum <- beds_table_sandgs_lam_features %>% group_by(code, sys_gs_category, element_general_type, climate) %>% summarise(n=n())
# sandy_sys_terminal_climate <- beds_sand_sandgs_lam_sum %>% filter(sys_gs_category == 'sandy system') %>%  filter(code %in% sandy_top_beds_count_thck_5)
# 
# 
# 
# bed_thickness_sandy_terminal_climate <- beds_table_sandgs_lam_features %>% select(code,bed_thickness,element_general_type,climate) %>%
#   filter(code %in% sandy_sys_terminal_climate_selected) %>% filter(element_general_type == 'terminal deposit') %>% filter(climate %in% c('icehouse','greenhouse'))
# 
# bed_thickness_sandy_terminal_climate_plot <- ggplot(bed_thickness_sandy_terminal_climate, 
#                                                   aes(x=code,y=bed_thickness, fill=climate))+
#   geom_boxplot(outlier.shape = NA, lwd=0.23)+
#   coord_cartesian(ylim = c(0,1.5))+
#   theme_classic()+
#   theme(legend.position = 'bottom')+
#   ggtitle('Sandy systems, terminal deposits climate bed thickness difference')+
#   theme(axis.line = element_line(colour = 'black', size = 0.23),
#         axis.ticks = element_line(colour = 'black', size = 0.23))
# bed_thickness_sandy_terminal_climate_plot
# 
# 
# beds_sand_sandgs_lam_sum_terminal <- beds_sand_sandgs_lam_sum %>% filter(element_general_type == 'terminal deposit')
# beds_sand_sandgs_lam_sum_terminal_selected <- beds_sand_sandgs_lam_sum_terminal %>% filter(n>50) %>% pull(code) %>% as.list()
# bed_thickness_terminal <- beds_table_sandgs_lam %>% select(code,bed_thickness) %>% filter(code %in% beds_sand_sandgs_lam_sum_terminal_selected)
# 
# 
# 
# 
# ggplot(bed_thickness_terminal, aes(x = bed_thickness, y = code, fill = code)) + 
#   geom_density_ridges2(alpha = 0.5, jittered_points = TRUE, point_size = 0.5, point_alpha = 0.1)+
#   coord_cartesian(xlim=c(0,1))+
#   scale_x_continuous(expand = c(0,0))+
#   theme_classic()
### MUD THICKNESS ----------------

thck_stacked_facies <- facies_data %>% 
  mutate(facies_type = case_when((facies_type %in% mud_facies ~ 'M'),(!facies_type %in% mud_facies ~ facies_type)))%>%
  mutate(clust_lag = 1) %>%
  mutate(clust_lag = case_when((log_ID != lag(log_ID) | facies_type != lag(facies_type) ~ 1), TRUE ~ 0))%>%
  mutate(cluster_group = cumsum(clust_lag))
thck_stacked_mud <- thck_stacked_facies %>% filter(!is.na(thickness)) %>% 
  filter(facies_type == "M") %>% group_by(cluster_group) %>% mutate(stacked_thickness = sum(thickness)) %>% distinct(cluster_group, .keep_all = TRUE) %>% 
  ungroup()  %>% filter(element_general_type %in% selected_elements) %>%   
  filter(climate %in% c('icehouse','greenhouse'))

thck_stacked_mud_sandy <- thck_stacked_mud %>% filter(sys_gs_category == 'sandy system')

thck_stacked_mud_summary<- thck_stacked_mud_sandy  %>% group_by(climate, element_general_type) %>% summarise(n = n(), mean_thck = mean(stacked_thickness))
thck_stacked_mud_summary_sysname<- thck_stacked_mud_sandy  %>% group_by(climate, element_general_type, sys_name) %>% summarise(n = n(), mean_thck = mean(stacked_thickness), min_thck = min(stacked_thickness))

stacked_mud_element_climate <- ggplot(thck_stacked_mud_sandy , aes(x=factor(element_general_type, level = selected_elements),
                                                                            y=stacked_thickness, 
                                                                            fill= climate))+
  geom_boxplot(outlier.shape = NA, lwd=0.23, width = 0.9, position = position_dodge2(0.9, preserve = 'single'))+
  geom_text(data = thck_stacked_mud_summary, aes(x=element_general_type, label = n), y = 2 , position = position_dodge2(0.9), size = 7/.pt)+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15, position = position_dodge2(0.9, preserve = 'total'))+
  coord_cartesian(ylim = c(0,2))+
  scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                               'icehouse' = icehouse_color
  ))+
  theme_classic()+
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
    legend.position = 'none',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    y= 'Stacked mud thickness',
    fill = 'Climate')
stacked_mud_element_climate

ggsave('plots/stacked_mud_element_climate.pdf', width = 90, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/stacked_mud_element_climate.jpeg', width = 90, height = 70, units = 'mm', device = 'jpeg')


stacked_mud_element_climate_violin_plot <- ggplot(thck_stacked_mud_sandy , aes(x=factor(element_general_type, level = selected_elements),
                                                                                                       y=stacked_thickness, 
                                                                                                       fill= climate))+
  geom_violin( lwd=0.23, width = 0.9)+
  scale_y_continuous(trans='log10', breaks = c(0.001,0.01,0.1,1,10))+ 
  scale_fill_manual(values = c('greenhouse' = greenhouse_color,
                               'icehouse' = icehouse_color
  ))+
  theme_classic()+
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
    legend.position = 'none',
    legend.key.height = unit(3.5, 'mm'),
    legend.key.width = unit(5, 'mm'))+
  labs(
    y= 'Stacked mud thickness',
    fill = 'Climate')
stacked_mud_element_climate_violin_plot

ggsave('plots/stacked_mud_element_climate_violin_plot.pdf', width = 90, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/stacked_mud_element_climate_violin_plot.jpeg', width = 90, height = 70, units = 'mm', device = 'jpeg')