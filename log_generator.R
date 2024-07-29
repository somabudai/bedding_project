#PACKAGES AND SOURCES
library('MASS')
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
# library(ggcats)
# source('config.R')
# source('bed_data.R')
# source('log_trend.R')


#ATTRIBUTES

selected_sys_type <- c('sandy system')
selected_element <- c('terminal deposit')
selected_climate <- c('greenhouse','icehouse','uncertain') #c('greenhouse','icehouse','uncertain')

if (length(selected_element) != 1) {stop('Select only one architectural element to modell!')}

#SET N-G
try_to_force_NG <- FALSE
selected_NG_margin <- 0.05

mean_NG_value <- element_ng_thck_element_type %>% 
  filter(sys_gs_category %in% selected_sys_type) %>%
  filter(general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  summarise(mean_ng = mean(NG))%>% pull(mean_ng) %>% as.numeric()

if (is.na(selected_NG_value)) {stop('Filtering the net-to-gross table gave no result!')}

selected_NG_value <- mean_NG_value #change this for specific NG value

#BED FREQUENCY
stochastic_bed_frequency <- TRUE

#include and exclude certain bed types
include_SM_beds <- TRUE
include_G_beds <- TRUE

#THICKNESS ATTRIBUTES
selected_sand_thickness <- 8
selected_sand_thickness_margin<- 0.5

average_bed_thickness <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type) %>% 
  summarise(mean_bed_thck = mean(bed_thickness)) %>% pull(mean_bed_thck) %>% as.numeric()

estimated_bed_number <-  round(selected_sand_thickness/average_bed_thickness, 0)
estimated_bed_number
selected_bed_number <- estimated_bed_number #Change this for specific bed number
selected_bed_number

#SEED
seed <- 3

### CODE --------------------

#BED NUMBERS

#calculate the frequency of each bed type given the input parameters

if (include_SM_beds == TRUE & include_G_beds == TRUE) {
  selected_bed_frequency <- beds_table_sandgs_lam_features %>% 
    filter(element_general_type %in% selected_element) %>% 
    filter(climate %in% selected_climate) %>%
    filter(sys_gs_category %in% selected_sys_type) %>% 
    group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
    mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100)
  } else if (include_SM_beds == FALSE & include_G_beds == TRUE) {
    selected_bed_frequency <- beds_table_sandgs_lam_features %>% 
      filter(element_general_type %in% selected_element) %>% 
      filter(climate %in% selected_climate) %>%
      filter(sys_gs_category %in% selected_sys_type) %>% 
      filter(gs_category != 'SM') %>%
      group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
      mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100) 
  } else if (include_SM_beds == TRUE & include_G_beds == FALSE) {
    selected_bed_frequency <- beds_table_sandgs_lam_features %>% 
      filter(element_general_type %in% selected_element) %>% 
      filter(climate %in% selected_climate) %>%
      filter(sys_gs_category %in% selected_sys_type) %>% 
      filter(!gs_category %in% c('G','sG','gS')) %>%
      group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
      mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100)
  } else if (include_SM_beds == FALSE & include_G_beds == FALSE) {
    selected_bed_frequency <- beds_table_sandgs_lam_features %>% 
      filter(element_general_type %in% selected_element) %>% 
      filter(climate %in% selected_climate) %>%
      filter(sys_gs_category %in% selected_sys_type) %>% 
      filter(!gs_category %in% c('G','sG','gS', 'SM')) %>%
      group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
      mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100)
  } else {stop('Invalid include_SM_beds and/or include_G_beds value try TRUE or FALSE')}


if (nrow(selected_bed_frequency) == 0) {stop('Filtering the bed table gave no result!')}

#deterministic approach
#calculate the percentage of each bed type based on the selected bed number

if (stochastic_bed_frequency == FALSE) {

bed_occurrence <- selected_bed_frequency %>% mutate(bed_type_n = round(selected_bed_number * percent/100,0))  %>% filter(bed_type_n !=0)

#GENERATE LIST IN WHICH BED TYPES ARE LISTED AS MANY TIMES AS THEIR OCCURRENCE CALCULATED ABOVE

#list of bed types present in the modeled element
present_bed_types <- bed_occurrence  %>% pull(code) %>% as.list()
#number of each bed type present in the modeled element based on the input bed number
present_bed_types_number <- bed_occurrence  %>% pull(bed_type_n) %>% as.list() 
bed_list <- list()

for (code_input in present_bed_types) {
  
  number_of_bed_type <- bed_occurrence %>% filter(code == code_input) %>% pull(bed_type_n)
  print(number_of_bed_type)
  
  
  for (i in 1:number_of_bed_type ) {
    
    bed_list[[length(bed_list)+1]] = code_input
  }
}
combined_df <- data.frame(code = unlist(bed_list))
bed_list
} else if (stochastic_bed_frequency == TRUE) {
#stochastic approach

bed_types <- selected_bed_frequency %>% pull(code) %>% as.list()
bed_type_prob <- selected_bed_frequency %>% pull(percent) %>% as.list()

set.seed(seed)
random_bed <- sample(bed_types, selected_bed_number , replace = TRUE, prob = bed_type_prob)
combined_df <- data.frame(code = unlist(random_bed)) %>% arrange(code)
bed_occurrence <- combined_df %>% group_by(code) %>% summarise(bed_type_n = n()) %>% ungroup()
present_bed_types <- combined_df %>% distinct(code) %>% pull(code) %>% as.list()

}


#ADD BED THICKNESS

selected_code_thickness <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type) %>%
  dplyr::select(code, bed_thickness)
if (nrow(selected_code_thickness) == 0) {stop('Filtering the bed table for bed thickness gave no result!')}

sum_sand_thickness = 0
seed_thck = seed
iteration_count_sand = 0
while (!between(sum_sand_thickness,selected_sand_thickness-selected_sand_thickness_margin,selected_sand_thickness+selected_sand_thickness_margin)) {
  
  iteration_count_sand <- iteration_count_sand+1
  if(iteration_count_sand == 100){stop('Desired sand thickness wasnt reached within 100 iterations, try changing margin value or selected bed number!')}
  
  thickness_list <- list()
  for (bed_type in present_bed_types) {
    # print(bed_type)
    selected_thickness_data <- selected_code_thickness %>% filter(code == bed_type)
    code_mean_thickness <- mean(selected_thickness_data$bed_thickness)
    code_median_thickness <- median(selected_thickness_data$bed_thickness)
    # print(code_mean_thickness)
    fitted_distribution <- fitdistr(selected_thickness_data$bed_thickness, 'lognormal')
    
    meanlog <- fitted_distribution$estimate[['meanlog']]
    sdlog <- fitted_distribution$estimate[['sdlog']]
    
    sample_number <- bed_occurrence %>% filter(code == bed_type) %>% pull(bed_type_n) %>% as.numeric()
    
    seed_thck <- seed_thck+1
    set.seed(seed_thck)
    
    
    if (sample_number == 1) {random_bed_thickness = code_median_thickness} else {random_bed_thickness <-  rlnorm(sample_number,meanlog,sdlog)}
    
    # print(random_bed_thickness)
    thickness_list[[length(thickness_list)+1]] = random_bed_thickness
    
  }
  # thickness_list
  
  
  sum_sand_thickness <- sum(unlist(thickness_list))

  # print(seed)
  print(sum_sand_thickness)
}
combined_df$thck <- unlist(thickness_list)
modeled_thck_avg <- mean(combined_df$thck)
#ADD MUD BEDS FROM TRANSITION

transition_probabilities <- bed_under_over %>% dplyr::select(code, overlying_ft, sys_gs_category, element_general_type, climate) %>%
                            filter(element_general_type %in% selected_element) %>% 
                            filter(climate %in% selected_climate) %>%
                            filter(sys_gs_category %in% selected_sys_type) %>%
                            filter(overlying_ft %in% c('S','M','G')) %>%
                            group_by(code, overlying_ft) %>% summarise(n = n())%>%
                            ungroup() %>% group_by(code) %>% mutate(sum_n = sum(n)) %>% mutate(trans_prob = n/sum_n*100) %>% ungroup()

if (nrow(transition_probabilities) == 0) {stop('Filtering the transition table gave no result!')}

random_trans_list <- list()
for (bed_type in present_bed_types) {
  transition_probabilities_selected <- transition_probabilities %>% filter(code == bed_type)
  
  overlying_type <- transition_probabilities_selected %>% pull(overlying_ft) %>% as.list()
  probabilities <- transition_probabilities_selected %>% pull(trans_prob) %>% as.list()
  
  sample_number <- bed_occurrence %>% filter(code == bed_type) %>% pull(bed_type_n) %>% as.numeric()
  print(sample_number)
  set.seed(seed)
  random_trans <- sample(overlying_type, sample_number, replace = TRUE, prob = probabilities)
  
  random_trans_list[[length(random_trans_list)+1]] = random_trans

}
combined_df$trans <- unlist(random_trans_list)

#ORDER BED TYPES

#create probabilities for negative, zero and positive log_trend_value from log_vertical_bed_thickness

#randomly order beds and calculate log_trend_value until value is within the experienced range

selected_log_vertical_bed_thickness <- log_vertical_bed_thickness %>%
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type)

if (nrow(selected_log_vertical_bed_thickness) == 0) {stop('Filtering the vertical changes table gave no result!')}

selected_log_vertical_bed_thickness_probability <- selected_log_vertical_bed_thickness %>% filter(log_trend_value != 0) %>% group_by(log_trend) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(sum_n = sum(n)) %>% mutate(probabilities = n/sum_n*100)

log_vertical_bed_thickness_trends <- selected_log_vertical_bed_thickness_probability %>% pull(log_trend) %>% as.list()
log_vertical_bed_thickness_probabilities <- selected_log_vertical_bed_thickness_probability %>% pull(probabilities) %>% as.list()

set.seed(seed)
random_thickness_trend <- sample(log_vertical_bed_thickness_trends, 1, replace = TRUE, prob = log_vertical_bed_thickness_probabilities)
random_thickness_trend

if (random_thickness_trend == 'thickening') {log_trend_value_min <- 0
log_trend_value_max <- max(selected_log_vertical_bed_thickness$log_trend_value)} else if (random_thickness_trend == 'thinning') {
  log_trend_value_min <- min(selected_log_vertical_bed_thickness$log_trend_value)
  log_trend_value_max <- 0}

log_trend_value_min
log_trend_value_max
#shuffle data

log_trend_seed <- seed

if (random_thickness_trend == 'thickening') {calculated_log_trend_value <- -1} else if (random_thickness_trend == 'thinning') {calculated_log_trend_value <- 1}

while (!between(calculated_log_trend_value,log_trend_value_min,log_trend_value_max)) {
  # calculated_log_trend_value > log_trend_value_max | calculated_log_trend_value < log_trend_value_min
log_trend_seed <- log_trend_seed+1
set.seed(log_trend_seed)

random_ordered_combined_df <- combined_df[sample(1:nrow(combined_df)),] 

calculated_bed_thck_difference <- random_ordered_combined_df %>% mutate(bed_thck_difference = lead(thck)-thck) %>% filter(!is.na(bed_thck_difference))

calculated_log_trend_value <- sum(calculated_bed_thck_difference$bed_thck_difference)/sum(calculated_bed_thck_difference$thck)

print(calculated_log_trend_value)

}

combined_df <- random_ordered_combined_df


#ADD MUD THICKNESS

#estimate mud content based on NG values from dataset


# estimated_mud_thickness <- as.numeric((sum_sand_thickness/selected_NG_value)-sum_sand_thickness)

#get number of mud beds based on transitions
number_of_mud_beds <- nrow(combined_df[combined_df$trans == 'M',])
# number_of_mud_beds

#estimate mean mud thickness based on dataset NG and mud bed number
# estimated_mud_thickness_mean <- estimated_mud_thickness/number_of_mud_beds

#select mud thickness values from dataset
selected_mud_thickness_values <- thck_stacked_mud %>%
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type)

if (nrow(selected_mud_thickness_values) == 0) {stop('Filtering the mud thickness table gave no result!')}

#mud thickness mean of data selected from dataset
selected_mud_thickness_mean <- mean(selected_mud_thickness_values$stacked_thickness)

# print(estimated_mud_thickness_mean)
# print(selected_mud_thickness_mean)

fitted_distribution_mud <- fitdistr(selected_mud_thickness_values$stacked_thickness, 'lognormal')

meanlog_mud <- fitted_distribution_mud$estimate[['meanlog']]
sdlog_mud <- fitted_distribution_mud$estimate[['sdlog']]
# mud_thickness_list <- list()
if (try_to_force_NG == TRUE) {

    sum_mud_thickness <- 0
    modelled_NG <- -1
    seed_mud_thck <- seed
    # print(seed_mud_thck)
    
    iteration_count_mud <- 0
    
    if (selected_NG_value + selected_NG_margin < 0 | selected_NG_value - selected_NG_margin > 1) {stop('Desired NG value is impossible because it is below 0 or above 1. Change selected_NG_margin value!')}
    
    while (!between(modelled_NG,selected_NG_value-selected_NG_margin,selected_NG_value+selected_NG_margin)) {
      
      iteration_count_mud <- iteration_count_mud+1
      if(iteration_count_mud == 100){stop('Desired NG value couldnt be reached! Try setting try_to_force_NG to FALSE or change margin value!')}
      
      
      seed_mud_thck <- seed_mud_thck+1
      set.seed(seed_mud_thck)
      mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
      sum_mud_thickness <- sum(unlist(mud_thickness_list))
      print(sum_mud_thickness)
      modelled_element_thickness <- sum_sand_thickness+sum_mud_thickness
      modelled_NG <- round(sum_sand_thickness/modelled_element_thickness,2)
      ng_calc <- 'forced'
  }
} else if(try_to_force_NG == FALSE) {
  set.seed(seed)
  mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
  sum_mud_thickness <- sum(unlist(mud_thickness_list))
  
  modelled_NG <- round(sum_sand_thickness/(sum_sand_thickness+sum_mud_thickness),2)
  ng_calc <- 'not forced'}  



# selected_mud_thickness
# mud_thickness_list


#CREATE ORDER
combined_df <- combined_df %>% mutate_at(c('trans'), as.character)

#add number to beds and overlying beds to generate order, it will be separated into two dataframes and then unionised
combined_df$n <- seq.int(1,nrow(combined_df)*2, by = 2)
combined_df$n_2 <- seq.int(2,nrow(combined_df)*2, by = 2)

#dataframe for beds and their order
combined_df_beds <- combined_df %>% dplyr::select(code,thck,n)

#dataframe for muddy intervals
combined_df_mud <- combined_df %>% dplyr::select(trans,n_2) %>% filter(trans == 'M') %>% rename('code' = 'trans', 'n' = 'n_2')

if (nrow(combined_df_mud) == 0) {combined_df_ordered <- combined_df} else {
  
  #create dataframe for mud intervals and their thickness with the same order number
  combined_df_mud_order <- combined_df_mud %>% pull(n) %>% as.list()
  mud_df <- data.frame(n = unlist(combined_df_mud_order), thck = unlist(mud_thickness_list))
  combined_df_mud <- left_join(combined_df_mud, mud_df, by = 'n')
  
  #union of the two dataframes and arranged by assigned number to generate vertical trend
  combined_df_ordered <- union(combined_df_beds, combined_df_mud) %>% arrange(n)
  
}



#REPORT

report_df <- data.frame(
  variable = c('seed','selected system type','selected element', 'selected climate', 'force net-to-gross', 'selected net-to-gross',
               'net-to-gross margin','modelled net-to-gross', 'stochastic bed frequency', 'selected bed number', 'selected sand thickness (m)',
               'sand thickness margin', 'modelled sand thickness (m)', 'modelled mud thickness (m)', 'modelled element thickness (m)' ,'vertical trend'),
  value = c(seed,
            paste(selected_sys_type, collapse = ','),
            paste(selected_element, collapse = ','),
            paste(selected_climate, collapse = ','),
            try_to_force_NG,
            round(selected_NG_value,2),
            selected_NG_margin,
            modelled_NG,
            stochastic_bed_frequency, 
            selected_bed_number,
            selected_sand_thickness,
            selected_sand_thickness_margin,
            round(sum_sand_thickness,2),
            round(sum_mud_thickness,2),
            round(sum_sand_thickness,2)+round(sum_mud_thickness,2),
            round(calculated_log_trend_value,2))
            )


#COLORS

colors_phi_df <- data.frame(
  code = c("G-N-sl",	"G-N-l",	"G-N-x",	"gS-F-sl",	"gS-F-x",	"gS-N-sl",	"gS-N-x",	"gS-B-sl",	"gS-C-sl",	"gS-B-x",	"S-N-sl",	"S-N-l",	"S-N-x",	"S-F-x",	"S-F-sl",	
           "S-F-l",	"S-B-x",	"S-B-sl",	"S-C-sl",	"S-C-x",	"S-C-l",	"S-B-l",	"sG-F-sl",	"sG-N-sl",	"sG-F-x",	"sG-N-l",	"sG-N-x",	"sG-B-sl",	"sG-C-sl",	"SM-N-sl",	
           "SM-F-sl",	"SM-N-l",	"SM-N-x",	"SM-B-x",	"SM-F-x",	"SM-C-l",	"SM-C-sl",	"SM-C-x",	"SM-B-sl",	"M"),
  color = c("#BA4E22",	"#BA6933",	"#C16D0D",	"#6787B2",	"#4C7BB1",	"#28649E",	"#005890",	"#064679",	"#113350",	"#10283E",	"#FFCC00",	"#F1C754",	"#EFE977",	
            "#2FAC66",	"#00A19A",	"#6EBD8D",	"#6A528C",	"#473781",	"#DC4541",	"#D60F3B",	"#AF1035",	"#EB5A6A",	"#6D8E40",	"#A6C176",	"#83A153",	"#CCE0A6",	
            "#A9C965",	"#E2D158",	"#FCD760",	"#74522D",	"#8A6538",	"#9E7745",	"#AE8A4F",	"#DDC08F",	"#7A6951",	"#DA9B73",	"#D4A155",	"#BE8B5E",	"#9DA27D",
            "#C5B9B8"),
  phi = c(5,	5,	5,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	4,	4,	4,	4,	4,	4,	4,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	1)
)


#GENERATE LOG

df_for_log <- combined_df_ordered %>% mutate(facies_top = cumsum(thck)) %>% mutate(facies_base = lag(facies_top)) %>%
  mutate(n = seq.int(1:nrow(combined_df_ordered))) %>% mutate(facies_base = replace(facies_base, n == 1, 0))

df_for_log <- left_join(df_for_log, colors_phi_df, by = 'code')

title <- paste(selected_sys_type, selected_element, paste(selected_climate, collapse = ','), sep = ' ')
subtitle <- paste('seed =', seed, ' N/G = ', modelled_NG, ng_calc, ' sand thickness =', selected_sand_thickness, 'sand mean thck =', round(modeled_thck_avg,2), '\n log trend =', round(calculated_log_trend_value,3))

log_plot <- ggplot(df_for_log) + geom_rect(xmin = 0, color = 'black', aes(xmax = phi, ymin = facies_base, ymax=facies_top, fill = color), lwd = 0.12)+
  scale_fill_identity(guide = 'legend', labels = df_for_log$code, breaks = df_for_log$color)+
  scale_x_continuous(expand = c(0,0), limits = c(0,5), breaks = seq(1,5, by = 1), labels = c('1' = 'M', '2' = 'SM', '3' = 'S', '4' = 'sG', '5' = 'G'))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,sum_mud_thickness+sum_sand_thickness, by=1))+
  labs(title = title,
       subtitle = subtitle)+
  theme_classic()+
  theme(
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    axis.text.x = element_text(color = "black", size = 7),
    legend.position = 'bottom',
    legend.text = element_text(size = 7),
    legend.key.height = unit(4.5, 'mm'),
    legend.key.width = unit(6, 'mm')
  )+
  labs(
    fill = 'Bed type'
  )
log_plot


#CREATE DIRECTORY AND SAVING OUTPUTS

#create folder for outputs

time <- format(Sys.time(), '%Y_%m_%d_%H_%M_%S')
file_name <- paste(seed, paste(selected_element, collapse = ','), time, sep = '_' )

main_dir <- 'log_generator_outputs'
file_path <- paste('log_generator_outputs/',selected_element, sep='')

if(!file.exists(main_dir)) {
  dir.create(main_dir)}
if(!file.exists(file_path)){
  dir.create(file_path)
}

#SAVE OUTPUTS

write.csv(report_df, file = paste(file_path,'/',file_name,'_attributes','.csv', sep=''), row.names = FALSE)
write.csv(combined_df_ordered, file = paste(file_path,'/',file_name,'_log_df','.csv', sep=''), row.names = FALSE)
ggsave(log_plot, filename = paste(file_path,'/',file_name,'_log','.pdf', sep=''), device = 'pdf')


#WRITE CSVS
# 
# write.csv(beds_table_sandgs_lam_features, 'beds_table.csv')
# write.csv(bed_under_over, 'bed_under_over.csv')
# write.csv(log_vertical_bed_thickness, 'log_trend_thck.csv')
# write.csv(element_ng_thck_element_type, 'ng_data.csv')
# write.csv(thck_stacked_mud, 'mud_thck.csv')

### VALIDATING PLOTS --------------------

#N-G comparison plot

selected_NG_data <- element_ng_thck_element_type %>% 
  filter(sys_gs_category %in% selected_sys_type) %>%
  filter(general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate)

NG_data_counts <- selected_NG_data %>% group_by(general_type) %>% summarise(n = n())

ng_validating_plot <- ggplot()+
  geom_boxplot(data = selected_NG_data, aes(x=general_type, y = NG, fill = general_type), outlier.shape = 4, lwd=0.23, width = 0.7)+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15)+
  geom_text(data = NG_data_counts,aes(x= general_type, label = paste('n=',n, sep = '')), y=1.02, size = 7/.pt)+
  geom_point(aes(x=selected_element,y = modelled_NG), size = 4, shape = 23, fill = 'red')+
  scale_fill_manual(values = c('terminal deposit' = '#EFDC53',
                               'channel' = '#5C8CC7',
                               'levee' = '#7EC07C'))+
  theme_classic()+
  theme(
    axis.text.x = element_text(color = "black", size = 9),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    plot.title = element_text(color = 'black', size = 10),
    legend.position = 'none')+
  labs(
    title = 'Sand fraction',
    y= 'Sand fraction')
ng_validating_plot

#bed thickness validating plot
selected_bed_data <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type) %>%
  filter(code %in% present_bed_types)

selected_bed_data<- left_join(selected_bed_data, colors_phi_df, by = 'code')

selected_bed_data_summary <- selected_bed_data %>% group_by(code) %>% summarise(n = n())
modeled_bed_data_summary <- combined_df %>% group_by(code) %>% summarise(n = n())

bed_thickness_validating_plot <- ggplot(selected_bed_data, aes(x= code, y=bed_thickness))+
  geom_boxplot(aes(fill = color), outlier.shape = NA, lwd=0.23, width = 0.7)+
  scale_fill_identity(guide = 'legend', labels = selected_bed_data$code, breaks = selected_bed_data$color)+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15)+
  geom_text(data = selected_bed_data_summary, aes(x=code, label = n), y = 2, size = 7/.pt)+
  coord_cartesian(ylim = c(0,2))+
  geom_point(data = combined_df, aes(x =code, y=thck), size = 4, shape = 23, fill = 'red', alpha = 0.5)+
  geom_text(data = modeled_bed_data_summary , aes(x=code, label = n), y = 1.9, color = 'red', size = 7/.pt)+
  theme_classic()+
  theme(
    legend.position = 'none',
    legend.text = element_text(size = 7),
    legend.title = element_text(face = 'bold', size = 7),
    axis.text.x = element_text(color = "black", size = 9, angle = 45, vjust = 1, hjust = 1),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    plot.title = element_text(color = 'black', size = 10))+
  labs(
    title = 'Sand bed thickness',
    y= 'Bed thickness')
bed_thickness_validating_plot


#system wise

# bed_thickness_climate_sandy_gen_selected <- bed_thickness_climate_sandy_gen %>% filter(element_general_type == selected_element)
# bed_thickness_climate_sandy_gen_selected[nrow(bed_thickness_climate_sandy_gen_selected)+1,] = list('all',0,'all','all','all','modelled')
# ggplot(bed_thickness_climate_sandy_gen_selected, aes(x=sys_name, y = bed_thickness)) + geom_boxplot(outlier.shape = NA) + geom_point()+ 
#   geom_point(data = combined_df, x = 'modelled', aes(y=thck), color = 'red') + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust=1))

#mud thickness validating plot


mud_thickness_validating_plot <- ggplot()+
  geom_boxplot(data = selected_mud_thickness_values, aes(x=facies_type, y = stacked_thickness), outlier.shape = 4, outlier.alpha = 0.5, lwd=0.23, width = 0.7)+
  stat_summary(fun.y = 'mean', size = 0.2, shape = 15)+
  geom_text(aes(label = paste('n=',nrow(selected_mud_thickness_values), sep = '')), x=0.75, y=0.75, size = 7/.pt)+
  geom_point(data = combined_df_mud, aes(y=thck), x = 'M', size = 4, shape = 23, fill = 'red', alpha = 0.5, stroke = 0.12,)+
  geom_text(aes(label = paste('n=',nrow(combined_df_mud), sep = '')), y=0.70, x=0.75, size = 7/.pt, color = 'red')+
  coord_cartesian(ylim = c(0,0.75))+
  scale_y_continuous(breaks = seq(0,0.75, by = 0.25))+
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
    title = 'Mud thickness',
    y= 'Mud thickness (m)')
mud_thickness_validating_plot

#Log trend validating plot

log_trend_validating_plot <- ggplot() + 
  geom_point(data = selected_log_vertical_bed_thickness, (aes(x=element_general_type, y = log_trend_value)),position=position_jitter(width = 0.1), shape = 21, size = 2, stroke = 0.12, fill = '#256D8C')+
  annotate(geom= 'text', x=0.5, y= 0.3, label='thickening upward', angle = 90, size = 7/.pt)+
  annotate(geom= 'text', x=0.5, y= -0.3, label='thinning upward', angle = 90, size = 7/.pt)+
  coord_cartesian(ylim = c(-0.75,0.75))+
  geom_hline(yintercept = 0, alpha = 0.3, lwd=0.23)+
  geom_text(label = paste('n=',nrow(selected_log_vertical_bed_thickness), sep = ''), y=0.75, size = 7/.pt)+
  geom_point(aes(x = selected_element, y = calculated_log_trend_value), fill = 'red', size = 4, shape = 23, stroke = 0.12, alpha = 0.75)+
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
      title = 'Vertical trends',
      y= 'Thickness trend value')
log_trend_validating_plot
# hist(selected_log_vertical_bed_thickness$log_trend_value)

#combined validating plots

validating_plots <- ggarrange(
  ggarrange(ng_validating_plot, mud_thickness_validating_plot, log_trend_validating_plot, nrow = 1), bed_thickness_validating_plot,
nrow =2
)
validating_plots
ggsave(validating_plots, filename = paste(file_path,'/',file_name,'_validation','.pdf', sep=''), device = 'pdf', width = 160, height = 160, units = 'mm')
