#PACKAGES AND SOURCES
library('MASS')
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggcats)
# source('config.R')
# source('bed_data.R')
# source('log_trend.R')


#ATTRIBUTES

selected_sys_type <- c('sandy system')
selected_element <- c('terminal deposit')
selected_climate <- c('icehouse')
try_to_force_NG <- TRUE
stochastic_bed_frequency <- TRUE

#THICKNESS ATTRIBUTES
selected_sand_thickness <- 15
selected_sand_thickness_buffer <- 0.5

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
seed <- 4

### CODE --------------------

#BED NUMBERS

#calculate the frequency of each bed type given the input parameters
selected_bed_frequency <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type == selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>%
  group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
  mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100)

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


sum_sand_thickness = 0
seed_thck = seed
while (!between(sum_sand_thickness,selected_sand_thickness-selected_sand_thickness_buffer,selected_sand_thickness+selected_sand_thickness_buffer)) {
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


# if (selected_element == 'terminal deposit') {combined_df <- combined_df %>% arrange(thck)} else if (selected_element == 'channel' | selected_element == 'levee') 
#   {combined_df <- combined_df %>% arrange(desc(thck))}


#ADD MUD THICKNESS

#estimate mud content based on NG values from dataset
selected_NG_value <- element_ng_thck_element_type %>% 
  filter(sys_gs_category %in% selected_sys_type) %>%
  filter(general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  summarise(mean_ng = mean(NG))%>% pull(mean_ng) %>% as.numeric()

estimated_mud_thickness <- as.numeric((sum_sand_thickness/selected_NG_value)-sum_sand_thickness)

#get number of mud beds based on transitions
number_of_mud_beds <- nrow(combined_df[combined_df$trans == 'M',])
# number_of_mud_beds

#estimate mean mud thickness based on dataset NG and mud bed number
estimated_mud_thickness_mean <- estimated_mud_thickness/number_of_mud_beds

#select mud thickness values from dataset
selected_mud_thickness_values <- thck_stacked_mud %>%
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type) %>%
  dplyr::select(stacked_thickness)
#mud thickness mean of data selected from dataset
selected_mud_thickness_mean <- mean(selected_mud_thickness_values$stacked_thickness)

# print(estimated_mud_thickness_mean)
# print(selected_mud_thickness_mean)

fitted_distribution_mud <- fitdistr(selected_mud_thickness_values$stacked_thickness, 'lognormal')

meanlog_mud <- fitted_distribution_mud$estimate[['meanlog']]
sdlog_mud <- fitted_distribution_mud$estimate[['sdlog']]
# mud_thickness_list <- list()
if (try_to_force_NG == TRUE) {

#if mud thickness mean estimated from set sand thickness and N/G is larger than the one originated from the dataset then it is impossible
#if I want to give mud thickness an order than I need to order mud thickness list
if (estimated_mud_thickness_mean > selected_mud_thickness_mean) {
  set.seed(seed)
  mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
  sum_mud_thickness <- sum(unlist(mud_thickness_list))
  ng_calc <- 'not forced'} else {

    sum_mud_thickness <- 0
    seed_mud_thck <- seed
    print(seed_mud_thck)
    
    while (!between(sum_mud_thickness,estimated_mud_thickness_thickness-selected_sand_thickness_buffer,estimated_mud_thickness+selected_sand_thickness_buffer)) {
      
      seed_mud_thck <- seed_mud_thck+1
      set.seed(seed_mud_thck)
      mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
      sum_mud_thickness <- sum(unlist(mud_thickness_list))
      print(seed_mud_thck)
      print(sum_mud_thickness)
      ng_calc <- 'forced'
  }
  }
} else if(try_to_force_NG == FALSE) {
  set.seed(seed)
  mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
  sum_mud_thickness <- sum(unlist(mud_thickness_list))
  ng_calc <- 'not forced'}  


modelled_NG <- round(sum_sand_thickness/(sum_sand_thickness+sum_mud_thickness),2)
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

#create dataframe for mud intervals and their thickness with the same order number
combined_df_mud_order <- combined_df_mud %>% pull(n) %>% as.list()
mud_df <- data.frame(n = unlist(combined_df_mud_order), thck = unlist(mud_thickness_list))
combined_df_mud <- left_join(combined_df_mud, mud_df, by = 'n')

#union of the two dataframes and arranged by assigned number to generate vertical trend
combined_df_ordered <- union(combined_df_beds, combined_df_mud) %>% arrange(n)


#GENERATE LOG

df_for_log <- combined_df_ordered %>% mutate(facies_top = cumsum(thck)) %>% mutate(facies_base = lag(facies_top)) %>%
  mutate(n = seq.int(1:nrow(combined_df_ordered))) %>% mutate(facies_base = replace(facies_base, n == 1, 0)) %>% 
  mutate(phi = case_when(code == 'M' ~ 1, TRUE ~ 2))

title <- paste(selected_sys_type, selected_element, selected_climate, sep = ' ')
subtitle <- paste('seed =', seed, ' N/G = ', modelled_NG, ng_calc, ' sand thickness =', selected_sand_thickness, 'sand mean thck =', round(modeled_thck_avg,2))

ggplot(df_for_log) + geom_rect(xmin = 0, color = 'black', aes(xmax = phi, ymin = facies_base, ymax=facies_top, fill = code))+
  scale_fill_brewer(palette="Paired")+
  scale_x_continuous(expand = c(0,0), limits = c(0,2), breaks = c(1,2), labels = c('1' = 'mud', '2' = 'sand'))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,sum_mud_thickness+sum_sand_thickness, by=1))+
  labs(title = title,
       subtitle = subtitle)+
  theme_classic()

file_name <- paste('seed_',seed,'_ng_calc_', try_to_force_NG, '_sys_',selected_sys_type,'_element_',selected_element,'_climate_',selected_climate,'_selected_sand_thickness_',selected_sand_thickness, sep = '' )
ggsave(filename = paste('plots/',file_name,'.pdf', sep=''), device = 'pdf')

### VALIDATING PLOTS --------------------

#NGPLOT

element_ng_sandy_log_gen <- element_ng_calc(sys_gs_category, selected_sys_type, general_type, selected_element, climate, selected_climate)

element_ng_climate_plot_gen <- element_ng_plot(element_ng_sandy_log_gen, climate)+
  geom_point(aes(x= selected_element, y = modelled_NG), size = 7, shape = 18, color = 'red')
element_ng_climate_plot_gen

#BED THICKNESS PLOT

bed_thickness_climate_sandy_gen <- beds_table_sandgs_lam_features %>% dplyr::select(code,bed_thickness,element_general_type,climate, sys_gs_category, sys_name) %>%
 filter(code %in% present_bed_types) %>% filter(element_general_type %in% selected_elements) %>%  
  filter(sys_gs_category == 'sandy system') %>% filter(climate %in% selected_climate)

bed_thickness_climate_sandy_summary_gen <- bed_thickness_climate_sandy_gen %>% group_by(element_general_type, climate,  code) %>% summarise(mean_bed_thck = mean(bed_thickness), n =n ())

modeled_bed_avg <- combined_df %>% group_by(code) %>% summarise(mean_thck = mean(thck))
modeled_thck_avg <- mean(combined_df$thck)
bed_thickness_climate_sandy_terminal_plot_gen <- climate_bed_thickness_plot(bed_thickness_climate_sandy_gen, bed_thickness_climate_sandy_summary_gen, selected_element ,3, selected_element)+
  geom_point(data = combined_df, aes(x=code, y = thck),color = 'red', size = 4, shape = 18, alpha = 0.5)+
  labs(title = title,
       subtitle = subtitle)
  # geom_cat(data = combined_df, aes(x=code, y = thck, cat = 'mouth'), size = 2, shape = 18, alpha = 0.5)
bed_thickness_climate_sandy_terminal_plot_gen

    #system wise

bed_thickness_climate_sandy_gen_selected <- bed_thickness_climate_sandy_gen %>% filter(element_general_type == selected_element)
bed_thickness_climate_sandy_gen_selected[nrow(bed_thickness_climate_sandy_gen_selected)+1,] = list('all',0,'all','all','all','modelled')
ggplot(bed_thickness_climate_sandy_gen_selected, aes(x=sys_name, y = bed_thickness)) + geom_boxplot(outlier.shape = NA) + geom_point()+ 
  geom_point(data = combined_df, x = 'modelled', aes(y=thck), color = 'red') + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust=1))

#log trend

ggplot() + geom_point(data = selected_log_vertical_bed_thickness, (aes(x=element_general_type, y = log_trend_value)), size = 2)+
  geom_point(aes(x=selected_element, y = calculated_log_trend_value), color = 'red', size = 4)+
  annotate(geom= 'text', x=0.5, y= 0.3, label='thickening upward', angle = 90)+
  annotate(geom= 'text', x=0.5, y= -0.3, label='thinning upward', angle = 90)+
  coord_cartesian(ylim = c(-0.75,0.75))+
  geom_hline(yintercept = 0, alpha = 0.3, lwd=0.23)+
  theme_classic()+
  theme(axis.line = element_line(colour = 'black', size = 0.23),
        axis.ticks = element_line(colour = 'black', size = 0.23))

hist(selected_log_vertical_bed_thickness$log_trend_value)

