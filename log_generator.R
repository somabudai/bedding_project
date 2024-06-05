library('MASS')
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#ATTRIBUTES

selected_sys_type <- 'sandy system'
selected_element <- 'channel'
selected_climate <- 'greenhouse'
try_to_force_NG <- FALSE

#THICKNESS ATTRIBUTES
selected_sand_thickness <- 20
selected_sand_thickness_buffer <- 1

average_bed_thickness <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type == selected_element) %>% 
  filter(climate == selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>% summarise(mean_bed_thck = mean(bed_thickness)) %>% pull(mean_bed_thck) %>% as.numeric()
estimated_bed_number <-  round(selected_sand_thickness/average_bed_thickness)
estimated_bed_number
selected_bed_number <- estimated_bed_number #Change this for specific bed number
selected_bed_number

#SEED
seed <- 1

#BED NUMBERS
bed_frequency <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type == selected_element) %>% 
  filter(climate == selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>%
  group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
  mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100)

bed_occurrence <- bed_frequency %>% mutate(bed_type_n = round(selected_bed_number * percent/100,0))  %>% filter(bed_type_n !=0)

#GENERATE LIST IN WHICH BED TYPES ARE LISTED AS MANY TIMES AS THEIR OCCURRENCE CALCULATED ABOVE

present_bed_types <- bed_occurrence  %>% pull(code) %>% as.list() 
present_bed_types_number <- bed_occurrence  %>% pull(bed_type_n) %>% as.list() 
bed_list <- list()

for (code_input in present_bed_types) {
  
  number_of_bed_type <- bed_occurrence %>% filter(code == code_input) %>% pull(bed_type_n)
  print(number_of_bed_type)
  
  
  for (i in 1:number_of_bed_type ) {
    
    bed_list[[length(bed_list)+1]] = code_input
  }
  
  
}

bed_list

#ADD BED THICKNESS

selected_code_thickness <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type == selected_element) %>% 
  filter(climate == selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>%
  dplyr::select(code, bed_thickness)


sum_sand_thickness = 0
seed_thck = seed
while (sum_sand_thickness < selected_sand_thickness-selected_sand_thickness_buffer | sum_sand_thickness > selected_sand_thickness+selected_sand_thickness_buffer) {
  thickness_list <- list()
  for (bed_type in present_bed_types) {
    # print(bed_type)
    selected_thickness_data <- selected_code_thickness %>% filter(code == bed_type)
    code_mean_thickness <- mean(selected_thickness_data$bed_thickness)
    # print(code_mean_thickness)
    fitted_distribution <- fitdistr(selected_thickness_data$bed_thickness, 'lognormal')
    
    meanlog <- fitted_distribution$estimate[['meanlog']]
    sdlog <- fitted_distribution$estimate[['sdlog']]
    
    sample_number <- bed_occurrence %>% filter(code == bed_type) %>% pull(bed_type_n) %>% as.numeric()
    
    seed_thck <- seed_thck+1
    set.seed(seed_thck)
    
    
    if (sample_number == 1) {random_bed_thickness = code_mean_thickness} else {random_bed_thickness <-  rlnorm(sample_number,meanlog,sdlog)}
    
    # print(random_bed_thickness)
    thickness_list[[length(thickness_list)+1]] = random_bed_thickness
    
  }
  # thickness_list
  
  combined_df <- data.frame(code = unlist(bed_list), thck = unlist(thickness_list))
  sum_sand_thickness <- sum(unlist(thickness_list))
  # print(seed)
  # print(sum_sand_thickness)
}



#ADD MUD BEDS FROM TRANSITION

transition_probabilities <- bed_under_over %>% dplyr::select(code, overlying_ft, sys_gs_category, element_general_type, climate) %>%
                            filter(element_general_type == selected_element) %>% 
                            filter(climate == selected_climate) %>%
                            filter(sys_gs_category == selected_sys_type) %>%
                            filter(overlying_ft %in% c('S','M')) %>%
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

if (selected_element == 'terminal deposit') {combined_df <- combined_df %>% arrange(thck)} else if (selected_element == 'channel' | selected_element == 'levee') 
  {combined_df <- combined_df %>% arrange(desc(thck))}


#ADD MUD THICKNESS

#estimate mud content based on NG values from dataset
NG_value <- element_ng_thck_element_type %>% 
  filter(general_type == selected_element) %>% 
  filter(climate == selected_climate) %>%
  summarise(mean = mean(NG))

estimated_mud_thickness <- as.numeric((sum_sand_thickness/NG)-sum_sand_thickness)

#get number of mud beds based on transitions
number_of_mud_beds <- nrow(combined_df[combined_df$trans == 'M',])
# number_of_mud_beds

#estimate mean mud thickness based on dataset NG and mud bed number
estimated_mud_thickness_mean <- estimated_mud_thickness/number_of_mud_beds

#select mud thickness values from dataset
selected_mud_thickness_values <- thck_stacked_mud %>%
  filter(element_general_type == selected_element) %>% 
  filter(climate == selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>%
  dplyr::select(stacked_thickness)
#mud thickness mean of data selected from dataset
selected_mud_thickness_mean <- mean(selected_mud_thickness_values$stacked_thickness)

# print(estimated_mud_thickness_mean)
# print(selected_mud_thickness_mean)

fitted_distribution_mud <- fitdistr(selected_mud_thickness_values$stacked_thickness, 'lognormal')

meanlog_mud <- fitted_distribution_mud$estimate[['meanlog']]
sdlog_mud <- fitted_distribution_mud$estimate[['sdlog']]

if (try_to_force_NG == TRUE) {

#if mud thickness mean estimated from set sand thickness and N/G is larger than the one originated from the dataset then it is impossible
if (estimated_mud_thickness_mean > selected_mud_thickness_mean) {
  set.seed(seed)
  mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
  sum_mud_thickness <- sum(unlist(mud_thickness_list))
  ng_calc <- 'not forced'} else {

    sum_mud_thickness <- 0
    seed_mud_thck <- seed
    print(seed_mud_thck)
    
    while (sum_mud_thickness < estimated_mud_thickness-selected_sand_thickness_buffer | sum_mud_thickness > estimated_mud_thickness+selected_sand_thickness_buffer) {
      
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
combined_df_mud <- combined_df %>% dplyr::select(trans,n_2) %>% filter(trans != 'S') %>% rename('code' = 'trans', 'n' = 'n_2')

#create dataframe for mud intervals and their thickness with the same order number
combined_df_mud_order <- combined_df_mud %>% pull(n) %>% as.list()
mud_df <- data.frame(n = unlist(combined_df_mud_order), thck = unlist(mud_thickness_list))
combined_df_mud <- left_join(combined_df_mud, mud_df, by = 'n')

#union of the two dataframes and arranged by assigned number to generate vertical trend
combined_df_ordered <- union(combined_df_beds, combined_df_mud) %>% arrange(n)


#GENERATE LOG

df_for_log <- combined_df_ordered %>% mutate(facies_top = cumsum(thck)) %>% mutate(facies_base = lag(facies_top)) %>%
  mutate(n = seq.int(1:nrow(combined_df_ordered))) %>% mutate(facies_base = replace(facies_base, n == 1, 0)) %>% 
  mutate(phi = case_when(code == 'M' ~ 1, TRUE ~ 2)) %>%
  mutate(color = case_when(code == 'M' ~ 'grey', TRUE ~ 'yellow'))

title <- paste(selected_sys_type, selected_element, selected_climate, sep = ' ')
subtitle <- paste('seed =', seed, ' N/G = ', modelled_NG, ng_calc)

ggplot(df_for_log) + geom_rect(xmin = 0, color = 'black', aes(xmax = phi, ymin = facies_base, ymax=facies_top, fill = code))+
  scale_fill_brewer(palette="Dark2")+
  scale_x_continuous(limits = c(0,2), breaks = c(1,2))+
  labs(title = title,
       subtitle = subtitle)+
  theme_classic()

#assign thickness

# bed_thickness_climate_sandy_summary_green_terminal <- bed_thickness_climate_sandy_summary %>% filter(element_general_type == 'terminal deposit') %>% 
#   filter(climate == 'greenhouse') 
# 
# 
# bed_thickness_list = list()
# 
# for (code_input in bed_list) {
#   thickness <- bed_thickness_climate_sandy_summary_green_terminal %>% filter(code == code_input) %>% pull(mean_bed_thck)
#   
#   bed_thickness_list[[length(bed_thickness_list)+1]] = thickness
#   
# }
# bed_thickness_list
# 
# combined_list <- list(code = bed_list, thickness = bed_thickness_list)
# combined_list
# combined_df <- data.frame(code = unlist(bed_list), thck = unlist(bed_thickness_list))
# 
# rnorm(1, 5, 2)


#second option

# bed_data_df <- data.frame(code = unlist(bed_list))
# bed_thickness_list = list()
# for (i in 1:nrow(bed_data_df)) {
#   random_number <- rlnorm(1,-1.579,1.33)
#   bed_thickness_list[[length(bed_thickness_list)+1]] = random_number
# }
# bed_data_df$thickness <- unlist(bed_thickness_list)
# 
# 
# hist(bed_data_df$thickness)
# fitteddist <- fitdistr(bed_data_df$thickness, 'lognormal')
# mean(bed_data_df$thickness)
# desiredmean <- 0.5
# bed_data_df <- bed_data_df %>% mutate(thickness2 = thickness-(mean(bed_data_df$thickness)-desiredmean))
# fitdistr(bed_data_df$thickness2, 'lognormal')
# mean(bed_data_df$thickness2)
# 
# 
# rn <- rlnorm(10000, -1.579,1.33)
# hist(rn)
# mean(rn)
# bed_data_df$thickness1 <- rn