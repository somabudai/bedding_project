# Log generator
Soma Budai
2024-07-24

## **Loading packages**

``` r
library('MASS')
library(ggplot2)
library(dplyr)
library(RColorBrewer)
```

## **Defining attributes**

``` r
#ATTRIBUTES
selected_sys_type <- c('sandy system')
selected_element <- c('terminal deposit')
selected_climate <- c('greenhouse')
try_to_force_NG <- TRUE
force_NG_margin <- 0.05
stochastic_bed_frequency <- TRUE

#THICKNESS ATTRIBUTES
selected_sand_thickness <- 15
selected_sand_thickness_buffer <- 0.5

average_bed_thickness <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type == selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>% 
  summarise(mean_bed_thck = mean(bed_thickness)) %>% pull(mean_bed_thck) %>% as.numeric()
estimated_bed_number <-  round(selected_sand_thickness/average_bed_thickness, 0)
estimated_bed_number
selected_bed_number <- estimated_bed_number #Change this for specific bed number
selected_bed_number

#SEED
seed <- 1
```

## Estimating the proportion of bed types

The first part of the code estimates the number of each bed type out of
the desired bed numbers (represented by `selected_bed_number`).

``` r
#BED NUMBERS

#calculate the frequency of each bed type given the input parameters
selected_bed_frequency <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type == selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category == selected_sys_type) %>%
  group_by(code) %>% summarise(n = n()) %>% ungroup() %>%
  mutate(sum_n = sum(n)) %>% mutate(percent = n/sum_n*100)

if (nrow(selected_bed_frequency) == 0) {stop('Filtering the bed table gave no result!')}
```

This block filters the `beds_table_sandgs_lam_features` dataframe based
on the input variables, such as element type and climate. And then
calculates the percentage of each bed type in the selected data.

The code can estimate the bed frequency in a deterministic way and a
more stochastic way, this can be changed using the
`stochastic_bed_frequency` variable, that can be either set to `TRUE` or
`FALSE`.

### 1.a Estimating the proportion of bed types - deterministic

``` r
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
}
```

This method uses the `selected_bed_number` and bed frequencies
calculated from the dataset to calculate the instance of each bed type
for the modeled log. Bed types whose number do not reach 1 were filtered
out. This information can be found in the `bed_occurrence` dataframe.
Then, a list (`bed_list`) is being created using two for loops nested
within each other that contains each bed type as many types as their
calculated occurrence suggests.

Then the content of the list is appended to a dataframe: `combined_df`

The advantage of this method is that this way the calcualted log will
only contain the most common bed types. It disadvantage is that rare bed
type are never present and because of the method of calculated the
generated bed count can be lower than the `selected_bed_number.`

### 1.b Estimating the proportion of bed types - stochastic

``` r
else if (stochastic_bed_frequency == TRUE) {
#stochastic approach

bed_types <- selected_bed_frequency %>% pull(code) %>% as.list()
bed_type_prob <- selected_bed_frequency %>% pull(percent) %>% as.list()

set.seed(seed)
random_bed <- sample(bed_types, selected_bed_number , replace = TRUE, prob = bed_type_prob)
combined_df <- data.frame(code = unlist(random_bed)) %>% arrange(code)
bed_occurrence <- combined_df %>% group_by(code) %>% summarise(bed_type_n = n()) %>% ungroup()
present_bed_types <- combined_df %>% distinct(code) %>% pull(code) %>% as.list()
}
```

Opposed to the previous method, this one uses the probability of each
bed type, calculated from the filtered dataset, to generate a number of
random beds, specified by `selected_bed_number`. The advantage of this
method is that rare beds can be present even with a lower
`selected_bed_number`.

So far the dataframe (`combined_df`) that will be used to generate the
log only contains the beds that will be present, organised
alphabetically. Another dataframe called `bed_occurrence` contains each
bed type and their number of occurrence and a list called
`present_bed_types` were also generated and they will be later used to
assign thickness to each bed.

## Estimating bed thickness

``` r
selected_code_thickness <- beds_table_sandgs_lam_features %>% 
  filter(element_general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  filter(sys_gs_category %in% selected_sys_type) %>%
  dplyr::select(code, bed_thickness)
if (nrow(selected_code_thickness) == 0) {stop('Filtering the bed table for bed thickness gave no result!')}

sum_sand_thickness = 0
seed_thck = seed
iteration_count_sand = 0
while (!between(sum_sand_thickness,selected_sand_thickness-selected_sand_thickness_buffer,selected_sand_thickness+selected_sand_thickness_buffer)) {
  
  iteration_count_sand <- iteration_count_sand+1
  if(iteration_count_sand == 100){stop('Desired sand thickness wasnt reached within 100 iterations, try changing margin value!')}
  
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
```

This section of the code first, using the input attributes filters the
`beds_table_sandgs_lam_features` dataframe containg information on beds
and their properties. This information is stored in the
`selected_code_thickness` dataframe.

Then, a while loop is run which assigns a thickness value to the
previously generated beds until their summarised thickness is within the
`selected_sand_thickness` +- `selected_sand_thickness_buffer` interval.

Within the while loop a for loop estimates the bed thickness
distribution of each bed type, present in the generated log using data
from `selected_code_thickness`. Using the attributes (`meanlog` and
`sdlog`) of the fitted lognormal distribution, a set of bed thickness
values are generated with the `rlnorm` function, where the sample number
is equal to the number of a specific bed type.

If a specific bed type is represented only once in the modeled log then
its thickness is assumed by the median value (`code_median_thickness`)
derived from the `selected_code_thickness` dataframe.

Estimated bed thickness values are appended to a list within the for
loop (thickness_list) and then appended to the `combined_df` dataframe,
which now contains the code and thickness of modeled beds still in an
alphabetical order.

## Estimating overlying facies type

``` r
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
```

The code filters the `bed_under_over` dataframe that contains
information on the underlying and overlying facies type for each stored
bed. Then it calculated the overlying facies probabilities for each bed
type.

As a next step using these probabilities the overlying facies type for
each modeled bed is determined using a for loop. Currently the code only
determines if a bed is overlain by mud or another bed, but in the future
transition probabilities between different bed types are planned to be
usable.

The determined transitions are appended to the `combined_df` dataframe.

## Order modeled beds based on their thickness

``` r

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
log_trend_seed <- log_trend_seed+1
set.seed(log_trend_seed)

random_ordered_combined_df <- combined_df[sample(1:nrow(combined_df)),] 

calculated_bed_thck_difference <- random_ordered_combined_df %>% mutate(bed_thck_difference = lead(thck)-thck) %>% filter(!is.na(bed_thck_difference))

calculated_log_trend_value <- sum(calculated_bed_thck_difference$bed_thck_difference)/sum(calculated_bed_thck_difference$thck)

print(calculated_log_trend_value)

}

combined_df <- random_ordered_combined_df
```

The `log_vertical_bed_thickness` dataframe contains data on vertical bed
thickness changes, calculated for each architectural element depicted on
individual sedimentary logs stored in the DMAKS database. The vertical
bed thickness trend is described by the `log_trend_value`. This value is
calculated by the summary of bed thickness differences between following
beds, divided by the summary of bed thickness values of each element
represented on individual logs. If this value is positive then the bed
thickness changes upwards if it is negative then the beds tend to thin
upward, this trend is recorded in the `log_trend` column.

First the `log_vertical_bed_thickness` dataframe is filtered based on
the selected filters. After this the probability for thickening or
thinning trend is calculated. Then based on these probabilities the
vertical trend of the modeled log is estimated
(`random_thickness_trend`). If the estimated trend is ‘thickening’ then
the `log_trend_value_min` is 0 while the `log_trend_value_max` equals to
the maximum value of the `log_trend_value` column of the filtered
database. If the modeled log is estimated to be ‘thinning’ upward than
the opposite.

After this a while loop keeps reorganizing the `combined_df` dataframe
and calculates the log_trend_value until it falls between the minimum
and maximum value measured on real examples and drawn from the filtered
`log_vertical_bed_thickness` dataframe.

## Estimate mud bed thickness

``` r
selected_NG_value <- element_ng_thck_element_type %>% 
  filter(sys_gs_category %in% selected_sys_type) %>%
  filter(general_type %in% selected_element) %>% 
  filter(climate %in% selected_climate) %>%
  summarise(mean_ng = mean(NG))%>% pull(mean_ng) %>% as.numeric()

if (is.na(selected_NG_value)) {stop('Filtering the net-to-gross table gave no result!')}

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
    
    if (selected_NG_value - force_NG_margin < 0 | selected_NG_value + force_NG_margin > 1) {stop('Desired NG value is impossible because it is below 0 or above 1. Change force_NG_margin value!')}
    
    while (!between(modelled_NG,selected_NG_value-force_NG_margin,selected_NG_value+force_NG_margin)) {
      
      iteration_count_mud <- iteration_count_sand+1
      if(iteration_count_mud == 100){stop('Desired NG value couldnt be reached! Try setting try_to_force_NG to FALSE or change margin value!')}
      
      
      seed_mud_thck <- seed_mud_thck+1
      set.seed(seed_mud_thck)
      mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
      sum_mud_thickness <- sum(unlist(mud_thickness_list))
      print(sum_mud_thickness)
      modelled_NG <- round(sum_sand_thickness/(sum_sand_thickness+sum_mud_thickness),2)
      ng_calc <- 'forced'
  }
} else if(try_to_force_NG == FALSE) {
  set.seed(seed)
  mud_thickness_list <- rlnorm(number_of_mud_beds,meanlog_mud,sdlog_mud)
  sum_mud_thickness <- sum(unlist(mud_thickness_list))
  modelled_NG <- round(sum_sand_thickness/(sum_sand_thickness+sum_mud_thickness),2)
  ng_calc <- 'not forced'}  
```

The code filters the `element_ng_thck_element_type` dataframe, based on
the selected input values and calculates the mean net-to-gross
(i.e. sand fraction) value for the selected element type
(`selected_NG_value`). From the sand summarized thickness of modeled
sand beds (`sum_sand_thickness`) and the average N/G value
(`selected_NG_value`) it calculates the mud thickness
(`estimated_mud_thickness`) required to reach the calculated N/G value.

The number of mud beds in the modeled log is derived from estimated
transitions located in the `trans` column of the `combined_df`
dataframe. Based on this number the `estimated_mud_thickness_mean` is
calculated.

In the next step the `thck_stacked_mud dataframe` (containing mud
thickness data from real geological analogues) is filtered based on the
selected input parameters. The mean of this mud thickness is stored in
the `selected_mud_thickness_mean` variable. A lognormal distirbution is
fitted to the mud thickness values and its parameters are stored in the
`meanlog_mud` and `sdlog_mud` variables.

At the top of the script the `try_to_force_NG` variable specifies if the
script should repeat mud thickness estimation until the sand fraction
measured on the modeled log has similar values (+- `force_NG_margin`)
than the `selected_NG_value`. If it is set to ‘FALSE’ then the script
assigns a thickness value to each mud interval in the modeled log. The
thickness values are assigned to the mud layers located in the
`combined_df` later.

In both cases the script calculates the sand fraction of the modeled log
(`modelled_NG`) from the `sum_sand_thickness` and `sum_mud_thickness`
values.

## Create lithological order

``` r
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
```

In the `combined_df` overlying mudstone layers are located in a separate
column (trans). To create a log the subsequent layers need to be located
within one column. This part of the code creates a new dataframe called
`combined_df_ordered` that suits this criteria and assigns the
previously estimated mud thickness values to the mud intervals.

## Create log

``` r
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
```

This section of the code creates a sedimentary log from the modeled bed
proportions, thickness and transitions.

  

## Generated log

![](log_generator_manual.markdown_strict_files/figure-markdown_strict/unnamed-chunk-2-1.png)
