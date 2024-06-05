library(RMariaDB)
library(ggplot2)
library(dplyr)
library(writexl)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "soma",
  password = "heR6w12o",
  host = "193.204.44.99",
  port = 3306,
  dbname = "dmaks_3"
)

age_global_events <- read.csv('climate_spreadsheet.csv')
#age id 136 was both but it was reclassified as greenhouse

#
#Systems
#

facies_data_db <- dbGetQuery(con, "SELECT d15_facies.facies_ID,d15_facies.1d_data_ID as log_ID, d15_facies.1d_data_order AS data_order, d15_facies.trans_from_below, d15_facies.trans_from_below_event_b,
d15_facies.facies_type, d15_facies.grain_size_sand, d15_facies.grain_size_gravel, d15_facies.grading, d15_facies.general_structure, d15_facies.lamination_type,
d15_facies.thickness,  d15_facies.thickness_type,
d13_element.element_ID, d13_element.original_interpretation AS element_interpret, 
d13_element.general_type AS element_general_type,
d10_1d_data.method,
d07_subset.subset_ID ,d07_subset.data_type, d07_subset.facies_suit_proportions_type,
d06_case_study.case_study_ID, d06_case_study.name AS cs_name, ifnull(d06_case_study.age_ID_from, d06_case_study.age_ID_to) AS cs_age_from, ifnull(d06_case_study.age_ID_to, d06_case_study.age_ID_from) AS cs_age_to,	
d04_system.system_ID ,d04_system.name AS sys_name, ifnull(d04_system.age_ID_from,d04_system.age_ID_to) AS sys_age_from, ifnull(d04_system.age_ID_to,d04_system.age_ID_from) AS sys_age_to, 
d04_system.palaeo_lat_range, d04_system.palaeo_latitude, d04_system.palaeo_lat_hemisphere
FROM d15_facies
JOIN d10_1d_data ON d15_facies.1d_data_ID = d10_1d_data.1d_data_ID
JOIN d13_element ON d13_element.element_ID = d15_facies.parent_element_ID
JOIN d07_subset ON d07_subset.subset_ID = d13_element.subset_ID
JOIN d06_case_study ON d06_case_study.case_study_ID = d07_subset.case_study_ID
JOIN d04_system ON d04_system.system_ID = d06_case_study.system_ID
WHERE d15_facies.thickness IS NOT NULL
ORDER BY d15_facies.1d_data_ID, d15_facies.1d_data_order ASC")
facies_data <- left_join(facies_data_db, age_global_events, by = "sys_age_to")

#
#add system grain size category
#

facies_data <- left_join(facies_data, sys_gs_category, by = 'system_ID')

#
# create beds
#

facies_data_bedboundary <- facies_data %>% filter(!is.na(trans_from_below_event_b)) %>% filter(facies_type %in% c('_G','_S','gS','sG','(g)S', 'mS', 'sM')) %>%
  filter(thickness_type %in% c('true (maximum)','true (not maximum)','apparent')) %>% filter(sys_name != 'Lower Atoka system')

facies_data_bednr <- facies_data_bedboundary  %>% mutate(new_bed = case_when(data_order != lag(data_order)+1 | trans_from_below_event_b == "Y" ~ 1, TRUE ~ 0))%>% 
  mutate(artificial_bed_id = cumsum(new_bed))
has_y <- facies_data_bednr %>% filter(trans_from_below_event_b == "Y") %>% group_by(artificial_bed_id)  %>% 
  count(trans_from_below_event_b) %>% select(artificial_bed_id,n)
beds_raw <- left_join(facies_data_bednr, has_y, by = "artificial_bed_id") %>% filter(!is.na(n))


# 
#Add bed thickness and data
#
beds_raw <- beds_raw %>% group_by(artificial_bed_id) %>% mutate(bed_thickness = sum(thickness)) %>% ungroup()
beds_raw <- beds_raw %>% group_by(artificial_bed_id) %>% mutate(start_facies = min(facies_ID)) %>% ungroup()
beds_raw <- beds_raw %>% group_by(artificial_bed_id) %>% mutate(end_facies = max(facies_ID)) %>% ungroup()



#
#Add gravel thickness
#
gravel_thickness <- beds_raw %>% filter(facies_type %in% c('sG','_G')) %>% group_by(artificial_bed_id) %>% mutate(gravel_thickness = sum(thickness)) %>% ungroup() %>%
  select(artificial_bed_id,gravel_thickness) %>% distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds_raw,gravel_thickness, by = 'artificial_bed_id') %>% 
  mutate(gravel_thickness = case_when(is.na(gravel_thickness)~ 0, TRUE ~ gravel_thickness)) %>%
  mutate(gravel_fraction = round((gravel_thickness/bed_thickness),2))

#
#Add gravelly sand thickness
#
gsand_thickness <- beds_raw %>% filter(facies_type %in% c('gS','(g)S')) %>% group_by(artificial_bed_id) %>% mutate(gsand_thickness = sum(thickness)) %>% ungroup() %>%
  select(artificial_bed_id,gsand_thickness) %>% distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,gsand_thickness, by = 'artificial_bed_id') %>% 
  mutate(gsand_thickness = case_when(is.na(gsand_thickness)~ 0, TRUE ~ gsand_thickness)) %>%
  mutate(gsand_fraction = round((gsand_thickness/bed_thickness),2))

#
#Add sand thickness
#
sand_thickness <- beds_raw %>% filter(facies_type %in% c('_S')) %>% group_by(artificial_bed_id) %>% mutate(sand_thickness = sum(thickness)) %>% ungroup() %>%
  select(artificial_bed_id,sand_thickness) %>% distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,sand_thickness, by = 'artificial_bed_id') %>% 
  mutate(sand_thickness = case_when(is.na(sand_thickness)~ 0, TRUE ~ sand_thickness)) %>%
  mutate(sand_fraction = round((sand_thickness/bed_thickness),2))

#
#Add heterolithic thickness
#
heterolithic_thickness <- beds_raw %>% filter(facies_type %in% c('mS', 'sM')) %>% group_by(artificial_bed_id) %>% mutate(heterolithic_thickness = sum(thickness)) %>% ungroup() %>%
  select(artificial_bed_id,heterolithic_thickness) %>% distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,heterolithic_thickness, by = 'artificial_bed_id') %>% 
  mutate(heterolithic_thickness = case_when(is.na(heterolithic_thickness)~ 0, TRUE ~ heterolithic_thickness)) %>%
  mutate(heterolithic_fraction = round((heterolithic_thickness/bed_thickness),2))

# #
# #Add laminated thickness
# #
# 
subset_no_lamination <- dbGetQuery(con, "SELECT d07_subset.subset_ID
FROM d15_facies
JOIN d13_element ON d13_element.element_ID = d15_facies.parent_element_ID
JOIN d07_subset ON d07_subset.subset_ID = d13_element.subset_ID
JOIN d06_case_study ON d06_case_study.case_study_ID = d07_subset.case_study_ID
JOIN d04_system ON d04_system.system_ID = d06_case_study.system_ID
WHERE d07_subset.facies_suit_proportions_type NOT LIKE '%general structures%'
GROUP BY d04_system.system_ID")

subset_no_lamination_list <- subset_no_lamination %>% pull(subset_ID) %>% as.list()

lamination_thickness <- beds_raw %>% filter(general_structure == 'laminated')%>% group_by(artificial_bed_id) %>% mutate(laminated_thickness = sum(thickness)) %>% ungroup() %>%
  select(artificial_bed_id,laminated_thickness) %>% distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,lamination_thickness, by = 'artificial_bed_id') %>% 
  mutate(laminated_thickness = case_when(is.na(laminated_thickness)~ 0, TRUE ~ laminated_thickness)) %>%
  mutate(laminated_fraction = round((laminated_thickness/bed_thickness),2)) %>% mutate(laminated_fraction = case_when( subset_ID %in% subset_no_lamination_list ~ 100, TRUE ~ laminated_fraction ))
#
#general coarsening (gravel to sand)
#
gs_vchanges <- beds_raw %>% mutate(simple_gs = case_when(facies_type %in% c('_G', 'sG')~ 'G', facies_type %in% c('(g)S','_S','gS') ~ 'S', facies_type %in% c('mS') ~ 'MS', facies_type %in% c('sM') ~ 'SM')) %>%
  mutate(gs_phi = case_when(simple_gs == 'G' ~ 3, simple_gs == 'S' ~ 2, simple_gs == 'MS' ~ 1, simple_gs == 'SM' ~ 0)) %>%
  mutate(v_diff = case_when(lead(artificial_bed_id) != artificial_bed_id & lag(artificial_bed_id) != artificial_bed_id ~ 100, lead(artificial_bed_id) != artificial_bed_id ~ 200, TRUE ~ lead(gs_phi)-gs_phi )) %>%
  filter(v_diff != 200)
gs_mon_vchanges <- gs_vchanges %>% group_by(artificial_bed_id) %>%
  mutate(v_min = min(v_diff)) %>%
  mutate(v_max = max(v_diff)) %>%
  ungroup() %>%
  mutate(gs_trend = case_when(v_max == 100 ~ "N", v_min == 0 & v_max == 0 ~ 'N' ,v_max <= 0 & v_min < 0 ~ 'F', v_min < 0 & v_max > 0 ~ 'B', v_min >= 0 & v_max > 0 ~ 'C'))%>%
  select(artificial_bed_id,gs_trend)%>%
  distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,gs_mon_vchanges, by='artificial_bed_id')

#
#sand coarsening
#
  #add values for sand grain sizes
sand_gs <- beds_raw %>% filter(!is.na(grain_size_sand)) %>% filter(!facies_type %in% c('sG','G'))
sand_gs_vchanges <- sand_gs %>% mutate(sand_phi = case_when(grain_size_sand == "very fine" ~ 1, grain_size_sand == "fine" ~ 2, grain_size_sand == "medium" ~ 3, grain_size_sand == "coarse" ~ 4, grain_size_sand == "very coarse" ~ 5 )) %>% 
  mutate(v_diff = case_when(lead(artificial_bed_id) != artificial_bed_id & lag(artificial_bed_id) != artificial_bed_id ~ 100, lead(artificial_bed_id) != artificial_bed_id ~ 200, TRUE ~ lead(sand_phi)-sand_phi )) %>%
  filter(v_diff != 200) 

  #filter purely gradational beds
#I filtered the beds based on: 1 they have only one sharp transition, 2: the rest of the transition is gradational, 3: if they only
#have one lamination type (it shouldnt be gradational between different lam types), 4: they have multiple sandgs (if they have one they are already in S-N-n)
    only_gradational_sand_beds <- beds_raw %>% filter(!is.na(grain_size_sand)) %>% 
      select(artificial_bed_id, trans_from_below, general_structure, lamination_type, grain_size_sand) %>%
      filter(!is.na(trans_from_below)) %>%
      group_by(artificial_bed_id) %>%
      mutate(distinct_lam_n = n_distinct(general_structure)) %>%
      mutate(distinct_sandgs = n_distinct(grain_size_sand)) %>%
      mutate(n_sharp = sum(trans_from_below == 'sharp' | trans_from_below == 'sharp (erosional)')) %>% 
      mutate(n_grad = sum(trans_from_below == 'gradational')) %>%
      mutate(n_trans = n_sharp+n_grad) %>%
      filter(n_sharp == 1 & n_grad != 0 & n_grad == n_trans-1 & distinct_lam_n == 1 & distinct_sandgs > 1) %>% #if facies and overlying on has different lam but same grain size with gradual trans this doesnt filter it out as a no gs trends
      distinct(artificial_bed_id, .keep_all = TRUE)
    only_gradational_sand_beds_list <- only_gradational_sand_beds %>% pull(artificial_bed_id) %>% as.list()
    
#look at the sand part of any bed, and checks is the sand bed has only gradational trans bed that have a gravel base which can have sharp, those are not included in the previous
    only_gradational_trans <- beds_raw %>% filter(!is.na(grain_size_sand)) %>% select(artificial_bed_id, trans_from_below, general_structure) %>%
      filter(!is.na(trans_from_below)) %>%
      group_by(artificial_bed_id) %>%
      mutate(distinct_lam_n = n_distinct(general_structure)) %>%
      ungroup() %>%
      filter(distinct_lam_n == 1) %>%
      group_by(artificial_bed_id, trans_from_below) %>%
      summarise(n=n()) %>%
      ungroup() %>%
      group_by(artificial_bed_id) %>%
      mutate(sum_trans = (sum(n))) %>% ungroup() %>%
      mutate(trans_percent = n/sum_trans) %>% filter(trans_from_below == 'gradational') %>% filter(trans_percent == 1)
    only_gradational_trans_list <- only_gradational_trans %>% pull(artificial_bed_id) %>% as.list()

    check_bed_trans <- beds_raw %>% filter(artificial_bed_id %in% only_gradational_trans_list)

  #monotonous changes
    
# has_sand_ng <- beds_raw %>% filter(!is.na(grain_size_sand)) %>% select(artificial_bed_id, grading) %>% 
#   filter(grading == 'normally graded') %>% distinct(artificial_bed_id) %>% pull(artificial_bed_id) %>% as.list()
# 
# sand_gs_mon_vchanges <- sand_gs_vchanges %>% group_by(artificial_bed_id) %>%
#   mutate(v_min = min(v_diff)) %>%
#   mutate(v_max = max(v_diff)) %>%
#   ungroup()%>%
#   mutate(sand_trend = case_when(artificial_bed_id %in% has_sand_ng ~ "f",v_max == 100 ~ "n", v_min == 0 & v_max == 0 ~ 'n' ,v_max <= 0 & v_min < 0 ~ 'f', v_min < 0 & v_max > 0 ~ 'b', v_min >= 0 & v_max > 0 ~ 'c')) %>%
#   select(artificial_bed_id,sand_trend)%>%
#   distinct(artificial_bed_id, .keep_all = TRUE)

sand_gs_mon_vchanges <- sand_gs_vchanges %>% group_by(artificial_bed_id) %>%
  mutate(v_min = min(v_diff)) %>%
  mutate(v_max = max(v_diff)) %>%
  ungroup()%>%
  mutate(sand_trend = case_when(artificial_bed_id %in% only_gradational_trans_list ~ "n", artificial_bed_id %in% only_gradational_sand_beds_list ~ "n" ,v_max == 100 ~ "n", v_min == 0 & v_max == 0 ~ 'n' ,v_max <= 0 & v_min < 0 ~ 'f', v_min < 0 & v_max > 0 ~ 'b', v_min >= 0 & v_max > 0 ~ 'c')) %>%
  select(artificial_bed_id,sand_trend)%>%
  distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,sand_gs_mon_vchanges, by='artificial_bed_id') %>%
  mutate(sand_trend = case_when(is.na(sand_trend) & sand_fraction > 0 ~ 'no sand gs', is.na(sand_trend) & gsand_fraction > 0 ~ 'no sand gs',  is.na(sand_trend) & heterolithic_fraction > 0 ~ 'no sand gs', is.na(sand_trend) & sand_fraction == 0 ~ 'ns',is.na(sand_trend) & gsand_fraction == 0 ~ 'ns' , TRUE ~ sand_trend))



check_sand_trends <- beds %>% select(artificial_bed_id, facies_type, grain_size_sand, sand_trend)
check_gs_trends <- beds %>% select(artificial_bed_id, facies_type, grain_size_sand, gs_trend)
gs_count <- beds %>% group_by(facies_type) %>% summarise(n = n())

#combined grain size trend

combined_gs_values <- read.csv('combined_gs.csv', header = TRUE)

combined_vchanges <- beds_raw %>% mutate(simple_gs = case_when(facies_type %in% c('_G', 'sG')~ 'G', facies_type %in% c('(g)S','_S','gS') ~ 'S', facies_type %in% c('mS') ~ 'MS', facies_type %in% c('sM') ~ 'SM')) %>%
  mutate(general_structure = case_when(is.na(general_structure) ~ 'nd', TRUE ~ general_structure )) %>% mutate(lamination_type = case_when(is.na(lamination_type) ~ 'nd', TRUE ~ lamination_type )) %>%
  mutate(combined_gs = case_when(is.na(grain_size_sand) ~ simple_gs, !is.na(grain_size_sand) ~ paste(simple_gs,grain_size_sand, sep = ''))) %>% 
  select(facies_ID,log_ID,data_order,artificial_bed_id,trans_from_below,trans_from_below_event_b, general_structure, lamination_type, combined_gs) %>% left_join(combined_gs_values, by = 'combined_gs') %>%
  mutate(v_diff = case_when(lead(artificial_bed_id) != artificial_bed_id & lag(artificial_bed_id) != artificial_bed_id ~ 100, lead(artificial_bed_id) != artificial_bed_id ~ 200, lead(trans_from_below == 'gradational') & (lead(general_structure) == general_structure | lead(lamination_type) == lamination_type)  ~ 0, TRUE ~ lead(gs_value)-gs_value )) %>%
  filter(v_diff != 200)

combined_mon_vchanges <- combined_vchanges %>% group_by(artificial_bed_id) %>%
  mutate(v_min = min(v_diff)) %>%
  mutate(v_max = max(v_diff)) %>%
  ungroup() %>%
  mutate(combined_trend = case_when(v_max == 100 ~ "N", v_min == 0 & v_max == 0 ~ 'N' ,v_max <= 0 & v_min < 0 ~ 'F', v_min < 0 & v_max > 0 ~ 'B', v_min >= 0 & v_max > 0 ~ 'C'))%>%
  select(artificial_bed_id,combined_trend)%>%
  distinct(artificial_bed_id, .keep_all = TRUE)
beds <- left_join(beds,combined_mon_vchanges, by='artificial_bed_id')


#
#
beds_heterolithic <- beds %>% select(facies_type, grain_size_sand, artificial_bed_id, trans_from_below_event_b, heterolithic_fraction, gs_trend, sand_trend) %>%
  filter(heterolithic_fraction > 0)  


#
#select distinct beds and specific columns
#
distinct_beds <- beds %>% distinct(artificial_bed_id, .keep_all = TRUE)

beds_table <- distinct_beds %>% select(artificial_bed_id, log_ID, element_ID, start_facies,bed_thickness ,gravel_fraction, gsand_fraction, sand_fraction, heterolithic_fraction, laminated_fraction, combined_trend, gs_trend, sand_trend)

#
#add gs category
#
beds_table <- beds_table %>% mutate(gs_category = case_when(heterolithic_fraction > 0 ~ 'SM' ,gravel_fraction == 1 ~ 'G', gravel_fraction == '0' ~ 'S', gravel_fraction < 1 & gravel_fraction >= 0.5 ~ 'sG', gravel_fraction < 0.5 & gravel_fraction > 0 ~ 'gS'))
gs_category_summ <- beds_table %>% group_by(gs_category) %>% summarise(n = n())

#
#add lamination category
#

beds_table <- beds_table %>% mutate(lam_category = case_when(laminated_fraction == 100 ~ 'nd' ,laminated_fraction == 1 ~ 'l', laminated_fraction == 0 ~ 'sl', TRUE ~ 'x',))
lam_category_sum <- beds_table %>% group_by(lam_category) %>% summarise(n = n())

#
#add codes
#
beds_table <- beds_table %>% mutate(gs_trend = case_when(is.na(gs_trend) ~ 'N', TRUE ~ gs_trend ))  %>%
  mutate(code = paste(gs_category,combined_trend,lam_category, sep = '-')) %>% 
  mutate(code2 = paste(combined_trend,lam_category, sep = '-')) %>%
  mutate(code_old = paste(gs_category,gs_trend,sand_trend,lam_category, sep = '-')) %>% 
  mutate(code2_old = paste(gs_trend,sand_trend,lam_category, sep = '-'))
bed_summary <- beds_table %>% group_by(code) %>% summarise(n = n())

#
#beds with sand gs FILTER THE ENTIRE SYSTEM OUT not only the beds it can lead to problems
#


  #subsets with no sand gs!!!!!!!!!!!
beds_table_sandgs <- beds_table %>% filter(sand_trend != "no sand gs")
bed_summary_sandgs <- beds_table_sandgs %>% group_by(code) %>% summarise(n = n())

 
sand_trend_summary <- beds_table %>% group_by(sand_trend) %>% summarise(n = n())
gs_trend_summary <- beds_table %>% filter(gravel_fraction < 1 & gravel_fraction > 0) %>% group_by(gs_trend) %>% summarise(n = n())

#
# beds with sand gs and lamination data
#

beds_table_sandgs_lam <- beds_table_sandgs %>% filter(lam_category != 'nd')

#
#bed characteristics
#
beds_features <- beds %>% select(artificial_bed_id,method,element_general_type,climate,palaeo_lat_range,palaeo_latitude,vegetation,age, period, sys_gs_category, combined_sand_fraction,sys_name, system_ID, cs_name, case_study_ID ) %>% distinct(artificial_bed_id, .keep_all = TRUE)
beds_summary_features <- beds_features %>% group_by(sys_name,element_general_type,climate) %>% summarise(n=n())


#
#individual beds with features
#
beds_table_features <- left_join(beds_table, beds_features, by='artificial_bed_id')
beds_table_sandgs_features <- left_join(beds_table_sandgs,beds_features, by = 'artificial_bed_id') 
beds_table_sandgs_lam_features <- left_join(beds_table_sandgs_lam,beds_features, by = 'artificial_bed_id') 


#
#individual beds with features and element dimension
#
element_data_db <- dbGetQuery(con,
'SELECT d13_element.element_ID, d13_element.general_type AS element_general_type, 
d13_element.`length` AS element_length, d13_element.length_type AS element_length_type, 
d13_element.width AS element_width, d13_element.width_type AS element_width_type,
d13_element.thickness AS element_thickness, d13_element.thickness_type AS element_thickness_type
FROM d13_element')


beds_table_sandgs_lam_features_elements <- left_join(beds_table_sandgs_lam_features,element_data_db, by = 'element_ID')

average_bed_number_element <- beds_table_sandgs_lam_features_elements %>% group_by(element_general_type.x, element_ID) %>% summarise(n=n())
