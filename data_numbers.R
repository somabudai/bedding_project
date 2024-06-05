library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(RMariaDB)
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(ggpubr)



con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "",
  password = "",
  host = "",
  port = ,
  dbname = ""
)

age_global_events <- read.csv('climate_spreadsheet.csv')
systems <- dbGetQuery(con,"SELECT d04_system.system_ID, d04_system.name ,d04_system.latitude, d04_system.longitude, ifnull(d04_system.age_ID_to,d04_system.age_ID_from) AS sys_age_to,
d04_system.palaeo_lat_range, d04_system.palaeo_latitude, d04_system.palaeo_lat_hemisphere
FROM d04_system")

systems <- left_join(systems, age_global_events, by = "sys_age_to")

systems_with_beds <- beds_table_sandgs_lam_features %>% select(system_ID) %>% distinct(system_ID)
systems_with_beds <- as.list(systems_with_beds$system_ID)
systems_bed_number <- beds_table_sandgs_lam_features %>% group_by(system_ID, sys_name) %>% summarise(bed_n=n()) %>% select(system_ID, bed_n)
systems_facies_number <- facies_data %>% group_by(system_ID, sys_name) %>% summarise(facies_n=n()) %>% select(system_ID, facies_n)
systems_data <- systems %>% filter(system_ID %in% systems_with_beds) %>% left_join(sys_gs_category, by = 'system_ID') %>% 
  left_join(systems_bed_number, by = 'system_ID') %>% left_join(systems_facies_number, by = 'system_ID') %>% 
  mutate(period = case_when(period == '-' ~ 'Precambrian', TRUE ~ period))

european_systems <- systems_data %>% filter(between(longitude, -15,30) & between(latitude, 35,60)) %>% pull(system_ID) %>% as.list()

systems_data_noeurope <- systems_data %>% filter(!system_ID %in% european_systems)


### MAP -----------------------
#WORLD MAP FOR PPT
map_of_systems_ppt <- ggplot() + geom_sf(data = world, color = 'grey80', fill = 'grey80', lwd=0.01)+
  geom_sf(data = coastline, color = 'black', lwd=0.115)+
  coord_sf(expand = FALSE)+
  geom_point(data=systems_data, aes(x=longitude, y=latitude, shape = sys_gs_category, fill=sys_gs_category), color = 'black',  size = 3, stroke = 0.3)+
  scale_fill_manual(values = c('gravelly-sand system' = 'darkorange',
                               'sandy system' = 'gold1'
  ))+
  scale_shape_manual(values = c('gravelly-sand system' = 22,
                                'sandy system' = 21
  ))+
  theme_classic()+
  theme(
    legend.background = element_rect(fill = alpha("lightblue",0)),
    legend.text = element_text(face = 'bold', size = 8),
    legend.title = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_blank(),
    panel.background = element_rect(fill = "lightblue"),
    panel.ontop = FALSE,
    axis.text.x = element_text(color = "black", size = 9),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9),
    legend.position= c(0.15,0.35))

map_of_systems_ppt
ggsave('plots/map_of_systems_ppt.pdf', width = 175, height = 86, units = 'mm', device = 'pdf')
ggsave('plots/map_of_systems_ppt.jpeg', width = 175, height = 86, units = 'mm', device = 'jpeg')




#WORLD MAP

world <- ne_countries(scale = "medium", returnclass = "sf")
coastline <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

# ggplot(systems_data) + geom_point(aes(x=longitude, y=latitude), fill = 'blue',  size = 3.5)

map_of_systems <- ggplot() + geom_sf(data = world, color = 'grey80', fill = 'grey80', lwd=0.01)+
  geom_sf(data = coastline, color = 'black', lwd=0.115)+
  coord_sf(expand = FALSE)+
  geom_point(data=systems_data, aes(x=longitude, y=latitude, shape = sys_gs_category, fill=sys_gs_category), color = 'black',  size = 2, stroke = 0.2)+
  scale_fill_manual(values = c('gravelly-sand system' = 'darkorange',
                               'sandy system' = 'gold1'
                               ))+
  scale_shape_manual(values = c('gravelly-sand system' = 22,
                                'sandy system' = 21
  ))+
  geom_text_repel(data=systems_data_noeurope, aes(x=longitude, y=latitude, label = system_ID), size = 6/.pt,
                   nudge_x = .15,
                   nudge_y = .2,
                   max.overlaps = Inf, min.segment.length = unit(0, 'lines'),
                   segment.color = 'black',
                   lwd = 0.23)+
  geom_rect(data=systems_data_noeurope, aes(xmin = -15,xmax = 30,ymin = 35, ymax = 60), color = 'black', fill = NA, lwd = 0.23)+
  theme_classic()+
  theme(
        legend.background = element_rect(fill = alpha("lightblue",0)),
        legend.text = element_text(face = 'bold', size = 8),
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = "lightblue"),
        panel.ontop = FALSE,
        axis.text.x = element_text(color = "black", size = 9),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))
        
#legend.position= c(0.15,0.35), add for singular plot
map_of_systems
ggsave('plots/map_of_systems.pdf', width = 126, height = 62, units = 'mm', device = 'pdf')
ggsave('plots/map_of_systems.jpeg', width = 126, height = 62, units = 'mm', device = 'jpeg')


map_europe <- ggplot() + geom_sf(data = world, color = 'grey30', fill = 'grey80', lwd=0.01)+
  geom_sf(data = coastline, color = 'black', lwd=0.115)+
  coord_sf(expand = FALSE, xlim = c(-15, 30), ylim = c(35,60))+
  geom_point(data=systems_data, aes(x=longitude, y=latitude, shape = sys_gs_category, fill=sys_gs_category), color = 'black',  size = 2, stroke = 0.2)+
  scale_fill_manual(values = c('gravelly-sand system' = 'darkorange',
                               'sandy system' = 'gold1'
  ))+
  scale_shape_manual(values = c('gravelly-sand system' = 22,
                                'sandy system' = 21
  ))+
  geom_label_repel(data=systems_data, aes(x=longitude, y=latitude, label = system_ID), size = 6/.pt)+
  theme_classic()+
  theme(legend.position= 'none',
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = "lightblue"),
        panel.ontop = FALSE,
        axis.text.x = element_text(color = "black", size = 9),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))
map_europe
ggsave('plots/map_of_europe.pdf', width = 77, height = 62, units = 'mm', device = 'pdf')
ggsave('plots/map_of_europe.jpeg', width = 77, height = 62, units = 'mm', device = 'jpeg')

combined_map <- ggarrange(map_of_systems, map_europe,
  widths = c(1,0.6),
  labels = c('A','B'),
  font.label = (size = 10),
  common.legend = TRUE, legend = 'bottom')
combined_map
ggsave('plots/combined_map.pdf', width = 190, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/combined_map.jpeg', width = 190, height = 70, units = 'mm', device = 'jpeg')



### CLIMATE MAP -----------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
coastline <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

# ggplot(systems_data) + geom_point(aes(x=longitude, y=latitude), fill = 'blue',  size = 3.5)

map_of_systems_climate <- ggplot() + geom_sf(data = world, color = 'grey80', fill = 'grey80', lwd=0.01)+
  geom_sf(data = coastline, color = 'black', lwd=0.115)+
  coord_sf(expand = FALSE)+
  geom_point(data=systems_data, aes(x=longitude, y=latitude, shape = sys_gs_category, fill=climate), color = 'black',  size = 2, stroke = 0.2)+
  scale_fill_manual(values = c('greenhouse' = '#509977',
                               'icehouse' = '#59A2CE',
                               'uncertain' = 'grey80'
  ))+
  scale_shape_manual(values = c('gravelly-sand system' = 22,
                                'sandy system' = 21
  ))+
  geom_rect(data=systems_data_noeurope, aes(xmin = -15,xmax = 30,ymin = 35, ymax = 60), color = 'black', fill = NA, lwd = 0.23)+
  theme_classic()+
  theme(
    legend.background = element_rect(fill = alpha("lightblue",0)),
    legend.text = element_text(face = 'bold', size = 8),
    legend.title = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_blank(),
    panel.background = element_rect(fill = "lightblue"),
    panel.ontop = FALSE,
    axis.text.x = element_text(color = "black", size = 9),
    axis.line = element_line(colour = 'black', size = 0.24),
    axis.ticks = element_line(colour = 'black', size = 0.24),
    axis.text.y = element_text(color = "black", size = 9))

#legend.position= c(0.15,0.35), add for singular plot
map_of_systems_climate
ggsave('plots/map_of_systems_climate.pdf', width = 126, height = 62, units = 'mm', device = 'pdf')
ggsave('plots/map_of_systems_climate.jpeg', width = 126, height = 62, units = 'mm', device = 'jpeg')


map_europe_climate <- ggplot() + geom_sf(data = world, color = 'grey30', fill = 'grey80', lwd=0.01)+
  geom_sf(data = coastline, color = 'black', lwd=0.115)+
  coord_sf(expand = FALSE, xlim = c(-15, 30), ylim = c(35,60))+
  geom_point(data=systems_data, aes(x=longitude, y=latitude, shape = sys_gs_category, fill=climate), color = 'black',  size = 2, stroke = 0.2)+
  scale_fill_manual(values = c('greenhouse' = '#509977',
                               'icehouse' = '#59A2CE',
                               'uncertain' = 'grey80'
  ))+
  scale_shape_manual(values = c('gravelly-sand system' = 22,
                                'sandy system' = 21
  ))+
  theme_classic()+
  theme(legend.position= 'none',
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = "lightblue"),
        panel.ontop = FALSE,
        axis.text.x = element_text(color = "black", size = 9),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))
map_europe_climate
ggsave('plots/map_of_europe_climate.pdf', width = 77, height = 62, units = 'mm', device = 'pdf')
ggsave('plots/map_of_europe_climate.jpeg', width = 77, height = 62, units = 'mm', device = 'jpeg')

combined_map_climate <- ggarrange(map_of_systems_climate, map_europe_climate,
                          widths = c(1,0.6),
                          labels = c('A','B'),
                          font.label = (size = 10),
                          common.legend = TRUE, legend = 'bottom')
combined_map_climate
ggsave('plots/combined_map_climate.pdf', width = 190, height = 70, units = 'mm', device = 'pdf')
ggsave('plots/combined_map_climate.jpeg', width = 190, height = 70, units = 'mm', device = 'jpeg')

### AGE DATA -----------------------
#DATA BY AGE

period_order <- c('Precambrian', 'Cambrian', 'Ordovician', 'Silurian', 'Devonian', 'Carboniferous', 'Permian', 'Triassic', 'Jurassic',
                  'Cretaceous', 'Paleogene', 'Neogene', 'Quaternary')
period_color_order <- c('#F74370','#7FA056','#009270', '#B3E1B6', '#CB8C37', '#67A599', '#F04028', '#812B92', '#34B2C9',
                        '#7FC64E', '#FD9A52', '#FFE619', '#F9F97F')
data_by_period <- systems_data %>% select(period, bed_n, facies_n, period_color) %>% group_by(period, period_color) %>% summarise(sum_bed_n = sum(bed_n),
                                                                                                   sum_facies_n = sum(facies_n)) %>%
  arrange(factor(data_by_period$period, levels = period_order))

data_by_period$period <- factor(data_by_period$period, levels = period_order)
data_by_period$period_color <- factor(data_by_period$period_color, levels = period_color_order)

facies_data_max <- max(data_by_period$sum_facies_n)
facies_data_sum <- sum(data_by_period$sum_facies_n)
bed_data_max <- max(data_by_period$sum_bed_n)
bed_data_sum <- sum(data_by_period$sum_bed_n)

  #FACIES DATA
facies_number_period_plot <- ggplot(data_by_period, aes(x= period, y=sum_facies_n, fill=period_color))+
  geom_bar(position = 'stack', stat ='identity', colour = 'black' ,lwd=0.24)+
  scale_fill_manual(values = period_color_order, drop = FALSE)+
  scale_x_discrete(drop = FALSE, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,facies_data_max+800))+
  geom_text(aes(label = sum_facies_n), hjust = -0.1, size = 7/.pt)+
  labs(y= 'Number of facies')+
  annotate('text', x = 'Precambrian', y = 6500, label = paste('total facies data = ',facies_data_sum,sep=''), fontface = 'italic', size = 7/.pt)+
  theme_classic()+
  theme(legend.position= 'none',
        axis.text.x = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))+
  coord_flip()
facies_number_period_plot
ggsave('plots/facies_number_period_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/facies_number_period_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')
  #BED DATA
bed_number_period_plot <-  ggplot(data_by_period, aes(x= period, y=sum_bed_n, fill=period_color))+
  geom_bar(position = 'stack', stat ='identity', colour = 'black' ,lwd=0.24)+
  scale_fill_manual(values = period_color_order, drop = FALSE)+
  scale_x_discrete(drop = FALSE, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,bed_data_max+300), breaks = seq(0, bed_data_max+300, by = 500))+
  geom_text(aes(label = sum_bed_n), hjust = -0.1, size = 7/.pt)+
  labs(y= 'Number of beds')+
  annotate('text', x = 'Precambrian', y = 2200, label = paste('total bed data = ',bed_data_sum,sep=''), fontface = 'italic', size = 7/.pt)+
  theme_classic()+
  theme(legend.position= 'none',
        axis.text.x = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))+
  coord_flip()
bed_number_period_plot
ggsave('plots/bed_number_period_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/bed_number_period_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')


data_numbers_combined <- ggarrange(facies_number_period_plot, bed_number_period_plot,
                          labels = c('A','B'),
                          font.label = (size = 10))
data_numbers_combined
ggsave('plots/data_numbers_combined.pdf', width = 190, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/data_numbers_combined.jpeg', width = 190, height = 100, units = 'mm', device = 'jpeg')

# facies_bed_number_period_plot <- ggplot(data_by_period)+
#   geom_bar(aes(x= factor(period, levels = period_order), y=sum_facies_n), position = 'stack', stat ='identity', colour = 'black', fill = 'green', lwd=0.23)+
#   geom_bar(aes(x= factor(period, levels = period_order), y=sum_bed_n), position = 'stack', stat ='identity', colour = 'black', fill = 'orange', lwd=0.23)+
#   scale_x_discrete(drop = FALSE, expand = c(0,0))+
#   scale_y_continuous(expand = c(0,0))+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.3),axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.line = element_line(colour = 'black', size = 0.23),
#         axis.ticks = element_line(colour = 'black', size = 0.23))
# facies_bed_number_period_plot

### CLIMATE DATA -----------------------

climate_period_order <- c('Cryogenian', 'Ediacaran-Ordovician','Late Ordovician - Early Silurian', 'Devonian', 'Late Paleozoic', 'Mesozoic-Cenozoic', 'Late Cenozoic')

# missing_climate_period <- list(climate_phases = 'Late Ordovician - Early Silurian', climate = 'icehouse', sum_bed_n = 0, sum_facies_n = 0)
# rbind(missing_climate_period) %>%

data_by_climate_period <- systems_data %>% select(climate_phases, climate, bed_n, facies_n) %>% group_by(climate_phases, climate) %>% summarise(sum_bed_n = sum(bed_n),
                                                                                                                                  sum_facies_n = sum(facies_n)) %>%
arrange(factor(climate_phases, levels = climate_period_order))

data_by_climate_period$climate_phases <- factor(data_by_climate_period$climate_phases, levels = climate_period_order)

facies_data_max_climate <- max(data_by_climate_period$sum_facies_n)
facies_data_sum_climate <- sum(data_by_climate_period$sum_facies_n)
bed_data_max_climate <- max(data_by_climate_period$sum_bed_n)
bed_data_sum_climate <- sum(data_by_climate_period$sum_bed_n)

#FACIES DATA
facies_number_climate_period_plot <- ggplot(data_by_climate_period, aes(x= climate_phases, y=sum_facies_n, fill=climate))+
  geom_bar(position = 'stack', stat ='identity', colour = 'black' ,lwd=0.24)+
  scale_fill_manual(values = c('greenhouse' = '#509977',
                               'icehouse' = '#59A2CE',
                               'uncertain' = 'grey80'
  ))+
  scale_x_discrete(drop = FALSE, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,facies_data_max_climate+2000))+
  geom_text(aes(label = sum_facies_n), hjust = -0.1, size = 7/.pt)+
  labs(y= 'Number of facies')+
  annotate('text', x = 'Cryogenian', y = 10000, label = paste('total facies data = ',facies_data_sum_climate,sep=''), fontface = 'italic', size = 7/.pt)+
  theme_classic()+
  theme(legend.position= 'none',
        axis.text.x = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))+
  coord_flip()
facies_number_climate_period_plot
ggsave('plots/facies_number_climate_period_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/facies_number_climate_period_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')

#BED DATA
bed_number_climate_period_plot <- ggplot(data_by_climate_period, aes(x= climate_phases, y=sum_bed_n, fill=climate))+
  geom_bar(position = 'stack', stat ='identity', colour = 'black' ,lwd=0.24)+
  scale_fill_manual(values = c('greenhouse' = '#509977',
                               'icehouse' = '#59A2CE',
                               'uncertain' = 'grey80'
  ))+
  scale_x_discrete(drop = FALSE, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,bed_data_max_climate+2000))+
  geom_text(aes(label = sum_bed_n), hjust = -0.1, size = 7/.pt)+
  labs(y= 'Number of beds')+
  annotate('text', x = 'Cryogenian', y = 5000, label = paste('total bed data = ',bed_data_sum_climate,sep=''), fontface = 'italic', size = 7/.pt)+
  theme_classic()+
  theme(legend.position= 'none',
        axis.text.x = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = 'black', size = 0.24),
        axis.ticks = element_line(colour = 'black', size = 0.24),
        axis.text.y = element_text(color = "black", size = 9))+
  coord_flip()
bed_number_climate_period_plot
ggsave('plots/bed_number_climate_period_plot.pdf', width = 90, height = 100, units = 'mm', device = 'pdf')
ggsave('plots/bed_number_climate_period_plot.jpeg', width = 90, height = 100, units = 'mm', device = 'jpeg')
