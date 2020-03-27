# Note: There are no congruent tracts that had no urbanicity information (e.g. acs_within_fcc10 == 1 & urbanicity == NA).
# Add conditional urbanicity indicators for plots
data_int <- data_int %>% mutate(urban_fcc200 = case_when(acs_within_fcc200 == 0 ~ NA_character_,
                                                         acs_within_fcc200 == 1 & urbanicity == "Rural" ~ "Rural", 
                                                         acs_within_fcc200 == 1 & urbanicity == "Small town" ~ "Small town", 
                                                         acs_within_fcc200 == 1 & urbanicity == "Micropolitan" ~ "Micropolitan", 
                                                         acs_within_fcc200 == 1 & urbanicity == "Metropolitan" ~ "Metropolitan"),
                                urban_fcc10 = case_when(acs_within_fcc10 == 0 ~ NA_character_,
                                                        acs_within_fcc10 == 1 & urbanicity == "Rural" ~ "Rural", 
                                                        acs_within_fcc10 == 1 & urbanicity == "Small town" ~ "Small town", 
                                                        acs_within_fcc10 == 1 & urbanicity == "Micropolitan" ~ "Micropolitan", 
                                                        acs_within_fcc10 == 1 & urbanicity == "Metropolitan" ~ "Metropolitan"),
                                urban_any = case_when(acs_within_fcc == 0 ~ NA_character_,
                                                      acs_within_fcc == 1 & urbanicity == "Rural" ~ "Rural", 
                                                      acs_within_fcc == 1 & urbanicity == "Small town" ~ "Small town", 
                                                      acs_within_fcc == 1 & urbanicity == "Micropolitan" ~ "Micropolitan", 
                                                      acs_within_fcc == 1 & urbanicity == "Metropolitan" ~ "Metropolitan"))
data_int$urban_fcc200 <- factor(data_int$urban_fcc200, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))
data_int$urban_fcc10 <- factor(data_int$urban_fcc10, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))
data_int$urban_any <- factor(data_int$urban_any, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))

# write_rds(data_int, "./rivanna_data/working/data_int.Rds")


#
# Select data -------------------------------------------------------------------------------------
#

int_contig <- data_int %>% filter(STATEFP != "02" & STATEFP != "15" & STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78")
int_alaska <- data_int %>% filter(STATEFP == "02")
int_hawaii <- data_int %>% filter(STATEFP == "15")


#
# Plot: ANY OVERLAP [min10, max200] -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot() +
  geom_sf(data = int_contig, aes(fill = urban_any), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "Tract-Level ACS and FCC Broadband Subscription Estimate\nInterval Congruence [min10mbps, max200kbps]", 
       subtitle = "Tracts with incongruent estimate ranges shown in grey (NA).",
       caption = "Note: FCC = Federal Communications Commission, December 2016. ACS = American Community Survey, 2014-18.\nAlaska and Hawaii not to scale.") +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot() +
  geom_sf(data = int_hawaii, aes(fill = urban_any), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot() +
  geom_sf(data = int_alaska, aes(fill = urban_any), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot all
plot_min10max200 <- plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)

ggsave("plot_min10max200.png", plot = plot_min10max200, device = "png", path = "./docs",
       scale = 2, width = 120, height = 80, units = "mm", dpi = 300)


#
# Plot: WITHIN FCC 200 -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot() +
  geom_sf(data = int_contig, aes(fill = urban_fcc200), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC 200kbps Broadband Subscription Estimate\nCongruence by Tract", 
       subtitle = "Tracts with ACS estimates incongruent with the FCC 200kbps range shown in grey (NA).",
       caption = "Note: FCC = Federal Communications Commission, December 2016. ACS = American Community Survey, 2014-18.\nAlaska and Hawaii not to scale.") +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot() +
  geom_sf(data = int_hawaii, aes(fill = urban_fcc200), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot() +
  geom_sf(data = int_alaska, aes(fill = urban_fcc200), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot all
plot_200kbps <- plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)

ggsave("plot_200kbps.png", plot = plot_200kbps, device = "png", path = "./docs",
       scale = 2, width = 120, height = 80, units = "mm", dpi = 300)


#
# Plot: ACS WITHIN FCC 10 -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot() +
  geom_sf(data = int_contig, aes(fill = urban_fcc10), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC 10mbps Broadband Subscription Estimate\nCongruence by Tract", 
       subtitle = "Tracts with ACS estimates incongruent with the FCC 10mbps range shown in grey (NA).",
       caption = "Note: FCC = Federal Communications Commission, December 2016. ACS = American Community Survey, 2014-18.\nAlaska and Hawaii not to scale.") +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot() +
  geom_sf(data = int_hawaii, aes(fill = urban_fcc10), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot() +
  geom_sf(data = int_alaska, aes(fill = urban_fcc10), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot all
plot_10mbps <- plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)

ggsave("plot_10mbps.png", plot = plot_10mbps, device = "png", path = "./docs",
       scale = 2, width = 120, height = 80, units = "mm", dpi = 300)


#
# Check urbanicity ------------------------------------------------------------------
#

# Urbanicity not coded for 84 tracts, but congruent tracts all have urbanicity information.

# Urbanicity of min10 - max200 congruence
table(data_int$acs_within_fcc)
table(data_int$acs_within_fcc, data_int$urbanicity, useNA = "always") # congruent tracts all have urbanicity information.
round(prop.table(table(data_int$acs_within_fcc, data_int$urbanicity)), 4)

round(prop.table(table(data_int$urbanicity, data_int$acs_within_fcc), margin = 2), 4)
round(prop.table(table(data_int$urbanicity, data_int$acs_within_fcc), margin = 1), 4)

# States with the highest proportion of congruent tracts
statesmax <- data_int %>% mutate(acs_within_fcc = as.numeric(acs_within_fcc),
                                 acs_within_fcc = acs_within_fcc - 1) %>%
  group_by(State) %>% 
  transmute(NAME.y = NAME.y,
            acs_within_fcc = acs_within_fcc,
            tractnumber = n(),
            tractcong = sum(acs_within_fcc),
            tractcongprop = tractcong/tractnumber) %>%
  st_set_geometry(NULL)
statesmax <- statesmax %>% select(State, tractcongprop) %>% 
  unique() %>%
  arrange(desc(tractcongprop))
head(statesmax, 60)
tail(statesmax, 10)

# Counties with the highest proportion of congruent tracts
countiesmax <- data_int %>% mutate(acs_within_fcc = as.numeric(acs_within_fcc),
                                   acs_within_fcc = acs_within_fcc - 1) %>%
  group_by(County) %>% 
  transmute(State = State,
            acs_within_fcc = acs_within_fcc,
            tractnumber = n(),
            tractcong = sum(acs_within_fcc),
            tractcongprop = tractcong/tractnumber) %>%
  st_set_geometry(NULL)
countiesmax <- countiesmax %>% select(State, County, tractcongprop) %>% 
  unique()

countiesmax <- countiesmax %>% group_by(State) %>%
  mutate(countiesnumber = n()) %>%
  ungroup() %>%
  arrange(desc(tractcongprop))

ggplot(countiesmax, aes(x = tractcongprop)) +
  geom_histogram(bins = 15) +
  labs(title = "Histogram of proportion congruent tracts within counties", x = "Proportion congruent tracts", y = "Number of counties") +
  scale_y_continuous(breaks = seq(0, 550, 50))

# Which counties are made up 100% by tracts with congruent estimates? Which states are they in?
countiesmax_1 <- countiesmax %>% 
  filter(tractcongprop == 1) %>%
  group_by(State) %>%
  mutate(nperstate = n())  %>%
  ungroup() %>%
  arrange(desc(nperstate))

countiesmax_1_table <- countiesmax_1 %>% select(State, nperstate) %>% unique()

# Looks like some of these are counties that are also only 1 census tract.

# Check in original data. Texas, Concho County
test <- data_int %>% filter(State == "TX" & County == "Concho County")
test <- data_int %>% filter(str_detect(NAME.y, "Concho"))

test <- data_int %>% filter(str_detect(NAME.y, "Brewster"))


#
# Check width of ACS intervals ------------------------------------------------------------------
#

# Distribution of ACS-MOE
hist(data_int[data_int$urbanicity == "Rural", ]$bbandmin, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Small town", ]$bbandmin, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Micropolitan", ]$bbandmin, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Metropolitan", ]$bbandmin, freq = TRUE, breaks = 10)

# Distance btween ACS-MOE, ACS+MOE
data_int$bbanddist <- data_int$bbandmax - data_int$bbandmin

hist(data_int[data_int$urbanicity == "Rural", ]$bbanddist, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Small town", ]$bbanddist, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Micropolitan", ]$bbanddist, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Metropolitan", ]$bbanddist, freq = TRUE, breaks = 10)

summary(data_int[data_int$urbanicity == "Rural", ]$bbanddist)
summary(data_int[data_int$urbanicity == "Small town", ]$bbanddist)
summary(data_int[data_int$urbanicity == "Micropolitan", ]$bbanddist)
summary(data_int[data_int$urbanicity == "Metropolitan", ]$bbanddist)


test <- data_int %>% select(NAME.y, bbandmin, bbandmax, bbanddist, urbanicity, connmin, connmax, acs_within_fcc)