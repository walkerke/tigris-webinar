# CRAN packages
install.packages(c("tidyverse", "sf", "rgdal", "tmap"))

# GitHub packages
library(devtools)

install_github("walkerke/tigris")
install_github("hrecht/censusapi")
install_github("tidyverse/ggplot2")

# Load tigris

library(tigris)

# Slide 11
us <- states()
plot(us)

# Slide 13
ri <- counties("RI")
ri20 <- counties("RI", cb = TRUE, resolution = "20m")
plot(ri)
plot(ri20, border = "red", add = TRUE)

# Slide 14
fw_zips <- zctas(cb = TRUE, starts_with = "761")
plot(fw_zips)

# Slide 15
loving <- roads("TX", "Loving")
plot(loving)

# Slides 16-17
sts <- c("DC", "MD", "VA") 
combined <- rbind_tigris(
  lapply(sts, function(x) {
    tracts(x, cb = TRUE)
  })
)
plot(combined)

# Slides 18-19
df <- read.csv("http://personal.tcu.edu/kylewalker/data/txlege.csv",
               stringsAsFactors = FALSE)
districts <- state_legislative_districts("TX", house = "lower", 
                                         cb = TRUE)
txlege <- geo_join(districts, df, "NAME", "District")
txlege$color <- ifelse(txlege$Party == "R", "red", "blue")
plot(txlege, col = txlege$color)
legend("topright", legend = c("Republican", "Democrat"),
       fill = c("red", "blue"))

# Slides 20-22
# If you have a Census API key, assign yours to the `key` variable. 
# If not, get one at http://api.census.gov/data/key_signup.html

library(censusapi)
library(tidyverse)
library(tmap)

chi_counties <- c("Cook", "DeKalb", "DuPage", "Grundy", "Lake", 
                  "Kane", "Kendall", "McHenry", "Will County")

chi_tracts <- tracts(state = "IL", county = chi_counties, cb = TRUE)

# key <- "YOUR KEY GOES HERE"

data_from_api <- getCensus(name = "acs5", vintage = 2015, key = key, 
                           vars = "B25077_001E", 
                           region = "tract:*", regionin = "state:17")

values <- data_from_api %>%
  transmute(GEOID = paste0(state, county, tract), 
            value = B25077_001E)

chi_joined = geo_join(chi_tracts, values, by = "GEOID")

tm_shape(chi_joined, projection = 26916) +
  tm_fill("value", style = "quantile", n = 7, palette = "Greens", 
          title = "Median home values \nin the Chicago Area") + 
  tm_legend(bg.color = "white", bg.alpha = 0.6) + 
  tm_style_gray()

# Slide 23

ttm()

tm_shape(chi_joined, projection = 26916) +
  tm_fill("value", style = "quantile", n = 7, palette = "Greens", 
          title = "Median home values \nin the Chicago Area") 


# Slides 26-28

library(sf)

city_hall <- c(-87.631969, 41.883835) %>%
  st_point() %>%
  st_sfc(crs = 4269) %>%
  st_transform(26916)

options(tigris_class = "sf")
chi_tracts_sf <- tracts(state = "IL", county = chi_counties, 
                        cb = TRUE)
chi_tracts_sf %>%
  st_transform(26916) %>%
  left_join(values, by = "GEOID") %>%
  mutate(dist = as.numeric(
    st_distance(
      st_centroid(.), city_hall
    )
  )) %>%
  ggplot(aes(x = dist, y = value)) + 
  geom_smooth(span = 0.3, method = "loess")

# Slide 29

set.seed(1983)

# key <- "YOUR KEY GOES HERE"

data_from_api <- getCensus(name = "acs5", vintage = 2015, key = key, 
                         vars = c("B19001_002E", "B19001_003E", "B19001_004E", 
                                  "B19001_005E", "B19001_017E"), 
                         region = "tract:*", regionin = "state:11")


income_data = data_from_api %>%
  transmute(GEOID = paste0(state, county, tract), 
            below25 = as.integer((B19001_002E + B19001_003E + B19001_004E + B19001_005E) / 10), 
            above200 = as.integer(B19001_017E / 10))

dc <- tracts("DC", cb = TRUE) %>%
  left_join(income_data, by = "GEOID") %>%
  st_transform(26918)

below25 <- st_sample(dc, dc$below25) %>%
  st_sf() %>%
  setNames(., "geometry") %>%
  mutate(type = "Below $25k") 

above200 <- st_sample(dc, dc$above200) %>%
  st_sf() %>%
  setNames(., "geometry") %>%
  mutate(type = "Above $200k")

dots <- rbind(below25, above200)

dots %>%
  ggplot() + 
  geom_sf(aes(color = type, fill = type), shape = ".") + 
  scale_color_manual(values = c("#fff04f", "#60eaf2")) + 
  scale_fill_manual(values = c("#fff04f", "#60eaf2")) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "bottom", 
        legend.background = element_rect(fill = "black"), 
        legend.title = element_blank(), 
        text = element_text(color = "white"), 
        panel.grid.major = element_blank(), 
        plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"), 
        plot.caption = element_text(size = 8)) + 
  labs(title = "Income inequality in Washington, DC", 
       subtitle = "1 dot = approximately 10 households", 
       caption = "Data: 2011-2015 ACS via the tigris and censusapi R packages")
