# Requires the AMMP_Sample Generation to be run first to generate the points, as
# well as you to have the CEPA-DAS samples locations
library(tidyverse)
library(sf)

ammp_sites <- sample_sites

cepadas_sites <-
  read_csv("data/CEPA-DAS/CEPA-DAS 2017 - Sampling Locations for AMP planning .csv") %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 'WGS84') %>%
  st_transform(crs = 32620) %>%
  st_crop(lease_limits)

buffer_distance <- 10

ammp_site_buffer <- ammp_sites %>%
  st_buffer(buffer_distance) %>%
  st_as_sf()

sites_within_buffer <-
  st_join(ammp_site_buffer, cepadas_sites) %>%
  filter(!is.na(Site)) %>%
  mutate(buffer = buffer_distance)

ggplot(data = ammp_site_buffer) +
  geom_sf(fill = NA, col = 'blue') +
  geom_sf(data = AOI) +
  geom_sf(data = cepadas_sites, pch = 4) +
  geom_sf(data = lease_boundary) +
  geom_sf(data = lease_200m, fill = NA) +
  geom_sf(data = lease_500m, fill = NA) +
  geom_sf(data = lease_1000m, fill = NA) +
  geom_sf(data = lease_1500m, fill = NA) +
  geom_sf(data = sites_within_buffer, col = 'red', fill = NA) +
  labs(title = paste0(
    "CEPA-DAS samples within ",
    buffer_distance,
    "m of AMMP sample sites"
  )) +
  coord_sf(expand = FALSE)

buffer_distance <- 25

ammp_site_buffer <- ammp_sites %>%
  st_buffer(buffer_distance) %>%
  st_as_sf()

sites_within_buffer <-
  st_join(ammp_site_buffer, cepadas_sites) %>%
  filter(!is.na(Site)) %>%
  mutate(buffer = buffer_distance)

ggplot(data = ammp_site_buffer) +
  geom_sf(fill = NA, col = 'blue') +
  geom_sf(data = AOI) +
  geom_sf(data = cepadas_sites, pch = 4) +
  geom_sf(data = lease_boundary) +
  geom_sf(data = lease_200m, fill = NA) +
  geom_sf(data = lease_500m, fill = NA) +
  geom_sf(data = lease_1000m, fill = NA) +
  geom_sf(data = lease_1500m, fill = NA) +
  geom_sf(data = sites_within_buffer, col = 'red', fill = NA) +
  labs(title = paste0(
    "CEPA-DAS samples within ",
    buffer_distance,
    "m of AMMP sample sites"
  )) +
  coord_sf(expand = FALSE)

buffer_distance <- 50

ammp_site_buffer <- ammp_sites %>%
  st_buffer(buffer_distance) %>%
  st_as_sf()

sites_within_buffer <-
  st_join(ammp_site_buffer, cepadas_sites) %>%
  filter(!is.na(Site)) %>%
  mutate(buffer = buffer_distance)

ggplot(data = ammp_site_buffer) +
  geom_sf(fill = NA, col = 'blue') +
  geom_sf(data = AOI) +
  geom_sf(data = cepadas_sites, pch = 4) +
  geom_sf(data = lease_boundary) +
  geom_sf(data = lease_200m, fill = NA) +
  geom_sf(data = lease_500m, fill = NA) +
  geom_sf(data = lease_1000m, fill = NA) +
  geom_sf(data = lease_1500m, fill = NA) +
  geom_sf(data = sites_within_buffer, col = 'red', fill = NA) +
  labs(title = paste0(
    "CEPA-DAS samples within ",
    buffer_distance,
    "m of AMMP sample sites"
  )) +
  coord_sf(expand = FALSE)

buffer_distance <- 100

ammp_site_buffer <- ammp_sites %>%
  st_buffer(buffer_distance) %>%
  st_as_sf()

sites_within_buffer <-
  st_join(ammp_site_buffer, cepadas_sites) %>%
  filter(!is.na(Site)) %>%
  mutate(buffer = buffer_distance)

ggplot(data = ammp_site_buffer) +
  geom_sf(fill = NA, col = 'blue') +
  geom_sf(data = AOI) +
  geom_sf(data = cepadas_sites, pch = 4) +
  geom_sf(data = lease_boundary) +
  geom_sf(data = lease_200m, fill = NA) +
  geom_sf(data = lease_500m, fill = NA) +
  geom_sf(data = lease_1000m, fill = NA) +
  geom_sf(data = lease_1500m, fill = NA) +
  geom_sf(data = sites_within_buffer, col = 'red', fill = NA) +
  labs(title = paste0(
    "CEPA-DAS samples within ",
    buffer_distance,
    "m of AMMP sample sites"
  )) +
  coord_sf(expand = FALSE)