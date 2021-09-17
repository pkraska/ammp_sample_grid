ammp_sample_grid <-
  function(# geospatial file for site delineation. ESRI shapefile (.shp), ESRI
    # geodatabase (.gdb), or Keyhole markup (.kml) preferred.
    file,
    # Provide a site name for folder creation and map labeling, if there
    # is a formal identification for the site in a provincial registry,
    # use that instead of a colloquial name
    site_name,
    # province site is located in, only used to speed up geospatial
    # activities. e.g. 'N.B.' or 'N.S.'
    province_abbr,
    # CRS 32620 is UTM 20N, suitable for NB and most of NS, see NRCAN
    # website for more detail :
    # https://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/utm-grid-map-projections/utm-grid-universal-transverse-mercator-projection/9779
    # Use ESPG codes for other projections based on where site is located
    # (i.e. UTM21/22N for N.L. and UTM8/9/10N for BC depending on where
    # the site is)
    crs_grid = 32620,
    # choose whether or not to download coastline file from Stats Canada,
    # choose FALSE if you have already downloaded and have the shapefile
    # saves as `coastlines` in your R environment)
    dl_coastline = TRUE,
    # output extension (haven't tried extensively, should work with 'shp',
    # 'kml')
    output_type = "shp") {
    library(tidyverse)
    library(sf)
    
    # Check if data/output and data/input folders are created, and create them if necessary
    if (!dir.exists(paste0("data/output/", site_name, "/"))) {
      message(
        paste0(
          "Creating directory for script out puts at in 'data/output/",
          site_name,
          "/'"
        )
      )
      dir.create(paste0("data/output/", site_name, "/"), recursive = TRUE)
    }
    
    if (!dir.exists("data/input/")) {
      message("Creating directory for script out puts at in 'data/input/")
      
      dir.create("data/input/", recursive = TRUE)
    }
    
    if (dl_coastline == TRUE) {
      temp <- tempfile()
      download.file(
        "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip",
        temp
      )
      coastlines <- st_read(unzip(temp, exdir = "data/input")[3])
    } else {
      message("Using user provided coastline shapefile found in the data/coastline folder.")
      coastlines <-
        st_read(list.files("data/input/coastline/", full.names = TRUE)[grep(pattern = ".shp",
                                                                            x = list.files("data/input/coastline/"))])
    }
    
    province <- coastlines %>%
      filter(PREABBR == province_abbr) %>%
      st_transform(crs = crs_grid)
    
    lease_boundary <- st_read(file) %>%
      st_transform(crs = crs_grid) %>%
      st_zm()
    
    lease_200m <- st_buffer(lease_boundary, dist = 200) %>%
      st_difference(province) %>%
      st_difference(lease_boundary) %>%
      mutate(distance = "200m") %>%
      select(distance)
    
    lease_500m <- st_buffer(lease_boundary, dist = 500) %>%
      st_difference(province) %>%
      st_difference(lease_boundary) %>%
      st_difference(lease_200m) %>%
      mutate(distance = "500m") %>%
      select(distance)
    
    lease_1000m <- st_buffer(lease_boundary, dist = 1000) %>%
      st_difference(province) %>%
      st_difference(lease_boundary) %>%
      st_difference(lease_200m) %>%
      st_difference(lease_500m) %>%
      mutate(distance = "1000m") %>%
      select(distance)
    
    lease_1500m <- st_buffer(lease_boundary, dist = 1500) %>%
      st_difference(province) %>%
      st_difference(lease_boundary) %>%
      st_difference(lease_200m) %>%
      st_difference(lease_500m) %>%
      st_difference(lease_1000m) %>%
      mutate(distance = "1500m") %>%
      select(distance)
    
    lease_limits <- st_bbox(lease_1500m)
    AOI <- province %>%
      st_crop(lease_limits)
    
    lease_buffers <- ggplot(data = AOI) +
      geom_sf(col = 'light grey') +
      geom_sf(data = lease_200m, fill = NA) +
      geom_sf(data = lease_500m, fill = NA) +
      geom_sf(data = lease_1000m, fill = NA) +
      geom_sf(data = lease_1500m, fill = NA)
    
    # These next `while` functions are necessary as the `st_sample` function
    # calculates the regular sample grid based on the bounding box of the
    # polygon, which does not always return the exact proper number of sample
    # points due to the odd shape of the polygon. It will simply keep running
    # the sample grid generation until it hase the proper number of sample
    # points in the polygon. Other options are random points, hexagonal, and
    # triangular.
    
    set.seed(0)
    
    x <- 0
    while (x != 10L) {
      lease_200m_sample <- lease_200m %>%
        st_sample(10, type = 'regular') %>%
        st_as_sf() %>%
        mutate(
          distance = "200m",
          X = st_coordinates(st_transform(., 'WGS84'))[, 1],
          Y = st_coordinates(st_transform(., 'WGS84'))[, 2]
        )
      x <- length(lease_200m_sample$x)
    }
    
    x <- 0
    while (x != 14L) {
      lease_500m_sample <- lease_500m %>%
        st_sample(14, type = 'regular') %>%
        st_as_sf() %>%
        mutate(
          distance = "500m",
          X = st_coordinates(st_transform(., 'WGS84'))[, 1],
          Y = st_coordinates(st_transform(., 'WGS84'))[, 2]
        )
      x <- length(lease_500m_sample$x)
    }
    
    x <- 0
    while (x != 20L) {
      lease_1000m_sample <- lease_1000m %>%
        st_sample(20, type = 'regular') %>%
        st_as_sf() %>%
        mutate(
          distance = "1000m",
          X = st_coordinates(st_transform(., 'WGS84'))[, 1],
          Y = st_coordinates(st_transform(., 'WGS84'))[, 2]
        )
      x <- length(lease_1000m_sample$x)
    }
    
    x <- 0
    while (x != 7L) {
      lease_1500m_sample <- lease_1500m %>%
        st_sample(7, type = 'regular') %>%
        st_as_sf() %>%
        mutate(
          distance = "1500m",
          X = st_coordinates(st_transform(., 'WGS84'))[, 1],
          Y = st_coordinates(st_transform(., 'WGS84'))[, 2]
        )
      x <- length(lease_1500m_sample$x)
    }
    
    ammp_sample_sites <- lease_buffers +
      geom_sf(data = lease_200m_sample, col = 'red') +
      geom_sf(data = lease_500m_sample, col = 'blue') +
      geom_sf(data = lease_1000m_sample, col = 'green') +
      geom_sf(data = lease_1500m_sample, col = 'orange') +
      geom_sf(data = lease_boundary, col = 'black', fill = 'red') +
      coord_sf(expand = FALSE)
    # print the sample site plan in R
    print(ammp_sample_sites)
    
    sample_sites <-
      bind_rows(lease_200m_sample,
                lease_500m_sample,
                lease_1000m_sample,
                lease_1500m_sample)
    
    # save the sample sites in "data/output/[site name]/[site
    # name]_sample_sites.shp
    
    if (!dir.exists(paste0("data/output/", site_name, "/"))) {
      message(
        paste0(
          "Creating directory for script out puts at in 'data/output/",
          site_name,
          "/'"
        )
      )
      dir.create(paste0("data/output/", site_name, "/"), recursive = TRUE)
    }
    
    st_write(
      sample_sites,
      paste0(
        "data/output/",
        site_name,
        "/",
        site_name,
        "_sample_sites.",
        output_type
      ),
      delete_layer = TRUE
    )
    
    st_write(
      sample_sites,
      paste0(
        "data/output/",
        site_name,
        "/",
        site_name,
        "_sample_sites.csv"
      ),
      layer_options = "GEOMETRY=AS_XY",
      append = FALSE
    )
    
    st_write(
      lease_200m,
      paste0(
        "data/output/",
        site_name,
        "/",
        site_name,
        "_200m_buffer.",
        output_type
      ),
      delete_layer = TRUE
    )
    st_write(
      lease_500m,
      paste0(
        "data/output/",
        site_name,
        "/",
        site_name,
        "_500m_buffer.",
        output_type
      ),
      delete_layer = TRUE
    )
    st_write(
      lease_1000m,
      paste0(
        "data/output/",
        site_name,
        "/",
        site_name,
        "_1000m_buffer.",
        output_type
      ),
      delete_layer = TRUE
    )
    st_write(
      lease_1500m,
      paste0(
        "data/output/",
        site_name,
        "/",
        site_name,
        "_1500m_buffer.",
        output_type
      ),
      delete_layer = TRUE
    )
  }
