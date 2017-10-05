library(readr)
library(dplyr)
library(stringr)
library(ggmap)

#' @title Load hurricane data from file
#' @description This function loads hurricane data from \code{filename}. 
#'     The file must exists and must be of type 'ebtrk_atlc_1988_2015.txt' or the function fails.
#' @param filename string. File from which to load hurricane data.
#' @return Data frame with data needed for elaboration
#' @examples 
#' \dontrun{
#' if(interactive()){
#'     myHurricaneData <- read_hurricane_data('ebtrk_atlc_1988_2015.txt')
#'  }
#' }
#' @seealso 
#'  \code{\link[readr]{read_fwf}}
#' @rdname read_hurricane_data
#' @export 
#' @importFrom readr read_fwf
read_hurricane_data <- function(filename) {
        ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                               4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
        ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                                 "hour", "year", "latitude", "longitude",
                                 "max_wind", "min_pressure", "rad_max_wind",
                                 "eye_diameter", "pressure_1", "pressure_2",
                                 paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                                 paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                                 paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                                 "storm_type", "distance_to_land", "final")

        myData <- readr::read_fwf(filename,
                               fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                               na = "-99")
        return(myData)

}

#' @title Tidy hurricane data in long form
#' @description This function tidies the data loaded with \code {read_hurricane_data()} function. 
#'     It convert the data to a “long” format, with separate rows for each of the three wind speeds 
#'     for wind radii (34 knots, 50 knots, and 64 knots)
#' @param rawHurricaneData data frame. Data in the form of output of \code{read_hurricane_data()} function.
#' @return data frame, in long form.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'    myTidyData <- tidy_hurricane_data(myHurricaneData) 
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate_}},\code{\link[dplyr]{select_}}

#'  \code{\link[stringr]{str_c}},\code{\link[stringr]{str_to_title}}

#'  \code{\link[tidyr]{gather}},\code{\link[tidyr]{spread}}
#' @rdname tidy_hurricane_data
#' @export 
#' @importFrom dplyr mutate_ select_
#' @importFrom stringr str_c str_to_title
#' @importFrom tidyr gather spread
tidy_hurricane_data <- function(rawHurricaneData){
        myData <- rawHurricaneData %>%
                dplyr::mutate_(storm_id = ~stringr::str_c(stringr::str_to_title(storm_name), year, sep = '-'),
                               date = ~stringr::str_c(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00'),
                               longitude = ~-longitude) %>%
                # Select only the relevant columns
                dplyr::select_(.dots = c('storm_id', 'date', 'latitude', 'longitude',
                                         'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                                         'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                                         'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')) %>%

                #There is a better way to do this part, this is the wide to long transfmration
                tidyr::gather(variable, value, -storm_id, -date,-latitude, -longitude, -storm_id, -date) %>%
                mutate_(wind_speed = ~str_extract(variable, "(34|50|64)"),
                        variable = ~str_extract(variable, "(ne|nw|se|sw)")) %>%
                tidyr::spread(variable, value)
        return(myData)
}

#' @title Create a new class for the new geom_hurricane  
#' @description This function create a new class of type "geom_hurricane_proto" that defines the new parameter wind_radii
#' @param required_aes character vector. Required aesthetic arguments for the geom_hurricane
#' @param default_aes list. Default values for aesthetic arguments
#' @param draw_key Each Geom has an associated function that draws the key when the geom needs to be displayed in a legend.
#' @param draw_group Definition of the geom
#' @return class 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'     geom_hurricane <- function(mapping = NULL, data = NULL,
#'                                stat = 'identity', position = 'identity',
#'                                na.rm = FALSE, show.legend = NA,
#'                                inherit.aes = TRUE, ...) { 
#'                              ggplot2::layer(geom = geom_hurricane_proto, mapping = mapping,
#'                                              data = data, stat = stat, position = position,
#'                                              show.legend = show.legend, inherit.aes = inherit.aes,
#'                                              params = list(na.rm = na.rm, ...))}
#'     }
#' }
#' @seealso 
#'  \code{\link[ggplot2]{layer}}
#' @rdname geom_hurricane
#' @importFrom ggplot2 ggproto
#' @importFrom base data.frame as.character
#' @importFrom dplyr bind_rows rename_ mutate_
#' @importFrom grid polygonGrob gpar 
#' @export 
geom_hurricane_proto <- ggplot2::ggproto(
        "geom_hurricane_proto", Geom,
         required_aes = c("x",
                          "y",
                          "r_ne", # radius Northeast
                          "r_se", #
                          "r_nw",
                          "r_sw" ),
         default_aes = aes(fill = 1,
                           colour = 1,
                           alpha = 0.8, # in assignment example there is some transparency
                           scale_radii = 1),
         draw_key = draw_key_polygon,
         draw_group = function(hkData, panel_scales, coord) {

                 ## Transform data
                 MILE2METERS = 1852 # constant for convertion factor Miles to Meters
                 coords <- coord$transform(hkData, panel_scales)
                 hkData <- hkData %>%
                         mutate_(r_ne = ~r_ne*MILE2METERS*scale_radii,
                                  r_se = ~r_se*MILE2METERS*scale_radii,
                                  r_sw = ~r_sw*MILE2METERS*scale_radii,
                                  r_nw = ~r_nw*MILE2METERS*scale_radii)


                 # Create points for each quandrant
                 for (i in 1:nrow(hkData)) {

                         # NorthEast Quandrant
                         quad_ne <- base::data.frame(colour = hkData[i,]$colour,
                                                   fill = hkData[i,]$fill,
                                                   geosphere::destPoint(p = c(hkData[i,]$x, hkData[i,]$y),
                                                                        b = 1:90,
                                                                        d = hkData[i,]$r_ne),
                                                   group = hkData[i,]$group,
                                                   PANEL = hkData[i,]$PANEL,
                                                   alpha = hkData[i,]$alpha
                         )

                         # SouthEast Quandrant
                         quad_se <- base::data.frame(colour = hkData[i,]$colour,
                                                   fill = hkData[i,]$fill,
                                                   geosphere::destPoint(p = c(hkData[i,]$x, hkData[i,]$y),
                                                                        b = 90:180,
                                                                        d = hkData[i,]$r_se),
                                                   group = hkData[i,]$group,
                                                   PANEL = hkData[i,]$PANEL,
                                                   alpha = hkData[i,]$alpha
                         )

                         # SouthWest Quandrant
                         quad_sw <- data.frame(colour = hkData[i,]$colour,
                                             fill = hkData[i,]$fill,
                                             geosphere::destPoint(p = c(hkData[i,]$x, hkData[i,]$y),
                                                                  b = 180:270,
                                                                  d = hkData[i,]$r_sw),
                                             group = hkData[i,]$group,
                                             PANEL = hkData[i,]$PANEL,
                                             alpha = hkData[i,]$alpha
                         )

                         # NorthWest Quandrant
                         quad_nw <- base::data.frame(colour = hkData[i,]$colour,
                                                   fill = hkData[i,]$fill,
                                                   geosphere::destPoint(p = c(hkData[i,]$x, hkData[i,]$y),
                                                                        b = 270:360,
                                                                        d = hkData[i,]$r_nw),
                                                   group = hkData[i,]$group,
                                                   PANEL = hkData[i,]$PANEL,
                                                   alpha = hkData[i,]$alpha
                         )

                         quad_points <- dplyr::bind_rows(list(quad_nw, 
                                                              quad_ne, 
                                                              quad_se, 
                                                              quad_sw))

                 }


                 quad_points <- quad_points %>% dplyr::rename_('x' = 'lon',
                                                               'y' = 'lat'
                 )

                 quad_points$colour <- base::as.character(quad_points$colour)
                 quad_points$fill <- base::as.character(quad_points$fill)
                 coords_data <- coord$transform(quad_points, panel_scales)

                 grid::polygonGrob(
                         x= coords_data$x,
                         y = coords_data$y,
                         gp = grid::gpar(col = coords_data$colour, 
                                         fill = coords_data$fill, 
                                         alpha = coords_data$alpha)
                 )

        }

)

#' @title Create \code{geom_hurricane} geometry for ggplot
#' @description This function create the new geometry \code{geom_hurricane} from ggproto class \{geom_hurricane_proto} 
#' @param mapping. Aesthetic mappings by aes or aes_. 
#'      If specified and inherit.aes = TRUE, it is combined with the default mapping of the plot. 
#'      Default: NULL
#' @param data data frame. It is the data to be plotted. If NULL uses the data frame og ggplot, Default: NULL
#' @param stat string. Statistical transformation to use on the data for this layer. Default: 'identity'
#' @param position string. Position adjustment on data for this layer. Default: 'identity'
#' @param na.rm boolean. Remove NA from data if TRUE. Default: FALSE
#' @param show.legend boolean. Show legend on plot.
#' @param inherit.aes If TRUE combines aesthetics, if FALSE overrides aesthetics. Default: TRUE
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[ggplot2]{layer}}
#' @rdname geom_hurricane
#' @export 
#' @importFrom ggplot2 layer
geom_hurricane <- function(mapping = NULL, data = NULL,
                           stat = 'identity', position = 'identity',
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
        ggplot2::layer(
                geom = geom_hurricane_proto, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)

        )
}

#' @title Filter hurricane data
#' @description This function filters hurricane data by date \code{observation}
#' @param hurricaneTidyData data frame. Data frame in long format.
#' @param hurricane string. Filter hurricane data by hurricane-year id
#' @param observation date. Filter hurricane data by date time
#' @return data frame. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'    myDataFiltered <- filter_hurricane_data(tidyHurricaneData, 
#'          hurricane = 'Ike-2008', 
#'          observation = '2008-09-13 06:00:00')
#'  }
#' }
#' @rdname filter_hurricane_data
#' @export 
filter_hurricane_data <- function(hurricaneTidyData, hurricane, observation) {

        myData <- filter_(hurricaneTidyData,
                          ~storm_id == hurricane & date == observation)
        return(myData)

}


#' @title Dataset of storm observation 
#' @description This dataset contains hurricane data already filtered by hurricane and date time 
#'     with separate rows for each of the three wind speeds for wind radii (34 knots, 50 knots, and 64 knots).
#' @format A data frame with 3 rows and 9 variables:
#' \describe{
#'   \item{\code{storm_id}}{character id of hurricane in form hurricane-year}
#'   \item{\code{date}}{character date time of observation}
#'   \item{\code{latitude}}{double latitude}
#'   \item{\code{longitude}}{double longitude}
#'   \item{\code{wind_speed}}{character wind speed at the wind radius}
#'   \item{\code{ne}}{integer wind speed in the NorthEast Quadrant}
#'   \item{\code{nw}}{integer wind speed in the NorthWest Quadrant}
#'   \item{\code{se}}{integer wind speed in the SouthEast Quadrant}
#'   \item{\code{sw}}{integer wind speed in the SouthWest Quadrant} 
#'}
#' @export 
"storm_observation"
storm_observation <- read_hurricane_data('ebtrk_atlc_1988_2015.txt') %>%
        tidy_hurricane_data %>%
        filter_hurricane_data(hurricane = 'Ike-2008', observation = '2008-09-13 06:00:00')


map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")

map_radius_1 <- base_map +
        geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude,
                                           r_ne = ne, r_se = se,
                                           r_nw = nw, r_sw = sw,
                                           fill = wind_speed,
                                           color = wind_speed)) +
        scale_color_manual(name = "Wind speed (kts)",
                           values = c("red", "orange", "yellow")) +
        scale_fill_manual(name = "Wind speed (kts)",
                          values = c("red", "orange", "yellow")) +
        ggtitle("Plot with scale_radii = 1")

map_radius_05 <- base_map +
        geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude,
                                                     r_ne = ne, r_se = se,
                                                     r_nw = nw, r_sw = sw,
                                                     fill = wind_speed,
                                                     scale_radii = 0.5, 
                                                     color = wind_speed)) +
        scale_color_manual(name = "Wind speed (kts)",
                           values = c("red", "orange", "yellow")) +
        scale_fill_manual(name = "Wind speed (kts)",
                          values = c("red", "orange", "yellow")) +
        ggtitle("Plot with scale_radii = 0.5")

gridExtra::grid.arrange(map_radius_1, map_radius_05, ncol = 2)
