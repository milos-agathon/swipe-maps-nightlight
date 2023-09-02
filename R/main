#########################################
# Visualize night light comparison with R
# Milos Popovic 2023/08/30
#########################################
libs <- c(
    "tidyverse", "terra", "sf",
    "giscoR", "shiny"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(lapply(
    libs, library, character.only = T
))

# 1. GET COUNTRY MAP
#-------------------

country_sf <- giscoR::gisco_get_countries(
    country = "UA",
    resolution = "3"
)

# 2. GET DATA
#------------

urls <- c(
"https://eogdata.mines.edu/nighttime_light/annual/v22/2022/VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average_masked.dat.tif.gz",
"https://eogdata.mines.edu/nighttime_light/annual/v21/2021/VNL_v21_npp_2021_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz"
)

for(url in urls){
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

raster_files <- list.files(
    path = getwd(),
    pattern = "npp",
    full.names = T
)

# 3. LOAD DATA
#-------------

globe_lights <- lapply(
    paste0("/vsigzip/", raster_files),
    terra::rast
)

# 4. CROP DATA
#-------------

country_lights_list <- lapply(
    globe_lights,
    function(x){
        terra::crop(
            x,
            terra::vect(country_sf),
            snap = "in",
            mask = T
        )
    }
)

# 5. TRANSFORM PROJECTION
#------------------------
crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_lights_reproj <- lapply(
    country_lights_list,
    function(x){
        terra::project(
            x,
            y = crs_lambert
        )
    }
)

# 6. REMOVE 0S AND SUBZEROS
#--------------------------

country_lights_final <- lapply(
    country_lights_reproj,
    function(x){
        terra::ifel(
            x <= 0,
            NA,
            x
        )
    }
)

# 7. RASTER TO DATAFRAME
#-----------------------

country_lights_df <- lapply(
    country_lights_final,
    function(x){
        as.data.frame(
            x,
            xy = T,
            na.rm = T
        )
    }
)

str(country_lights_df)
col_names <- c("x", "y", "value")
country_lights_df <- lapply(
    country_lights_df,
    setNames,
    col_names
)

# 8. MAP
#-------

cols <- c("#1f4762", "#FFD966", "white")
pal <- colorRampPalette(
    cols, bias = 8
)(512)

years <- c(2021, 2022)
names(country_lights_df) <- years
str(country_lights_df)

map <- lapply(
    names(country_lights_df),
    function(df){
        ggplot(
            data = country_lights_df[[df]]
        ) +
        geom_sf(
            data = country_sf,
            fill = NA,
            color = cols[[1]],
            size = .05
        ) +
        geom_tile(
            aes(
                x = x,
                y = y,
                fill = value
            )
        ) +
        scale_fill_gradientn(
            name = "",
            colors = pal
        ) +
        coord_sf(
            crs = crs_lambert
        ) +
        theme_void() +
        theme(
            legend.position = "none",
            plot.title = element_text(
                size = 80,
                color = "white",
                hjust = .5,
                vjust = 0
            ),
            plot.margin = unit(
                c(
                    t = 0, r = 0,
                    l = 0, b = 0
                ), "lines"
            )
        ) +
        labs(title = df)
    }
)

for (i in 1:2){
    file_name = paste0(
        "ukraine_map_", i,
        ".png")
    png(
        file_name,
        width = 800,
        height = 800,
        units = "px",
        bg = "#182833"
    )

    print(map[[i]])
    dev.off() 
}

# 9. MOVE IMAGES TO SHINY FOLDER
#-------------------------------

.libPaths()

current_dir <- getwd()
shiny_dir <- paste0(.libPaths(), "/shiny/www")
images_list <- list.files(
    path = current_dir,
    pattern = "ukraine_map"
)

file.copy(
    from = file.path(
        current_dir,
        images_list
    ),
    to = shiny_dir,
    overwrite = T,
    recursive = F,
    copy.mode = T

)

# 10. SWIPE EFFECT
#----------------

# CSS
css <- HTML('div#comparison { 
  width: 80vw;
  height: 80vw;
  max-width: 800px;
  max-height: 800px;
  overflow: hidden; }
div#comparison figure { 
  background-image: url('ukraine_map_1.png'); 
  background-size: cover;
  position: relative;
  font-size: 0;
  width: 100%; 
  height: 100%;
  margin: 0; 
}
div#comparison figure > img { 
  position: relative;
  width: 100%;
}
div#comparison figure div { 
  background-image: url('ukraine_map_2.png');
  background-size: cover;
  position: absolute;
  width: 50%; 
  box-shadow: 0 5px 10px -2px rgba(0,0,0,0.3);
  overflow: hidden;
  bottom: 0;
  height: 100%;
}

input[type=range]{
  -webkit-appearance:none;
  -moz-appearance:none;
  position: relative;
  top: -2rem; left: -2%;
  background-color: rgba(255,255,255,0.1);
  width: 102%; 
}
input[type=range]:focus { 
  outline: none; 
}
input[type=range]:active { 
  outline: none;  
}

input[type=range]::-moz-range-track { 
  -moz-appearance:none;
    height:15px;
    width: 98%;
    background-color: rgba(255,255,255,0.1); 
    position: relative;
    outline: none;    
 }
input[type=range]::active { 
  border: none; 
  outline: none;
}
input[type=range]::-webkit-slider-thumb {
    -webkit-appearance:none;
    width: 20px; height: 15px;   
    background: #fff;
    border-radius: 0;
   }
input[type=range]::-moz-range-thumb {
  -moz-appearance: none;
  width: 20px;
  height: 15px;
  background: #fff;
  border-radius: 0;
     }   
input[type=range]:focus::-webkit-slider-thumb {
    background: rgba(255,255,255,0.5);
   }
input[type=range]:focus::-moz-range-thumb {
    background: rgba(255,255,255,0.5);
   }'
)

# JS
js <- HTML('
function moveDivisor() { 
	divisor.style.width = slider.value+"%";
}

$(document).on("shiny:connected", function(event){
var divisor = document.getElementById("divisor"),
slider = document.getElementById("slider");
});
'
)

# HTML
ui <- shiny::fluidPage(
    tags$head(tags$style(css)),
    tags$head(tags$script(js)),

    HTML(
        '<div id="comparison">
        <figure>
        <div id="divisor"></div>
    </figure>
    <input type="range" min="0" max="100" value="50" id="slider" oninput="moveDivisor()">
    </div>'
    )
)

server <- function(input, output, session){}

shiny::shinyApp(ui, server)
