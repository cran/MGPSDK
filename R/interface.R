#' Interface class for interacting with WMS, WFS, and WMTS Streaming classes.
#'
#' This class provides a unified interface to access the WMS, WFS, and WMTS Streaming classes.
#' @title Interface
#' @importFrom R6 R6Class
#' @import reticulate
#' @export Interface
Interface <- R6::R6Class(
  "Interface",
  public = list(
    #' @field mgp_sdk (Optional) An instance of the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    mgp_sdk = NULL,
    #' @field py_interface (Optional) An instance of the Interface class from the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    py_interface = NULL,
    #' @field env_name = (Optional) The name of the environment where the MGP_SDK Python library is installed. Default is "R-MGP-SDK".
    env_name = NULL,
    #' @description
    #' Initializes the `Interface` object. Sets up the environment for using the MGP_SDK Python library.
    #'
    #' @param mgp_sdk (Optional) An instance of the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    #' @param py_interface (Optional) An instance of the Interface class from the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    #' @param env_name (Optional) The name of the environment where the MGP_SDK Python library is installed. Default is "R-MGP-SDK".
    initialize = function(mgp_sdk=NULL, py_interface = NULL, env_name = "R-MGP-SDK") {
      temp_dir <- tempdir()

      if (private$check_virtualenv(env_name)) {
        #do nothing the venv is already set up
        library(reticulate)
        reticulate::use_virtualenv(file.path(temp_dir, ".virtualenvs", env_name))
        }
      else{
        private$initialize_python_env("MGP-SDK")
        library(reticulate)
        reticulate::use_virtualenv(file.path(temp_dir, ".virtualenvs", env_name))
      }
      self$mgp_sdk <- reticulate::import("MGP_SDK")
      self$py_interface <- self$mgp_sdk$interface$Interface()
      },

    #' @description Perform a search for features within the specified bounding box and/or with a specified filter.
    #'
    #' @param bbox A string indicating the bounding box of the area of interest (miny,minx,maxy,maxx).
    #' @param filter A string containing a CQL filter used to refine the data of the search. Default is NULL.
    #' @param shapefile A logical indicating whether to return a shapefile. Default is FALSE.
    #' @param csv A logical indicating whether to return a CSV file. Default is FALSE.
    #' @param ... Additional arguments to pass to the `search` method.
    #'
    #' @details The `search` function performs a search for features within the specified bounding box and/or with a specified filter.
    #'
    #' @return If `shapefile` is TRUE, the function returns a shapefile of all features and associated metadata.
    #' If `csv` is TRUE, the function returns a CSV file. If neither is specified, the function returns a list of features.
    #' @export
    streaming_search = function(bbox=NULL, filter=NULL, shapefile=FALSE, csv=FALSE, ...) {
      kwargs <- list(...)
      args <- c(list(bbox = bbox, filter = filter, shapefile = shapefile, csv = csv), kwargs)
      result <- do.call(self$py_interface$streaming$search, args)
      return(result)
    },

    #' @description Download an image from a WMS or WMTS service
    #'
    #' @description This function allows you to download an image from a Web Map Service (WMS) or a
    #' @description Web Map Tile Service (WMTS). You can specify the bounding box, image dimensions, image
    #' @description format, and other parameters to customize the downloaded image.
    #'
    #' @param bbox A vector of four numeric values specifying the bounding box of the image.
    #' @param srsname A string specifying the spatial reference system (SRS) of the bounding box. Default is "EPSG:4326".
    #' @param height The height of the image in pixels.
    #' @param width The width of the image in pixels.
    #' @param img_format A string specifying the image format. Must be one of "jpeg", "png", or "geotiff".
    #' @param identifier A string specifying the identifier of the image.
    #' @param gridoffsets A vector of two numeric values specifying the grid offsets of the image.
    #' @param zoom_level An integer specifying the zoom level of the WMTS image.
    #' @param download A logical value indicating whether to download the image (TRUE) or return the raw image data (FALSE).
    #' @param outputpath A string specifying the directory where the downloaded image should be saved.
    #' @param display A logical value indicating whether to display the downloaded image (TRUE) or not (FALSE).
    #' @param ... Additional parameters to be passed to the WMS or WMTS service.
    #'
    #' @return If `download` is TRUE, the function returns the filename of the downloaded image. If `download` is FALSE,
    #' the function returns the raw image data as a binary vector.
    #' @export
    streaming_download_image = function(bbox = NULL, srsname = "EPSG:4326", height = NULL, width = NULL, img_format = "jpeg", identifier = NULL,  zoom_level = NULL, download = TRUE, outputpath = NULL, display = FALSE) {
      args <- c(list(bbox = bbox, srsname = srsname, height = as.integer(height), width = as.integer(width), img_format = img_format, identifier = identifier, zoom_level = zoom_level, download = download, outputpath = outputpath, display = display))
      result <- do.call(self$py_interface$streaming$download_image, args)
      #result <- self$py_interface$streaming$download_image(bbox = bbox, srsname = srsname, height = as.integer(height), width = as.integer(width), img_format = img_format, identifier = identifier, zoom_level = zoom_level, download = download, outputpath = outputpath, display = display)
      return(result)
      },

    #'
    #' @description This function is a wrapper for a Python function that retrieves full resolution images.
    #' @description The function downloads an image with the specified feature ID and additional parameters.
    #'
    #' @param featureid A character string representing the unique ID of the feature for which the image is required.
    #' @param thread_number An integer indicating the number of threads to use for the download process. Default is 100.
    #' @param bbox A character string representing the bounding box coordinates in the format 'xmin, ymin, xmax, ymax'. If NULL, the bounding box will be determined based on the feature ID. Default is NULL.
    #' @param mosaic A logical value indicating whether to mosaic the images or not. If TRUE, images covering the defined area will be combined into a single image. Default is FALSE.
    #' @param srsname A character string representing the spatial reference system to be used for the image. Default is 'EPSG:4326'.
    #' @param outputdirectory A character string representing the directory where the image should be saved. If NULL, the image will be saved in the current working directory. Default is NULL.
    #' @param image_format A character string representing the format of the image file to be downloaded. Default is 'jpeg'.
    #' @param filename A character string representing the name of the file to be saved. Default is "Maxar_Download".
    #'
    #' @return The function returns the result of the Python function call. The nature of this result depends on the Python function implementation.
    #' @export

    streaming_get_full_res_image = function(featureid, thread_number = 100, bbox = NULL, mosaic = FALSE,
                                   srsname = 'EPSG:4326', outputdirectory=getwd(),image_format='jpeg', filename="Maxar_Download") {
      arguments <- c(list(featureid=featureid,thread_number=as.integer(thread_number),bbox=bbox,mosaic=mosaic,srsname=srsname,outputdirectory=outputdirectory,image_format=image_format,filename=filename))
      result <- do.call(self$py_interface$streaming$get_full_res_image,arguments)
      return(result)
    },

    #basemaps

    #'
    #' @description
        #' Function searchs using WFS
    #' @param bbox Type:str, Bounding box of the AOI. Comma delimited set of coordinates. (miny,minx,maxy,maxx)
    #' @param srsname Type:str, The desired projection. Defaults to EPSG:4326
    #' @param filter Type: str, CQL filter used to refine the data returned from the search.
    #' @param shapefile Type: bool, Optional Boolean of whether to return in shapefile format. Defaults to false
    #' @param csv Type: bool, Optional Boolean of whether to return in csv format. Defaults to false
    #' @param featureprofile Type: str, Optional. Represents the desired stacking profile. Defaults to account default.
    #' @param typename Type:str, Optional The typename of the desired feature type. Defaults to FinishedFeature.
    basemaps_search = function(bbox, srsname = "EPSG:4326", filter, shapefile=FALSE, csv= FALSE,seamlines=FALSE, ...){
      arguments <- c(list(bbox = bbox, srsname=srsname,filter = filter, shapefile = shapefile, csv = csv, seamlines=seamlines), list(...))
      results <- do.call(self$py_interface$basemap_service$search,arguments)
      return(results)
    },
    #'
    #' @description
        #' Function Downloads a seamline image using the WMS method
    #' @param bbox Type:str, Bounding box of the AOI. Comma delimited set of coordinates. (miny,minx,maxy,maxx)
    #' @param srsname Type:str, The desired projection. Defaults to EPSG:4326
    #' @param height Type:int, The vertical number of pixels to return. Defaults to 512
    #' @param width Type:int, The horizontal number of pixels to return. Defaults to 512
    #' @param img_format Type: str, The format of the response image either jpeg, png or geotiff
    #' @param zoom_level Type: int, The zoom level. Used for WMTS
    #' @param download Type: bool, User option to download file locally. Default True
    #' @param outputpath Type: str Output path must include output format. Downloaded path default is user home path.
    #' @returns The downloaded file path
    basemaps_download_image = function(bbox, srsname = "EPSG:4326", height=NULL, width=NULL, img_format = "jpeg", download = TRUE, seamlines = FALSE, outputpath){
      arguments <- c(list(bbox = bbox, srsname = srsname, height = as.integer(height), width = as.integer(width), img_format = img_format, seamlines = seamlines,download = download, outputpath=outputpath))
      results <- do.call(self$py_interface$basemap_service$download_image,arguments)
      return(results)
    },

    #' @description
        #' Function downloads all tiles within a bbox dependent on zoom level
    #' @param bbox Type:str, Bounding box of the AOI. Comma delimited set of coordinates. (miny,minx,maxy,maxx)
    #' @param zoom_level Type: int, The zoom level. Used for WMTS
    #' @param srsname Type:str, The desired projection. Defaults to EPSG:4326
    #' @param img_format Type: str, The format of the response image either jpeg, png or geotiff
    #' @param download Type: bool, User option to download file locally. Default True
    #' @param outputpath Type: str Output path must include output format. Downloaded path default is user home path.
    #' @returns Message displaying success and location of downloaded tiles
    basemaps_download_tiles = function(bbox, zoom_level, srsname = "EPSG:4326", img_format = 'jpeg',seamlines = FALSE, outputpath= NULL) {
      arguments <- c(list(bbox = bbox, zoom_level=as.integer(zoom_level),srsname=srsname,img_format=img_format,seamlines = seamlines, outputpath=file.path(outputpath,sprintf("MaxarTile.%s",img_format)),display=FALSE))
      results <- do.call(self$py_interface$basemap_service$download_tiles,arguments)
      return(results)
    },

    #Discovery

    #' @description
    #' Returns a list of STAC items
    #' @param collections (string) = Comma-separated list of collections to search in. Use str format not a Python list
    #' @param sub_catalog_id (string) = Name of the subCatalogId to search in
    #' @param sub_catalog_collection (string) = Used to denote collections inside of sub catalogs
    #' @param  bbox (string) = Bounding box in format "minx,miny,maxx,maxy" in WGS84 decimal degrees
    #' @param datetime (string) = Date range filter in ISO 8601 format "start-date/end-date" or exact datetime
    #' @param stac_id (string) = Comma-separated list of STAC item IDs to return. Use str format not a Python list
    #' @param intersects (string) = GeoJSON geometry to search by
    #' @param where (string) = SQL-style WHERE clause for filtering STAC items by properties
    #' @param orderby (string) = SQL-style ORDER BY clause. Only for id and datetime e.g. 'orderby=id'
    #' @param limit (int) = Maximum number of items to return
    discovery_stac_search = function(...){
      arguments <- c(list(...))
      results <- do.call(self$py_interface$discovery_service$stac_search,arguments)
      return(results)
    },

    #' @description
    #' Retrieve items for a given collectionId by audit fields
    #'
    #' @param collection_id (string) = Name of the collection to search e.g. wv01 Required
    #' @param audit_insert_date (string) = Date range filter in ISO 8601 format "start-date/end-date" or exact datetime
    #' @param audit_update_date (string) = Date range filter in ISO 8601 format "start-date/end-date" or exact datetime
    #' @param limit (int) = Maximum number of items to return
    discovery_search_by_audit_fields = function(collection_id, ...){
      arguments <- c(list(collection_id = collection_id),list(...))
      results <- do.call(self$py_interface$discovery_service$search_by_audit_fields,arguments)
      return(results)
    },
    #' @description
        #' Returns the root STAC Catalog or STAC Collection that is the entry point for users to browse
        #'
    discovery_get_root_catalog = function(...){
      arguments <- c(list(...))
      results <- do.call(self$py_interface$discovery_service$get_root_catalog,args = arguments)
      return(results)
    },
    #' @description
        #' Return a collection definition by collection ID
    #' @param collection_id (string) = Name of the collection to search e.g. wv01 Required
    discovery_get_collection_definition = function(collection_id){
      arguments <- c(list(collection_id = collection_id))
      results <- do.call(self$py_interface$discovery_service$get_collection_definition, arguments)
      return(results)
    },
    #' @description
        #'  Return definitions for all collections
    #' @param orderby (string) = SQL-style ORDER BY clause. Only for id and datetime e.g. 'orderby=id ASC' default 'datetime DESC, id ASC'
    #' @param limit (int) = Maximum number of items to return
    discovery_get_all_collections = function(...){
      arguments <- c(list(...))
      results <- do.call(self$py_interface$discovery_service$get_all_collections, arguments)
      return(results)
    },
    #' @description
        #'  View details about a specific STAC item
        #'  Dictionary of the desired item's information
    #' @param collection_id (string) = Name of the collection to search e.g. wv01
    #' @param item_id (string) = Identifier of the desired item
    discovery_get_stac_item = function(collection_id, item_id){
      arguments <- c(list(collection_id=collection_id,item_id=item_id))
      results <- do.call(self$py_interface$discovery_service$get_stac_item, arguments)
      return(results)
    },
    #' @description
        #' View the available Maxar Sub-Catalogs that can be navigated as a self-contained STAC catalog
        #'
    #' @param orderby (string) = SQL-style ORDER BY clause. Only for id and datetime e.g. 'orderby=id ASC' default'datetime DESC, id ASC'
    #' @param limit (int) = Maximum number of items to return
    discovery_get_top_level_sub_catalog = function(...){
      arguments <- c(list(...))
      results <- do.call(self$py_interface$discovery_service$get_top_level_sub_catalog, arguments)
      return(results)
    },
    #' @description
        #' View the definition of a Maxar Sub-Catalog
    #' @param sub_catalog_id (string) = Identifier of the sub catalog to view
    discovery_get_sub_catalog = function(sub_catalog_id){
      arguments <- c(list(sub_catalog_id = sub_catalog_id))
      results <- do.call(self$py_interface$discovery_service$get_sub_catalog, arguments)
      return(results)
    },
    #' @description
        #' View the definition of a collection that belongs to a Sub-Catalog
    #' @param sub_catalog_id (string) = Identifier of the sub catalog to view
    #' @param sub_catalog_collection_id (string) = Identifier of the sub catalog collection to view
    discovery_get_sub_catalog_collection_definition = function(sub_catalog_id,sub_catalog_collection_id){
      arguments <- c(list(sub_catalog_id = sub_catalog_id, sub_catalog_collection_id = sub_catalog_collection_id))
      results <- do.call(self$py_interface$discovery_service$get_sub_catalog_collection_definition, arguments)
      return(results)
    }
  ),
  private = list(
    initialize_python_env = function(package_name, env_name = "R-MGP-SDK") {
      # Get the current Python configuration
      base_python = private$get_system_python_path()
      # Create virtual environment if it does not exist
      if (!private$check_virtualenv(env_name)) {
        # create the virtual environment
        private$create_virtualenv(package_name = package_name, env_name = env_name)
      }
    },
    get_system_python_path = function() {
      # Attempt to find Python in common locations
      base_python_path <- NULL
      if (Sys.info()["sysname"] == "Windows") {
        base_python_path <- system("where python", intern = TRUE)[1]
      } else { # For Unix and MacOS
        base_python_path <- system("which python", intern = TRUE)
      }
      # If base_python_path is NULL or empty, Python was not found
      if (is.null(base_python_path) || base_python_path == "") {
        message("Python not found, installing...")
        reticulate::install_python(version = "3.9:latest", list = FALSE, force = FALSE)
        # Try finding Python again after installing Miniconda
        if (Sys.info()["sysname"] == "Windows") {
          base_python_path <- system("where python", intern = TRUE)[1]
        } else { # For Unix and MacOS
          base_python_path <- system("which python", intern = TRUE)
        }
        # If Python is still not found, stop the function
        if (is.null(base_python_path) || base_python_path == "") {
          stop("Python installation failed.")
        }
      }
      return(base_python_path)
    },
    check_virtualenv = function(env_name) {
      env_path <- file.path(tempdir(), ".virtualenvs", env_name)
      env_exists <- dir.exists(env_path)
      if (env_exists == TRUE){
        if (Sys.info()['sysname'] == "Windows") {
          pip_interpreter <- file.path(env_path, "Scripts", "pip.exe")
        } else {
          pip_interpreter <- file.path(env_path, "bin", "pip")
        }
        system(paste(pip_interpreter, "install","pip","--upgrade","--quiet", "--no-cache-dir"))
        system(paste(pip_interpreter, "install","mgp-sdk","--upgrade","--quiet", "--no-cache-dir"))
      }
      return(dir.exists(env_path))
    },
    create_virtualenv = function(package_name, env_name = "R-MGP-SDK") {
      # Define the base Python interpreter.
      base_python <- private$get_system_python_path()
      # Define the path to the new virtual environment
      venv_path <- file.path(tempdir(), ".virtualenvs", env_name)
      # Command to create the virtual environment
      create_venv_cmd <- paste(paste0("\"", base_python, "\""), "-m venv", venv_path)
      system(create_venv_cmd)
      if (Sys.info()['sysname'] == "Windows") {
        pip_upgrade_cmd <- paste(file.path(tempdir(), ".virtualenvs", env_name, "Scripts", "activate"), "&&", "python -m pip install pip --upgrade --quiet", "--no-cache-dir")
        pip_install_cmd <- paste(file.path(tempdir(), ".virtualenvs", env_name, "Scripts", "activate"), "&&", "python -m pip install MGP_SDK --quiet", "--no-cache-dir")
        system(pip_upgrade_cmd)
        system(pip_install_cmd)
      } else {
        pip_upgrade_cmd = paste("source", file.path(venv_path, "bin", "activate"), "&&", "python -m pip install", "pip","--upgrade", "--quiet", "--no-cache-dir")
        pip_install_cmd = paste("source", file.path(venv_path, "bin", "activate"), "&&", "python -m pip install", package_name, "--quiet","--no-cache-dir")
        system(pip_upgrade_cmd)
        system(pip_install_cmd)
      }
      # Set the RETICULATE_PYTHON environment variable to the Python interpreter of the new environment
      if (Sys.info()['sysname'] == "Windows") {
        python_interpreter <- file.path(venv_path, "Scripts", "python.exe")
      } else {
        python_interpreter <- file.path(venv_path, "bin", "python")
      }
      Sys.setenv(RETICULATE_PYTHON = python_interpreter)
    }
  )
)


