#' @author Brian Mainter
#' @description Function to grab and download mean cloud cover data from earth engine
#' @import rgee
#' @import googledrive
#' @param sf_poly Simple features polygon
#' @param datestart the first date (y-m-d format) to include (defaults to 2000-01-01)
#' @param datestop the first date (y-m-d format) to include (defaults to present)
#' @param daystart the first day of each year to include.  (defaults to 1)
#' @param daystop the first day of each year to include (defaults to 366)
#' @param folder the google drive folder to store the results in (defaults to "clouds_temp")
#' @param local_file the local file to save the results to (defaults to "temp/cloud_stats.csv")
#' @param id_field a field in the sf_poly that contains unique ids.  Defaults to "id".
get_cloud_mean_ee <- function(sf_poly,
                              datestart = "2000-01-01",
                              datestop = paste(format(Sys.time(), "%Y-%m-%d"),sep = ""),
                              daystart = 1,
                              daystop = 366,
                              folder = "clouds_temp",
                              local_file = "temp/cloud_stats.csv",
                              id_field = "id"){
  
  #Intialize rgee
    ee_Initialize()
  
  #Convert polygon to ee object
  ee_poly <- sf_as_ee(x = sf_poly)
  
  
  #pull modis cloud data into a single ee object
    sen1 <- ee$ImageCollection('MODIS/061/MOD09GA')
    sen2 <- ee$ImageCollection('MODIS/006/MYD09GA')
    
    sens <- sen1$merge(sen2)
    rm(sen1, sen2)
  
  # subset data to relevent days/dates
  
    filter_dates <- function(collection,datestart,datestop,daystart,daystop){
      getdates <- ee$Filter$date(datestart,datestop);
      getday <- ee$Filter$calendarRange(daystart,daystop,"day_of_year");
      return(ee$ImageCollection(collection)$filter(getdates)$filter(getday))
    }
    
    sens_filtered <-
      filter_dates(collection = sens,
                   datestart = "2000-01-01",
                   datestop = paste(format(Sys.time(), "%Y-%m-%d"),sep = ""),
                   daystart = 273,
                   daystop = 366)
    
    
  # Convert the data into binary cloud data
    
    #Helper function to extract the values from specific bits
    # The input parameter can be a ee.Number() or ee.Image()
    # Code adapted from https://gis.stackexchange.com/a/349401/5160
    bitwiseExtract <- function(input, fromBit, toBit) {
      maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
      mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
      return(input$rightShift(fromBit)$bitwiseAnd(mask))
    }
    
    # function to convert QA to binary cloud cover
    
    get_binary_cloud <- function(img) {
      
      # Extract the NDVI band
      qa <- img$select("state_1km")
      
      # extract the relevant bits
      
      clouds <- bitwiseExtract(input = qa,fromBit = 0,toBit = 1)$eq(1)
      
      # set the date
      
      date <- img$date()$format('yyyy-MM-dd')
      
      clouds$set("date_char",date)
      
    }
    
    
    clouds_sens <- sens_filtered$map(get_binary_cloud)
    
    
    # reducer function to allow mapping across layers
    
      get_polygon_stats <- function(img){
        
        date <- img$get("date_char")
        
        img$reduceRegions(
          collection = ee_poly,
          reducer = ee$Reducer$mean(),
          scale = 1000 #native resolution is ~1 km, so no need to go finer than this
        )$map(function(x){ x$set("date",date) }) #sets the date for later use
        
      }
    
    cloud_stats <- clouds_sens$map(get_polygon_stats) # the extraction
    
    flat_stats <- cloud_stats$flatten() #flatter into a table
    
    # Download table to drive
    
    flat_dl<-
      ee_table_to_drive(collection = flat_stats,
                        description = "cloud_stats",
                        folder = folder,
                        timePrefix = FALSE,
                        fileFormat = "CSV",
                        selectors = c(id_field,"date","mean"))
    
    flat_dl$start()#starts the processing
    ee_monitoring(flat_dl,max_attempts = 100000) #keeps track of progress
    
    
    googledrive::drive_download(file = paste(folder,"/cloud_stats.csv",sep = ""),
                                path = local_file,
                                overwrite = TRUE) #note: if dling more than one need to add a prefix or something
    
    
    return(invisible(NULL))
  
}