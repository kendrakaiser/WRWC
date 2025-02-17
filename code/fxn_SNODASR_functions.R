###########---------download.SNODAS.r-----------------------
#' @name download.SNODAS
#' @title Downloads SNODAS data from the UC Boulder FTP server
#' @param dates A vector of date objects or strings that can be converted to dates by lubridate::ymd()
#' @param path A path to save the files locally, with a trailing slash (e.g. the default is './SNODAS/'). If the directory does not exist it will be created.
#' @param overwrite Should extant files be overwritten?
#' @param masked Do you want CONUS snodas data or unmasked North America (NB: unmasked has much more restricted dates available)
#' @param parallel  Do you want to download and process files in parallel (for greatly enhanced performance?) via the doParallel package.
#' @param ncores If parallel=TRUE, how many cores should R use for the process. Default is 3 as a "safe" number, but can be adjusted higher if your computer has more cores available.
#' @param unzip Should the files be unzipped or will they be accessed in their compressed state? Unzipping dramatically increases read performance but will take up much more hard drive space. If you're only going to make one spatial subset of the data, leaving the files comopressed will be faster and take up less storage. But if you're going to use the SNODAS data over and over again, unzip the data (storage space permitting)
#'
#'
#' @return NULL (This function is only used for its side effects. SNODAS data for the specified dates will be downloaded and untarballed in the specified path.)
#'
#'@examples download.SNODAS(dates=c('2010-02-20', '2015-02-10'), path='./SNODAStest/')
#'


#Package check fails without marking i as global due to nonstandard eval of foreach()
utils::globalVariables('i')

#'@export

download.SNODAS <- function(dates,
                            path = './SNODAS/',
                            overwrite=TRUE,
                            masked=TRUE,
                            parallel=F,
                            ncores=2,
                            unzip=TRUE){

  #Contstructs URLs given specified dates
  #Good god these are annoying URLs to construct.
  if(masked==TRUE){

    URLpaths <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      URLpath <- paste0('ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/',
                        lubridate::year(x),
                        '/',
                        stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                        '_',
                        as.character(lubridate::month(x, label=TRUE, abbr=TRUE)),
                        '/')
      return(URLpath)
    }, USE.NAMES = FALSE)

    filenames <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      filename <- paste0('SNODAS_',
                         lubridate::year(x),
                         stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                         stringr::str_pad(lubridate::day(x), width=2, side='left', pad='0'),
                         '.tar')
      return(filename)
    }, USE.NAMES = FALSE)

    URLs <- paste0(URLpaths, filenames)

  } else { #if unmasked rasters are wanted

    URLpaths <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      URLpath <- paste0('ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/',
                        lubridate::year(x),
                        '/',
                        stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                        '_',
                        as.character(lubridate::month(x, label=TRUE, abbr=TRUE)),
                        '/')
      return(URLpath)
    }, USE.NAMES = FALSE)

    filenames <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      filename <- paste0('SNODAS_unmasked_',
                         lubridate::year(x),
                         stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                         stringr::str_pad(lubridate::day(x), width=2, side='left', pad='0'),
                         '.tar')
      return(filename)
    }, USE.NAMES = FALSE)

    URLs <- paste0(URLpaths, filenames)

  }

  #Create path if it doesn't exist.
  if(!(dir.exists(path))){
    dir.create(path)
  }


  #Does the actual downloading, either in parallel or not.
  if(parallel==TRUE){
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)

    foreach::"%dopar%"(foreach::foreach(i = 1:length(URLs)), {
      if(!file.exists(paste0(path,filenames[i])) | overwrite==TRUE){
        tryCatch({
          utils::download.file(URLs[i], paste0(path,filenames[i]), mode='wb')
        },error = function(e){warning(e)})
        if(file.exists(paste0(path,filenames[i]))){
          utils::untar(paste0(path,filenames[i]), exdir=path)
        }

      } else {
        warning(paste(filenames[i], 'exists in the directory specified. Did you want overwrite=TRUE?'))
      }
    })

    files_to_unzip <- list.files(path, pattern='.gz', full.names = TRUE)

    if(unzip==TRUE){
    foreach::"%dopar%"(foreach::foreach(i = 1:length(files_to_unzip)), {
    R.utils::gunzip(files_to_unzip[i])
    })
    }

    parallel::stopCluster(cl)



  } else { #not parallel
    foreach::"%do%"(foreach::foreach(i = 1:length(URLs)),  {
      if(!file.exists(paste0(path,filenames[i])) | overwrite==TRUE){
        tryCatch({
          utils::download.file(URLs[i], paste0(path,filenames[i]), mode='wb')
        },error = function(e){warning(e)})
        if(file.exists(paste0(path,filenames[i]))){
          utils::untar(paste0(path,filenames[i]), exdir=path)
        }
      } else {
        warning(paste(filenames[i], 'exists in the directory specified. Did you want overwrite=TRUE?'))
      }
    })

    if(unzip==TRUE){
      files_to_unzip <- list.files(path, pattern='.gz', full.names = TRUE)
      foreach::"%do%"(foreach::foreach(i = 1:length(files_to_unzip)), {
        R.utils::gunzip(files_to_unzip[i],overwrite=T)
      })
    }
  }
  return()
}





##############---------------------------extract.SNODAS.subset.r---------------------
#' @title Extracts spatial and temporal subsets of SNODAS data and returns them as multiband rasters (one layer per date, one raster per dataset)
#'
#' @param dates Either a vector of date objects or a character vector that can be interpreted by lubridate::ymd(), specifying the dates wanted
#' @param values_wanted A vector of character strings giving the names of the datasets to create subsets of. Possible values are: 'SWE','Depth','Runoff','Sublim_Pack', 'Sublim_Blow', 'P_Solid', 'P_Liquid', 'T_Mean'. The default is to return all.
#' @param extent a 2x2 matrix of the extent desired, following  the format format of input to extent(), or an Extent object. Must be latlong (negative degrees E, degrees N) with WGS84 datum. No modular arithmetic will be performed on the coordinates.
#' @param masked Logical. Are the source files masked to CONUS or unmasked?
#' @param read_path A character string specifying the source directory of SNODAS data, with a trailing slash (e.g. './SNODAS/')
#' @param compressed Logical. Are the datasets compressed? If so, they will be uncompressed before data extraction, then deleted. This will save a LOT of storage space but will blow up processing times approximately 10-fold.
#' @param nodata_handling A character string. If data does not exist, do you want NAs ('NA'), an error ('error') or just no band in the returned rasters ('skip') for that day?
#' @param write_file Logical. Should the extracted data be written to storage or returned in memory. If returned in memory, the function will return a list of rasters with one object for each values_wanted.
#' @param write_path A character string specifying the path to write data, if write_file=TRUE. With trailing slash (e.g. './SNODAS/')
#' @param filename_prefix A character string that will be prepended to the files written to storage. Helpful for including a unique identifier (e.g. Site ID) in the file names.
#' @param write_extension #A character string. File extension to save outputted data. Will be parsed by raster::writeRaster do determine appropriate format (see ?writeRaster for possible formats). Including leading period (e.g. '.img')
#'
#' @return NULL if write_file=TRUE, otherwise a list of the desired multiband rasters.
#'
#' @examples download.SNODAS(dates=c('2010-02-20', '2015-02-10'), path='./SNODAStest/')
#'

#' @export
extract.SNODAS.subset <- function(dates,
                                  values_wanted = c('SWE','Depth','Runoff','Sublim_Pack', 'Sublim_Blow', 'P_Solid', 'P_Liquid', 'T_Mean'),
                                  extent, #
                                  masked=TRUE,
                                  read_path = './SNODAS/',
                                  compressed = FALSE,
                                  nodata_handling='NA',
                                  write_file = TRUE,
                                  write_path = './SNODAS/extracted', #With trailing slash
                                  filename_prefix = '',
                                  write_extension='.img'
) {

  #Translates desired extent into pixel values desired (grid_extent). Note that for grid_extent ymin>ymax because grid index increases N to S, but lat increases S to N.

  dates <- lubridate::ymd(dates)

  #Create path if it doesn't exist.
  if(!(dir.exists(write_path))){
    dir.create(write_path)
  }

  #This code translates the desired geographic coordinates into grid indices for data extraction, and also calculates the extent of the retrieved grid.
  {
    extent <- as.matrix(extent)

    SNODAS_extent <- matrix(c(-124.733749999998366,-66.942083333334011,24.949583333333454,52.874583333332339), nrow=2, byrow = TRUE)

    if(!(identical(as.logical(SNODAS_extent<extent),c(T,T,F,F)))){
      stop('Specified extent exceeds SNODAS data spatial extent.')
    }


    temp <- abs(SNODAS_extent-extent)/0.008333333333333
    #Size of the grid, USING ZERO-BASED NUMBERING (easier for indexing, as seen later)
    grid_extent <- matrix(c(floor(temp[1,1]),
                            6935-floor(temp[1,2]), #The grids are 6935 x 3351, which should have this code make more sense.
                            3351-floor(temp[2,1]),
                            floor(temp[2,2])),
                          nrow=2,
                          byrow=T)

    #Note, grid_extent_latlong will be slightly larger than extent because the grid includes all pixels partially in extent.
    grid_extent_latlong <- matrix(c(SNODAS_extent[1,1]+(grid_extent[1,1])*0.008333333333333,
                                    SNODAS_extent[1,1]+(grid_extent[1,2])*0.008333333333333,
                                    SNODAS_extent[2,2]-(grid_extent[2,1])*0.008333333333333,
                                    SNODAS_extent[2,2]-(grid_extent[2,2])*0.008333333333333), nrow=2, byrow=T)
  }

  #Initialize list if needed.
  if(write_file==FALSE){
    list_to_return <- list()
  }

  for(l in 1:length(values_wanted)){
    #This constructs the file names of the data desired.
    {
      name_template <- switch(values_wanted[l],
                              SWE= 'YY_ssmv11034tS__T0001TTNATSXXXXXXXX05HP001.dat',
                              Depth='YY_ssmv11036tS__T0001TTNATSXXXXXXXX05HP001.dat',
                              Runoff='YY_ssmv11044bS__T0024TTNATSXXXXXXXX05DP000.dat',
                              Sublim_Pack='YY_ssmv11050lL00T0024TTNATSXXXXXXXX05DP000.dat',
                              Sublim_Blow='YY_ssmv11039lL00T0024TTNATSXXXXXXXX05DP000.dat',
                              P_Solid='YY_ssmv01025SlL01T0024TTNATSXXXXXXXX05DP001.dat',
                              P_Liquid='YY_ssmv01025SlL00T0024TTNATSXXXXXXXX05DP001.dat',
                              T_mean='YY_ssmv11038wS__A0024TTNATS2004112405DP001.dat')

      if(masked==FALSE){
        name_template <- stringr::str_replace(name_template, 'YY', 'zz')
      } else{
        name_template <- stringr::str_replace(name_template, 'YY', 'us')
      }

      filenames <- stringr::str_replace(name_template, 'XXXXXXXX', stringr::str_remove_all(as.character(dates), '-'))
      print(filenames)

    }

    extracted <- array(dim=c(grid_extent[2,1]-grid_extent[2,2], ncol=grid_extent[1,2]-grid_extent[1,1], length(filenames)))
    toskip <- integer()

    for(k in 1:length(filenames)){

      #Handlings for missing data.
      if(!file.exists(paste0(read_path,filenames[k]))){
        if(nodata_handling=='error'){
          stop(paste(values_wanted[l], 'data for', dates[k], ' not found.'))
        } else if (nodata_handling=='skip'){
          warning(paste(values_wanted[l], 'data for', dates[k], ' not found.'))
          toskip <- c(toskip, k)
        } else if (nodata_handling=='NA'){
          warning(paste(values_wanted[l], 'data for', dates[k], ' not found.'))
        }else {
          stop('Invalid value for argument nodata_handling.')
        }
      }else {

        #The meat of the data reading.

        if(compressed==TRUE){
          #Reads the compressed bytestream up until the last row needed in the raster (to save loading e.g. southeast Texas every time into memory) and then extracts the values needed.
          #seek() doesn't play nice with compressed files is why this code is necessary.
          zipped_file <- gzfile(paste0(read_path, filenames[k],'.gz'), open='rb')
          current_file <- matrix(readBin(zipped_file, 'integer', n = (grid_extent[2,1]*6935), size=2), ncol=6935)
          extracted[,,k] <- current_file[grid_extent[1,1]:grid_extent[1,2],grid_extent[2,2]:grid_extent[2,1]]

        } else {
          current_file <- file(paste0(read_path, filenames[k]), open='rb')
          j <- 1
          for(i in grid_extent[2,2]:(grid_extent[2,1]-1)){ #Iterate over raster rows, going N to S
            seek(con=current_file, (i*6935 + grid_extent[1,1])*2) #Set the file pointer to the byte representing the xmin pixel in the ith row. pointer byte is multiplied by 2 because each pixel in the raster has length of 2 bytes.
            extracted[j,,k] <- readBin(con=current_file, what='integer', n=(grid_extent[1,2]-grid_extent[1,1]), size=2, endian='big') #Read the desired # of pixels from each row.
            j <- j+1
          }
          close(current_file)
        }

      }
    }


    #Clean up and write out data.
    {
      extracted[extracted==-9999] <- NA
      if(length(toskip)>0){
        extracted <- extracted[,,-toskip]
      }
      #Convert extracted data to a georeferenced raster brick.
      raster_output <- raster::brick(extracted, xmn=grid_extent_latlong[1,1], xmx=grid_extent_latlong[1,2], ymn=grid_extent_latlong[2,1], ymx=grid_extent_latlong[2,2], crs= "+proj=longlat +datum=WGS84")
      if(length(toskip>0)){
        names(raster_output) <- dates[-toskip]
      } else {
        names(raster_output) <- dates
      }



      if(write_file==FALSE){
        list_to_return[l] <- raster_output
      }else{
        raster::writeRaster(raster_output, paste0(write_path, filename_prefix,'_',values_wanted[l],'_from_', min(dates), '_to_', max(dates),write_extension))
      }
    }


  }

  if(write_file==TRUE){
    print('Processing completed.')
    return()
  } else {
    print('Processing completed.')
    return(list_to_return)
  }
}




############-------------read.SNODAS.masked.R----------------------
#' @title Converts a raw SNODAS file into a georeferenced raster, saving the file if desired,
#' @param filename A character string specifying the .dat file to convert
#' @param read_path A character string specifying the path to the location of filename, with a trailing slash (e.g. './SNODAS/')
#' @param write_file Logical. Should the converted file be written to storage or returned in memory?
#' @param write_path  A character string specifying the path to write to, with a trailing slash (e.g. './SNODAS/') Directory will be created if it does not exist.
#' @param write_extension  #A character string specifying the file extension to save outputted data. Will be parsed by raster::writeRaster do determine appropriate format (see ?writeRaster for possible formats). Including leading period (e.g. '.img')
#' @return If write_file=FALSE, returns a RasterLayer object, otherwise only used for its side effect of writing a raster file to storage.
#' @examples read.SNODAS('us_ssmv01025SlL01T0024TTNATS2010022005DP001.dat', read_path='./SNODAStest/')
#' @export
read.SNODAS.masked <- function(filename,
                        read_path = './SNODAS/',
                        write_file = TRUE,
                        write_path = './SNODAS/',
                        write_extension='.img'){
  file_opened <- file(paste0(read_path, filename), open='rb')
  rawdat <- readBin(file_opened, n=3351*6935, what='integer', size=2, endian='big')
  close(file_opened)
  rawdat[rawdat==-9999]  <- NA
  rawmatrix <- matrix(data=rawdat, nrow=3351, ncol=6935, byrow=TRUE)
  image <- raster::raster(rawmatrix, xmn=-124.733749999995013, xmx=-66.942083333330658, ymn=24.949583333332331, ymx=52.874583333331216, crs="+proj=longlat +datum=WGS84")
  if(write_file==TRUE){
    if(!(dir.exists(write_path))){
      dir.create(write_path)
    }
    raster::writeRaster(image, paste0(write_path, unlist(strsplit(filename, '.dat')),write_extension))
    return()
  } else{
  return(image)
  }
}



####################------------read.SNODAS.unmasked.R--------------
#' @title Converts a raw SNODAS file into a georeferenced raster, saving the file if desired,
#' @param filename A character string specifying the .dat file to convert
#' @param read_path A character string specifying the path to the location of filename, with a trailing slash (e.g. './SNODAS/')
#' @param write_file Logical. Should the converted file be written to storage or returned in memory?
#' @param write_path  A character string specifying the path to write to, with a trailing slash (e.g. './SNODAS/') Directory will be created if it does not exist.
#' @param write_extension  #A character string specifying the file extension to save outputted data. Will be parsed by raster::writeRaster do determine appropriate format (see ?writeRaster for possible formats). Including leading period (e.g. '.img')
#' @return If write_file=FALSE, returns a RasterLayer object, otherwise only used for its side effect of writing a raster file to storage.
#' @examples read.SNODAS('us_ssmv01025SlL01T0024TTNATS2010022005DP001.dat', read_path='./SNODAStest/')
#' @export
read.SNODAS.unmasked <- function(filename,
                        read_path = './SNODAS/',
                        write_file = TRUE,
                        write_path = './SNODAS/',
                        write_extension='.img'){
  file_opened <- file(paste0(read_path, filename), open='rb')
  rawdat <- readBin(file_opened, n=4096*8192, what='integer', size=2, endian='big')
  close(file_opened)
  rawdat[rawdat==-9999]  <- NA
  rawmatrix <- matrix(data=rawdat, nrow=4096, ncol=8192, byrow=TRUE)
  image <- raster::raster(rawmatrix, xmn=-130.512499999995, xmx=-62.2499999999977, ymn=24.0999999999990, ymx=58.2333333333310, crs="+proj=longlat +datum=WGS84")
  if(write_file==TRUE){
    if(!(dir.exists(write_path))){
      dir.create(write_path)
    }
    raster::writeRaster(image, paste0(write_path, unlist(strsplit(filename, '.dat')),write_extension))
    return()
  } else{
  return(image)
  }
}








