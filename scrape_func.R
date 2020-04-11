# function to scrape a usgs peak flow gage dataset
# loops over the gageID values
# for each gage, it takes the data and saves it to .txt files

scrape_gage <- function(gageID # character (single value or vector)
                        ,agencyID='USGS' # agencyID, only verified for USGS gages
                        ,keepComments = F # drop lines in file beginning with '#' and the string length specifiers
                        ,nm = NA # name corresponding to gageID, if NA use gageID
                        ,pathFile = NA # path for saving output .txt file
                        ,printProgress=F # printing progress bar, NA means no progress
){
  
  # not appropriate number of names
  if(is.na(nm) == F){
    if(length(nm) != length(gageID)){
      print('Length of gageID and nm do not match\n')
    }
  }
  
  # agency ID potential problems
  if(min(agencyID== 'USGS')==0){
    print('Non-USGS gages should be checked manually\n')
    print(paste(agencyID[agencyID!='USGS'],sep='\t'))
  }else if(length(agencyID)!=1 && length(agencyID) != length(gageID)){
    print('Length of gageID and agencyID do not match\n')
  }else{
    
    # loop over the gages
    for(i in 1:length(gageID)){
      
      # standard format of the url for the website with the tab separated peak flow data
      url <- paste('https://nwis.waterdata.usgs.gov/nwis/peak?site_no=',ifelse(nchar(as.character(home_zip_finFilter$gage[i]))==7,'0',''),as.character(home_zip_finFilter$gage[i]),'&agency_cd=',ifelse(length(agencyID==1),agencyID,agencyID[i]),'&format=rdb',sep='')
      
      # reading values from the url
      a <- readLines(url)
      
      if(keepComments==F){
        ind_keep <- c()
        
        for(j in 1:length(a)){
          
          # agencyID column, the first, always specified with 5s
          if((substring(a[[j]],1,1)=='#') || (substring(a[[j]],1,2)=='5s')){
            
          }else{
            ind_keep <- c(ind_keep,j)
          }
        }
      }else{ # keep everything
        ind_keep <- 1:length(a)
      }
     
      # writing files to the output directory
      # file name
      file_name <- paste(path,'/',ifelse(is.na(nm),gageID[i],nm[i]),'.txt',sep='')
      
      # opening connection
      file_conn <- file(file_name)
      # writing file
      writeLines(a[ind_keep],file_conn)
      # closing connection
      close(file_conn)
    }
    
    # printing the progress of the results
    if(printProgress==T){
      if(length(gageID) <= 20){
        print(paste(i,' out of ',length(gageID),sep=''))
      }else{
        printers <- round(seq(0,1,0.05)*length(gageID))
        if(i %in% printers){
          print(paste(seq(0,1,0.05)[pinters == i], '% scraped',sep=''))
        }
      }
    }
  }
  
  print('Scraping complete!')
}