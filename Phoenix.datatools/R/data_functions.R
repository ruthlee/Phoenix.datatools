#' Turn categorical covariates into numeric values.
#' 
#' This function takes the categorical covariate covariate_name within an excel file and converts each level of the variable to a numeric value. Based on the as.numeric function, the categorical variables are assigned numbers in alphabetical order starting with 1. The new covariate values will be added as a column to the original excel sheet and a new file will be created in the directory specified. 
#' @param directory The directory of the excel file. This must be the full directory name. An easy way to find this is to set the directory using RStudio's "Files" tab and copying and pasting the directory name from the console. Enter as a string (with quotations). 
#' @param filename The excel file's name. Make sure to include the file extension, in this case .xlsx. Enter as a string.
#' @param sheetindex The sheet to be read into R. Default = 1.
#' @param types A vector of strings indicating the types of each of the columns in the excel sheet being read into R. The data types must be "numeric" or "character".
#' @param covariate_name The name of the covariate that takes the numeric values. Must match the header of the column with the covariates. Enter as a string.
#' @keywords numeric covariates categorical
#' @export
#' @examples types <- c( "numeric", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric", "numeric" )
#' numeric_covariates ( directory = "~/Desktop/Clinical Pharmacology/Kyelee Fitts/Docetaxel_phoenix", filename = "popPK data.xlsx", sheetindex = 1, types = types, covariate_name = "Race" )
#' numeric_covariates()

numeric_covariates <- function ( directory, filename, sheetindex = 1, types, covariate_name ) {
  
  library("xlsx")
  
  setwd(directory)
  
  raw_data <- read.xlsx2 ( filename, sheetindex, colClasses = types) 
  
  new_covariate <- as.numeric ( raw_data [ , covariate_name ] ) 
  
  raw_data [ , paste ( covariate_name, "1", sep = "" ) ] <- new_covariate
  
  name <- paste ( "data_", covariate_name, "_", filename, sep = "" ) 
  
  write.xlsx2( raw_data, file = name )
  
}



#' Make negative times zeros.
#' 
#' This function takes all times in the column time_column within a sheet of an excel file and converts the negative time values to zeros.
#' @param directory The directory of the excel file. This must be the full directory name. An easy way to find this is to set the directory using RStudio's "Files" tab and copying and pasting the directory name from the console. Enter as a string (with quotations). 
#' @param filename The excel file's name. Make sure to include the file extension, in this case .xlsx. Enter as a string.
#' @param sheetindex The sheet to be read into R. Default = 1.
#' @param types A vector of strings indicating the types of each of the columns in the excel sheet being read into R. The data types must be "numeric" or "character".
#' @param time_column The heading of the column of the column containing the times. Must match the header of the column of the Excel file. Enter as string.
#' @keywords times zeros negative
#' @export
#' @examples types <- c( "numeric", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric", "numeric" )
#' times_as_zeros ( "C:/Users/FiggLab4357/Desktop/Clinical Pharmacology/Kyelee Fitts/Docetaxel_phoenix", "PopPK Data.xlsx", sheetindex = 1, types, "DTX_Time" )
#' times_as_zeros()

times_as_zeros <- function ( directory, filename, sheetindex, types, time_column ) { 
  
  library("xlsx")
  
  setwd(directory)
  
  raw_data <- read.xlsx2 ( filename, sheetindex, colClasses = types ) 
  
  
  for ( i in 1:nrow( raw_data ) ) {
    if ( raw_data [ i, time_column ] <= 0 & !is.na ( raw_data [ i, time_column ] ) )  {
      raw_data [ i, time_column ] <- 0 
    }
  }
  
  write.xlsx2 ( raw_data, file = paste ( "time_zero_", filename, collapse = NULL ) )
  
}

