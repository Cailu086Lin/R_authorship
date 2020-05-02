#install.packages( c("stringdist", "tm" ), repos = "http://cran.us.r-project.org" )

# Finds an number for "name" in "name_map" or generates a new number and adds in "name_map".
string_to_number <- function( name_map, name )
  {
  if( is.null( name ) )
    {
    return( NULL )
    }

  # Normalize "name" for matching.

  normalized_name <- tolower(tm::stripWhitespace( name ) )

  # Look for an entry similar to "normalized_name" in the "name_map" dataframe.

  if( nrow( name_map ) > 0 )
    {
    for( row in 1 : nrow( name_map ) )
      {
      # Return the number of this name if it is similar enough to "normalized_name".

      name_similarity <- stringdist::stringsim( name_map[ row, "normalized_name" ], normalized_name )
      min_similarity <- 0.8

      if( name_similarity > min_similarity )
        {
        return( list( "new_name_map"=name_map, "number"=name_map[ row, "number" ] ) )
        }
      }
    }
  
  # Add a new entry with a new unique number in the "name_map" dataframe.

  new_name_map <- name_map
  new_row <- nrow( new_name_map ) + 1
  new_name_map[ new_row, "name" ] <- name
  new_name_map[ new_row, "normalized_name" ] <- normalized_name
  new_name_map[ new_row, "number" ] <- new_row

  return( list( "new_name_map"=new_name_map, "number"=new_row ) )
  }

# Reads authors and translates their institution names to numbers.
read_and_process_authors <- function( file_name )
  {
  institutions_name_map <- data.frame( name=c(), number=c() )
  authors <- read.csv( file_name, sep="," , stringsAsFactors = FALSE)

  for( row in 1 : nrow( authors ) )
    {
    # Determine and store "inst_number" for "inst".

    result <- string_to_number( institutions_name_map, authors[ row, "inst" ] )
    if( !is.null( result ) )
      {
      institutions_name_map <- result$new_name_map
      authors[ row, "inst_number" ] <- result$number
      }
    }

  return( list( "authors"=authors, "institutions_name_map"=institutions_name_map ) )
  }

# Writes .rmd file with authors and their affiliations.
write_rmd_authors <- function( authors_packet, file_name )
  {
  header <- c(
    "---",
    "title: Authors" )
  footer <- c(
    "output:",
    "  pdf_document: default",
    "---"
    )

  authors_section <- "author:\n"
  authors <- authors_packet$authors
  for( row in 1 : nrow( authors ) )
    {
    if( row > 1 )
      {
      authors_section <- paste( authors_section, ", \\and \n", sep="" )
      }

    authors_section <- paste(
      authors_section,
      "  ",
      authors[ row, "name" ],
      "$^{", authors[ row, "inst_number" ], "}$",
      sep="" )
    }

  affiliations_section <- "date:\n"
  institutions <- authors_packet$institutions_name_map
  for( row in 1 : nrow( institutions ) )
    {
    if( row > 1 )
      {
      affiliations_section <- paste( affiliations_section, ", \n", sep="" )
      }

    affiliations_section = paste(
      affiliations_section,
      "  $^{", institutions[ row, "number" ], "}$",
      institutions[ row, "name" ],
      sep="" )
    }

  writeLines( c( header, authors_section, affiliations_section, footer ), file_name )  
  }

# Main work.

authors_packet <- read_and_process_authors( "author_list.csv")


write_rmd_authors( authors_packet, "filled_authors.rmd" )
