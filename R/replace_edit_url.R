#' Replace the (github) "Edit"-URLs
#' 
#' This function replaces the URL that normally links to the Rmd File on github 
#'   with the URL of the source text file. 
#'
#' @param songbook_yamlfile,bookdown_yamlfile the songbook- / bookdown- yamlfile containing
#'   yaml the songbook metadata. Must be relative to the input.
#'
#' @return Does not return a value, simply modiefies the html files.
#' @export
#'
replace_edit_url <- function(
  songbook_yamlfile = list.files(pattern = "_songbookdown.ya*ml"), 
  bookdown_yamlfile = list.files(pattern = "_bookdown.ya*ml") 
){
  bookdownyaml <- yaml::read_yaml(bookdown_yamlfile)
  rmd_subdir <- bookdownyaml$rmd_subdir
  
  songs_meta_data_df <- utils::read.csv(file.path(rmd_subdir,"meta_data.csv"))
  
  songbookdownyaml <- yaml::read_yaml(songbook_yamlfile)
  
  edit <- bookdownyaml$edit
  output_dir <- bookdownyaml$output_dir
  
  if(!is.null(edit)){
    edit2 <- stringr::str_remove(edit, "%s")
    
    list.files(output_dir,".html",full.names = TRUE)
    
    songs_meta_data_df %>%
      dplyr::mutate(html_file = file.path(output_dir,stringr::str_replace(basename(rmd_file_name),"\\.Rmd","\\.html"))) %>%
      dplyr::select(html_file,rmd_file_name,fullpath) %>%
      purrr::pmap(function(html_file,rmd_file_name,fullpath){
        file_exists = file.exists(html_file)
        
        if(file_exists){
          
          
          html_rl <- html_file %>%
            readLines()
          
          html_rl <- stringr::str_replace(html_rl, paste0(edit2,rmd_file_name), paste0(edit2,fullpath))
          
          writeLines(html_rl,html_file)
        } else{
          print(paste(html_file,"not found"))
        }
        
      })
  }
  
}