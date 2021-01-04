templatepath <- function(filename){
  system.file("templates", filename, package = "songbookdown")
}

copy_to_local <- function(filename,folder){
  file.copy(templatepath(filename),file.path(folder,filename))
}

#' Create a songbook template
#'
#' @param folder optional: path to folder where this template should be created. Defaults to the cwd
#'
#' @return does not return anything, simply creates files in the specified directory
#' @export
create_template <- function(folder = ".") {
  
  copy_to_local("_bookdown.yaml",folder)
  copy_to_local("_songbookdown.yaml",folder)
  input_folder <- "songs_raw"
  input_folder_full <- file.path(folder,input_folder)
  
  rmd_folder <- "rmd"
  rmd_folder_full <- file.path(folder,rmd_folder)
  
  dir.create(input_folder_full)
  dir.create(rmd_folder_full)
  
  path1 <- file.path(input_folder_full,"classics")
  dir.create(path1)
  copy_to_local("song1.txt",path1)
  
  path2 <- file.path(input_folder_full,"christmas")
  
  dir.create(path2)
  copy_to_local("song2.txt",path2)
  
  
  copy_to_local("index.Rmd",folder)
}