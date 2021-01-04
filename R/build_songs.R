################################################################################
## Songs to RMarkdown ##########################################################
################################################################################


## Helper Functions ############################################################

# get the position of the yaml header
yaml_headerpos <- function(input){
  which(stringr::str_detect(input, "---+"))[1:2]
}

# Reads the yaml header and imports as a named list
read_yamlheader <- function(input,isfile = TRUE){
  if(isfile) input <- readLines(input)
  start_stop <- yaml_headerpos(input)
  yamldata <- yaml::yaml.load(input[(start_stop[1]+1):(start_stop[2]-1)])
  yamldata
}

# removes the yaml header from a text file or character vector
remove_yamlheader <- function(input,isfile = TRUE){
  if(isfile) input <- readLines(input)
  start_stop <- yaml_headerpos(input)
  input[-(start_stop[1]:start_stop[2])]
}


# Creates the song's chapter name
create_songtitle <- function(title,song_tag,artist = NULL,level = 2){
  paste0(
    paste(rep("#",level),collapse = ""),
    " ",
    title,
    ifelse(is.null(artist),"",paste0(" - ",artist)),
    paste0(" {",song_tag,"}")
  )
}

# trims leading and/or trailing elements that are equal to "compare"
trim_lines <- function(char_vec, compare = ""){
  leading <- cumsum(char_vec != compare) == 0
  trailing <- rev(cumsum(rev(char_vec) != compare)) == 0
  char_vec[!(leading | trailing)]
}

# Pads an integer with a specified number of zeros and adds an underscore
mypad <- function(input, by){
  paste0(stringr::str_pad(input,by,pad = "0"),"_")
}

# Creates an html or latex glossary
create_glossary <- function(df, output_type){
  if(output_type == "html"){
    df %>%
      dplyr::transmute(value = glue::glue("[{value}]({song_tag})")) %>%
      kableExtra::kable(format = "html",escape = FALSE,col.names = c(""))
  } else if (output_type == "latex") {
    df %>%
      dplyr::mutate(
        value = stringr::str_replace(value,"\\*(.+)\\*","\\\\emph\\{\\1\\}"),
        song_tag = stringr::str_remove(song_tag,"#"),
      ) %>%
      purrr::pmap_chr(function(song_tag,value){
        
        pagenumber <- paste0("\\pageref{",song_tag,"}")
        hypervalue <- paste0("\\hyperref[",song_tag,"]{",value,"\\dotfill}")
        paste0(hypervalue,pagenumber,"\n")
      })
  }
}

# creates an RMarkdown chunk where a glossary can be inserted
glossary_chunk <- function(output_type,glossary){
  c(
    "",
    glue::glue("```{{asis, echo = (output_type == '{output_type}')}}"),
    glossary,
    "```",
    ""
  )
}

# combines `create_glossary` and `glossary_chunk`
glossary_chunk_full <- function(df, output_type){
  glossary <- create_glossary(df, output_type)
  glossary_chunk(output_type,glossary)
}

# set's a default value if the value is not specified yet (and is therfore NULL)
default_if_null <- function(input,default){
  if(is.null(input)){
    default
  } else{
    input
  }
}

# Takes a character matching either "true" or "false" (case insensitive) and
# turns it into a real R Boolean (TRUE or FALSE). Throws and error if the input
# is ambiguous 
char_to_bool <- function(input){
  istrue <- stringr::str_detect(input, stringr::regex("true", ignore_case = T))
  isfalse <- stringr::str_detect(input, stringr::regex("false", ignore_case = T))
  
  if(istrue & !isfalse) TRUE else if(isfalse & !istrue) FALSE else stop("value is neither true nor false")
}

#' Create the rmd files
#'
#' @param input a folder
#' @param songbook_yamlfile,bookdown_yamlfile the songbook- / bookdown- yamlfile containing
#'   yaml the songbook metadata. Must be relative to the input.
#'
#' @return A batch of Rmd Files which can be used to create the songbook.
#' @export
create_inputfiles <- function(
  input = getwd(),
  songbook_yamlfile = list.files(pattern = "_songbookdown.ya*ml"), 
  bookdown_yamlfile = list.files(pattern = "_bookdown.ya*ml")  
  ){
  
  ## Get settings from yaml ####################################################
  songbookdownyaml <- yaml::read_yaml(songbook_yamlfile)
  bookdownyaml <- yaml::read_yaml(bookdown_yamlfile)
  
  # https://github.com/rstudio/bookdown/blob/92c59d32ecb46aa8cb7150ba1139621705e23901/R/render.R#L69
  # https://r-pkgs.org/r.html?q=www#restore-state-with-baseon.exit
  owd = setwd(input); on.exit(setwd(owd), add = TRUE)
  
  rmd_subdir <- bookdownyaml$rmd_subdir
  output_dir <- bookdownyaml$output_dir
  
  
  cleansubdir <- default_if_null(songbookdownyaml$rmd_subdir,"FALSE") %>%
    char_to_bool()
  glossary <- default_if_null(songbookdownyaml$glossary,"TRUE") %>%
    char_to_bool()
  inputdir <- songbookdownyaml$inputdir
  subfolders <- songbookdownyaml$subfolders
  filetypes <- default_if_null(songbookdownyaml$filetypes,"txt")
  
  
  ## Create Folders ############################################################
  
  if(!dir.exists(rmd_subdir)){dir.create(rmd_subdir)}
  
  if(cleansubdir){for (file in list.files(rmd_subdir,full.names = TRUE)){
    file.remove(file)}
  }
  
  subfolders_dfr <- purrr::imap_dfr(subfolders,function(chapter,folder){
    dplyr::tibble(chapter = chapter[[1]],
                  folder = folder)
  }) %>%
    dplyr::mutate(i = dplyr::row_number())
  
  
  ## Edit Lines ################################################################

  allfiles <- purrr::pmap_dfr(subfolders_dfr, function(chapter,folder,i){
    fullpath <- file.path(inputdir,folder) %>%
      list.files(pattern = paste(filetypes,collapse = "|"),full.names = TRUE)
    
    mylines <- purrr::map(fullpath, ~readLines(.x,warn = FALSE))
    
    dplyr::tibble(fullpath = fullpath, 
                  folder = folder, chapter = chapter, chapter_i = i, lines = mylines) 
  }) 
  
  metadata_dfr <- purrr::map_dfr(allfiles$lines, function(x){
    x %>%
      read_yamlheader(FALSE) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate_all(~as.character(.))
  })
  
  if(!"artist" %in% colnames(metadata_dfr)){
    metadata_dfr$artist <- NA_character_
  }
  
  allfiles <- cbind(allfiles,metadata_dfr)
  
  npad1 <- (length(subfolders)+1) %>% log10() %>% ceiling()
  npad2 <- allfiles %>% split(.$folder) %>% purrr::map_int(nrow) %>% max() %>% log10() %>% ceiling()
  
  
  allfiles <- allfiles %>%
    dplyr::mutate(sortval = purrr::map_dbl(fullpath,~mean(utf8ToInt(basename(.x))))) %>%
    dplyr::arrange(folder,sortval) %>%
    dplyr::group_by(folder) %>%
    dplyr::mutate(
      i_file = dplyr::row_number(),
      rmd_file_prefix = paste0(mypad(chapter_i,npad1),mypad(i_file,npad2))
    ) %>%
    dplyr::select(-c(i_file,chapter_i,sortval))
  
  
  allfiles$lines <- purrr::map(allfiles$lines,function(song_rl){
    remove_yamlheader(song_rl,FALSE)
  })
  # Make sure that the first verse / chorus etc starts with an empty line
  # and the last ends with an empty line
  allfiles$lines <- purrr::map(allfiles$lines,function(song_rl){
    c("",trim_lines(song_rl, ""))
  })
  
  # wraps each verse/chorus etc with a chunk. 
  allfiles$lines <- purrr::map(allfiles$lines,function(song_rl){
    split(song_rl, cumsum(song_rl == "")) %>%
      purrr::map(function(song_part){
        print(song_part)
        song_part <- trim_lines(song_part)
        directives_regex <- "\\{(start|end)_of_(\\w+)\\}" # todo: implement {start_of_verse: verse 1}
        directivepos <- stringr::str_detect(song_part,directives_regex)
        part_class <- unique(stringr::str_match(song_part[directivepos],directives_regex)[,3])
        part_class <- ifelse(is.na(part_class),"",paste0(", class = '",part_class,"'"))
        part_class = "" # todo: remove this line and maby test engines?
        song_part <- song_part[!directivepos]
        if(length(song_part)>0){
          c(
            paste0("```{",part_class,"}"),
            song_part,
            "```",
            ""
          )
        }
      }) %>%
      unlist() %>%
      purrr::set_names(NULL)
  })
  
  
  # Builds the song chapter name
  allfiles <- allfiles %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      song_i = dplyr::row_number(),
      song_tag = paste0("#song",song_i),
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      song_header = create_songtitle(title,song_tag,artist)
    )
  
  # Includes the chapter name in the lines
  allfiles$lines <- purrr::map2(allfiles$lines,allfiles$song_header,function(song_rl,song_header){
    c(
      "<!-- # Generated by songbokdown: do not edit by hand -->",
      song_header,
      "",
      song_rl
    )
  })
  
  
  
  metadata_other_dfr <- allfiles[,colnames(metadata_dfr)[!colnames(metadata_dfr) %in% c("title","artist")]]
  
  
  # This is quite an ugly hack, at least give some better names
  metadata_other_dfr2 <- metadata_other_dfr %>%
    split(1:nrow(metadata_other_dfr)) %>%
    purrr::map(function(x){
      x <- as.list(x)
      any_nonempty <- any(purrr::map_lgl(x,function(y){!is.na(y)}))
      list(any_nonempty, x)
    })
  
  allfiles$lines <- purrr::map2(allfiles$lines,metadata_other_dfr2, function(song_rl,any_nonempty){
    if(any_nonempty[[1]]){
      meta_pander <- any_nonempty[[2]] %>%
        purrr::imap_dfr(~data.frame(key = .y,val = .x) %>% dplyr::filter(!is.na(.x))) %>%
        knitr::kable(col.names = c("",""),format = "pandoc")
      c(song_rl,"",meta_pander)
    } else{
      song_rl
    }
  })
  
  
  allfiles <- allfiles %>%
    dplyr::mutate(
      rmd_file_name = file.path(rmd_subdir,paste0(rmd_file_prefix, stringr::str_replace(basename(fullpath),".txt",".Rmd")))
    ) 
  
  
  purrr::map2(allfiles$lines,allfiles$rmd_file_name,function(song_rl,rmd_file_name){
    writeLines(song_rl,rmd_file_name)
  })
  
  
  allfiles %>%
    dplyr::select(-lines) %>%
    utils::write.csv(file.path(rmd_subdir,"meta_data.csv"))
  
  
  subfolders_dfr %>%
    dplyr::mutate(filename = paste0(mypad(i,npad1),mypad(0,npad2),folder,".Rmd")) %>%
    dplyr::select(chapter,filename) %>%
    purrr::pmap(function(chapter,filename){
      writeLines(paste("#",chapter),
                 file.path(rmd_subdir,filename))
    })
  
  
  if(glossary){
    
    glossary_filename <- paste0(mypad(max(subfolders_dfr$i) + 1,npad1),mypad(0,npad2),"glossary.Rmd")
    
    
    glossary_keywords <- c("title", "subtitle", "artist", "composer", "lyricist", "album")
    glossary_keywords <- glossary_keywords[glossary_keywords %in% colnames(allfiles)]
    glossary_df <- allfiles[,c(glossary_keywords,"song_tag")] %>%
      tidyr::pivot_longer(-c(song_tag,title)) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(
        title = paste0("*",title,"*"),
        value = paste0(value, " - ",title)
      ) %>%
      dplyr::select(-name) %>%
      tidyr::pivot_longer(c(title,value)) %>%
      dplyr::select(-name) %>%
      dplyr::arrange(stringr::str_remove(value,"\\*"))
    
    
    c(
      "<!-- # Generated by songbokdown: do not edit by hand -->",
      "# Glossary",
      "",
      "```{r, echo = FALSE}",
      "output_type <- knitr::opts_knit$get('rmarkdown.pandoc.to')",
      "```",
      "",
      glossary_chunk_full(glossary_df, "html"),
      glossary_chunk_full(glossary_df, "latex")
    ) %>%
      writeLines(file.path(rmd_subdir,glossary_filename))
  }
}