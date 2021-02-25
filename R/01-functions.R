#' Add Line Breaks (superceded)
#'
#' @param thetext a character vector with one line of a song.
#' @param output a string describing what kind of output. defaults to character. If anything else, output is sent to writeLines().
#'
#' @return
#'
add_br  <- function(thetext, output = 'character') {

  results <- str_replace_all(thetext, "(.)\n","\\1<br>\n") %>%
      str_replace("^(##.*)<br>", "\\1")

  if(output == 'character') {
    return(results)
  } else {
    writeLines(results)
  }

}





#' Prepare slide titles
#'
#' @param thetext the markdown text of a song with h2 style headers for each slide (eg. ## Verse 2)
#' @param thetitle the song title
#'
#' @return a markdown file with the song name in each slide title
#' @export
#'
rename_titles <- function(thetext, thetitle) {

  thetitle <- str_trim(thetitle)

  with_title <- str_replace_all(thetext, "##\\s*(.*)", paste0("## ", thetitle, " \\1") )

  i_are_titles <- str_which(with_title, "##")
  i_last_title <- max(i_are_titles)


  with_title[i_last_title] <- str_replace(with_title[i_last_title], "(.*)(<br>)*", "\\1 (Last)\\2")

  with_title

}



#' Add line breaks to each line
#'
#' @param thetext a character vector of a markdown file, one line per entry.
#'
#' @return a character vector with end of line punctuation stripped away and <br> at the end of each line
#' @export
#'
add_br_to_lines  <- function(thetext) {

  i_are_lines <- !str_detect(thetext, "##")

  thetext[i_are_lines] <- thetext[i_are_lines] %>%
    str_trim() %>%
    str_remove('[:punct:]+$') %>%
    str_replace_all("(.)$","\\1<br>")

  thetext

}




#' Read Song from Markdown
#'
#' @description use in a set's markdown file to include a song in that set
#'
#' @param songfile_path the path to a markdown file for a song
#' @param thetitle  optional title of the song. If NULL, song title is parsed from file name
#' @param output_type if "character" then the character vector is returned. If anything else, results are passed to writeLines()
#'
#' @return see output_type
#' @export
#'
read_song <- function(songfile_path, thetitle = NULL, output_type = 'raw') {

  if(is.null(thetitle)) {

    thetitle <- str_remove(songfile_path, ".*\\/") %>%
      str_remove(".mkd") %>%
      str_replace_all("_|-", " ")

  }


  results <- read_lines(songfile_path) %>%
    rename_titles(thetitle = thetitle) %>%
    add_br_to_lines() %>%
    paste(collapse = "\n")


  results <- c(results, "\n#### Last\n\n## End of Song\n \n")


  if(output_type == 'character') {
    return(results)
  } else {
    writeLines(results)
  }


}




#' Make Slides
#'
#' @param ... any number of paths to songs
#' @param title the title for the entire presentation
#' @param header a YAML header file, should specify slidify presentation. Use QQQ in the YAML if you want to use the css parameter to specify a CSS file
#' @param footer a footer file
#' @param css a CSS file to be applied to the knitted document
#'
#' @return
#' @export
#'
#' @examples
make_slides  <- function(...
  , title = 'Slides'
  , header = system.file('markdown', 'set-template-header.rmd', package = 'setbuildr')
  , footer = system.file('markdown', 'set-template-footer.rmd', package = 'setbuildr')
  , css    = system.file('css'     , 'style.css'              , package = 'setbuildr')
  ) {

  dots  <- list(...)

  thesongs  <- map(dots, read_song, output_type = 'character') %>%
    unlist() %>%
    paste(collapse = "\n")

  theheader  <- paste(read_lines(header), collapse = '\n') %>%
    str_replace("QQQ", css)


  paste(
      theheader
    , thesongs
    , paste(read_lines(footer), collapse = '\n')
    , collapse = "\n"
  ) %>%
  str_replace('(\\n)\\s+##', '\\1##') # remove any extra spaces between line breaks and slide titles

}




