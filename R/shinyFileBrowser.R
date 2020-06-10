getSession <- function() {
  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) {
    stop(paste(
      "could not find the Shiny session object. This usually happens when a",
      "shinyjs function is called from a context that wasn't set up by a Shiny session."
    ))
  }

  session
}



#' Create a function that returns fileinfo according to the NOT given restrictions
#'
#'
#' @param dir2scan A logical value specifying whether hidden files should be
#' returned or not
#'
#' @return A function taking a single path relative to the specified root, and
#' returns a list of files to be passed on to shiny
#'
#' @importFrom tools file_ext
#' @importFrom fs path file_access file_exists dir_ls file_info path_file path_ext
#' @importFrom tibble as_tibble
#'
#' @export
fileGetter<-function(dir2scan=getwd()){
  files <- suppressWarnings(dir_ls(dir2scan, all = F, fail = FALSE))

  breadcrumps <- strsplit(dir2scan, .Platform$file.sep)[[1]]

  fileInfo <- suppressWarnings(file_info(files, fail = FALSE))
  fileInfo$filename <- path_file(files)
  fileInfo$extension <- tolower(path_ext(files))
  fileInfo$isdir <- dir.exists(files)
  fileInfo$mtime <- as.integer(fileInfo$modification_time) * 1000
  fileInfo$ctime <- as.integer(fileInfo$birth_time) * 1000
  fileInfo$atime <- as.integer(fileInfo$access_time) * 1000

  list(
    files = as_tibble(fileInfo[, c("filename", "extension", "isdir", "size", "mtime", "ctime", "atime")]),
    writable = T,
    exist = as.logical(file_exists(dir2scan)),
    breadcrumps = I(c("", breadcrumps[breadcrumps != ""])),
    root = dir2scan
  )
}



#' Create a function that returns a tiny file browser with upload capabilities
#'
#'
#' @family shinyFileBrowser
#'
#' @param input A
#'
#' @param input Input element from shiny
#'
#' @param elementId Custom elementId
#'
#' @param session The session object of the shinyServer call (usually
#' `session`).
#'
#' @param width Width of the widget
#'
#' @param height  Height of the widget
#'
#' @param rootDirServer The full path to the folder in the server.
#'
#' @param rootDirHtml The relative path to access the file online and download it.
#'
#' @param updateFreq The default relative path specified given the `defaultRoot`.
#'
#' @param fileUploader Should upload button be created - default TRUE.
#'
#' @param ellipsized Should text be shortened and title added - if 0 is false, otherwise the number of characters.
#'
#' @param ... Arguments to be passed on to [fileGetter()]
#'
#' @return A reactive observer that takes care of the server side logic of the
#' filesystem connection.
#'
#' @importFrom shiny observe invalidateLater req observeEvent fileInput fileupload
#' @importFrom fs path
#' @export
shinyFileBrowser <- function(input,
                             elementId = paste0("shinyFileBrowser_", as.character( as.integer(Sys.time()) ) ),
                             session = getSession(),
                             message="", ellipsized=0,
                             width = 300, height = NULL,
                             rootDirServer=getwd(),
                             rootDirHtml="/",
                             markerid=0, updateFreq=0, fileUploader=T, ...) {

  if(is.null(rootDirServer) || !dir.exists(rootDirServer) ){
    stop("Devi specificare un path esistente nel server....")
  }
  currentDirContents <- shinyFileBrowser::fileGetter(rootDirServer)
  currentDirContents[['markerid']]<-markerid
  x = list(
    fileUploader = fileUploader,
    dirContents = currentDirContents,
    rootDirServer=rootDirServer,
    markerid=markerid,
    ellipsized=ellipsized,
    rootDirHtml = ifelse( substr(rootDirHtml, nchar(rootDirHtml) , nchar(rootDirHtml) )=="/",
                          rootDirHtml, sprintf("%s/",rootDirHtml ) )
  )



  htmlwidgets::createWidget(
    name = 'shinyFileBrowser',
    x,
    width = width,
    height = height,
    elementId = elementId,
    package = 'shinyFileBrowser'
  )


}

#' Shiny bindings for shinyFileBrowser
#'
#' Output and render functions for using shinyFileBrowser within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a shinyFileBrowser
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name shinyFileBrowser-shiny
#'
#' @export
shinyFileBrowserOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'shinyFileBrowser', width, height, package = 'shinyFileBrowser')
}

#' @rdname shinyFileBrowser-shiny
#' @export
rendershinyFileBrowser <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, shinyFileBrowserOutput, env, quoted = TRUE)


}

