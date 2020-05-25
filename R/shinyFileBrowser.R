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




#' Shiny bindings for shinyFileBrowser
#'
#' Output and render functions for using shinyFileBrowser within Shiny
#' applications and interactive Rmd documents.
#'
#' @param rootDirServer output folder to read from
#' @param rootDirHtml output html of folder to read from
#'
#' @family shinyFiles
#'
#' @importFrom shiny observe invalidateLater req observeEvent
#' @importFrom fs path
#'
#' @export
shinyFileBrowserExample<-function(      rootDirServer = "~/",
                                        rootDirHtml= "/"){


  ui <- bootstrapPage(
    # example use of the automatically generated output function
    useShinyjs(),
    div(id="DropZoneSuccessSelector")
  )
  server <- function(input, output, session) {

    #shinyFileChoose(input, 'files', root=c(root=dir2scan), filetypes=c('', 'txt', 'r', '*'))


    #
    #     input[['myShinyFileBrowser']] <- rendershinyFileBrowser({
    #       shinyFileBrowser(input=input,  "filePanel", dir2scan = dir2scan, updateFreq=0)
    #     })

    observe({


      insertUI(
        selector = "#DropZoneSuccessSelector",
        where = "beforeEnd",
        ui = div(
          shinyFileBrowserOutput("shinyFileBrowserDiv", height = NULL)  ,
          fileInput("fileInputSFB", label = NULL, buttonLabel = "Carica", placeholder = NULL, multiple = T)
        )
      )

      output$shinyFileBrowserDiv<-rendershinyFileBrowser({
        shinyFileBrowser(input=input,  "fileInputSFB",
                         rootDirServer = rootDirServer,
                         rootDirHtml=rootDirHtml ,
                         updateFreq=0 )
      })

      observeEvent(input$shinyFileBrowserFileDeleted, {

        req(input$shinyFileBrowserFileDeleted)
        print(input$shinyFileBrowserFileDeleted)
        file2rem<-paste0(rootDirServer, input$shinyFileBrowserFileDeleted)
        if(!file.exists(file2rem)){
          warning( sprintf("%s is Not a file!", input$shinyFileBrowserFileDeleted ))
          return(NULL)
        }
        if(is_dir(file2rem)){
          warning( sprintf("%s is  a directory!", input$shinyFileBrowserFileDeleted ))
          return(NULL)
        }

        file.remove(file2rem)
        dd<-shinyFileBrowser::fileGetter(rootDirServer)
        session$sendCustomMessage("shinyFileBrowserFileAdded", list(dirContents=dd))

      })

      observeEvent(input$fileInputSFB, {
        file.copy(input$fileInputSFB$datapath, paste0(rootDirServer, input$fileInputSFB$name) )
        dd<-shinyFileBrowser::fileGetter(rootDirServer)
        session$sendCustomMessage("shinyFileBrowserFileAdded", list(dirContents=dd))
      })

    })




    session$onSessionEnded(function() {

      stopApp()
    })
  }


  shinyApp(ui = ui, server = server)


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



#' @examples
#' \dontrun{
#' # File selections
#' ui <- shinyUI(bootstrapPage(
#'   shinyFilesButton('files', 'File select', 'Please select a file', FALSE)
#' ))
#' server <- shinyServer(function(input, output) {
#'   shinyFileChoose(input, 'files', roots=c(wd='.'), filetypes=c('', 'txt'),
#'                   defaultPath='', defaultRoot='wd')
#' })
#'
#' runApp(list(
#'   ui=ui,
#'   server=server
#' ))
#' }
#'
#'
#' @family shinyFiles
#'
#' @importFrom shiny observe invalidateLater req observeEvent
#' @importFrom fs path
#' @export
shinyFileBrowser <- function(input, id, session = getSession(),  message="",
                             width = NULL, height = NULL,
                             elementId = paste0("shinyFileBrowser", as.character( as.integer(Sys.time()) ) ),
                             rootDirServer=NULL, rootDirHtml="/", updateFreq=0, ...) {

  if(is.null(rootDirServer) || !dir.exists(rootDirServer) ){
    stop("Devi specificare un path esistente nel server....")
  }
  currentDirContents <- shinyFileBrowser::fileGetter(rootDirServer)
  clientId <- elementId

  x = list(
    dirContents = currentDirContents,
    rootDirHtml = ifelse( substr(rootDirHtml, nchar(rootDirHtml) , nchar(rootDirHtml) )=="/",
                          rootDirHtml, sprintf("%s/",rootDirHtml ) )
  )


 htmlwidgets::createWidget(
    name = 'shinyFileBrowser',
    x,
    width = width,
    height = height,
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

