library(shinyFileBrowser)
library(shiny)
library(shinyjs)



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
      rootDirServer = "/store/ms4w/Apache/htdocs/DATI_CONDIVISI/LORAPLAN_MEDIA/UPLOADS/53677/"
      rootDirHtml= "/DATI_CONDIVISI/LORAPLAN_MEDIA/UPLOADS/53677/"

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

