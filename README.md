# shinyFiles <img src="man/figures/logo.png" align="right" />

 
This package extends the functionality of shiny by providing read-only access to the server file system. 

The package can be installed from CRAN using `install.packages('shinyFileBrowser')`.

Usage
----------
The package is designed to make it extremely easy to share files. An example of implementing a file chooser would be:

In the ui.R file
```R
shinyUI(bootstrapPage(
    shinyFileBrowser('files', label='File select')
))
```
In the server.R file
```R
shinyServer(function(input, output) {
    shinyFileBrowser(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))
})
```

It is equally simple to implement directly in your custom html file as it only requires a single `<button>` element. The equivalent of the above in raw html would be:
```html
<button id="files" type="button" class="shinyFiles btn" data-title="Please select a file" data-selecttype="single">
    File select
</button>
```

For an overview of all the different modules try the `shinyFileBrowserExample(rootDirServer = "~/", rootDirHtml= "/")` function in the package. It will open a panel with files inside your "rootDirServer" directory and link them using the "rootDirHtml" path.

Credits
----------
* A lot of code was taken from Thomas Pedersen - thomasp85 (https://github.com/thomasp85/shinyFiles)
* The file icons used in the file system navigator are taken from FatCows Farm-Fresh Web Icons (http://www.fatcow.com/free-icons)
* RStudio is a trademark of RStudio, Inc. File icons used by permission of RStudio, Inc. 
