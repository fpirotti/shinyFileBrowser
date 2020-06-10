HTMLWidgets.widget({

  name: 'shinyFileBrowser',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance
    var initialized = false;
    var rootDirHtml="/";
    var thisWindow = el;
    var ellipsized = 0;


    if (HTMLWidgets.shinyMode) {
        Shiny.addCustomMessageHandler( 'shinyFileBrowserFileAdded' , function(data) {
        var el = HTMLWidgets.find('#'+data.elementId+'.shinyFileBrowser');
        if (el) {
          el.populateFileChooser( data, true);
        } else {
          console.log(data);
          console.log(el);
          alert('Problema contattare lo sviluppatore');
        }

        });
    }
    return {

      renderValue: function(data) {

        ellipsized=data.ellipsized;
        rootDirHtml=data.rootDirHtml;
        $(el).children().remove();

        $(el).append(
          $('<div>').addClass('sF-fileWindow').append(
              $('<div>').addClass('sF-fileList').addClass('sF-detail')
                        .append( this.populateFileChooser(data, true))
            )
          );
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      },

      formatSize: function(bytes, si) {
        var thresh = si ? 1000 : 1024;
        if(bytes < thresh) return bytes + ' B';
        var units = si ? ['kB','MB','GB','TB','PB','EB','ZB','YB'] : ['KiB','MiB','GiB','TiB','PiB','EiB','ZiB','YiB'];
        var u = -1;
        do {
          bytes /= thresh;
          ++u;
        } while(bytes >= thresh);
        return bytes.toFixed(1)+' '+units[u];
      },

      formatDate: function(date) {
          var monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
          var dayNames = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

          if (typeof Intl == 'undefined') {
            return dayNames[date.getDay()]+' '+monthNames[date.getMonth()]+' '+date.getDate()+' '+date.getFullYear()+' '+date.getHours()+':'+("0" + date.getMinutes()).substr(-2);
          } else {
            return date.toLocaleString([], {weekday:'long', year:'numeric', month:'short', day:'numeric', hour:'2-digit', minute:'2-digit'});
          }
        },

      parseFiles: function(data) {
        var parsedFiles = {};
        data.files.filename.forEach(function(d, i) {
          try{
            parsedFiles[d] = {
              name: d,
              extension: data.files.extension[i],
              isDir: data.files.isdir[i],
              size: data.files.size[i],
              mTime: new Date(data.files.mtime[i]),
              cTime: new Date(data.files.ctime[i]),
              aTime: new Date(data.files.atime[i])
            };
          } catch(err) {
            //This can happen if there is a broken link, for example
          }
        });

        return {
          files: parsedFiles,
          location: data.breadcrumps,
          writable: data.writable,
          rootNames: data.roots,
          selectedRoot: data.root
        };
      },

      returnHeader: function(){
        return $('<div>').addClass('sF-file-header').append(
                $('<div>').append(
                  $('<div>').addClass('sF-file-icon')
                ).append(
                  $('<div>', {text: 'Nome'}).addClass('sF-file-name')
                ).append(
                  $('<div>', {text: 'Dimen.'}).addClass('sF-file-size')
                ).append(
                  $('<div>', {text: 'Modificato'}).addClass('sF-file-mTime')
                ).append(
                  $('<div>', {text: ''}).addClass('sF-file-cTime')
                 )
                // .append(  $('<div>', {text: ''}).addClass('sF-file-aTime')  )
              );

      },

      returnFileElements: function(dFile, linkroot, markerid ){
            var delornot;
            if( !dFile.isDir ) delornot =$('<div>' ).addClass('sF-filetype-deleteFile').attr('onclick','Shiny.onInputChange("shinyFileBrowserFileDeleted", { markerid:"'+ markerid +'",  dirName:"'+ linkroot +'", fileName:"'+dFile.name+'"})');
            else $('<div>' );

            var ellipName=dFile.name;
            if(ellipsized!==0 && dFile.name.length < ellipsized) ellipName = ellipName.substring(ellipName,0,ellipsized)+"...";

            return  $('<div>').toggleClass('sF-file', !dFile.isDir).toggleClass('sF-directory', dFile.isDir).append(
                    $('<div>').addClass('sF-file-icon').addClass('sF-filetype-'+dFile.extension)
                  ).append(
                    $('<div>').addClass('sF-file-name').append(
                      $('<a>',  {text: ellipName }).attr("title",dFile.name).attr("target","_blank").attr("href", rootDirHtml+dFile.name)
                    )
                  ).append(
                    $('<div>', {text: dFile.isDir ? '' : this.formatSize(dFile.size, true)}).addClass('sF-file-size')
                  ).append(
                    $('<div>', {text: this.formatDate(dFile.mTime)}).addClass('sF-file-mTime')
                  ).append( delornot  )
                    //.append('<a data-toggle="tooltip" title="<img src=http://getbootstrap.com/apple-touch-icon.png>" >prova  </a>')
                    .data('sF-file', dFile);
                  //.append(
                  //  $('<div>', {text: this.formatDate(dFile.cTime)}).addClass('sF-file-cTime')
                  // ).append(
                   // $('<div>', {text: this.formatDate(dFile.aTime)}).addClass('sF-file-aTime')
                   // )
      },

      populateFileChooser: function(data, forceUpdate) {
          var rdata = data;
          var parsedFiles = this.parseFiles(data.dirContents);
          var modal = $(el); //.data('modal');


          modal.data('dataCache', parsedFiles);
         // modal.data('dataCache', data);
          var currentData = modal.data('currentData');

          var newFiles = {};
          if (currentData) {
            for ( i in parsedFiles.files) {
              if (!currentData.files[i]) newFiles[i] = parsedFiles.files[i];
            }
          }
          var oldFiles = {};
          if (currentData) {
            for ( i in currentData.files) {
              if (!parsedFiles.files[i]) oldFiles[i] = currentData.files[i];
            }
          }

          if (forceUpdate) {
            //modal.find('.sF-fileList').append(
            var header = this.returnHeader();
            //);
            var elements = [];
            elements.push(header);
            for (var i in parsedFiles.files) {
              var dFile = parsedFiles.files[i];
              var element = this.returnFileElements(dFile,  rdata.dirContents.root,  rdata.markerid  );
              elements.push(element);
            }

            if( modal.find('.sF-fileList') ){
              modal.find('.sF-fileList').children().remove();
              modal.find('.sF-fileList').append(  elements  );
            }
            return elements;
          } else {

            if (Object.keys(oldFiles).length === 0) {
              modal.find('.sF-fileList').children().filter(function() {
                return oldFiles[$(this).find('.sF-file-name div').text()];
              }).remove();
            }

            if (Object.keys(newFiles).length === 0) {
              for (var i in newFiles) {
                var dFile = newFiles[i];
                modal.find('.sF-fileList').append(
                  this.returnFileElements(dFile, rdata.dirContents.root,  rdata.dirContents.markerid )
                );
              }
            }
          }



          modal.data('currentData', parsedFiles);
          $(modal).trigger('change');
        }
    };
  }
});





