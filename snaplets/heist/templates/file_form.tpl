<!DOCTYPE html>
<html lang="en">
 <head>
     <link rel="stylesheet" href="//aui-cdn.atlassian.com/aui-adg/5.4.3/css/aui.css" media="all">
     <script src="${productBaseUrl}/atlassian-connect/all.js" type="text/javascript"></script>
 </head>
 <body>
     <section id="content" class="ac-content">
         <div class="aui-page-header">
             <div class="aui-page-header-main">
             </div>
         </div>
        <form id="file-form" class="aui" enctype="multipart/form-data" action="/create" method="post">
            <input type="hidden" name="page-token" value="${connectPageToken}">
            <fieldset>
               <legend><span>File upload</span></legend>
                <div class="field-group">
                    <label for="file-upload">Upload file</label>
                    <input class="upfile" type="file" id="file-upload" name="file-upload">
                </div>
            </fieldset>
            <div class="field-group">
                <label for="title-input">Page Title</label>
                <input class="text" type="text"
                    id="page-title" name="page-title" placeholder="">
            </div>
            <div class="field-group">
                <label for="title-input">Space</label>
                <select class="select" id="space-key" name="space-key">
                </select>
            </div>
        </form>
     </section>
     <script type="text/javascript">
       (function() {
         var upload = document.getElementById('file-upload'),
             title = document.getElementById('page-title');

         upload.onchange = function() {
           var path = upload.value,
               slash = path.lastIndexOf('\\') + 1,
               dot = path.lastIndexOf('.');

           if(!title.value)
             title.value = path.slice(slash, dot < slash ? path.length : dot);
         };

         AP.request({
           url: '/rest/api/space',
           success: function(responseText) {
             var results = JSON.parse(responseText).results,
                 select = document.getElementById('space-key'),
                 option,
                 i;

             for(i = 0; i < results.length; i++) {
               option = document.createElement('option');
               option.value = results[i].key;
               option.text = results[i].name;
               select.add(option);
             }
           }
         });

         AP.Dialog.getButton('submit').bind(function() {
           document.getElementById('file-form').submit();
         });
       })();
     </script>
 </body>
</html>
