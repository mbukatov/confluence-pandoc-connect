<!DOCTYPE html>
<html lang="en">
 <head>
     <link rel="stylesheet" href="//aui-cdn.atlassian.com/aui-adg/5.4.3/css/aui.css" media="all">
     <script src="${productBaseUrl}/atlassian-connect/all.js" type="text/javascript"></script>
 </head>
 <body>
     <section id="content" class="ac-content">
         <form id="file-form" class="aui" enctype="multipart/form-data" action="/create" method="post">
             <input type="hidden" name="page-token" value="${connectPageToken}">
             <input type="hidden" id="current-space-key" value="${spaceKey}">
             <input type="hidden" id="current-page-id" value="${contentId}">
             <fieldset>
                 <legend><span>File upload</span></legend>
                 <div class="field-group">
                     <label for="file-upload">Upload file
                         <span class="aui-icon icon-required">(required)</span></label>
                     <input class="upfile" type="file" id="file-upload" name="file-upload">
                     <div class="description">The file to be converted.</div>
                 </div>
             </fieldset>
             <div class="field-group">
                 <label for="space-key">Space</label>
                 <select class="select" id="space-key" name="space-key" onchange="spaceKeyChanged();"></select>
                 <fieldset class="group" style="padding: 4px 0 4px 15px;">
                     <div class="radio">
                         <label for="page-selector-child">as a child of the current page (<span id="currentPageName"></span>)</label>
                         <input class="radio" type="radio" checked="checked"
                                name="page-selectors" id="page-selector-child" value="${contentId}">
                     </div>
                     <div class="radio">
                         <label for="page-selector-root">at the root of the space</label>
                         <input class="radio" type="radio"
                                name="page-selectors" id="page-selector-root" value="root">
                     </div>
                 </fieldset>
             </div>
             <div class="field-group">
                 <label for="page-title">Page title
                     <span class="aui-icon icon-required">(required)</span></label>
                 <input class="text" type="text"
                        id="page-title" name="page-title" placeholder="">
             </div>
         </form>
     </section>
     <script type="text/javascript">
       var currentSpaceKey = document.getElementById('current-space-key').value,
           select = document.getElementById('space-key'),
           radioPageSelectorChild = document.getElementById('page-selector-child'),
           radioPageSelectorRoot = document.getElementById('page-selector-root'),
           userSelectedPageUnderRoot = null;

       (function() {
         var upload = document.getElementById('file-upload'),
             title = document.getElementById('page-title'),
             currentContentId = document.getElementById('current-page-id').value;

         upload.onchange = function() {
           var path = upload.value,
               slash = path.lastIndexOf('\\') + 1,
               dot = path.lastIndexOf('.');

           if(!title.value)
             title.value = path.slice(slash, dot < slash ? path.length : dot);
         };

         AP.request({
           url: '/rest/api/space?limit=2147483647',
           success: function(responseText) {
             var results = JSON.parse(responseText).results,
                 option,
                 i;

             for(i = 0; i < results.length; i++) {
               option = document.createElement('option');
               option.value = results[i].key;
               option.text = results[i].name;
               if(results[i].key == currentSpaceKey) {
                   option.selected = 'selected';
               }
               select.add(option);
             }
           }
         });

         AP.request({
           url: '/rest/api/content/' + currentContentId,
           success: function(responseText) {
             var object = JSON.parse(responseText),
                 spanPageName = document.getElementById('currentPageName');

             if(object) {
               spanPageName.appendChild(document.createTextNode(object.title));
             }
           }
         });

         AP.Dialog.getButton('submit').bind(function() {
           document.getElementById('file-form').submit();
         });
       })();

       function spaceKeyChanged() {
         var selectedValue = select.options[select.selectedIndex].value;
         if(selectedValue == currentSpaceKey) {
           radioPageSelectorChild.checked = !userSelectedPageUnderRoot;
           userSelectedPageUnderRoot = null;

           radioPageSelectorChild.disabled = false;
           radioPageSelectorRoot.disabled = false;
         } else {
           if(userSelectedPageUnderRoot == null) {
             userSelectedPageUnderRoot = radioPageSelectorRoot.checked;
           }
           radioPageSelectorRoot.checked = true;

           radioPageSelectorChild.disabled = true;
           radioPageSelectorRoot.disabled = true;
         }
       }
     </script>
 </body>
</html>
