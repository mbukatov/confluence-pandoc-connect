<!DOCTYPE html>
<html lang="en">
<head>
    <link rel="stylesheet" href="//aui-cdn.atlassian.com/aui-adg/5.4.3/css/aui.css" media="all">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"></script>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"></script>
    <script src="http://cdnjs.cloudflare.com/ajax/libs/sinon.js/1.15.4/sinon.js"></script>
    <script src="//aui-cdn.atlassian.com/aui-adg/6.0.3/js/aui.js"></script>
    <script src="//aui-cdn.atlassian.com/aui-adg/6.0.3/js/aui-experimental.js"></script>
    <script src="//aui-cdn.atlassian.com/aui-adg/6.0.3/js/aui-datepicker.js"></script>
    <link rel="stylesheet" type="text/css" href="//aui-cdn.atlassian.com/aui-adg/6.0.3/css/aui.css"/>
    <link rel="stylesheet" type="text/css" href="//aui-cdn.atlassian.com/aui-adg/6.0.3/css/aui-experimental.css"/>
    <script src="${productBaseUrl}/atlassian-connect/all.js" type="text/javascript"></script>
</head>
<body>
<section id="content" class="ac-content">
    <form id="file-form" class="aui" enctype="multipart/form-data" action="/create" method="post">
        <input type="hidden" name="page-token" value="${connectPageToken}">
        <input type="hidden" id="current-space-key" value="${spaceKey}">
        <input type="hidden" id="current-page-id" value="${contentId}">

        <div class="field-group">
            <fieldset class="group" style="padding: 4px 0 4px 15px;">

                <div class="radio">
                    <label for="page-selector-child">as a child of the current page (<span id="currentPageName"></span>)</label>
                    <input class="radio" type="radio" checked="checked"
                           name="page-selectors" id="page-selector-child" value="${contentId}" onclick="checkRadio()">
                </div>

                <div class="radio">
                    <label for="page-selector-root">at the root of the space: </label>
                    <input class="radio" type="radio"
                           name="page-selectors" id="page-selector-root" value="root" onclick="checkRadio()">
                </div>
                <br>
                <select class="select" id="space-key" name="space-key" onchange="spaceKeyChanged();"></select>
            </fieldset>
        </div>
        <br>
        <br>
        <fieldset>
            <legend><span>File upload</span></legend>
            <div class="field-group">
                <label for="file-upload">Upload directory</label>
                <input class="upfile" type="file" webkitdirectory mozdirectory id="file-upload"
                       name="file-upload">
            </div>
            <div class="field-group">
                <label for="file-upload1">Upload file(s)</label>
                <input class="upfile" type="file" multiple id="file-upload1"
                       name="file-upload1">
            </div>
            <div class="description">The file to be converted.</div>
        </fieldset>
    </form>
</section>
<section id="content1" style="display: none; padding: 20px;">
    Progress
    <div id="progress" class="aui-progress-indicator">
        <span class="aui-progress-indicator-value"></span>
    </div>
    <div style="width: 100%; text-align: right">
        <span id="files-done">0</span> / <span id="total-files">0</span>
    </div>
    <div id="completed-files">
        <ul id="completed-files-list">
        </ul>
    </div>
</section>
<script type="text/javascript">
    var currentSpaceKey = document.getElementById('current-space-key').value,
            select = document.getElementById('space-key'),
            radioPageSelectorChild = document.getElementById('page-selector-child'),
            radioPageSelectorRoot = document.getElementById('page-selector-root'),
            userSelectedPageUnderRoot = null,
            totalFiles = 0,
            filesDone = 0;
    select.values = currentSpaceKey;

    (function() {
        var upload = document.getElementById('file-upload'),
                currentContentId = document.getElementById('current-page-id').value;

        upload.onchange = function() {
            var path = upload.value,
                    slash = path.lastIndexOf('\\') + 1,
                    dot = path.lastIndexOf('.');
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
//               AJS.$("#space-key").auiSelect2({tags:select});
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

        AP.Dialog.getButton('submit').bind(submit);
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
         }
       }

       if (radioPageSelectorChild.checked) {
           select.disabled = true;
       }

       function checkRadio() {
           if (radioPageSelectorChild.checked) {
               select.disabled = true;
           } else {
               select.disabled = false;
           }
       }
    function submit() {
        document.getElementById('content').style.display = 'none';
        document.getElementById('content1').style.display = 'block';
        select.disabled = false;

        var files = $.merge( $.merge( [], document.getElementById('file-upload').files ), document.getElementById('file-upload1').files );
        totalFiles = files.length;
        document.getElementById('total-files').innerHTML = totalFiles;
        for (var i = 0; i < totalFiles; i++) {

            var pagename = files[i].name.substr(0, files[i].name.lastIndexOf('.'));
            console.log(pagename);

            var oReq = new XMLHttpRequest();
            oReq.open("POST", "/create", true);
            oReq.setRequestHeader('contentType', 'multipart/form-data');
            oReq.onload = requestDone;

            var formData = new FormData(document.getElementById('file-form'));
            formData.delete('file-upload');
            formData.delete('file-upload1');
            formData.append('file-upload', files[i]);
            formData.delete('page-title');
            formData.append('page-title', pagename);

            oReq.send(formData);
        }
    }

    function requestDone() {
        var response = JSON.parse(this.responseText);
        filesDone++;
        document.getElementById('files-done').innerHTML = filesDone;
        var pagename = "jkl;";
        if (this.status === 200) {
            document.getElementById('completed-files-list').innerHTML =
                    document.getElementById('completed-files-list').innerHTML +
                    '<li><a onclick="redirect(' + "'" + response.rrUri + "'" + ')" data-destination="' +
                    response.rrUri +
                    '">' +
                    response.rrPageTitle +
                    '</a> was successfully uploaded</li>';
        } else {
            document.getElementById('completed-files-list').innerHTML =
                    document.getElementById('completed-files-list').innerHTML + '<li class="red">' +
                    pagename +
                    ' failed to be uploaded</li>';
        }
        AJS.progressBars.update("#progress", filesDone / totalFiles);
        if (filesDone === totalFiles) {
            AP.Dialog.getButton('cancel').disable();
        }
    }

    function redirect(url) {
        parent.location = url;
    }

     </script>
 </body>
</html>
