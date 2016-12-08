<!DOCTYPE html>
<html lang="en">
<head>
    <style>
        #completed-files {
            height: 250px;
            width: 100%;
            overflow-y: scroll;
        }
        .red {
            color: red;
        }
        .link {
            cursor: pointer;
        }
        .ac-content {
            max-width: 400px;
            margin: 20px auto !important;
        }
        #content1 {
            display: none;
        }
        #progress-num {
            width: 100%;
            text-align: right;
        }
        .the-group {
            padding: 4px 0 4px 15px !important;
        }
        .bold {
            font-weight: bold;
        }
        .nothing {
            position: absolute;
            left: 20px;
        }
    </style>
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

        <p class="bold">Select where in Confluence to add the new Pages:</p>
            <fieldset class="group the-group">

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
        <br>
        <br>
        <fieldset>

            <p class="bold">Select the file(s) and/or directory to be uploaded:</p>
            <legend><span>File upload</span></legend>
            <div class="field-group">
                <p class="nothing">Upload file(s)</p>
                <label class="ffi is-active" data-ffi-button-text="Browse">
                    <input type="file" multiple id="file-upload1" name="file-upload1" aria-label="Upload file(s)">
                </label>
            </div>
            <div class="field-group">
                <p class="nothing">Upload directory</p>
                <label class="ffi is-active" data-ffi-button-text="Browse">
                    <input type="file" webkitdirectory mozdirectory id="file-upload" name="file-upload" aria-label="Upload directory">
                </label>
            </div>
        </fieldset>
    </form>
</section>
<section id="content1" class="ac-content">
    Progress
    <div id="progress" class="aui-progress-indicator">
        <span class="aui-progress-indicator-value"></span>
    </div>
    <div id="progress-num">
        <span id="files-done">0</span> / <span id="total-files">0</span>
    </div>
    <div id="completed-files">
        <ul id="completed-files-list">
        </ul>
    </div>
</section>
<script type="text/javascript">
    var fakePathRegex = /^.*[\\\/]/;
    var multipleFileTextRegex = /\{0\}/gi;
    FancyFileInput.prototype.change = function () {
        var files;
        var val = '';

        this.checkValidity();

        // multiple file selection
        if (this.el.files.length > 1) {
            files = this.formatMultipleFileText(this.el.files.length); // '5 files'
        } else {
            files = this.el.value; // 'README.txt'
        }

        if (files.length) {
            val = files.replace(fakePathRegex, ''); // Strips off the C:\fakepath nonsense
            this.$clearButton.appendTo(this.$label);
        } else {
            this.$clearButton.detach();
        }

        this.$el.focus();
        this.setFieldText(val);
        this.fireEvent('value-changed');
    };


    var currentSpaceKey = document.getElementById('current-space-key').value,
            select = document.getElementById('space-key'),
            radioPageSelectorChild = document.getElementById('page-selector-child'),
            radioPageSelectorRoot = document.getElementById('page-selector-root'),
            userSelectedPageUnderRoot = null,
            totalFiles = 0,
            filesDone = 0;
    select.value = currentSpaceKey;

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
           select.value = currentSpaceKey;
       }

       function checkRadio() {
           if (radioPageSelectorChild.checked) {
               select.disabled = true;
               select.value = currentSpaceKey;
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

            var pagename = files[i].webkitRelativePath.substr(0, files[i].webkitRelativePath.lastIndexOf('.'));

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
        console.log(this.responseText);
        var response = JSON.parse(this.responseText);
        filesDone++;
        document.getElementById('files-done').innerHTML = filesDone;
        if (this.status === 200) {
            document.getElementById('completed-files-list').innerHTML =
                    document.getElementById('completed-files-list').innerHTML +
                    '<li><a class="link" onclick="redirect(' + "'" + response.rrUri + "'" + ')" data-destination="' +
                    response.rrUri +
                    '">' +
                    response.rrPageTitle +
                    '</a> was successfully uploaded</li>';
        } else {
            document.getElementById('completed-files-list').innerHTML =
                    document.getElementById('completed-files-list').innerHTML + '<li class="red">' +
                    response.emPageTitle +
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
//    $(function(){
//        $('input[type=file]').fancyFileInput();
//    });


    $('input[type=file]').fancyFileInput({
        multipleFileTextPattern: "{0} files" // Shown when multiple files are chosen
    });

     </script>
 </body>
</html>
