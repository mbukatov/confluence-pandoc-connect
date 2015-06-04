<!DOCTYPE html>
<html lang="en">
 <head>
     <link rel="stylesheet" href="//aui-cdn.atlassian.com/aui-adg/5.4.3/css/aui.css" media="all">
     <script src="//localhost:2990/jira/atlassian-connect/all.js" type="text/javascript"></script>
 </head>
 <body>
     <section id="content" class="ac-content">
         <div class="aui-page-header">
             <div class="aui-page-header-main">
             </div>
         </div>
        <form class="aui" enctype="multipart/form-data" action="/upload/image" method="post">
            <div class="field-group">
                <label for="title-input">Page Title</label>
                <input class="text medium-field" type="text"
                    id="page-title" name="page-title" placeholder="">
            </div>
           <fieldset>
               <legend><span>File upload</span></legend>
                <div class="field-group">
                    <label for="file-upload">Upload file</label>
                    <input class="upfile" type="file" id="file-upload" name="file-upload">
                </div>
            </fieldset>
            <div class="buttons-container">
                <div class="buttons">
                    <input class="button submit" type="submit" value="Import" id="submit-button">
                    <!--<a class="cancel" href="#">Cancel</a>-->
                </div>
            </div>
        </form>
     </section>
 </body>
</html>
