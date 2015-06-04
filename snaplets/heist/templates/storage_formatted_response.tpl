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
                 <h1> Page sucessfully created </h1>
             </div>
         </div>
     </section>
 </body>
 <div style="display: none" id="storage-format-response">
   <storageFormatResponse/>
 </div>
 <script>
     var storageFormat = document.getElementById("storage-format-response").innerHTML;
    // Display an alert box with a list of JIRA dashboards using the JIRA REST API.
    AP.require('request', function(request){
      request({
        url: 'rest/api/content',
        type: 'POST',
        data: {
            "type":"page",
            "title":"page title",
            "space":{"key":"TST"},
            "body":{"storage":{"value": storageFormat,"representation":"storage"}}
        },
        success: function(responseText){
          alert(responseText);
        }
      });
    }); 
 </script>
</html>
