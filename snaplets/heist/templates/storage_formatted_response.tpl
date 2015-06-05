<!DOCTYPE html>
<html lang="en">
 <head>
     <link rel="stylesheet" href="//aui-cdn.atlassian.com/aui-adg/5.4.3/css/aui.css" media="all">
     <script src="//localhost:8080/confluence/atlassian-connect/all.js" type="text/javascript"></script>
 </head>
 <body>
     <section id="content" class="ac-content">
         <div class="aui-page-header">
             <div class="aui-page-header-main">
                 <h1> Creating page... </h1>
             </div>
         </div>
     </section>
 </body>
 <div style="display: none" id="storage-format-response">
   <storageFormatResponse/>
 </div>
 <div style="display: none" id="new-page-title"><newPageTitle/></div>
 <script>
     var storageFormat = document.getElementById("storage-format-response").innerHTML;
     // TODO get a decoded string not HTML for the title
     var newPageTitle = document.getElementById("new-page-title").textContent;
    // Display an alert box with a list of JIRA dashboards using the JIRA REST API.
    /*AP.require('request', function(request){
      request({
        url: '/rest/api/content',
        type: 'POST',
        data: {
            "type":"page",
            "title":newPageTitle,
            "space":{"key":"TST"},
            "body":{"storage":{"value": storageFormat,"representation":"storage"}}
        },
        success: function(responseText){
          alert(responseText);
        }
      });
    }); */
/*   AP.request({
         url: "/rest/api/content/1",
         success: function(response) {
           console.log(response);
         }
   }); */
    AP.require('request', function(request){
      request({
        url: '/rest/api/content/1',
        success: function(responseText){
          alert(responseText);
        }
      });
    }); 
 </script>
</html>
