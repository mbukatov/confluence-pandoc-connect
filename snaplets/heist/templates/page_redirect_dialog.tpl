<!doctype html>
<html>
 <head>
   <!-- Bit indirect, since Snap can't just interpolate fragments into script bodies -->
   <script id="redirect-script" data-destination="${destination}">
     parent.location = document.getElementById('redirect-script').dataset.destination;
   </script>
 </head>
 <body>
 </body>
</html>
