$(document).on('shiny:sessioninitialized', function(event) {
  document.getElementById('speciesImage').insertAdjacentHTML(
    'afterbegin',
    `'<img id="loader" src="Curve-Loading.gif"></img>'`      
  ) 
  
  window.addEventListener('keypress',difficultyInput)
  
  document.getElementById("defaultOpen").click();
})

difficultyInput = function(event) {
  console.log(event)
  if ([49,50,51,52].some(function(code) {return code == event.which}) && canUpdateDifficulty) {
    canUpdateDifficulty = false
    console.log(event.which)
    Shiny.setInputValue("difficulty",String(event.which),{priority: "event"})
  }
}
  
  
rotateImg = function(){
   var img = document.getElementById('speciesImg'); 
   console.log(img)
   var width = img.clientWidth
   var height = img.clientHeight;
   // var trans = "translateX(" + String(width/2 - height) + "px) translateY(" + String(height/2) + "px) rotate(90deg)"
   // console.log(trans)
   if (width/height > 1) {
     document.getElementById('speciesImg').className = 'wideImage';
     // document.getElementById('speciesImg').style.transform = trans;
   }
}


openTab = function(evt, tabName) {
  // Declare all variables
  var i, tabcontent, tablinks;

  // Get all elements with class="tabcontent" and hide them
  tabcontent = document.getElementsByClassName("tabcontent");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].className = tabcontent[i].className.replace(/(?:^|\s)activeTab(?!\S)/g, ' inactiveTab')
  }

  // Get all elements with class="tablinks" and remove the class "active"
  tablinks = document.getElementsByClassName("tablinks");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace("active", "");
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  document.getElementById(tabName).className = document.getElementById(tabName).className.replace(/(?:^|\s)inactiveTab(?!\S)/g, ' activeTab')
  
  evt.currentTarget.className += " active";
}