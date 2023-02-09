$(document).on('shiny:sessioninitialized', function(event) {
  document.getElementById('speciesImage').innerHTML = '<img id="loader" src="www/Curve-Loading.gif"></img>'
  
  window.addEventListener('keypress',difficultyInput)
  
  document.getElementById("defaultOpen").click();
})


$( document ).ready(function() {
  difficultyInput = function(event) {
    if ([49,50,51,52].some(function(code) {return code == event.which}) && canUpdateDifficulty) {
      canUpdateDifficulty = false
      Shiny.setInputValue("difficulty",String(event.which),{priority: "event"})
    }
  }
    
    
  rotateImg = function(){
     var img = document.getElementById('speciesImage'); 
     var width = img.clientWidth
     var height = img.clientHeight;

     if (width/height > 1) {
       img.className = 'wideImage';
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

})