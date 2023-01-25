$( document ).ready(function() {
  
  shinyjs.backgroundCol = function(params) {
        var defaultParams = {
          id : null,
          col : "red"
        };
        params = shinyjs.getParams(params, defaultParams);
        
        var el = $("#" + params.id);
        el.css("background-color", params.col);
  }
  
  shinyjs.addLoader = function() {
    document.getElementById('speciesImage').innerHTML = `'<img id="loader" src="www/Curve-Loading.gif"></img>'`
  }
  
  shinyjs.toggleElement = function(params) {
        var defaultParams = {
          id : null,
          state : "none"
        };
        params = shinyjs.getParams(params, defaultParams);
    
    $("#" + params.id).css("display", params.state)
  }
  
  shinyjs.canUpdateDifficulty = function(params) {
        canUpdateDifficulty = true
  }
  
  shinyjs.showSpecies= function(params) {
    var defaultParams = {
      html : null
    };
    params = shinyjs.getParams(params, defaultParams);
    
    console.log(params.html)
    
    document.getElementById('speciesImage').innerHTML = params.html
  }

})