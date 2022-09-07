shinyjs.backgroundCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);
      
      console.log("TEST??")

      var el = $("#" + params.id);
      el.css("background-color", params.col);
}

shinyjs.addLoader = function() {
  document.getElementById('speciesImage').innerHTML = `'<img id="loader" src="Curve-Loading.gif"></img>'`
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