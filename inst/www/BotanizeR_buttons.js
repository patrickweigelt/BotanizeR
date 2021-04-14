$(document).keyup(function(event) {
  if ($("#sp_answer").is(":focus") && (event.key == "Enter")) {
    $("#submit").click();
  }
  
  if ($("#sp_answer").is(":focus") && (event.key == "ArrowUp")) {
    $("#newplant").click();
  }
  
  if ($("#sp_answer").is(":focus") && (event.key == "ArrowDown")) {
    $("#real_answer").click();
  }

});