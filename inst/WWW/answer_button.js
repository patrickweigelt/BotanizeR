$(document).keyup(function(event) {
  if ($("#sp_answer").is(":focus") && (event.key == "ArrowDown")) {
    $("#real_answer").click();
  }
});