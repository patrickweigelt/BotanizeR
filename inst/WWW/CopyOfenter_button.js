$(document).keyup(function(event) {
  if ($("#sp_answer").is(":focus") && (event.key == "Enter")) {
    $("#submit").click();
  }
});