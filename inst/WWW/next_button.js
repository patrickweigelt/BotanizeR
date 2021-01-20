$(document).keyup(function(event) {
  if ($("#sp_answer").is(":focus") && (event.key == "ArrowUp")) {
    $("#newplant").click();
  }
});