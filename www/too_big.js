var tooBig = new Shiny.InputBinding();
$.extend(tooBig, {
  listenBig: function() {
  if($(".bar-danger")[0]) {
    alert("HERE");
    document.getElementById("upload_file_progress").childNodes[1].innerHTML = "HELLO";
    }
  }
});

Shiny.inputBindings.register(tooBig);
