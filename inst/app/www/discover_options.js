$(document).ready(function() {
  $("header").find("nav").append('<span class="header-title"> <i>discove</i>R </span>');
  
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
});
