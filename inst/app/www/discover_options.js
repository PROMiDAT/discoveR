$(document).ready(function() {
  $("header").find("nav").append('<span class="header-title"> <i>discove</i>R </span>');
  
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
  
  $("[data-widget='left']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-left");
  });
  $("[data-widget='centerleft']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-centerleft");
  });
  $("[data-widget='centeright']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-centeright");
  });
  $("[data-widget='center']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-center");
  });
  $("[data-widget='right']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-left");
    a.toggleClass("box-option-open-right");
  });
});
