$(document).ready(function() {
  $("header").find("nav").append('<span class="header-title"> <i>discove</i>R </span>');
});

shinyjs.init = function() {
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

  $("ul#BoxNormal li")[2].remove();
  $("ul#BoxDisp li")[1].remove();
  $("ul#tabDyA li")[2].remove();
  $("ul#tabCor li")[2].remove();
  $("ul#tabPCA li")[10].remove();
  $("ul#tabjerar li")[8].remove();
  $("ul#tabkmedias li")[8].remove();
};

Shiny.addCustomMessageHandler("updateLabel",
  function(message) { 
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]");
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
    }
  }
);


//ShinyWidgets
var exports4but = window.Shiny = window.Shiny || {};
var $escape4but = exports4but.$escape = function(val) {
  return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
};

// radioGroupButtons input binding
var radioGroupButtonsBinding = new Shiny.InputBinding();
$.extend(radioGroupButtonsBinding, {
  find: function(scope) {
    return $(scope).find('.radioGroupButtons');
  },
  getId: function(el) {
    return el.id;
  },
  getValue: function(el) {
    return $('input:radio[name="' + $escape4but(el.id) + '"]:checked').val();
  },
  setValue: function(el, value) {
    $('input:radio[name="' + $escape4but(el.id) + '"][value="' + $escape4but(value) + '"]').prop('checked', true);
    $('input:radio[name="' + $escape4but(el.id) + '"]').parent().removeClass('active');
    $('input:radio[name="' + $escape4but(el.id) + '"][value="' + $escape4but(value) + '"]').parent().addClass('active');
  },
  subscribe: function(el, callback) {
    $(el).on('change.radioGroupButtonsBinding', function (event) {
        callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.radioGroupButtonsBinding');
  },
  getState: function getState(el) {
      var $objs = $('input:radio[name="' + $escape4but(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
        label: this._getLabel($objs[i]) };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape4but(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
    };
  },
  receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        $el.find('div.btn-group-container-sw').empty();
        $el.find('div.btn-group-container-sw').append(data.options);
      }

      if (data.hasOwnProperty('selected'))
        this.setValue(el, data.selected);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape4but(el.id) + '"]').text(data.label);

      $(el).trigger('change');
  }
});

Shiny.inputBindings.register(radioGroupButtonsBinding, 'shiny.radioGroupButtonsInput');