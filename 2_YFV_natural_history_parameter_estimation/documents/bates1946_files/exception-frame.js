(function() {
  define(["./dom"], function(dom) {
    var clear, create, write;
    write = function(container, content) {
      var iframe, iframeDocument;
      iframe = (container.findFirst("iframe")).element;
      iframeDocument = iframe.contentWindow || iframe.contentDocument;
      if (iframeDocument.document) {
        iframeDocument = iframeDocument.document;
      }
      iframeDocument.open();
      iframeDocument.write(content);
      return iframeDocument.close();
    };
    clear = function() {
      var container;
      container = this.closest('.exception-container');
      container.remove();
      return false;
    };
    create = function() {
      var container;
      container = dom.create({
        "class": "exception-container"
      }, "<iframe> </iframe>\n<div>\n  <button class=\"pull-right btn btn-primary\">\n    <i class=\"icon-remove icon-white\"></i>\n    Close\n  </button>\n</div>");
      dom.body.append(container.hide());
      container.on("click", "button", clear);
      return container;
    };
    return function(exceptionContent) {
      var container;
      container = create();
      write(container, exceptionContent);
      container.show();
    };
  });

}).call(this);
