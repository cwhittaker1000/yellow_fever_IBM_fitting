(function() {
  define(["./dom", "underscore", "./bootstrap"], function(dom, _, arg) {
    var button, consoleAttribute, display, e, error, exports, filter, floatingConsole, forceFloating, glyph, level, messages, nativeConsole, noFilter, noop, updateFilter;
    glyph = arg.glyph;
    nativeConsole = null;
    try {
      nativeConsole = window.console || {};
    } catch (error) {
      e = error;
    }
    floatingConsole = null;
    messages = null;
    noFilter = function() {
      return true;
    };
    filter = noFilter;
    updateFilter = function(text) {
      var words;
      if (text === "") {
        filter = noFilter;
        return;
      }
      words = text.toLowerCase().split(/\s+/);
      filter = function(e) {
        var content, i, len, word;
        content = e.text().toLowerCase();
        for (i = 0, len = words.length; i < len; i++) {
          word = words[i];
          if (content.indexOf(word) < 0) {
            return false;
          }
        }
        return true;
      };
    };
    consoleAttribute = dom.body.attr("data-floating-console");
    forceFloating = (consoleAttribute === "enabled") || (consoleAttribute === "invisible");
    button = function(action, icon, label, disabled) {
      if (disabled == null) {
        disabled = false;
      }
      return "<button data-action=\"" + action + "\" class=\"btn btn-default btn-mini\">\n  " + (glyph(icon)) + " " + label + "\n</button>";
    };
    display = function(className, message) {
      var div;
      if (!floatingConsole) {
        floatingConsole = dom.create({
          "class": "tapestry-console"
        }, "<div class=\"message-container\"></div>\n<div class=\"row\">\n  <div class=\"btn-group btn-group-sm col-md-4\">\n    " + (button("clear", "remove", "Clear Console")) + "\n    " + (button("enable", "play", "Enable Console")) + "\n    " + (button("disable", "pause", "Disable Console")) + "\n  </div>\n  <div class=\"col-md-8\">\n    <input class=\"form-control\" size=\"40\" placeholder=\"Filter console content\">\n  </div>\n</div>");
        dom.body.prepend(floatingConsole);
        if (consoleAttribute === "invisible") {
          floatingConsole.hide();
        }
      }
      messages = floatingConsole.findFirst(".message-container");
      floatingConsole.findFirst("[data-action=enable]").attr("disabled", true);
      floatingConsole.on("click", "[data-action=clear]", function() {
        floatingConsole.hide();
        return messages.update("");
      });
      floatingConsole.on("click", "[data-action=disable]", function() {
        this.attr("disabled", true);
        floatingConsole.findFirst("[data-action=enable]").attr("disabled", false);
        messages.hide();
        return false;
      });
      floatingConsole.on("click", "[data-action=enable]", function() {
        this.attr("disabled", true);
        floatingConsole.findFirst("[data-action=disable]").attr("disabled", false);
        messages.show();
        return false;
      });
      floatingConsole.on("change keyup", "input", function() {
        var i, len, ref, visible;
        updateFilter(this.value());
        ref = messages.children();
        for (i = 0, len = ref.length; i < len; i++) {
          e = ref[i];
          visible = filter(e);
          e[visible ? "show" : "hide"]();
        }
        return false;
      });
      div = dom.create({
        "class": className
      }, _.escape(message));
      if (!filter(div)) {
        div.hide();
      }
      messages.append(div);
      return _.delay(function() {
        return messages.element.scrollTop = messages.element.scrollHeight;
      });
    };
    level = function(className, consolefn) {
      return function(message) {
        if (forceFloating || (!consolefn)) {
          display(className, message);
          if (!forceFloating) {
            return;
          }
        }
        if (window.console && (_.isFunction(consolefn))) {
          consolefn.call(window.console, message);
          return;
        }
        if (consolefn) {
          consolefn(message);
        }
      };
    };
    exports = {
      info: level("info", nativeConsole.info),
      warn: level("warn", nativeConsole.warn),
      error: level("error", nativeConsole.error),
      debugEnabled: (document.documentElement.getAttribute("data-debug-enabled")) != null
    };
    noop = function() {};
    exports.debug = exports.debugEnabled ? level("debug", nativeConsole.debug || nativeConsole.log || noop) : noop;
    window.t5console = exports;
    requirejs.onError = function(err) {
      var message, modules;
      message = "RequireJS error: " + ((err != null ? err.requireType : void 0) || 'unknown');
      if (err.message) {
        message += ": " + err.message;
      }
      if (err.requireType) {
        modules = err != null ? err.requireModules : void 0;
        if (modules && modules.length > 0) {
          message += ", modules " + (modules.join(", "));
        }
      }
      if (err.fileName) {
        message += ", " + err.fileName;
      }
      if (err.lineNumber) {
        message += ", line " + err.lineNumber;
      }
      if (err.columnNumber) {
        message += ", line " + err.columnNumber;
      }
      return exports.error(message);
    };
    return exports;
  });

}).call(this);
