(function() {
  define(["underscore", "./utils", "./events", "jquery"], function(_, utils, events) {
    var $, ElementWrapper, EventWrapper, RequestWrapper, ResponseWrapper, activeAjaxCount, adjustAjaxCount, ajaxRequest, convertContent, createElement, exports, fireNativeEvent, getDataAttributeAsObject, getEventUrl, onevent, parseSelectorToElements, scanner, scanners, wrapElement;
    $ = window.$;
    fireNativeEvent = function(element, eventName) {
      var event;
      if (document.createEventObject) {
        event = document.createEventObject();
        return element.fireEvent("on" + eventName, event);
      }
      event = document.createEvent("HTMLEvents");
      event.initEvent(eventName, true, true);
      element.dispatchEvent(event);
      return !event.defaultPrevented;
    };
    parseSelectorToElements = function(selector) {
      if (_.isString(selector)) {
        return $$(selector);
      }
      if (_.isArray(selector)) {
        return selector;
      }
      return [selector];
    };
    convertContent = function(content) {
      if (_.isString(content)) {
        return content;
      }
      if (_.isElement(content)) {
        return content;
      }
      if (content instanceof ElementWrapper) {
        return content.element;
      }
      throw new Error("Provided value <" + content + "> is not valid as DOM element content.");
    };
    EventWrapper = (function() {
      function EventWrapper(event) {
        var i, len, name, ref;
        this.nativeEvent = event;
        this.memo = event.memo;
        ref = ["type", "char", "key"];
        for (i = 0, len = ref.length; i < len; i++) {
          name = ref[i];
          this[name] = event[name];
        }
      }

      EventWrapper.prototype.stop = function() {
        return this.nativeEvent.stop();
      };

      return EventWrapper;

    })();
    onevent = function(elements, eventNames, match, handler) {
      var element, eventHandlers, eventName, i, j, len, len1, wrapped;
      if (handler == null) {
        throw new Error("No event handler was provided.");
      }
      wrapped = function(prototypeEvent) {
        var elementWrapper, eventWrapper, result;
        elementWrapper = new ElementWrapper(prototypeEvent.findElement());
        eventWrapper = new EventWrapper(prototypeEvent);
        result = prototypeEvent.stopped ? false : handler.call(elementWrapper, eventWrapper, eventWrapper.memo);
        if (result === false) {
          prototypeEvent.stop();
        }
      };
      eventHandlers = [];
      for (i = 0, len = elements.length; i < len; i++) {
        element = elements[i];
        for (j = 0, len1 = eventNames.length; j < len1; j++) {
          eventName = eventNames[j];
          eventHandlers.push(Event.on(element, eventName, match, wrapped));
        }
      }
      return function() {
        var eventHandler, k, len2, results;
        results = [];
        for (k = 0, len2 = eventHandlers.length; k < len2; k++) {
          eventHandler = eventHandlers[k];
          results.push(eventHandler.stop());
        }
        return results;
      };
    };
    ElementWrapper = (function() {
      function ElementWrapper(element1) {
        this.element = element1;
      }

      ElementWrapper.prototype.toString = function() {
        var markup;
        markup = this.element.outerHTML;
        return "ElementWrapper[" + (markup.substring(0, (markup.indexOf(">")) + 1)) + "]";
      };

      ElementWrapper.prototype.hide = function() {
        this.element.hide();
        return this;
      };

      ElementWrapper.prototype.show = function() {
        this.element.show();
        return this;
      };

      ElementWrapper.prototype.css = function(name, value) {
        if (arguments.length === 1) {
          return this.element.getStyle(name);
        }
        this.element.setStyle({
          name: value
        });
        return this;
      };

      ElementWrapper.prototype.offset = function() {
        return this.element.viewportOffset();
      };

      ElementWrapper.prototype.remove = function() {
        this.element.remove();
        return this;
      };

      ElementWrapper.prototype.attr = function(name, value) {
        var attributeName, current;
        if (_.isObject(name)) {
          for (attributeName in name) {
            value = name[attributeName];
            this.attr(attributeName, value);
          }
          return this;
        }
        current = this.element.readAttribute(name);
        if (arguments.length > 1) {
          this.element.writeAttribute(name, value === void 0 ? null : value);
        }
        return current;
      };

      ElementWrapper.prototype.focus = function() {
        this.element.focus();
        return this;
      };

      ElementWrapper.prototype.hasClass = function(name) {
        return this.element.hasClassName(name);
      };

      ElementWrapper.prototype.removeClass = function(name) {
        this.element.removeClassName(name);
        return this;
      };

      ElementWrapper.prototype.addClass = function(name) {
        this.element.addClassName(name);
        return this;
      };

      ElementWrapper.prototype.update = function(content) {
        this.element.update(content && convertContent(content));
        return this;
      };

      ElementWrapper.prototype.append = function(content) {
        this.element.insert({
          bottom: convertContent(content)
        });
        return this;
      };

      ElementWrapper.prototype.prepend = function(content) {
        this.element.insert({
          top: convertContent(content)
        });
        return this;
      };

      ElementWrapper.prototype.insertBefore = function(content) {
        this.element.insert({
          before: convertContent(content)
        });
        return this;
      };

      ElementWrapper.prototype.insertAfter = function(content) {
        this.element.insert({
          after: convertContent(content)
        });
        return this;
      };

      ElementWrapper.prototype.findFirst = function(selector) {
        var match;
        match = this.element.down(selector);
        if (match) {
          return new ElementWrapper(match);
        } else {
          return null;
        }
      };

      ElementWrapper.prototype.find = function(selector) {
        var e, i, len, matches, results;
        matches = this.element.select(selector);
        results = [];
        for (i = 0, len = matches.length; i < len; i++) {
          e = matches[i];
          results.push(new ElementWrapper(e));
        }
        return results;
      };

      ElementWrapper.prototype.findParent = function(selector) {
        var parent;
        parent = this.element.up(selector);
        if (!parent) {
          return null;
        }
        return new ElementWrapper(parent);
      };

      ElementWrapper.prototype.closest = function(selector) {
        if (this.element.match(selector)) {
          return this;
        }
        return this.findParent(selector);
      };

      ElementWrapper.prototype.parent = function() {
        var parent;
        parent = this.element.parentNode;
        if (!parent) {
          return null;
        }
        return new ElementWrapper(parent);
      };

      ElementWrapper.prototype.children = function() {
        var e, i, len, ref, results;
        ref = this.element.childElements();
        results = [];
        for (i = 0, len = ref.length; i < len; i++) {
          e = ref[i];
          results.push(new ElementWrapper(e));
        }
        return results;
      };

      ElementWrapper.prototype.visible = function() {
        return this.element.visible();
      };

      ElementWrapper.prototype.deepVisible = function() {
        var element;
        element = this.element;
        return element.offsetWidth > 0 && element.offsetHeight > 0;
      };

      ElementWrapper.prototype.trigger = function(eventName, memo) {
        var event;
        if (eventName == null) {
          throw new Error("Attempt to trigger event with null event name");
        }
        if (!((_.isNull(memo)) || (_.isObject(memo)) || (_.isUndefined(memo)))) {
          throw new Error("Event memo may be null or an object, but not a simple type.");
        }
        if ((eventName.indexOf(':')) > 0) {
          event = this.element.fire(eventName, memo);
          return !event.defaultPrevented;
        }
        if (memo) {
          throw new Error("Memo must be null when triggering a native event");
        }
        if (!(Prototype.Browser.WebKit && eventName === 'submit' && this.element instanceof HTMLFormElement)) {
          return fireNativeEvent(this.element, eventName);
        } else {
          return this.element.requestSubmit();
        }
      };

      ElementWrapper.prototype.value = function(newValue) {
        var current;
        current = this.element.getValue();
        if (arguments.length > 0) {
          this.element.setValue(newValue);
        }
        return current;
      };

      ElementWrapper.prototype.checked = function() {
        return this.element.checked;
      };

      ElementWrapper.prototype.meta = function(name, value) {
        var current;
        current = this.element.retrieve(name);
        if (arguments.length > 1) {
          this.element.store(name, value);
        }
        return current;
      };

      ElementWrapper.prototype.on = function(events, match, handler) {
        exports.on(this.element, events, match, handler);
        return this;
      };

      ElementWrapper.prototype.text = function() {
        return this.element.textContent || this.element.innerText;
      };

      return ElementWrapper;

    })();
    RequestWrapper = (function() {
      function RequestWrapper(req) {
        this.req = req;
      }

      RequestWrapper.prototype.abort = function() {
        throw "Cannot abort Ajax request when using Prototype.";
      };

      return RequestWrapper;

    })();
    ResponseWrapper = (function() {
      function ResponseWrapper(res) {
        this.res = res;
        this.status = this.res.status;
        this.statusText = this.res.statusText;
        this.json = this.res.responseJSON;
        this.text = this.res.responseText;
      }

      ResponseWrapper.prototype.header = function(name) {
        return this.res.getHeader(name);
      };

      return ResponseWrapper;

    })();
    activeAjaxCount = 0;
    adjustAjaxCount = function(delta) {
      activeAjaxCount += delta;
      return exports.body.attr("data-ajax-active", activeAjaxCount);
    };
    ajaxRequest = function(url, options) {
      var finalOptions;
      if (options == null) {
        options = {};
      }
      finalOptions = {
        method: options.method || "post",
        contentType: options.contentType || "application/x-www-form-urlencoded",
        parameters: options.data,
        onException: function(ajaxRequest, exception) {
          adjustAjaxCount(-1);
          if (options.exception) {
            options.exception(exception);
          } else {
            throw exception;
          }
        },
        onFailure: function(response) {
          var message, text;
          adjustAjaxCount(-1);
          message = "Request to " + url + " failed with status " + (response.getStatus());
          text = response.getStatusText();
          if (!_.isEmpty(text)) {
            message += " -- " + text;
          }
          message += ".";
          if (options.failure) {
            options.failure(new ResponseWrapper(response), message);
          } else {
            throw new Error(message);
          }
        },
        onSuccess: function(response) {
          adjustAjaxCount(-1);
          if ((!response.getStatus()) || (!response.request.success())) {
            finalOptions.onFailure(new ResponseWrapper(response));
            return;
          }
          options.success && options.success(new ResponseWrapper(response));
        }
      };
      adjustAjaxCount(+1);
      return new RequestWrapper(new Ajax.Request(url, finalOptions));
    };
    scanners = null;
    scanner = function(selector, callback) {
      var scan;
      scan = function(root) {
        var el, i, len, ref;
        ref = root.find(selector);
        for (i = 0, len = ref.length; i < len; i++) {
          el = ref[i];
          callback(el);
        }
      };
      scan(exports.body);
      if (scanners === null) {
        scanners = [];
        exports.body.on(events.initializeComponents, function() {
          var f, i, len;
          for (i = 0, len = scanners.length; i < len; i++) {
            f = scanners[i];
            f(this);
          }
        });
      }
      scanners.push(scan);
    };
    exports = wrapElement = function(element) {
      if (_.isString(element)) {
        element = $(element);
        if (!element) {
          return null;
        }
      } else {
        if (!element) {
          throw new Error("Attempt to wrap a null DOM element");
        }
      }
      return new ElementWrapper(element);
    };
    createElement = function(elementName, attributes, body) {
      var element;
      if (_.isObject(elementName)) {
        body = attributes;
        attributes = elementName;
        elementName = null;
      }
      if (_.isString(attributes)) {
        body = attributes;
        attributes = null;
      }
      element = wrapElement(document.createElement(elementName || "div"));
      if (attributes) {
        element.attr(attributes);
      }
      if (body) {
        element.update(body);
      }
      return element;
    };
    getDataAttributeAsObject = function(element, attribute) {
      var value;
      value = $(element).readAttribute('data-' + attribute);
      if (value !== null) {
        return value = JSON.parse(value);
      } else {
        return value = {};
      }
    };
    getEventUrl = function(eventName, element) {
      var data, ref, ref1, url;
      if (!(eventName != null)) {
        throw 'dom.getEventUrl: the eventName parameter cannot be null';
      }
      if (!_.isString(eventName)) {
        throw 'dom.getEventUrl: the eventName parameter should be a string';
      }
      eventName = eventName.toLowerCase();
      if (element === null) {
        element = document.body;
      } else if (element instanceof ElementWrapper) {
        element = element.element;
      } else if (element.jquery != null) {
        element = element[0];
      }
      url = null;
      while ((url == null) && (element.previousElementSibling != null)) {
        data = getDataAttributeAsObject(element, 'component-events');
        url = data != null ? (ref = data[eventName]) != null ? ref.url : void 0 : void 0;
        element = element.previousElementSibling;
      }
      if (url == null) {
        while ((url == null) && (element.parentElement != null)) {
          data = getDataAttributeAsObject(element, 'component-events');
          url = data != null ? (ref1 = data[eventName]) != null ? ref1.url : void 0 : void 0;
          element = element.parentElement;
        }
      }
      return url;
    };
    _.extend(exports, {
      getEventUrl: getEventUrl,
      wrap: wrapElement,
      create: createElement,
      ajaxRequest: ajaxRequest,
      on: function(selector, events, match, handler) {
        var elements;
        if (handler == null) {
          handler = match;
          match = null;
        }
        elements = parseSelectorToElements(selector);
        events = utils.split(events);
        return onevent(elements, events, match, handler);
      },
      onDocument: function(events, match, handler) {
        return exports.on(document, events, match, handler);
      },
      body: wrapElement(document.body),
      scanner: scanner
    });
    return exports;
  });

}).call(this);
