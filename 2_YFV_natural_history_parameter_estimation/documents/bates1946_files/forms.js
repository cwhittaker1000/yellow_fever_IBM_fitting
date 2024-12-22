(function() {
  define(["./events", "./dom", "underscore"], function(events, dom, _) {
    var DATA_ATTRIBUTE, DATA_ATTRIBUTE_VALUE, SKIP_VALIDATION, TAPESTRY_CORE_FORM_SELECTOR, clearSubmittingHidden, defaultValidateAndSubmit, exports, gatherParameters, setSubmittingHidden;
    SKIP_VALIDATION = "t5:skip-validation";
    DATA_ATTRIBUTE = "data-generator";
    DATA_ATTRIBUTE_VALUE = "tapestry/core/form";
    TAPESTRY_CORE_FORM_SELECTOR = "form[" + DATA_ATTRIBUTE + "='" + DATA_ATTRIBUTE_VALUE + "']";
    clearSubmittingHidden = function(form) {
      var hidden;
      hidden = form.findFirst("[name='t:submit']");
      hidden && hidden.value(null);
      form.meta(SKIP_VALIDATION, null);
    };
    setSubmittingHidden = function(form, submitter) {
      var firstHidden, hidden, isCancel, mode, name;
      mode = submitter.attr("data-submit-mode");
      isCancel = mode === "cancel";
      if (mode && mode !== "normal") {
        form.meta(SKIP_VALIDATION, true);
      }
      hidden = form.findFirst("[name='t:submit']");
      if (!hidden) {
        firstHidden = form.findFirst("input[type=hidden]");
        hidden = dom.create("input", {
          type: "hidden",
          name: "t:submit"
        });
        firstHidden.insertBefore(hidden);
      }
      name = isCancel ? "cancel" : submitter.element.name;
      hidden.value("[\"" + submitter.element.id + "\",\"" + name + "\"]");
    };
    gatherParameters = function(form) {
      var fields, result;
      result = {};
      fields = form.find("input, select, textarea");
      _.each(fields, function(field) {
        var existing, name, type, value;
        if (field.attr("disabled")) {
          return;
        }
        type = field.element.type;
        if (type === "file" || type === "submit") {
          return;
        }
        if ((type === "checkbox" || type === "radio") && field.checked() === false) {
          return;
        }
        value = field.value();
        if (value === null) {
          return;
        }
        name = field.element.name;
        if (name === "") {
          return;
        }
        existing = result[name];
        if (_.isArray(existing)) {
          existing.push(value);
          return;
        }
        if (existing) {
          result[name] = [existing, value];
          return;
        }
        return result[name] = value;
      });
      return result;
    };
    defaultValidateAndSubmit = function() {
      var error, error1, field, focusField, hasError, i, len, memo, ref, where;
      where = function() {
        return "processing form submission";
      };
      try {
        if (((this.attr("data-validate")) === "submit") && (!this.meta(SKIP_VALIDATION))) {
          this.meta(SKIP_VALIDATION, null);
          hasError = false;
          focusField = null;
          ref = this.find("[data-validation]");
          for (i = 0, len = ref.length; i < len; i++) {
            field = ref[i];
            memo = {};
            where = function() {
              return "triggering " + events.field.inputValidation + " event on " + (field.toString());
            };
            field.trigger(events.field.inputValidation, memo);
            if (memo.error) {
              hasError = true;
              if (!focusField) {
                focusField = field;
              }
            }
          }
          if (!hasError) {
            memo = {};
            where = function() {
              return "trigging cross-form validation event";
            };
            this.trigger(events.form.validate, memo);
            hasError = memo.error;
          }
          if (hasError) {
            clearSubmittingHidden(this);
            if (focusField) {
              focusField.focus();
            }
            where = function() {
              return "triggering validation in error event";
            };
            this.trigger(events.form.validateInError);
            return false;
          }
        }
        where = function() {
          return "triggering " + events.form.prepareForSubmit + " event (after validation)";
        };
        this.trigger(events.form.prepareForSubmit);
      } catch (error1) {
        error = error1;
        console.error("Form validiation/submit error `" + (error.toString()) + "', in form " + (this.toString()) + ", " + (where()));
        console.error(error);
        return false;
      }
    };
    dom.onDocument("submit", TAPESTRY_CORE_FORM_SELECTOR, defaultValidateAndSubmit);
    dom.onDocument("click", TAPESTRY_CORE_FORM_SELECTOR + " input[type=submit], " + TAPESTRY_CORE_FORM_SELECTOR + " input[type=image]", function() {
      setSubmittingHidden(dom(this.element.form), this);
    });
    dom.onDocument("click", "a[data-submit-mode]", function() {
      var form;
      form = this.findParent("form");
      if (!form) {
        console.error("Submitting link element not contained inside a form element.");
        return false;
      }
      setSubmittingHidden(form, this.closest("a[data-submit-mode]"));
      form.trigger("submit");
      return false;
    });
    return exports = {
      gatherParameters: gatherParameters,
      setSubmittingElement: setSubmittingHidden,
      skipValidation: function(form) {
        return form.meta(SKIP_VALIDATION, true);
      }
    };
  });

}).call(this);
