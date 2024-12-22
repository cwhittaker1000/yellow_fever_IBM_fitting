/**
 * @file This file contains the javascript functionality to support configurable layout
 * components.
 */

 runOnDomLoaded(function() { // added line
	( function($) {
	'use strict';

	/**
     * Creates a new ConfigurableLayout
	 * @class
     */
	function ConfigurableLayout(pContainer, pOptions) {
		const mLayout = this;
		const mContainer = pContainer;
		const mColumnGroups = new Array();

		/**
		 * Find and create a new ColumnGroup instance for all column group in a container
		 * @method init
		 * @private
		 */
		const init = function() {
			mContainer.find('> .column-group').each(function() {
				mColumnGroups.push(new ColumnGroup($(this), mLayout));
			});

		};



		init();

	}
	/***
	 * Track SIQ events
	 */
	function trackSiqEvent(eventType) {
		var bodyElement  = document.getElementById("siq-events");
		if(bodyElement) {
			var span = document.createElement('span'); // is a node
			span.innerHTML = eventType;
			console.log(eventType);
			bodyElement.appendChild(span);
		}
	}
	/**
     * Creates a new ColumnGroup
	 * @class
     */
	function ColumnGroup(pContainer, pLayout) {
		const mColumnGroup = this;
		const mContainer = pContainer;
		const mLayout = pLayout;
		const mColumns = new Array();

		/**
		 * Find and create a new Column instance for each column in a container
		 * @method init
		 * @private
		 */
		const init = function() {
			mContainer.find('> .column').each(function() {
				mColumns.push(new Column($(this), mColumnGroup));
			});
		};

		/**
         * Called by the columns when they are collapsed.
		 * @method columnCollapsed
		 * @public
         */
		this.columnCollapsed = function(pColumn) {
			fastdom.mutate(function() {
				mContainer[0].classList.add(pColumn.getName() + '-collapsed');
			});
		};

		/**
         * Called by the columns when they are uncollapsed (i.e. move from collapsed to
		 * regular state).
		 * @method columnUncollapsed
		 * @public
         */
		this.columnUncollapsed = function(pColumn) {
			fastdom.mutate(function() {
				mContainer[0].classList.remove(pColumn.getName() + '-collapsed', 'c-Drawer--close');
			});
		};

		/**
         * Called by the columns when they are expanded.
		 * @method columnExpanded
		 * @public
         */
		this.columnExpanded = function(pColumn) {
			mContainer[0].classList.add(pColumn.getName() + '-expanded');
			$.each(mColumns, function(index, column) {
				if (column != pColumn) {
					column.unexpand();
				}
			});
		};

		/**
         * Called by the columns when they are unexpanded (i.e. moved from expanded to
		 * regular state).
		 * @method columnUnexpanded
		 * @public
         */
		this.columnUnexpanded = function(pColumn) {
			fastdom.mutate(function() {
				mContainer[0].classList.remove(pColumn.getName() + '-expanded');
			});
		};

		init();
	}

	/**
     * Creates a new Column
	 * @class
     */
	function Column(pContainer, pGroup) {
		const mColumn = this;
		const mContainer = pContainer;
		const mColumnGroup = pGroup;
		const mName = mContainer.data('columnname');
		const mCollapsible = mContainer.hasClass('column-collapsible');
		const mExpandable = mContainer.hasClass('column-expandable');
		const mCollapseControl = mContainer.find('> .column-controls .control-column-collapse');
		const mExpandControl = mContainer.find('> .column-controls .control-column-expand');
		const mContainerBlocks = new Array();
		let mExpanded = mContainer.hasClass('column-expanded');
		let mCollapsed = mContainer.hasClass('column-collapsed');

		/**
		 * Expand/collapse event listeners.  Finds and creates new Container instances
		 * in the column.
		 * @method init
		 * @private
		 */
		const init = function() {
			if (mCollapsible || mExpandable) {
				mCollapseControl.on('click', collapse);
				mExpandControl.on('click', expand);
			}

			// if (mExpandable) {
			// 	mExpandControl.on('click', expand);
			// }

			mContainer.find('.component-container').each(function() {
				mContainerBlocks.push(new Container($(this)));
			});
		};

		/**
         * Collapses the column.
		 * @method collapse
		 * @private
         */
		const collapse = function(pEvent) {
			pEvent.preventDefault();
			pEvent.stopPropagation();

			if (mExpanded) {
				fastdom.mutate(function() {
					mContainer[0].classList.remove('column-expanded', 'c-Drawer--expand');
					mColumnGroup.columnUnexpanded(mColumn);
					mExpanded = false;
				});
			} else {
				fastdom.mutate(function() {
					mContainer[0].classList.add('column-collapsed', 'c-Drawer--close');
					mColumnGroup.columnCollapsed(mColumn);
					mCollapsed = true;
				});
			}

			return false;
		};

		/**
         * Expands the column.
		 * @method expand
		 * @private
         */
		var expand = function(pEvent) {
			pEvent.preventDefault();
			pEvent.stopPropagation();

			if (mCollapsed) {
				mContainer[0].classList.remove('column-collapsed', 'c-Drawer--close');
				mColumnGroup.columnUncollapsed(mColumn);
				mCollapsed = false;
			} else {
				mContainer[0].classList.add('column-expanded', 'c-Drawer--expand');
				mColumnGroup.columnExpanded(mColumn);
				mExpanded = true;
			}

			return false;
		};

		/**
         * Returns the name of the column.
		 * @method getName
		 * @public
         */
		this.getName = function() {
			return mName;
		};

		/**
         * Unexpands the column if it's expanded (i.e. goes back to default state).
		 * @method unexpand
		 * @public
         */
		this.unexpand = function() {
			if (mExpanded) {
				fastdom.mutate(function() {
					mContainer[0].classList.remove('column-expanded', 'c-Drawer--expand');
					mColumnGroup.columnUnexpanded(mColumn);
					mExpanded = false;
				});
			}
		};

		init();
	}

	/**
     * Container class
	 * @class
     */
	function Container(pContainer) {
		const mContainer = pContainer;
		const mSliders = mContainer.find('.slider');
		let mStyle = 'list';
		let tabAddress;

		/**
		 * @method init
		 * @private
		 */
		const init = function() {
			if (mContainer[0].classList.contains('container-tabbed')) {
				mStyle = 'tabbed';
				tabAddress = mContainer.find("[data-container-tab-address]").data("container-tab-address")
				initTabs();
				if (tabAddress) {
					initTabAddressing();
				}
			} else if (mContainer[0].classList.contains('container-accordion')) {
				mStyle = 'accordion';
				initAccordion();
			}

		};

		/**
		 * @method initTabs
		 * @private
		 */
		const initTabs = function() {
			const disabledTabs = mContainer.find('nav').first().find('li.disabled');
			const activeTab = mContainer.find('nav').first().find('li.active').index();
			const initialTabIndex = activeTab >= 0 ? activeTab : 0;
			const disabledTabIndexes = [];

			disabledTabs.each(function() {
				disabledTabIndexes.push($(this).index());
			});

			$(mContainer).tabs({
				active: initialTabIndex,
				disabled: disabledTabIndexes,
				activate: function(event, ui) {

					mSliders.slick('reinit');
					processCounterUsage(ui,false);

				},

			});
		};

		/**
		 * Setup behavior for adding tab addresses to the browser history as a user switches tabs.
		 * Also reacts to the pop state so it can swap the tab if the browser history changes.
		 * @return
		 */
		const initTabAddressing = function () {
			if (!tabAddress) { return }

			// it doesn't look like jqueryui tabs support passing arbitrary data to their events, so i'm using a
			// dirty flag here for some control
			var inPopState = false

			mContainer.on('tabsactivate', function (event, ui) {
				// if a browser history change caused the tab to change, do not update the history again
				if (!inPopState) {
					var tabviewid = ui.newTab.find('[data-tab-id]').data('tab-id')
					if (tabviewid) {
						var queryParams = getQueryParams()
						queryParams[tabAddress] = tabviewid
						if(tabviewid.indexOf("pdf") >= 0){
							var iframeElem = document.querySelector('.component-content-pdf iframe');
							if(iframeElem){
								//iframeElem.contentWindow.location.reload(true);
								iframeElem.style.width = "99.8%";
								setTimeout(function(){ 
									iframeElem.style.width = "100%";
									console.log('refreshing ');
								 }, 100);
								
							}
						}
						history.replaceState(null, null, '?' + toQueryString(queryParams))
					}
				}
			})

			window.addEventListener('popstate', function (event) {
				// set the dirty flag so the tab change callback knows that what invoked the change
				// to prevent further writes to the history
				inPopState = true

				var tabIndex = getAddressedTabIndex()
				if (tabIndex > -1) {
					mContainer.tabs('option', { active: tabIndex })
				}

				// defer clearing the flag in case any of the ui tabs methods are also deferred or promisfied
				setTimeout(function () {
					inPopState = false
				}, 1)
			})
		}

		/**
		 * @method initAccordion
		 * @private
		 */
		const initAccordion = function() {
			const accordionRoot = mContainer
				.children('.content-box')
				.children('.content-box-body')
				.children('.container-body');

			accordionRoot.accordion({
				collapsible: true,
				heightStyle: 'content',
				beforeActivate: function(event, ui) {
					// don't allow the tab to be activated if it's disabled
					if (event.currentTarget && event.currentTarget.classList.contains('disabled')) {
						return false;
					}
				},
				activate: function(event, ui) {
				// removing this line of code since is causing issues with the slider on the homepage we are initializing it 2 times and it makes that the arrows and dots gets duplicated ticket #IMFSG-331
				//	$('.slider').slick('reinit');
					processCounterUsage(ui,false);
				},
			});
		};

		/**
		 * Find the index of a specific tab that's requested in the query string
		 * @return {int} The index or -1 not found or the container tab is not being addressed
		 */
		const getAddressedTabIndex = function () {
			let queryParams = getQueryParams()
			let tabid = queryParams[tabAddress]

			return tabAddress && tabid
				? mContainer.find('[data-tab-id="' + tabid + '"]').closest('li').index()
				: -1
		}

		init();
	};

	/**
	 * Parses the current browser's query string into a hash map
	 * @return {Object}
	 */
	function getQueryParams () {
		return window.location.search.substring(1).split('&')
			.filter(function (kvp) { return !!kvp })
			.reduce(function (acc, kvp) {
				var keyValuePair = kvp.split('=')
				acc[decodeURIComponent(keyValuePair[0])] = keyValuePair.length > 1 ? decodeURIComponent(keyValuePair[1]) : undefined
				return acc
			}, {})
	}

	/**
	 * Converts a hash map into a query string
	 * @return {String}
	 */
	function toQueryString (params) {
		return Object.keys(params).map(function (key) {
			var value = params[key]
			var noVal = !value && value !== ''
			return noVal ? key : encodeURIComponent(key) + '=' + encodeURIComponent(value)
		}).join('&')
	}

	var processCounterUsage = function(ui,pageLoadEvent) {
		processCounterEvent(getCounterDataTypeToSend($(ui.newPanel.find(".counterData").filter(":visible"))),pageLoadEvent);
	};

	/**
	 *
	 * There is an order or precendence here, to not break C4 Logging.
	 * If there is an FT_X event, then that is sent.
	 */
	var getCounterDataTypeToSend = function(counterDataItems, ignoreOnLoad) {
		var eventType = "";
		$(counterDataItems).each(function(index) {
			var counterDatatype = $(this).data("datatype");
			var logOnLoad = $(this).data("logonload");
			//The first onload events, not all should be loaded on load
			//even if visible
			if (ignoreOnLoad === true && logOnLoad == false) {
				return;
			}
			if (counterDatatype === "restricted") {
				processCounterEvent(this);
				return;
			} else if (counterDatatype === "restrictedTA") {
				processCounterEvent(this);
				return;
			}

			if (eventType === "") {
				eventType = this;
			} else {
				var counterDatatype = $(this).data("datatype");
        if (counterDatatype === "fthtml" || counterDatatype === "pdf" || counterDatatype == "pdf-preview") {
					eventType = this;
				} else if (counterDatatype === "abstract") {
					if (eventType !== "") {
						var eventData = $(this).data("datatype");
						if (!(eventData === "fthtml" || counterDatatype === "pdf" || counterDatatype == "pdf-preview")) {
							eventType = this;
						}
					} else {
						eventType = this;
					}
				}
			}
		});
		return eventType;

	}

	var processCounterEvent = function(counterData,pageLoadEvent) {
		var counterDatatype = $(counterData).data("datatype");
		var service = $(counterData).data("service");
		var uri = "";
		var logRequest = true;
		if ($("#usageLoggingAjax"))
		if (counterDatatype === "fthtml") {
			uri = "logHTMLView";
		} else if (counterDatatype === "abstract") {
			uri = "logAbstractView";
		} else if (counterDatatype === "pdf") {
			uri = "logPDFView";
		} else if (counterDatatype === "pdf-preview") {
			uri = "logPDFPreview";
		} else if (counterDatatype === "toc") {
			uri = "logTOCView";
		} else if (counterDatatype === "epub") {
				uri = "logEPUBView";
		} else if (counterDatatype === "video") {
			uri = "logVideo";
		} else if (counterDatatype === "restricted") {
			uri = "logRestricted";
		} else if (counterDatatype === "restrictedTA") {
			uri = "logRestrictedTA";
		} else if (counterDatatype === 'other') {
			uri = "logOther";
		} else if (counterDatatype === 'logShare') {
			uri = "logShare";
		}
		if (uri) {
			var docUri = $(counterData).data("docuri");
			if (counterDatatype === 'logShare') {
				docUri = $(counterData).data("url");
			}
			window.dataLayer = window.dataLayer || [];
			jQuery.ajax({
				url: "/rest/counterLogging/" + uri,
				type: "POST",
				data : JSON.stringify({docuri :docUri, logRequest: logRequest, destination: service, pageLoadEvent: pageLoadEvent}),
				contentType: "application/json",
				dataType: "json",
				error:function(gaString) {
					// dataLayer.push pushes events to GA
					window.dataLayer.push(gaString);
					processCounterLogging(false, uri, counterDatatype);
				},
				success:function(gaString){
					// dataLayer.push pushes events to GA
					window.dataLayer.push(gaString);
					processCounterLogging(true, uri, counterDatatype);
					if(!service  || (service && service !== 'download'))
						processSIQTagging(gaString);
				}
			});
		}

	}

	$('.dropdown-button').on('click', '.dropdown-control-container, #modalInnerContent', function (e) {
		processCounterEvent($(this.parentElement).find(".counterData"));
	});
	$('#pdf-download,#epub-download').click( function (e) {
		processCounterEvent($(this));
	});
	$('button.c-IconButton').click( function (e) {
		processCounterEvent($(this));
	});
	$('#submitEmail').click( function (e) {
		processCounterEvent($(this));
	});
	$('a.c-IconButton[data-service="email"]').click( function (e) {
		processCounterEvent($j(this));
	});
	var processSIQTagging = function(gaString) {
		//I don't like repeated code, but I will make an exception
		//This is also in image-gallery.js
		var usage = $("#siqenabled");
		var siqEnabled = false;
		if(gaString) {
			if(usage.length === 1 || gaString.hasOwnProperty("pf:siqenabled")) {
				var eventType = gaString['pf_contentType'];
				var queryString = Object.keys(gaString).map(function(key) { return key.replace('?', '').
																							replace(':','_').
																							replace('pf_contentType','event_type') + '=' + encodeURIComponent(gaString[key]) } ).join('&');
				try {
					var NTPT_PGEXTRA= '&'+queryString;
					ntptEventTag(NTPT_PGEXTRA);
					trackSiqEvent(eventType);
				} catch(err) {
					var siqElement = document.getElementById("body");
					var newScript = document.createElement("script");
					newScript.id ="siq-enabled";
					newScript.src = "//pftag.scholarlyiq.com/siqpagetag.js";
					siqElement.appendChild(newScript);
					ntptEventTag(NTPT_PGEXTRA);
				}
			}
		}
	};

	var processCounterLogging = function(success, uri, counterDataType) {
		//I don't like repeated code, but I will make an exception
		//This is also in image-gallery.js
		var usage = $("#usageLoggingAjax")
		if (usage.length === 1) {
			var textToAppend = "&#09;" + counterDataType + " sent to uri " + uri + ": ";
			if (success) {
				textToAppend += "Sent Successfully\n";
			} else {
				textToAppend += "Failed to send\n";
			}
			usage.append(textToAppend);
		}
	};



	/**
     * Global configurable layout method declarations.
     */
	const gMethods = {
		init: function(pOptions) {
			if (pOptions === undefined) {
				pOptions = {};
			}

			var returnValue = this.each(function () {
				const layout = $(this);
				let configurableLayout = layout.data('CONFIGURABLELAYOUT');

				if (!configurableLayout) {
					configurableLayout = new ConfigurableLayout(layout, pOptions);
					layout.data('CONFIGURABLELAYOUT', configurableLayout);
				}

				return configurableLayout;
			});
			if (returnValue.length > 0) {

				$(".counterFiveOther").click(function () {
					processCounterEvent(this);
				});


				processCounterEvent(getCounterDataTypeToSend($(".counterData").filter(":visible").not(".counterData.video-counter")),true);
				$(document).ready(function (){
					var usage = $("#usageLoggingEvents");
					if (usage.length === 1) {
						var allEvents = "";
						$(".counterData").each(function () {
							allEvents += "&#09;" + $(this).data("datatype") + "\n";
						});
						usage.append(allEvents);
					}
					$("video").on('play', function(eventObject) {
						var counterData = $(".video-counter").first();
						processCounterEvent(counterData);
					});

				});


			}
			return returnValue;
		},
	};

	/**
     * ConfigurableLayout jQuery namespace.
	 * @namespace
     */
	$.fn.configurablelayout = function(pMethod) {
		let returnValue = null;

		if (gMethods[pMethod]) {
			returnValue = gMethods[pMethod].apply(this, Array.prototype.slice.call(arguments, 1));
		} else if (typeof pMethod === 'object' || !pMethod) {
			returnValue = gMethods.init.apply(this, arguments);
		} else {
			$.error('Unknown method');
		}

		return returnValue;
	};

	/**
     * This was the old functionality for having a container with a scroll lock that would stop
     * scrolling when it hit the top of the viewport.  It never was styled fully and was a little
     * rough in functionality.  I'm taking this out for now and we'll revisit as we build out
     * the configurable page front-end styling.
     */

/*
    $(document).ready(function() {

        // select all of the scroll locks on the page
        var scrollLocks = $(".scroll-lock");

        // update any sibling columns of the one containing the scroll lock to have a min height of the window height
        scrollLocks.each(function() {
            var lock = $(this),
                column = lock.closest(".column"),
                siblings = column.siblings(".column"),
                windowHeight = $(window).height();

            siblings.css("min-height", windowHeight);
        });

        // if there are any scroll-lock containers, update them on scroll
        if (scrollLocks.size() > 0) {
            $(window).scroll(function(e) {
                var windowTop = $(window).scrollTop();
                scrollLocks.each(function() {
                    var lock = $(this),
                        lockTop = lock.offset().top,
                        container = lock.next(),
                        width = container.width();

                    if (windowTop > lockTop) {
                        if (!container.hasClass("scroll-locked")) {
                            container.width(width); // set the width to a specific pixel width because 'fixed' elements end up getting sized relative to the window
                            container.addClass("scroll-locked");
                        }
                    }
                    else if(container.hasClass("scroll-locked")) {
                        container.scrollTo(0, 0, {});
                        container.removeClass("scroll-locked");
                        container.width("auto"); // go back to 'auto' width so it sizes according to the container
                    }
                });
            });
        }

    });
    */
   
	// initialize the configurable layout itself
	$('.layout').configurablelayout();

})(jQuery);
});

window.initAccordion = function (parameters) {
  runOnDomLoaded(
    function () {
      function initAccordion(wrapperId, active) {
        var jQueryObject = jQuery(wrapperId).find('.ui-accordion');
        jQueryObject.accordion({
          active: active,
          beforeActivate: function (event, ui) {
            if (ui.newHeader.length > 0) {
              var button = ui.newHeader.find(".c-Button");
              button.removeClass(button.attr("data-collapsed-icon"));
              button.addClass(button.attr("data-active-icon"));
            } else {
              var button = ui.oldHeader.find(".c-Button");
              button.removeClass(button.attr("data-active-icon"));
              button.addClass(button.attr("data-collapsed-icon"));
            }
          }
          // autoHeight: false,
          // navigation: true,
          // collapsible: true
        });

        jQueryObject.find('[data-collapsed-icon]').each(function (index, button) {
          if (index === active) {
            button.classList.remove(button.getAttribute('data-collapsed-icon'));
            button.classList.add(button.getAttribute('data-active-icon'));
          }
        });

        jQueryObject.show();
      }
      if (parameters !== null && parameters !== undefined) {
        initAccordion(parameters.wrapperId, parameters.active);
      }
    })
};