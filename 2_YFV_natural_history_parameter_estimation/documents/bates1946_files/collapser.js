'use strict';

/**
 *  The Collapser object can have mutliple instances,
 *  and should work with any nested ul/li...
 *
 *  This function is used in both the loader and baseapp - so if you make
 *  changes, be sure to check both for potential problems.
 *
 *  Example structure:
 *
 *  <div class="taxonomyTree">
 *    <ul>
 *      <li>
 *        <a class="toggle">
 *        <a class="taxonomyNode" id="0001">My Node's Wonderful Name</a>
 * 		  <ul>
 *          <li>
 *           ...
 *           </li>
 *           <li>
 *           </li>
 *         </ul>
 *       </li>
 *       <li>
 *        ...
 *       </li>
 *     </ul>
 *   </div>
 */
(function ($) {
	var COLLAPSER = function COLLAPSER(collapser, shutAll, openAll) {
		var c = this.collapser = collapser = $(collapser);
		var CPR = this;

		shutAll = shutAll === undefined ? true : shutAll; // make sure this is set if it is not passed
		openAll = openAll === undefined ? false : openAll; // make sure this is set if it is not passed

		// if there are any open/close all links
		c.on('click', '.openAll a', function () {
			CPR.openAll();
			return false;
		});

		c.on('click', '.shutAll a', function () {
			CPR.shutAll();
			return false;
		});

		c.addClass('collapser');

		var a = this.allLis = c.find('ul').not('.actions-list').parentsUntil(c, 'li');

		if (a.length == 0) {
			c.addClass('collapser-flat'); // signifies that a collapser was added, but it is flat (eg. no expandable/collapsable nodes)
			return; // nothing left to do, there are no expandable elements
		}

		if (CPR.getToggle(a).length > 0) {
			c.addClass('collapser-with-toggles');
		} else {
			c.addClass('collapser-without-toggles');
		}

		// attach the listener to the <li> expandable tag
		a.each(function () {
			var li = $(this);
			if (!li.data('COLLAPSER')) {
				li.data('COLLAPSER', c);
				var toggleBtn = CPR.getToggle(li);
				if (toggleBtn) {
					toggleBtn.on('click', function (event) {
						if (li.hasClass('expandable')) {
							if (li.hasClass('open')) {
								toggleBtn.removeClass('c-Icon--remove').addClass('c-Icon--add');
							} else {
								toggleBtn.removeClass('c-Icon--add').addClass('c-Icon--remove');
							}
						}
					});
				}

				li.on('click', function (event) {
					var t = $(this); // this
					// event target
					var tg = $(event.target);
					// event target node name
					var tgnn = tg.prop('nodeName');
					var toggle = CPR.getToggle(t);
					// closest expandable li
					var cli = tg.closest('li.expandable');
					// the closests "stop" tag, meaning that if they are clicked, stop propagtion... TODO: make this overridable in a plugin!
					var cstop = tg.closest('input, label, a, li:not(.expandable)');
					// the distance to the closest LI
					var dcli = tg.parentsUntil(cli).length;
					// the distance to the closest "stop" tag.
					var dcstop = tg.parentsUntil(cstop).length;

					// this is necessary so that if an "a" tag (for example) is within a collapser, it doesn't also collapse/expand the collapser!
					if (
					// there is no expandable section, so give up
					!cli ||
					// if the target is also the closest stop element... but it is not explicitly a toggle, then we need to stop propagation!
					tg.is(cstop) && !tg.is(toggle) ||
					// if somewhere between the target and the collapsing element, there is a stop element, then don't collapse either
					dcstop < dcli && !cstop.is(toggle)) {
						event.stopPropagation();
						// if the closest li is not an expandable one, or if what was clicked was a link, stop propagation
						return;
					}

					CPR.toggleSection(t);
					return false;
				});
			}
		});

		a.addClass('expandable');
		if (shutAll) {
			CPR.shutAll();
			c.find('li.active-section').each(function (li) {
				CPR.openSection(this);
			});
		} else if (openAll) {
			CPR.openAll();
		} else {
			// we have to make sure that if it is not "open" or "shut", we set them
			CPR.openSection(a.not('shut').not('open'));
		}
	};

	var CP = COLLAPSER.prototype;

	CP.openAll = function () {
		this.openSection(this.allLis);
	};

	CP.shutAll = function () {
		this.shutSection(this.allLis);
	};

	CP.setSection = function (section, addClass, removeClass, text) {
		section = $(section);
		var toggle = this.getToggle(section);
		var span = toggle.find('span').first();

		section.addClass(addClass).removeClass(removeClass);

		// The toggle is the reverse state of the parent
		if (addClass === 'open') {
			toggle.addClass('ico-toggle-' + removeClass + ' c-Icon--remove').removeClass('ico-toggle-' + addClass + ' c-Icon--add');
		} else {
			toggle.addClass('ico-toggle-' + removeClass + ' c-Icon--add').removeClass('ico-toggle-' + addClass + ' c-Icon--remove');
		}
		span.html(text);
	};

	CP.openSection = function (section) {
		this.setSection(section, 'open', 'shut', 'Close section');
	};

	CP.shutSection = function (section) {
		this.setSection(section, 'shut', 'open', 'Open section');
	};

	CP.getToggle = function (section) {
		return section.find('.toggle:first');
	};

	CP.toggleSection = function (section) {
		var s = $(section);
		if (s.hasClass('open')) {
			this.shutSection(s);
		} else {
			this.openSection(s);
		}
	};

	$(function () {
		$('.taxonomyTree').each(function () {
			var t = $(this),
			    s = t.hasClass('all-shut'),
			    o = t.hasClass('all-open'),
			    c = new COLLAPSER(this, s, o);
			c.openSection($(this).find('.active').closest('li').parents('.expandable'));
			var t = $(this);
			if (t.hasClass('relatedTaxonomyTree')) {
				c.openAll();
			}
			$.data(this, 'COLLAPSER', c);
		});

		$('.toc-menu').each(function () {
			if ($(this).find('.issue-toc').length <= 0) {
				//Do not add collapser to issue toc main content
				var t = $(this),
				    s = t.hasClass('all-shut'),
				    c = new COLLAPSER(this, s);

				if (t.find('.book-toc').length > 0) {
					// this is book TOCs...
					//t.find(".expandable").prepend("<a class='toggle minustoggle'><span>toggle</span></a>");
					c.shutSection(t.find("span:contains('Front Matter')").parents('.expandable'));
					c.shutSection(t.find("span:contains('Back Matter')").parents('.expandable'));
				} else if (t.find('.journal-toc').length > 0) {
					// journal TOCs
					var curr = t.find('.current');
					c.openSection(curr.closest('li').parents('.expandable'));
					// By default, always expand the first Volume if no other volumes are selected
					if (!curr) {
						c.openSection(t.find('.js-first').closest('li'));
					}
				}
				c.openSection($(this).find('.active').closest('li').parents('.expandable'));

				$.data(this, 'COLLAPSER', c);
			}
		});
	});
})(jQuery);
