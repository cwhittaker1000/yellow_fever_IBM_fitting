"use strict";

/*
	Recently Viewed Searches
*/

// Read's the body tag for classes matching `site-foobar`.
function getSiteNameFromBody() {
	return document.querySelector("body").dataset.siteName;
}

function saveRecentlyViewedSearch(item) {
	fetch('/rest/personalization/v1/saved-searches', {
		method: "POST",
		headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			searchParameters: item.searchParameterMap,
			title: item.title,
			resultCount: parseInt(item.resultCount.replace(/,/g, '')),
			siteName: item.siteName
		})
	}).then(function (response) {
		return response.text();
	}).then(function (data) {
		location.reload();
	}).catch(function (error) {
		console.error('Error: ', error);
	});
}

var qsa = function qsa(sel) {
	return Array.from(document.querySelectorAll(sel));
};
function initializeRecentlyViewedSearches() {
	var allrecentlyViewedSearches = qsa('.recentlyViewedSearches');
	var siteName = getSiteNameFromBody();
	// <span data-resulttotal="127" id="resultTotal" class="count">127</span>
	var resultTotal = document.getElementById('resultTotal');
	var resultCount = '';
	if (resultTotal) {
		resultCount = resultTotal.dataset.resulttotal;
	}
	// <ul> ...
	var recentlyViewedSearches = document.querySelector('.recentlyViewedSearches');
	var resultLabel = '';
	var isSearching = false;
	if (recentlyViewedSearches) {
		resultLabel = recentlyViewedSearches.dataset.resultregionlabel;
		isSearching = recentlyViewedSearches.dataset.issearching == 'true';
	}

	// .recentlyViewedSearches > <li> <span class="criteria"> ... <a>save icon
	var recentlyViewedSearchTemplate = document.querySelector('.recentlyViewedSearchTemplate');
	if (recentlyViewedSearchTemplate) {
		recentlyViewedSearchTemplate = recentlyViewedSearchTemplate.cloneNode(true);
		allrecentlyViewedSearches.forEach(function (el) {
			var temel = el.querySelector('.recentlyViewedSearchTemplate');
			if (temel) {
				el.removeChild(temel);
			}
		});
	}

	var getTitleWithResult = function getTitleWithResult(title, resultCount) {
		return title !== null && title.length > 25 ? title.substring(0, 22) + '... (' + resultCount + ' ' + resultLabel + ') ' : title + ' (' + resultCount + ' ' + resultLabel + ') ';
	};

	var getTemplate = function getTemplate(title, href, searchParameterMap, resultCount) {
		var templateElement = recentlyViewedSearchTemplate.cloneNode(true);
		var button = templateElement.querySelector('[data-component="saveButton"]');
		button.dataset.searchparametermap = JSON.stringify(searchParameterMap);
		button.setAttribute('href', href);
		button.innerHTML = getTitleWithResult(title, resultCount);
		return templateElement;
	};

	// Deserialize sessionStorage
	var searchesFromSessionStorage = sessionStorage.getItem('recentlyViewedSearches');
	if (!searchesFromSessionStorage) {
		searchesFromSessionStorage = [];
	} else {
		searchesFromSessionStorage = JSON.parse(searchesFromSessionStorage);
	}

	// if template exist, retrieve info from it
	if (recentlyViewedSearches && recentlyViewedSearchTemplate) {

		// Serialize the data of the current template
		var button = recentlyViewedSearchTemplate.querySelector('[data-component="saveButton"]');

		var templateData = {
			searchParameterMap: JSON.parse(button.dataset.searchparametermap),
			title: button.getAttribute('title'),
			href: button.getAttribute('href'),
			resultCount: resultCount,
			siteName: siteName
		};

		// if href is unique among stored searches, add to front
		if (isSearching && searchesFromSessionStorage.filter(function (e) {
			return button.getAttribute('href') === e.href;
		}).length === 0) {
			searchesFromSessionStorage.unshift(templateData);
		}
	}

	// check if all fields exist. discard otherwise
	var properties = ['title', 'href', 'searchParameterMap', 'resultCount', 'siteName'];
	searchesFromSessionStorage = searchesFromSessionStorage.filter(function (item) {
		var hasAllProperties = true;
		var propertiesLength = properties.length;
		for (var i = 0; i < propertiesLength && hasAllProperties; i++) {
			hasAllProperties = item.hasOwnProperty(properties[i]);
		}
		return hasAllProperties;
	});

	// Update the count strings on the page
	var recentlyViewedSearchesCount = qsa('.count.recentlyViewedSearchesCount');
	var mySearchesButtonCount = document.querySelector('.mySearches .count');

	var totalCount = 0;
	if (mySearchesButtonCount) {
		totalCount = parseInt(mySearchesButtonCount.innerHTML.replace(/\D/g, ''));
	}
	totalCount += searchesFromSessionStorage.length;

	if (recentlyViewedSearchesCount && mySearchesButtonCount) {
		recentlyViewedSearchesCount.forEach(function (el) {
			el.innerHTML = '(' + searchesFromSessionStorage.length + ')';
		});

		mySearchesButtonCount.innerHTML = '(' + totalCount + ')';
	}

	// if recently viewed searches exist, deserialize array from session storage and add the entries
	if (recentlyViewedSearches) {
		recentlyViewedSearches.innerHTML = '';
		searchesFromSessionStorage.filter(function (item) {
			return item.siteName === siteName;
		}).forEach(function (item) {
			var template = getTemplate(item.title, item.href, item.searchParameterMap, item.resultCount);
			allrecentlyViewedSearches.forEach(function (el) {
				el.appendChild(template);
			});
			if (document.querySelector('body').classList.contains('loggedin')) {
				template.querySelector('a.save').addEventListener('click', function (event) {
					event.preventDefault();
					saveRecentlyViewedSearch(item);
				});
				template.querySelector('a.save').addEventListener('keydown', function (event) {
					if (event.keyCode == 13) {
						event.preventDefault();
						saveRecentlyViewedSearch(item);
					}
				});
			} else {
				template.querySelector('a.save').remove();
			}
		});
	}

	// Reserialize the array and store it in sessionStorage
	sessionStorage.setItem('recentlyViewedSearches', JSON.stringify(searchesFromSessionStorage));
}

// Return the first element of a list that starts with "start"
var pickItem = function pickItem(elements, start) {
	var result = Array.from(elements).filter(function (item) {
		return item.startsWith(start);
	});
	if (result.length > 0) {
		return result[0];
	} else {
		return null;
	}
};

// test 2
var hasItem = function hasItem(elements, start) {
	return pickItem(elements, start) !== null;
};

function saveRecentlyViewedEntry(item) {
	fetch('/rest/v1/savedContent', {
		method: "POST",
		headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			uri: item.uri,
			title: item.title,
			siteName: item.siteName
		})
	}).then(function (response) {
		return response.text();
	}).then(function (data) {
		location.reload();
	}).catch(function (error) {
		console.error('Error: ', error);
	});
}

function initializeRecentlyViewedEntries() {
	var body = document.querySelector('body');
	var siteName = getSiteNameFromBody();
	var allrecentlyViewedEntries = qsa('.recentlyViewedEntries');
	var isContentPage = hasItem(body.classList, 'page-content') || hasItem(body.classList, 'page-view') || hasItem(body.classList, 'nonConfigurableViewPage');

	var isNonConfigurable = hasItem(body.classList, 'nonConfigurableViewPage');

	var nonConfigurableTitle = body.dataset.myContentTitle;

	var titleEl = document.querySelector('.component-content-title .title');
	var pageTitle = document.getElementById('pagetitle'); // alternative source in OUP sites
	var recentlyViewedEntries = document.querySelector('.recentlyViewedEntries');
	var contentTitle = titleEl ? titleEl.innerText : nonConfigurableTitle ? nonConfigurableTitle : pageTitle ? pageTitle.childElementCount === 0 ? pageTitle.innerText : pageTitle.firstChild.innerText : null;

	if (pageTitle && (nonConfigurableTitle === undefined || nonConfigurableTitle === null)) {
		if (pageTitle.childElementCount > 0 && pageTitle.firstChild.textContent !== undefined) {
			contentTitle = pageTitle.firstChild.textContent;
		}
	}

	if (contentTitle == null) {
		var tempTitle = document.querySelector('#mainTitle');
		if (tempTitle) {
			contentTitle = tempTitle.innerText;
		}
	}

	var recentlyViewedEntriesCount = document.querySelector('.recentlyViewedEntriesCount');
	var myEntriesButtonCount = document.querySelector('#savedEntries .count');
	var allEntriesButtonCount = qsa('#savedEntries .count');
	var allrecentlyViewedEntriesCount = qsa('.recentlyViewedEntriesCount');

	var getTemplate = function getTemplate(title, href) {
		var templateElement = recentlyViewedEntryTemplate.cloneNode(true);
		var button = templateElement.querySelector('[data-component="saveButton"]');
		button.setAttribute('href', href);
		button.innerHTML = title.length > 25 ? title.substr(0, 22) + "..." : title;
		return templateElement;
	};

	// Deserialize sessionStorage
	var entriesFromSessionStorage = sessionStorage.getItem('recentlyViewedEntries');

	if (!entriesFromSessionStorage) {
		entriesFromSessionStorage = [];
	} else {
		entriesFromSessionStorage = JSON.parse(entriesFromSessionStorage);
	}

	var recentlyViewedEntryTemplate = document.querySelector('.recentlyViewedEntryTemplate');
	if (recentlyViewedEntryTemplate) {
		recentlyViewedEntryTemplate = recentlyViewedEntryTemplate.cloneNode(true);
		allrecentlyViewedEntries.forEach(function (el) {
			var temel = el.querySelector('.recentlyViewedEntryTemplate');
			if (temel) {
				el.removeChild(temel);
			}
		});
	}

	// if template exist, retrieve info from it
	if (isContentPage && recentlyViewedEntries && recentlyViewedEntryTemplate) {
		var button = recentlyViewedEntryTemplate.querySelector('[data-component="saveButton"]');

		var templateData = {
			title: contentTitle,
			link: button !== undefined && button !== null ? button.href : window.location.pathname,
			uri: button !== undefined && button !== null ? button.dataset.uri : window.location.pathname !== null ? window.location.pathname.substring(window.location.pathname.indexOf("/view/") + 5) : window.location.pathname,
			siteName: siteName
		};

		if (isContentPage && entriesFromSessionStorage.filter(function (e) {
			return button.dataset.uri === e.uri;
		}).length === 0) {
			entriesFromSessionStorage.unshift(templateData);
		}

		while (entriesFromSessionStorage.length > 10) {
			entriesFromSessionStorage.pop();
		}
	}

	// check if all fields exist. discard otherwise
	var properties = ['title', 'link', 'uri', 'siteName'];
	entriesFromSessionStorage = entriesFromSessionStorage.filter(function (item) {
		var hasAllProperties = true;
		var propertiesLength = properties.length;
		for (var i = 0; i < propertiesLength && hasAllProperties; i++) {
			hasAllProperties = item.hasOwnProperty(properties[i]);
		}
		return hasAllProperties;
	});

	if (isNonConfigurable && !recentlyViewedEntryTemplate && contentTitle !== undefined && contentTitle !== null && contentTitle.length > 0) {
		var uriJs = window.location.pathname !== null ? window.location.pathname.substring(window.location.pathname.indexOf("/view/") + 5) : window.location.pathname;

		var _templateData = {
			title: contentTitle,
			link: window.location.pathname,
			uri: uriJs,
			siteName: siteName
		};

		if (isContentPage && entriesFromSessionStorage.filter(function (e) {
			return uriJs === e.uri;
		}).length === 0) {
			entriesFromSessionStorage.unshift(_templateData);
		}

		while (entriesFromSessionStorage.length > 10) {
			entriesFromSessionStorage.pop();
		}
	}

	if (recentlyViewedEntries) {
		recentlyViewedEntries.innerHTML = '';
		var recentlyViewedEntriesForSite = 0;
		entriesFromSessionStorage.filter(function (item) {
			return item.siteName === getSiteNameFromBody();
		}).forEach(function (item) {
			if (siteName !== null && item.siteName !== null && siteName.length > 0 && siteName.length == item.siteName.length && siteName.indexOf(item.siteName) === 0) {
				var template = getTemplate(item.title, item.link);
				allrecentlyViewedEntries.forEach(function (el) {
					el.appendChild(template);
				});
				//recentlyViewedEntries.appendChild(template);
				if (document.querySelector('body').classList.contains('loggedin')) {
					template.querySelector('a.save').addEventListener('click', function (event) {
						event.preventDefault();
						saveRecentlyViewedEntry(item);
					});
					template.querySelector('a.save').addEventListener('keydown', function (event) {
						if (event.keyCode == 13) {
							event.preventDefault();
							saveRecentlyViewedEntry(item);
						}
					});
				} else {
					template.querySelector('a.save').remove();
				}
				recentlyViewedEntriesForSite++;
			}
		});

		if (allrecentlyViewedEntriesCount.length > 1) {
			allrecentlyViewedEntriesCount.forEach(function (el) {
				el.innerHTML = '(' + recentlyViewedEntriesForSite + ')';
			});
		} else {
			recentlyViewedEntriesCount.innerHTML = '(' + recentlyViewedEntriesForSite + ')';
		}
		var totalCount = parseInt(myEntriesButtonCount.innerHTML.replace(/\D/g, ''));
		totalCount += recentlyViewedEntriesForSite;
		if (allEntriesButtonCount.length > 1) {
			allEntriesButtonCount.forEach(function (el) {
				el.innerHTML = '(' + totalCount + ')';
			});
		} else {
			myEntriesButtonCount.innerHTML = '(' + totalCount + ')';
		}
	}

	// Reserialize the array and store it in sessionStorage
	sessionStorage.setItem('recentlyViewedEntries', JSON.stringify(entriesFromSessionStorage));

	// Scrolling fix
	var chapterBody = document.querySelector(".container-chapterBody");
	if (chapterBody) {
		var lastScroll = 0;
		document.querySelector(".content-page").addEventListener('click', function (e) {
			var anchor = e.target.closest('a');
			if (anchor !== null) {
				if (anchor.hasChildNodes()) {
					var path = anchor.hash;
					path = path.split("#")[1];
					var el = document.getElementById(path);
					if (el) {
						var children = anchor.childNodes;
						for (var i = 0; i < children.length; i++) {
							//if(children[0].tagName === "SUP"){
							if (anchor.hash) {
								e.preventDefault();
								var yOffset = document.querySelector(".c-Appbar").offsetHeight;
								var y = el.getBoundingClientRect().top + window.pageYOffset;
								//el.scrollIntoView(true);
								if (y < lastScroll) {
									y = el.getBoundingClientRect().top + window.pageYOffset - yOffset;
									window.scrollTo({ top: y, behavior: 'smooth' });
								} else {
									window.scrollTo({ top: y, behavior: 'smooth' });
								}
								lastScroll = y;
							}
						}
					}
				}
			}
		}, false);
	}
}

initializeRecentlyViewedSearches();
initializeRecentlyViewedEntries();
