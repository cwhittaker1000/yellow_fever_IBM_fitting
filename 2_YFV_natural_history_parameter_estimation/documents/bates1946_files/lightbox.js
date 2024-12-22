var LOADING_CONTENT = "Loading Content";
var OPEN_CLASS = "is-open";

function showNewLightbox(anchorElement, width, height, title, id, description) {
	var lightBoxHref = anchorElement.href.replace(/\?nojs=true/g, "");
	lightBoxHref = lightBoxHref.replace(/&nojs=true/g, "");
	showLightbox(lightBoxHref, width, height, title, id, description);
	return false;
}

function crateIframeEl (id, url) {
			var elem; 
			elem = document.createElement("iframe");
			elem.onload = function () { document.getElementById(id + "Actual").style.visibility = "visible"; document.getElementById(id + "LoadingScreen").style.display = "none"; };
			elem.setAttribute("src", url);
			elem.setAttribute("data-modal-id", id);
			elem.setAttribute("scrolling", 'no');
			elem.setAttribute("id", id + "Iframe");
			return elem;
}

function showLightbox(url, width, height, title, id, description) {
	var existingModal = document.getElementById(id);
	var elementToDisplay;
	var checkIframe = document.getElementById(id + "Iframe");
	var isNewIframe = false;
	if (existingModal === null) {
		elementToDisplay = crateIframeEl(id,url);
		isNewIframe = true;
	} else if (checkIframe){
		if (checkIframe.src != url){
			id = id + url.split("/").pop();
			elementToDisplay = crateIframeEl(id,url);
			isNewIframe = true;
		}else{
			elementToDisplay = document.getElementById(id + "Iframe");
		}
	}
	else {
		elementToDisplay = document.getElementById(id + "Iframe");
		if (elementToDisplay === null) {
			//this should be a pre-rendered modal that does not use an iframe
			//we do not have a fixed ID for these, but .micromodaljs-modal-body always should be present for them
			elementToDisplay = existingModal.getElementsByClassName("micromodaljs-modal-body")[0];
			isNewIframe = false;
		}
		
	}
	createLightbox(elementToDisplay, width, height, title, id, description, true,isNewIframe);
}

function showExistingDataAsLightbox(elementId, width, height, title, id, description) {
	var existingModal = document.getElementById(id);
	var modalBody = document.getElementById(elementId);
	if (existingModal === null) {
		if (String(height).length === String(height).replace(/[^0-9]+/g, "").length) {
			height = height + "px";
		}
		modalBody.className = modalBody.className.replace(/display-none/g, "");
		modalBody.className = modalBody.className + " micromodaljs-modal-body";
		modalBody.parentElement.removeChild(modalBody);
	}
	createLightbox(modalBody, width, height, title, id, description, false,false);
	return false;
}

function createLightbox(elementToDisplay, width, height, title, id, description, showLoadingScreen, isNewIframe) {
	var focusShouldBeSet = false;
	var isIframe = elementToDisplay.tagName === "IFRAME";
	if (document.getElementById(id) === null || isNewIframe) {

		var ua = window.navigator.userAgent;
		var isIE = /MSIE|Trident/.test(ua);
	
	
		var docBody = document.getElementsByTagName("body")[0];
		var modalWrapper = document.createElement("div");
		if (String(width).length === String(width).replace(/[^0-9]+/g, "").length) {
			width = width + "px";
		}
		if (String(height).length === String(height).replace(/[^0-9]+/g, "").length) {
			height = height + "px";
		}
		modalWrapper.setAttribute("id", id);
		modalWrapper.setAttribute("aria-hidden", true);
		modalWrapper.className = "micromodaljs-modal";
		
		var modalOverlay = document.createElement("div");
		modalOverlay.setAttribute("tabindex", -1);
		modalOverlay.setAttribute("data-micromodal-close", "");
		modalOverlay.addEventListener("click", function(e){
			if(e.target == e.currentTarget){
				e.preventDefault();
				closeLightboxFromOutsideModal(id);
			}
		});
		modalOverlay.className = "micromodaljs-modal-overlay";
		if (showLoadingScreen) {
			var modalLoadingScreen = document.createElement("div");
			modalLoadingScreen.setAttribute("id", id + "LoadingScreen");
			modalLoadingScreen.className = "loading";
			var modalLoadingSpinner = document.createElement("div");
			modalLoadingSpinner.className = "throbber circular";
			var modalLoadingDetails = document.createElement("span");
			modalLoadingDetails.innerHTML = LOADING_CONTENT;
			modalLoadingSpinner.appendChild(modalLoadingDetails);
			modalLoadingScreen.appendChild(modalLoadingSpinner);
			modalOverlay.appendChild(modalLoadingScreen);
		}
		var modalActual = document.createElement("div");
		modalActual.setAttribute("id", id + "Actual");
		modalActual.setAttribute("role", "dialog");
		modalActual.setAttribute("aria-modal", true);
		modalActual.setAttribute("aria-labelledby", id + "Title");
		modalActual.setAttribute("aria-describedby", id + "Description");
		modalActual.style.width = width;
		if (showLoadingScreen) {
			modalActual.style.visibility = "hidden";
		}
		modalActual.style.height = height;
		if (!isIframe) {
			modalActual.className = "micromodaljs-modal-container micromodaljs-modal-existing";
		}
		else {
			modalActual.className = "micromodaljs-modal-container";
			elementToDisplay.setAttribute("height", height);
		}
		var modalHeader = document.createElement("header");
		var modalTitle = document.createElement("h2");
		modalTitle.setAttribute("id", id + "Title");
		modalTitle.className = "screen-reader-text";
		modalTitle.innerHTML = title;
		var modalCloseButton = document.createElement("a");
		modalCloseButton.setAttribute("aria-label", "Close modal");
		modalCloseButton.setAttribute("role", "button");
		modalCloseButton.setAttribute("tabindex", "0");
		modalCloseButton.setAttribute("id", id + "Close");
		modalCloseButton.setAttribute("href", "#");
		modalCloseButton.onclick = function (e) {
			closeLightboxFromOutsideModal(id);
			e.preventDefault();
		};
		if (!isIframe) {
			modalCloseButton.onkeydown = function (e) { processCloseButtonKeyDown(e, id, elementToDisplay); };
		}
		modalCloseButton.className = "micromodaljs-modal-close-button";
		modalCloseButton.innerHTML = "<i class=\"fa fa-times ico-close c-IconButton c-IconButton--close\" aria-hidden=\"true\"></i>";
		var modalDescription = document.createElement("p");
		modalDescription.innerHTML = description;
		modalDescription.className = "screen-reader-text";
		modalDescription.setAttribute("id", id + "Description");
		var modalContent = document.createElement("div");
		modalContent.setAttribute("id", id + "Content");
		modalContent.className = "micromodaljs-modal-content";
		modalHeader.appendChild(modalTitle);
		modalHeader.appendChild(modalCloseButton);
		modalHeader.appendChild(modalDescription);
		modalContent.appendChild(elementToDisplay);
		modalActual.appendChild(modalHeader);
		modalActual.appendChild(modalContent);
		modalOverlay.appendChild(modalActual);
		modalWrapper.appendChild(modalOverlay);
	
		docBody.appendChild(modalWrapper);
	
		if (!isIframe) {
			focusShouldBeSet = true;
		}
	}
	else
	{
		//if we have left the modal and returned to it, we want to reset the focus
		focusShouldBeSet = true;
	}

	preventScrollingFromOutsideModal();
	MicroModal.show(id, { disableFocus: true });
	if (focusShouldBeSet) {
		if (isIframe) {
			setFocus(elementToDisplay.contentDocument, id + "Close");
		}
		else {
			setFocus(elementToDisplay, id + "Close");
		}
	}
	return false;
}

function processCloseButtonKeyDown(event, id, targetElement) {
	var continueEvent = true;
	if (event.keyCode == 13) {
		closeLightboxFromOutsideModal(id);
		continueEvent = false;
	}
	else {
		if (event.keyCode == 9) {
			setFocus(targetElement, id + "Close", true);
		}
	}
	if (!continueEvent) {
		event.preventDefault();
	}
	return continueEvent;
}

function closeLightboxFromOutsideModal(id) {
	MicroModal.close(id);
	restoreScrollingFromOutsideModal();
}

function preventScrollingFromOutsideModal() {
	document.getElementsByTagName("html")[0].className = document.getElementsByTagName("html")[0].className + " has-open-modal";
}

function restoreScrollingFromOutsideModal() {
	document.getElementsByTagName("html")[0].className = parent.document.getElementsByTagName("html")[0].className.replace("has-open-modal", "");
}

function restoreScrolling() {
	parent.document.getElementsByTagName("html")[0].className = parent.document.getElementsByTagName("html")[0].className.replace("has-open-modal", "");
}

function closeLightbox() {
	parent.MicroModal.close(window.frameElement.getAttribute('data-modal-id'));
	restoreScrolling();
}

function closeLightboxAndReload() {
	if (window.frameElement !== null) {
		closeLightbox();
		window.parent.location.reload(true);
	}
}

function closeLightboxAndReturnFalse() {
	if (window.frameElement !== null) {
		closeLightbox();
	}
	return false;
}

function returnToCloseButton(e, closeButtonId) {
	if (e.keyCode == 9 && !e.shiftKey) { 
		parent.document.getElementById(closeButtonId).focus(); 
		e.preventDefault();
	}
}

function disableTab(e) {
	if (e.keyCode == 9) { 
		e.preventDefault();
	}
}

function setFocus(elementToQuery, closeButtonId, excludeTabIndexMinusOne) {
	var focusable = elementToQuery.querySelectorAll("button, [href]:not([type='css/stylesheet']):not([as='script']):not([rel='stylesheet']), input:not([type='hidden']), select, textarea, [tabindex]:not([tabindex='-1'])");
    var lastFocusableElement = null;
    if (focusable.length > 0)
    {
    	lastFocusableElement = focusable[focusable.length - 1];
    	lastFocusableElement.addEventListener("keydown", function(e) {returnToCloseButton(e, closeButtonId);});
    }
    else
    {
    	parent.document.getElementById(closeButtonId).onkeydown = function(e) {disableTab(e);};
    }
    var focusableText = elementToQuery.querySelectorAll("[tabindex='-1']:not(body)");
    if (focusableText.length > 0 && excludeTabIndexMinusOne !== true)
    {
    	//use setTimeout to support Internet Explorer
    	var ieFocusTimeout = setTimeout(function() {focusableText[0].focus(); clearTimeout(ieFocusTimeout);}, 0);
    	//If there is no lastFocusableElement, this modal probably is text-only.  Check for focusable text
    	//and disable tabbing out of the modal
    	if (lastFocusableElement === null) {
    		focusableText[0].onkeydown = function(e) {returnToCloseButton(e, closeButtonId);};
    	}
    }
    else
    {
    	if (focusable.length > 0)
    	{
    		var ieFocusTimeout = setTimeout(function() { focusable[0].focus(); clearTimeout(ieFocusTimeout);}, 0);
    		var focusedValue = focusable[0].value;
    		if (focusable[0].value !== null && typeof focusable[0].value !== "undefined" && focusable[0].value.length > 0)
    		{
    		    focusable[0].value = "";
    		    focusable[0].value = focusedValue;
    		}
    	}
    	else
    	{
    		setTimeout(function() { parent.document.getElementById(closeButtonId).focus();}, 0);
    	}
    }
}