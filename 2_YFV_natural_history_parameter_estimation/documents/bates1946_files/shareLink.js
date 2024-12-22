"use strict";

function copyShareableLink(targetElement) {
	var shareableLinkField = targetElement.parentElement.querySelector("[data-role='shareLinkCopyText']");
	if (shareableLinkField.tagName === "TEXTAREA") {
		shareableLinkField.select();
		try {
			shareableLinkField.setSelectionRange(0, Number.MAX_SAFE_INTEGER);
		} catch (probableRangeError) {
			console.log(probableRangeError);
		}
	} else {
		var selectRange = document.createRange();
		window.getSelection().removeAllRanges();
		window.getSelection().addRange(selectRange);
		selectRange.selectNode(shareableLinkField);
	}
	var copied = document.execCommand("copy");
	if (shareableLinkField.tagName !== "TEXTAREA") {
		window.getSelection().removeAllRanges();
	}
	var confirmationMessage = targetElement.parentElement.querySelectorAll("[data-role='shareLinkCopyConfirmation'")[0];
	var errorMessage = targetElement.parentElement.querySelectorAll("[data-role='shareLinkCopyError'")[0];
	if (copied) {
		var confirmationMessageText = targetElement.parentElement.querySelector("[data-role='shareLinkCopyConfirmationMessage'");
		confirmationMessage.className = confirmationMessage.className.replace(/fade-out-hidden/g, "");
		confirmationMessage.innerHTML = confirmationMessageText.innerHTML;
		var confirmationMessageFadeTimeout = setTimeout(function () {
			confirmationMessage.className = confirmationMessage.className + " fade-out ";clearTimeout(confirmationMessageFadeTimeout);
		}, 2500);
		var confirmationMessageHideTimeout = setTimeout(function () {
			clearTimeout(confirmationMessageHideTimeout);confirmationMessage.innerHTML = "";confirmationMessage.className = confirmationMessage.className + " fade-out-hidden";confirmationMessage.className = confirmationMessage.className.replace(/fade-out /g, "");confirmationMessage.removeAttribute("role");
		}, 3000);
		confirmationMessage.setAttribute("role", "alert");
		if (errorMessage.className.indexOf("display-none") === -1) {
			errorMessage.className = errorMessage.className + " display-none";
		}
		errorMessage.removeAttribute("role");
	} else {
		errorMessage.className = errorMessage.className.replace(/display-none/g, "");
		errorMessage.setAttribute("role", "alert");
		if (confirmationMessage.parentElement.className.indexOf("display-none") === -1) {
			confirmationMessage.parentElement.className = confirmationMessage.className + " display-none";
		}
		confirmationMessage.removeAttribute("role");
	}
}

function copyShareableLinkFromKeyDown(event, targetElement) {
	if (event.keyCode == 13) {
		copyShareableLink(targetElement);
	}
}
