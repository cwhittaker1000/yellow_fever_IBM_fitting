'use strict';

var copyButtons = document.querySelectorAll('[data-role="shareLinkCopyButton"]');
var copyButtonsSize = copyButtons.length;
for (var i = 0; i < copyButtonsSize; i++) {
	copyButtons[i].onclick = function () {
		copyShareableLink(this.parentElement);
	};
	copyButtons[i].onkeydown = function (event) {
		copyShareableLinkFromKeyDown(event, this.parentElement);
	};
}
