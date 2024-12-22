(function($) {
	$('.add-to-download-zone').on(Tapestry.ZONE_UPDATED_EVENT, function(event, ui) {
		console.log('dd', ui);
		$(this).delay(3200).fadeOut(300);

		$(this)
			.parents('.component-add-to-download-button')
			.children('.dropdown-button')
			.children('.dropdown-menu')
			.children('.dropdown-ul')
			.hide();
	});

	$('.new-download-form-zone').on(Tapestry.ZONE_UPDATED_EVENT, function(event) {
		setTimeout(location.reload.bind(location), 1600);
	});
})(jQuery);
