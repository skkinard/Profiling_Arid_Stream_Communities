/**
 * Copyright (c) 2010 Goodreads
 *
 * https://www.goodreads.com
 *
 * Usage:
 * <a class="update_goodreads_status" href="https://www.goodreads.com/update_status">Update Goodreads Status</a>
 * <script src="https://www.goodreads.com/assets/widgets/update_status.js" type="text/javascript"></script>
 **/
GOODREADS = window.GOODREADS || { ready: false, loaded: false };

(function(f) {
  var oldF = window.onload;
  if(typeof window.onload != 'function') {
    window.onload = f;
  } else {
    window.onload = function() {
      if(oldF) { oldF(); }
      f();
    }
  }
})(function() { GOODREADS.ready = true; });

(function() {
	if(GOODREADS && GOODREADS.loaded) { return; }
	GOODREADS.loaded = true;

	document.write('<link href="https://www.goodreads.com/assets/widgets/update_status.css" media="screen" rel="stylesheet" type="text/css" />');

	function init() {
		if(GOODREADS.ready) {
			var updateLinks = $$('.goodreads_update_status');
			updateLinks.each(function(updateLink) {
				updateLink.observe('click', updateStatusPopup);
			});
		} else {
			setTimeout(init, 500);
		}
	}

	function updateStatusPopup(evt) {
		var source = evt.element();
		var url = source.readAttribute('href'),
				window_title = 'update_status_popup',
				window_options = 'height=250,width=500';

		// attach some tracking params
		url += url.indexOf('?') > 0 ? '&' : '?';
		url += 'utm_medium=api&utm_campaign=update_status';
		if(window.location && window.location.host) {
			url += '&utm_source=' + window.location.host;
		}

		var popup = window.open(url, window_title, window_options);
		if(!popup) { // for safari
		 	popup = window.open('', window_title, window_options);
			popup.location.href = url;
		}
		if(window.focus) { popup.focus(); }

		evt.stop();
	}

	function includeDependency(scriptSrc, callback) {
		var script = document.createElement('script');
	  script.type = 'text/javascript';
	  script.onreadystatechange = function() {
			if(this.readyState == 'complete') {
	  		callback();
	  	}
	  }
	  script.onload = callback;
	  script.src = scriptSrc;
	  document.getElementsByTagName('head')[0].appendChild(script);
	}
	includeDependency('https://ajax.googleapis.com/ajax/libs/prototype/1.6.0.2/prototype.js', init);
})();
