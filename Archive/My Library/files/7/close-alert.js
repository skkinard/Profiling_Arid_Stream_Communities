/**
 *  @file
 *  This file handles the JS for closing sitewide alerts.
 */

(function ($) {

Drupal.behaviors.epaCloseAlert = {
  attach: function (context) {
    var $alerts = $('.sitewide-alert--information, .sitewide-alert--warning', context);
    var cacheLimitDays = 14;
    var closedHashes = {};
    var now = Date.now();

    function getClosedAlerts() {
      var closedHashes = localStorage.getItem('epaClosedAlerts')
      if (closedHashes == null) {
        closedHashes = {};
      }
      else {
        closedHashes = JSON.parse(closedHashes);
      }
      return closedHashes;
    }

    function setClosedAlerts(closedHashes) {
      return localStorage.setItem('epaClosedAlerts', JSON.stringify(closedHashes));
    }

    var hasLocalStorage = function () {
      try {
        localStorage.setItem('epa_test', 'test');
        localStorage.removeItem('epa_test');
        return true;
      }
      catch (e) {
        return false;
      }
    };

    if (hasLocalStorage) {
      closedHashes = getClosedAlerts();

      // Hide previously closed alerts.
      $alerts.each(function () {
        var $this = $(this);

        // Alert hasn't been dismissed, or was dismissed over cacheLimitDays ago
        if (closedHashes[$this.data('alert')] == null || (now - (cacheLimitDays * 1000 * 60 * 60 * 24) > closedHashes[$this.data('alert')])) {
          $this.removeClass('element-hidden');
        }
      });
    }

    // Add close button to appropriate alerts.
    var $activeAlerts = $alerts.not('.element-hidden');
    $activeAlerts.find('.sitewide-alert__content').prepend('<button class="sitewide-alert__close">Close</button>');

    // Hide alert when clicking the close button.
    $activeAlerts.find('.sitewide-alert__close').click(function () {
      var $alert = $(this).parents('.sitewide-alert');
      var alertHash = $alert.data('alert');
      var timestamp = Date.now();

      $alert.addClass('element-hidden');

      if (hasLocalStorage) {
        closedHashes = getClosedAlerts();
        closedHashes[alertHash] = timestamp;
        setClosedAlerts(closedHashes);
      }
    });
  }
}

})(jQuery);
