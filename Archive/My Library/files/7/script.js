!function(i,a){a.behaviors.epa={attach:function(a){i("html.no-js",a).removeClass("no-js")}},a.behaviors.skiplinks={attach:function(a){var e=-1<navigator.userAgent.toLowerCase().indexOf("webkit"),t=-1<navigator.userAgent.toLowerCase().indexOf("opera");i("#main-content, #site-navigation",a).attr("tabindex",-1),(e||t)&&i('.skip-links a[href^="#"]',a).click(function(){var a="#"+this.href.split("#")[1];i(a).focus()})}},a.behaviors.epaNew={attach:function(a){var t=new Date;t=t.getTime(),i("ins[data-date]",a).each(function(){var a=i(this).data().date.replace(/\,/g,"/"),e=Date.parse(a)+2592e6;t<e&&i(this).addClass("new")})}},a.behaviors.tablesorter={attach:function(a){i("table.tablesorter",a).tablesorter()}},a.behaviors.accordion={attach:function(a){i(".accordion",a).each(function(){var t=i(this).find(".accordion-title"),s=t.next(".accordion-pane");s.addClass("is-closed"),t.each(function(){var e=i(this).next(".accordion-pane");i(this).click(function(a){i(this).hasClass("is-active")?(i(this).removeClass("is-active"),e.addClass("is-closed")):(t.removeClass("is-active"),s.addClass("is-closed"),i(this).addClass("is-active"),e.removeClass("is-closed")),a.preventDefault()})})})}},a.behaviors.headerImages={attach:function(a){i(".box",a).each(function(){var a=i(".image.view-mode-block_header:not(.caption, .block_header-processed)",this).first(),e=i(this);a.addClass("block_header-processed"),a.detach(),e.prepend(a)})}},a.behaviors.setLoginFocus={attach:function(a){i("body.page-user #edit-name").focus()}}}(jQuery,Drupal);