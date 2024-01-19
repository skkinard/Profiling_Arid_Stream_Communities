$(document).ready(function(){

    // LIT-108978: Bind Google Analytics event to language change
    $(".js__lang_change").click(function(e){
        ga('send','event','toggle','click', 'french-toggle');
    });

    /* check if the user select more than 5 of interests */
    var last_valid_selection = null;
    $('#interestsTag').change(function(event) {
        if ($(this).val().length > 5) {
            alert('You can only choose 5');
            $(this).val(last_valid_selection);
        } else {
            last_valid_selection = $(this).val();
        }
    });

    /* advanced search */
    handleDateSelects();
    $("#issuetypefield").click(function(){
        handleDateSelects();
        if($(this).is(':checked')){
            $(this).val('on');
        }
        else{
            $(this).val('');
        }
    });
    /* advanced search */

    $("#saveSearchForm").submit(runSavedSearch); // saved search

    /* toc headers in journal box of toc page */
    var tocHeaders = $(".subject");
    appendHeadersForThisIssue(tocHeaders);
    /* / toc headers in journal box of toc page */

    /* journal page tabs */
    handleTabsInJournalPage(); //takes care of tabs behavior in journal home page

    /* on click of tab show/hide content accordingly */
    $('#tabs > ul li a').click(function() {
        $('#tabs ul li').removeClass('active-tab'); // remove active class from current active tab
        $(this).parent().addClass('active-tab'); // set active class to selected tab
        var currentTab = $(this).attr('href'); // find id of selected tab content
        $('#tabs > div').hide(); // hide all tabs
        $(currentTab).show(); // show the selected tab
        return false;
    });
    /* / journal page tabs */

    $('#category').change(function(){
        window.location = $(this).children(":selected")[0].value;
    });
    $("input[name='markall']").click(function() {
        var parentForm = $(this).parents("form");
        if (parentForm) onClickMarkAll(parentForm.get(0));
    });

    $(".viewMultiAbs").click(function() {
        var parentForm = $(this).parents("form");
        if (parentForm) submitArticles(parentForm.get(0), '/action/showMultipleAbstracts', false);
    });

    $(".dlCitations").click(function() {
        var parentForm = $(this).parents("form");
        if (parentForm) submitArticles(parentForm.get(0), '/action/showCitFormats', false);
    });

    $("select[name=cardType]").change(function () {
        var visa="1";
        $('#visaCard-note').css({'visibility':this.value==visa ? 'visible' : 'hidden'});
    });
});

function handleDateSelects() {
    var dateBlock = $("#issuetypefield").parent().next();
    if ($("#issuetypefield").is(':checked')) {
        $(dateBlock).find('select').attr('disabled', 'disabled');
    } else {
        $(dateBlock).find('select').removeAttr('disabled');
    }
}

function handleTabsInJournalPage() {
    $('#tabs > div').hide(); // hide all content of tabs
    $('#tabs > div:first').show(); // just open the first one
    $('#tabs ul.tabs li:first').addClass('active-tab');   // give correct class name to open tab
}

function appendHeadersForThisIssue(tocHeaders) {
    if (tocHeaders) {
        var htmlToAppend = "<ul class='bullet-red'>";
        for (var i = 0; i < tocHeaders.length; i++) {
            var currentHeader = tocHeaders[i];
            htmlToAppend += "<li><a href='#" + $(currentHeader).attr('id') + "'>" + $(currentHeader).html() + "</a></li>";
        }
        htmlToAppend += "</ul>";

        var inIssueSection = $(".in-issue");
        if (inIssueSection) inIssueSection.append(htmlToAppend);
    }
}

/* Function to open up a new browser window, without a navigation bar */
function newWindow(url) {
    var new_window;
    var windowProperties;
    windowProperties = "width=1000,height=700,top=30,left=230,toolbar=0,menubar=0,resizable=1,scrollbars=yes";
    if(new_window==null) {
       new_window = window.open(url,null,windowProperties);
    } else {
       new_window.document.replace(url);
    }
}

function newEbscoWindow(anchor) {
    var width = 680;
    var x = ($(window).width() - width) / 2 + $(window).scrollLeft();
    var link = $(anchor).attr("href");
    window.open(link,'','width=' + width + ',height=840,top=40,left=' + x + ',scrollbars=yes');
}

function runSavedSearch() {
    var params = constructParamsForURI();
    jQuery.ajax({
        url: 'doSaveSearch',
        type: 'POST',
        data: params,
        success: function(data, status) {
            $('#searchSavedConfirm').html(data);
        },
        error: function (XMLHttpRequest, textStatus, errorThrown) {
            $('#searchSavedConfirm').html('Please login to save a search.');
        }
    });

    return false;
}

function constructParamsForURI() {
    // construct map with key param name and value the param value
    var data = {
        perform: 'save-search',
        ajax: 'true',
        AllField: jQuery("input[name=AllField]").val(),
        Title: jQuery("input[name=Title]").val(),
        Contrib: jQuery("input[name=Contrib]").val(),
        Abstract: jQuery("input[name=Abstract]").val(),
        stemming: jQuery("input[name=stemming]").val(),
        earlycite: jQuery("input[name=earlycite]").val(),
        AfterYear: jQuery('select[name=AfterYear] option:selected').val(),
        AfterMonth: jQuery('select[name=AfterMonth] option:selected').val(),
        BeforeYear: jQuery('select[name=BeforeYear] option:selected').val(),
        BeforeMonth: jQuery('select[name=BeforeMonth] option:selected').val(),
        startPage: jQuery("input[name=startPage]").val(),
        pageSize: jQuery("input[name=pageSize]").val(),
        sortBy: jQuery("input[name=sortBy]").val(),
        alertme: jQuery('input:radio[name=alertme]:checked').val(),
        saveSearchName: jQuery("#searchSavedQuery").val()
    };

    // handle multiple values for publications in url
    var pubs = "";
    $("input[name=publication]:checked").each(function() {
       pubs += "publication=" + encodeURIComponent($(this).val()) + "&";
    });

    // start building the params string
    var params= "";
    for (var key in data) {
        params += key + "=" + encodeURIComponent(data[key]) + "&";
    }
    // append in params the publications params
    params += pubs;

    return params;
}

function GoTo(sel, targetstr)
{
  var index = sel.selectedIndex;
  if (sel.options[index].value != '') {
	 if (targetstr == 'blank') {
	   window.open(sel.options[index].value, 'win1');
	 } else {
	   var frameobj;
	   if ((frameobj = eval(targetstr)) != null)
		 frameobj.location = sel.options[index].value;
	 }
  }
}
// ToDo: move RUM into bundler
!function (w, nav) {
    var perf = w.performance,
        lit = w.LIT || (w.LIT={}),                              // bootstrap LIT object
        beaconData = lit.beaconData || (lit.beaconData = []);   // bootstrap beaconData

    if (perf && nav.sendBeacon) {
        // this is window
        this.addEventListener('unload', function () {
            var table = {},
                message = {},
                values = message.k = [],
                data = message.d = {},
                l = location;

            // maps a value to index in table
            function map(val) {
                return typeof table[val] === 'undefined' ? (table[val] = values.push(val) - 1) : table[val];
            }

            // compacts the 'what' object by converting each key and value into index to lookup table
            function compact(what, where, key, val, type) {
                where = where || {};
                for (key in what) {
                    val = what[key];
                    type = typeof val;
                    // we will skip functions
                    if (type !== 'function') {
                        where[map(key)] = type === 'object' ? compact(val) // handle objects recursively
                            : map(type !== 'number' ? val
                                : val > 1000 ? Math.round(val)             // trim precision of numbers
                                    : Math.round(val * 1000) / 1000            // to 3 digits
                            );
                    }
                }
                return where;
            }

            // Add Literatum Beacon data
            if (beaconData && beaconData.length) data.lit = compact(beaconData);

            // start with main page performance, we will get most indexes small numbers
            data.page = compact(perf);

            // add stats for external resources if available
            if (perf.getEntries) {
                for (var i = 0, entries = data.entries = perf.getEntries(); i < entries.length; i++) {
                    entries[i] = compact(entries[i]);
                }
            }

            // create payload to be sent
            message = JSON.stringify(message).
            replace(new RegExp(l.protocol + "//" + l.host, "gi"), "").
            replace(/"(\d+)"/g, "$1"); // strip origin from URLs and un quote numbers

            //console.log(message.length);
            nav.sendBeacon('/rum', message);
        });
    }
}(window, navigator);

/**	user clicks "Mark or unmark all items" checkbox */
function onClickMarkAll(aForm, aNamePrefix)
{
    if (aForm instanceof String) aForm = document.forms[aForm];
    if (! aNamePrefix) {
		aNamePrefix = "";
	}

    var markall;

    try {
        markall = aForm.markall.checked;
    }
    catch(err) { // markall might not be in the form
        markall = $('markall').checked;
    }

	markAllCheckboxes(aForm, aNamePrefix, markall);
}

function submitArticles(aForm, action, errorMessage) {
	submitMultiArticles(aForm, action, false, errorMessage);
}

function validateNRCRecommendationForm(form, alert1, alert2, alert3) {
    if (!isEmail(form.mailToAddress)) {
        alert(alert1);
        return false;
    } else if (form.doi.type != 'hidden' && countSelected(form.doi) < 1) {
        alert(alert2);
        return false;
    } else if(!isEmail(form.fromEmail)){
        alert(alert3);
        return false;
    }
    return true;
}