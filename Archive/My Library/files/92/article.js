var NrcLayers = {
    // get with ajax the response of a showFullPopup action and build tables layer
    openLayer: function() {
        var itemId = $(this).attr("id");
        var artDoi = $(this).attr("doi");
        if (itemId != 'undefined' && artDoi != 'undefined') {
            $.ajax({
                url: '/action/showFullPopup',
                data: "id=" + itemId + "&doi=" + artDoi,
                success: NrcLayers.buildLayer
            });
        }
    },

    // builds tables layer similar to figures layer
    buildLayer: function(data) {
        var response = $(data);
        var layer = NrcLayers.finalizeLayer(response); //attaches js behavior to elements
        var content = $('#content');

        // close already open layer (if any), we are now going to create a new one
        NrcLayers.hideLayer();

        // show overlay and background and append layer in overlay
        $("#background").show();
        $("#overlay").show();
        content.append(layer);

        $("#overlay").vCenter(); // position layer in middle of user's viewport

        NrcLayers.centerPrevNextLinks(); // vertically center links

        $("#closeBtn").click(NrcLayers.hideLayer); // attach close behaviour in close button
        $(".paginate").click(NrcLayers.openLayer); // attach prev/next navigation behavior in prev/next buttons
    },

    // mainly takes care of prev/next links in layer
    finalizeLayer : function(response) {
        var tableImg = response.find(".tableImg"); // get the image from response
        if (tableImg.length > 0) {
            response = NrcLayers.constructTableLinks(response);
        } else {
            response = NrcLayers.constructFiguresLinks(response);
        }

        return response;
    },

    constructFiguresLinks: function(response) {
        var figure = response.find("#figWrapper img"); // get the figure from response
        var currFigureId = figure.attr("id"); // figure's id

        var openFiguresLayer = $(".openFigLayer"); // figures in article
        var currIndex = $("#" + NrcLayers.escapeJQString(currFigureId) + ".openFigLayer").index(".openFigLayer"); // get index of figure currently user views
        var doi = $(openFiguresLayer.get(0)).attr("doi"); // get doi of article

        NrcLayers.constructPrevNextLinks(currIndex, openFiguresLayer, response, doi);

        // append correct current index in html
        response.find("#currentPage").html(currIndex+1);
        response.find(".magnifier").loupe(); // add image magnifier behavior

        return response;
    },

    constructTableLinks: function(response) {
        var tableImg = response.find(".tableImg"); // get the image from response
        var currTableId = tableImg.attr("id"); // table's id

        var openTablesLayer = $(".openTablesLayer");
        var tablesCount = openTablesLayer.length; // get total number of tables in article
        var currIndex = $("#" + NrcLayers.escapeJQString(currTableId)).index(".openTablesLayer"); // get index of table currently user views
        var doi = $(openTablesLayer.get(0)).attr("doi"); // get doi of article

        NrcLayers.constructPrevNextLinks(currIndex, openTablesLayer, response, doi);

        // append correct # of tables and current index in html
        response.find("#countOfTables").append(tablesCount);
        response.find("#currentPageTables").html(currIndex+1);

        return response;
    },

    // constructs prev/next links in figures/tables viewer
    constructPrevNextLinks: function(currIndex, layer, response, doi) {
        // get id of previous item
        if (currIndex != 0) var prevId = $(layer.get(currIndex - 1)).attr("id");
        // get id of next item
        if (currIndex != currIndex - 1) var nextId = $(layer.get(currIndex + 1)).attr("id");

        // links to prev/next item
        var prevItem = response.find(".prevItem");
        var nextItem = response.find(".nextItem");

        // if there is prev/next item then add required attrs otherwise remove elements from response
        if (prevId)  prevItem.attr("id", prevId).attr("doi", doi);
        else  prevItem.remove();

        if (nextId) nextItem.attr("id", nextId).attr("doi", doi);
        else nextItem.remove();
    },

    // verically centers previous/next table links
    centerPrevNextLinks: function() {
        var prevItemLink = $(".prevItem");
        var nextItemLink = $(".nextItem");

        var layerHeight = $("#overlay").height();
        var top = -(layerHeight / 2);

        if (prevItemLink) prevItemLink.css('top', top+20);
        if (nextItemLink) nextItemLink.css('top', top);
    },

    // hide popup layer
    hideLayer: function() {
        $("#background").hide();
        $("#overlay").remove();
    },

    // open figures or tables layer depending on itemid
    openLayerForItem: function() {
        var itemId = $(this).attr("itemid"); // get itemid attr
        // if itemid contains two tables ids, then we have to pick one...pick first
        if (itemId.indexOf(' ') != -1) itemId = itemId.substring(0, itemId.indexOf(' '));
        var refElement = $("#" + itemId); // find element with id = itemid
        if (refElement) refElement.click(); // if element exists imitate a click
    },

    // attach tooltip behavior to given element and for given html to be added in tooltip
    buildToolTip: function(el, content) {
        el.qtip({
            content: "<span>" + content + "</span>",
            show: 'click',
            hide: 'unfocus',
            position: {
                adjust: {
                    screen: true // Keep the tooltip on-screen at all times
                }
            },
            style: {
                name: 'nrcTooltipStyle'
            }
       });
    },

    // build references tooltips
    buildRefTooltip: function(el) {
        var rid = el.attr("rid");
        var content = $("#" + rid);
        var html;
        var intRegex = /^\d+$/;
        if(intRegex.test(rid.charAt(rid.length-1))){
          html = content.html();
        }else{
          html = $("#"+rid).parent().find("span.numbering").text().replace(/&nbsp;/g,'') + content.html().replace(/&nbsp;/g,' ');
        }
        var div = $("<div />").html(html);
        div.children("script").remove()
        NrcLayers.buildToolTip(el, div.html());
    },

    // build footnotes tooltips
    buildSecFnTooltip: function(el) {
        var rid = el.attr("rid");
        var content = $("#" + rid);
        NrcLayers.buildToolTip(el, content.html());
    },

    // some ids include special chars like : or ., we need to escape those
    // source: http://docs.jquery.com/Frequently_Asked_Questions#How_do_I_select_an_element_by_an_ID_that_has_characters_used_in_CSS_notation.3F
    escapeJQString: function (input) {
        return input.replace(/(:|\.)/g,'\\$1');
    }
};

(function($) {
// custom tooltip style for nrc, last part is the name of the style
    $.fn.qtip.styles.nrcTooltipStyle = {
        background: '#E6E7E8',
        color: 'black',
        textAlign: 'left',
        width: 300,
        border: {
            width: 0,
            radius: 1,
            color: '#BE1E2E'
        },
        tip: 'topLeft',
        name: 'dark' // Inherit the rest of the attributes from the preset dark style
    };


    // centers element vertically in user's viewport
    $.fn.vCenter = function(options) {
        var pos = {
            sTop : function() {
                return window.pageYOffset
                        || document.documentElement && document.documentElement.scrollTop
                        || document.body.scrollTop;
            },
            wHeight : function() {
                return window.innerHeight
                        || document.documentElement && document.documentElement.clientHeight
                        || document.body.clientHeight;
            }
        };
        return this.each(function(index) {
            if (index == 0) {
                var $this = $(this);
                var elHeight = $this.height();
                var elTop = pos.sTop() + (pos.wHeight() / 2) - (elHeight / 2);
                $this.css({
                    position: 'absolute',
                    marginTop: '0',
                    top: elTop
                });
            }
        });
    };

})(jQuery);

$(document).ready(function() {
    // figures link, when clicked open figures layer, with first figure open
    $("#figures").click(function() {
        var figures = $(".openFigLayer");
        if (figures[0]) $(figures[0]).click();
    });

    // tables link, when clicked open tables layer, with first table open
    $("#tables").click(function() {
        var tables = $(".openTablesLayer");
        if (tables[0]) $(tables[0]).click();
    });

    $("#closeBtn").click(NrcLayers.hideLayer); // close button of layer
    $(".openTablesLayer").click(NrcLayers.openLayer); // opens tables layer
    $(".openFigLayer").click(NrcLayers.openLayer); // opens figures layer
    $(".openLayerForItem").click(NrcLayers.openLayerForItem); // opens tables or figure layer depending on id of element "clicked"

    $(".tooltip").each(function(){
        NrcLayers.buildRefTooltip($(this));
    });

    $(".secFnTrigger").each(function(){
        NrcLayers.buildSecFnTooltip($(this));
    });
});