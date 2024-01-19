;jQuery(function(){
    //var home = new lucidCarousel();
    //home.config({visibilityArrows: true }).init('home-carousel');

    jQuery('.home-carousel-container-list').slick({
        infinite: true,
        slidesToShow: 3,
        slidesToScroll: 3,
        arrows: true,
        responsive: [
            {
                breakpoint: 640,
                settings: {
                    slidesToShow: 2,
                    slidesToScroll: 2
                }
            }
        ]
    });
    var homeFeatures = ['.feature-one ul','.feature-two ul','.feature-three ul', '.home-author ul'];
    for(var i = 0; i < homeFeatures.length; i++){
        jQuery(homeFeatures[i]).slick({
            infinite: true,
            slidesToShow: 5,
            slidesToScroll: 5,
            arrows: true,
            responsive: [
                {
                    breakpoint: 1024,
                    settings: {
                        slidesToShow: 4,
                        slidesToScroll: 4
                    }
                },
                {
                    breakpoint: 768,
                    settings: {
                        slidesToShow: 2,
                        slidesToScroll: 2
                    }
                }
            ]
        });
    }

    jQuery('.featured-books').slick({
        infinite: true,
        slidesToShow: 5,
        slidesToScroll: 5,
        arrows: true,
        responsive: [
            {
                breakpoint: 780,
                settings: {
                    slidesToShow: 4,
                    slidesToScroll: 4
                }
            },
            {
                breakpoint: 640,
                settings: {
                    slidesToShow: 2,
                    slidesToScroll: 2
                }
            },
            {
                breakpoint: 500,
                settings: {
                    slidesToShow: 1,
                    slidesToScroll: 1
                }
            }
        ]
    });

    /********
     * Cookies
     ********/
    function setCookie(cname,cvalue,exdays) {
        var d = new Date();
        d.setTime(d.getTime() + (exdays*24*60*60*1000));
        var expires = "expires=" + d.toGMTString();
        document.cookie = cname+"="+cvalue+"; "+expires+'; path=/';
    }
    function getCookie(cname) {
        var name = cname + "=";
        var ca = document.cookie.split(';');
        for(var i=0; i<ca.length; i++) {
            var c = ca[i];
            while (c.charAt(0)==' ') c = c.substring(1);
            if (c.indexOf(name) != -1)
                return c.substring(name.length, c.length);
        }
        return "";
    }
    /********
     * Font Resize
     ********/
    var fontCookie = "utp_fontSize";
    function updateFontSize(size){
        var target = jQuery('body, html');
        //remove selected
        jQuery('#font-size li.selected').removeClass('selected');
        //clean up body
        target.removeClass('small-font');
        target.removeClass('large-font');
        if(size+'-font' != 'medium-font') {
            target.addClass(size+'-font');
        }
        setCookie(fontCookie,size,60)
        jQuery('#font-size li.'+size+'-font').addClass('selected');
    }
    jQuery('#font-size li').click(function(){
        if(!jQuery(this).hasClass('selected')){
            var size = jQuery(this).attr('class').split('-font')[0];
            updateFontSize(size);
        }
    });
    if(getCookie(fontCookie) == '')
        setCookie(fontCookie,'medium',160);
    updateFontSize(getCookie(fontCookie));
    /********
     * Footer Mobile
     ********/
    jQuery('.footer-nav h4').click(function(){
       if(jQuery(this).parent().hasClass('mobile-drop')){
            jQuery(this).parent().removeClass('mobile-drop');
       }else {
            jQuery('.footer-nav .mobile-drop').removeClass('mobile-drop');
            jQuery(this).parent().addClass('mobile-drop');
       }
    });

    /********
     * Toggle content in volumes
     ********/
    //Toggle show more/less
    jQuery('.volumes-read-more').live('click', function(e){
        e.preventDefault();
        var contentToToggle = jQuery(this).siblings('.read-more-content');
        if ( contentToToggle.is(":visible") ){
            contentToToggle.hide();
            jQuery(this).text('Read More');
        } else{
            contentToToggle.show();
            jQuery(this).text('Read Less');
        }
    });

    //Toggle French/English content in volumes
    var englishToggleButton = jQuery(".toggle-button .toggle-english"),
        frenchToggleButton  = jQuery(".toggle-button .toggle-french");

    englishToggleButton.live("click", function(){
        var englishContent = jQuery(this).parent().siblings('.toggle-content').find('.english-content'),
            frenchContent  = jQuery(this).parent().siblings('.toggle-content').find('.french-content');
        frenchContent.hide();
        englishContent.show();
        jQuery(this).addClass("active");
        jQuery(this).siblings(".toggle-french").removeClass("active");
    });

    frenchToggleButton.live("click", function(){
        var englishContent = jQuery(this).parent().siblings('.toggle-content').find('.english-content'),
            frenchContent  = jQuery(this).parent().siblings('.toggle-content').find('.french-content');
        frenchContent.show();
        englishContent.hide();
        jQuery(this).addClass("active");
        jQuery(this).siblings(".toggle-english").removeClass("active");
    });

    //If URL contains certain params,
    //Scroll below featured books section
    if( window.location.href.indexOf("cat=") > -1 || window.location.href.indexOf("dir=") > -1 || window.location.href.indexOf("product_form=") > -1 || window.location.href.indexOf("product_config_type=") > -1 ) {
        // if featured books section exists
        if ( jQuery('#featuredBooks').length > 0 ){
            var listingsOffset = jQuery('.content-wrap.main').offset().top;
            jQuery("html, body").animate({ scrollTop: listingsOffset }, 700);
        }
    }

    //Switch product view image on selection of different format
    //ie. hitting a radio button change's product image if it is not default
    function changeProductImageOnSelect(){
        if( jQuery('body').hasClass('catalog-product-view') && !jQuery('body').hasClass('demac-enhancedquickview-index-index')){
            var defaultImage = jQuery('.product-image-container .product-image img').attr('src');
            var radioSelector = jQuery('.publishing-price-selections').find('label');
            radioSelector.click(function(){
                var radioImgSrc = jQuery(this).parent().find('.price-box').attr('data-img-src');
                if ( radioImgSrc !== defaultImage && radioImgSrc !== undefined ){
                    jQuery('.product-image-container .product-image img').attr('src', radioImgSrc);
                    defaultImage = jQuery('.product-image-container .product-image img').attr('src');
                }
            });
        }
    }
    changeProductImageOnSelect();

    //Product format on side navigation
    //Functionality
    jQuery('.ebook-title').live('click',function(e){
        e.preventDefault();
        if(jQuery(this).hasClass('checked')){
            jQuery(this).removeClass('checked');
        }else {
            jQuery(this).addClass('checked');
        }
    });
    function checkEbookOnLoad(){
        var ebookTitle = jQuery('.ebook-title'),
            ebookChild = jQuery('.ebook-child');
        //If child boxes are checked
        ebookChild.each(function(){
            if( jQuery(this).hasClass('checked') ){
                ebookTitle.addClass('checked');
            }
        });
    }

    checkEbookOnLoad();


    /********
     * Listing page Show More
     ********/
    if(jQuery('.toggle-cats').length){
        jQuery('.toggle-cats').on('click',function(){
            jQuery(this).parents('dd').toggleClass('show-less');
        });
    }
    if(jQuery('#wp-archive-list').length){
        jQuery('#wp-archive-list').on('click', '.toggle-cats', function () {
            jQuery('#wp-archive-list').toggleClass('show-less');
        });
    }

    /**************
     * Swap Block */
    function swapBlock(items,content,elm,fn){
        items.on('click',(elm === undefined)?'> li':elm,function(){
            var i = 0;
            items.children().each(function(){jQuery(this).removeClass('active')});
            jQuery(this).addClass('active');
            var index = jQuery(this).index();
            content.children().each(function(){
                jQuery(this).removeClass('active');
                if(fn !== undefined) {
                    fn.call(jQuery(this));
                }
                if(i == index){
                    jQuery(this).addClass('active');
                }
                i++;
            });
        });
        content.children().eq(0).addClass('active');
        items.children().eq(0).addClass('active');
    }
    if(jQuery('.swap-block').length){
        jQuery('.swap-block').each(function(){
            swapBlock(jQuery(this).find('.swap-items'),jQuery(this).find('.swap-content'));
        });
    }
    Validation.add('validate-cdn-postal', 'Please enter a valid Postal Code. For example A1A 1A1.', function(v) {
        return Validation.get('IsEmpty').test(v) || (/[A-Za-z]{1}\d{1}[A-Za-z]{1} \d{1}[A-Za-z]{1}\d{1}/.test(v));
    });

    /*******************
     * Advanced Search */
    if(jQuery('.catalogsearch-advanced-index #advanced-search-list').length && jQuery('.input-children-attr').length){
        swapBlock(jQuery('#advanced-search-list .swap-items'), jQuery('#advanced-search-list .swap-content'),'> label', function(){
            jQuery(this).find('input').each(function(){
                if(jQuery(this).attr('type') == 'radio'){

                }else {
                    jQuery(this).val('').parent().removeClass('validation-passed');
                }
            });
            jQuery(this).find('select').each(function(){
                jQuery(this).val('').parent().removeClass('validation-passed');
            });
        });
    }


    //Make tweets and posts same height
    function makeTweetsAndPostsSameHeight(){
        var recentTweets = jQuery('.recent-tweets');
        var recentPosts  = jQuery('.recent-posts');
        if( recentTweets.length > 0 && recentPosts.length > 0 ){
            var postsHeight = recentPosts.height();
            recentTweets.height(postsHeight);
        }
    }
    makeTweetsAndPostsSameHeight();

    jQuery(window).resize(function(){
        jQuery(this).trigger('windowResize');
    });


    var subjectFilter = jQuery('.filter-items-cat.listing-show-more');
    if(subjectFilter.length){
            if(subjectFilter.find('.m-selected-ln-item').length){
                subjectFilter.addClass('show-less');
            }
    }

    /*************
     * Read More */
    if(jQuery('.sidebarViewMore').length && jQuery('.viewMoreNext').length){
        jQuery('.sidebarViewMore').on('click','span',function(e){
            e.preventDefault();
            jQuery(this).parent('.sidebarViewMore').siblings('.viewMoreNext').toggleClass('expanded');
        });
    }
    var addressExam = jQuery('input[type=radio][data-address]');
    if(addressExam){
        var showToggle = jQuery('input[type=radio][data-show]');
        var addressBox = jQuery('.address-box');
        if(showToggle.length) {
            showToggle.on('click',function(){
                addressBox.hide();
            });
        }
        addressExam.on('click',function(){
            (jQuery(this).attr('data-address').toLowerCase() == 'true')? addressBox.show() : addressBox.hide();
        })
    }

    function checkTextHeight(text){
        if(text[0].scrollHeight > text[0].clientHeight){
            if(!text.hasClass('ellipse')){
                text.addClass('ellipse');
            }
        }
    }
    function changeImageHeight(product, imgHeight){
        product.each(function(){
            jQuery(this).find('.product-image').height(imgHeight);
        })
    }

    function checkProducts(mode){
        var product = mode.children('li');
        var imgHeight = null;
        product.each(function(){
            var _ = jQuery(this);
            var name = _.find('.product-name a');
            var author = _.find('.product-author');
            var image = _.find('.product-image-listings a img');
            if(name.length){
                checkTextHeight(name);
            }
            if(author.length){
                checkTextHeight(author);
            }
            if(image.length){
                var currentHeight = image.height();
                if(imgHeight == null || currentHeight > imgHeight){
                    imgHeight = currentHeight;
                }
            }
        })
        if(imgHeight != null) {
            changeImageHeight(product, imgHeight);
        }
    }

    var grid = jQuery('.products-grid');
    var list = jQuery('.products-list');
    if(grid.length){
        checkProducts(grid);
    }
    if(list.length){
        checkProducts(list);
    }
    jQuery(window).bind('windowResize', function(){
        var _ = this;
        if(_.isResized) clearTimeout(_.isResized);
        _.isResized = setTimeout(function(){
            makeTweetsAndPostsSameHeight();
            if(grid.length){
                checkProducts(grid);
            }
            if(list.length){
                checkProducts(list);
            }
        }, 500);
    });

    //
    //Listing page
    //
    //if ( ! Modernizr.objectfit ) {
    //    var imgContainer = jQuery('.book-cover-container');
    //    if(imgContainer.length > 0){
    //        imgContainer.each(function () {
    //            var container = jQuery(this),
    //                imgUrl = container.find('.book-image').prop('src');
    //            if (imgUrl) {
    //                container
    //                    .css('backgroundImage', 'url(' + imgUrl + ')')
    //                    .addClass('compact-object-fit');
    //            }
    //        });
    //    }
    //}

    function createSearchObject(){
        var params = window.location.search.replace("?","").split("&");
        if(typeof searchObj === 'undefined'){
            window.searchObj = {};
        }
        for(var i = 0; i < params.length; i++){
            var _query = params[i].split('=');
            searchObj[_query[0]] = _query[1];
        }
    }
    createSearchObject();
    var searchResults = jQuery('.catalogsearch-result-index');
    if(searchResults.length && typeof searchObj.cat !== 'undefined'){
        var li = jQuery('.pagination').children();
        for(var i = 0; i < li.length; i++){
            var _ = jQuery(li[i]);
            if(!_.hasClass('current')){
                var a = _.find('a');
                var hasQuery = (a.attr('href').indexOf('?') != -1);
                a.attr('href',a.attr('href') + ((!hasQuery)?'?':'') + '&cat=' + searchObj.cat);
            }
        }
    }
});

