Donations = Class.create();

Donations.prototype = {

    initialize: function(defaultCharity, redirectUrl, addUrl, removeUrl, roundUrl, minDonation, selector){
        this.currentCharity = defaultCharity;
        this.redirectUrl = redirectUrl;
        this.addUrl = addUrl;
        this.removeUrl = removeUrl;
        this.roundUrl = roundUrl;
        this.reloadPage = true;
        this.minDonation = minDonation;
        this.selector = selector;
    },

    initPopup: function() {
        this.togglePopup();

        width = window.innerWidth || document.documentElement.clientWidth;
        height = window.innerHeight || document.documentElement.clientHeight;
        if($('charity-select-box')){
            hMax = 0;
            maxSize = height - 300;

            $$("div[id^=charity-cont-] .content").each(function (contentDiv) {
                contentDiv.setStyle({maxHeight: maxSize+'px'});

                contentDivHeight = contentDiv.getHeight() + 20;
                if (contentDivHeight > hMax) {
                    hMax = contentDivHeight;
                }
               // alert(contentDiv.down('img'));
                img = contentDiv.down('img');
                if(img){
                    imgHeight = img.style.maxHeight.replace('px', '');
                    if (img && imgHeight > hMax) {
                        hMax = imgHeight;
                    }
                }
            });

            if ($('charity-select-box').getHeight() > hMax) {
                $('charity-content-box').setStyle({'height':$('charity-select-box').getHeight() + 'px'});
            } else {
                $('charity-select-box').setStyle({height:hMax + 'px'});
                $('charity-content-box').setStyle({height:hMax + 'px'});
            }

            this.changeCharity(this.currentCharity);
        }

        posLeft = (width - $('donations_popup').getWidth()) / 2;
        posLeft += 'px';
        posTop = (height - $('donations_popup').getHeight()) / 2;
        posTop += 'px';
        $('donations_popup').setStyle({left:posLeft, top:posTop});

        this.togglePopup();
    },

    togglePopup: function(noReloadFlag){
        if(noReloadFlag){
            this.reloadPage = false;
        }
        $('donations_popup').toggle();
        $('donations-overlay').toggle();
    },

    changeCharity: function(charityId){
        if(!$('donation_charity')){
            return;
        }
        if (charityId) {
            $$("div[id^=charity-cont-]").each(function (el) {
                if (el.id == 'charity-cont-' + charityId) {
                    el.setStyle({display:'block'});
                } else {
                    el.setStyle({display:'none'});
                }
            });
        }
        currentCharity = charityId;
        $$('.charity-item').each(function (el) {
            if (el.id == 'charity-' + charityId) {
                el.addClassName('current');
            } else {
                el.removeClassName('current');
            }
        });
        $('donation_charity').value = charityId;
    },

    showSpinner: function(){
        spinner = $('donations-spinner');
        spinner.setStyle({width: $('dоnation_content').getWidth() + 'px', 'min-height': '60px', height: $('dоnation_content').getHeight() + 'px', display: 'block'});
        $('dоnation_content').setStyle({display: 'none'});
    },

    submitDonation: function(formEl, params){

        if(!formEl){
            return false;
        }
        form = new VarienForm(formEl.id);
        if(form.validator && form.validator.validate()){
            donation = $('donation_value').value;
            if($('donation_charity')){
                charity = $('donation_charity').value;
            } else {
                charity = false;
            }
            url = formEl.action;

            if(this.reloadPage){

                if(parseFloat(donation) < parseFloat(this.minDonation)){
                    $$('#donation-actions div.minimum_donation')[0].setStyle({color: 'red'});
                    $$('#donation-actions input#donation_value')[0].setStyle({'border-color': 'red'});
                    return false;
                }

                this.showSpinner();
                if(!params){
                    params = $(formEl).serialize()
                }
                redirectTo = this.redirectUrl;
                new Ajax.Request(url,{
                    method: 'post',
                    parameters: params,
                    onSuccess:function(transport){
                        setLocation(redirectTo);
                    }
                });
            } else {
                this.togglePopup();
                if($('donations_roundup')){
                    $('donations_roundup').remove();
                }
                automaticRoundAmount(false, true);
                //this.handleDonation(this.addUrl, {donation: donation, charity: charity});
            }
        }
    },

    submitSimpleDonation: function(url){
        donation = $('donation_value_product').value;

        params = {donation: donation, charity: false}

        if(parseFloat(donation) < parseFloat(this.minDonation)){
            $$('div.minimum_donation')[0].setStyle({color: 'red'});
            $$('input#donation_value_product')[0].setStyle({'border-color': 'red'});
            return false;
        }

        this.fadeDiv($('product_donation_form'));

        redirectTo = this.redirectUrl;
        new Ajax.Request(url,{
            method: 'post',
            parameters: params,
            onSuccess:function(transport){
                setLocation(redirectTo);
            }
        });
    },

    removeCartDonation: function(){
        this.handleDonation(this.removeUrl, {donation: 0, charity: 0});
    },

    removeDonation: function(isCart){
        if(isCart){
            this.reloadPage = false;
        }
        if(this.reloadPage){
            this.showSpinner();
            redirectTo = this.redirectUrl;
            new Ajax.Request(this.removeUrl,{
                method: 'post',
                onSuccess:function(transport){
                    setLocation(redirectTo);
                }
            });
        } else {
            this.togglePopup();
            donations.fadeDiv($$('#checkout-review-table tfoot')[0]);
            params = {donation: 0, charity: false, status: 1};
            var request = new Ajax.Request(this.roundUrl, {
                method: 'post',
                parameters: params,
                onComplete: function(transport){

                    var jsonResponse=transport.responseText;

                    if(jsonResponse.error){
                        alert("<?php echo Mage::helper('mageworx_donations')->__('Error Occured'); ?>");
                        return false;
                    }
                    else{
                        $$('#checkout-review-table tfoot')[0].replace(jsonResponse.evalJSON().outputHtml);
                    }
                }
            }
        );
        }
    },

    addCartDonation: function(isCart){
        donation = $('donation').value;
        if($('charity-select')){
            charity = $('charity-select').value;
        } else {
            charity = false;
        }
        this.handleDonation(this.addUrl, {donation: donation, charity: charity});

    },

    handleDonation: function (Url, parameters){
        if(parameters){
            params = parameters;
        } else {
            params = Form.serialize($("donation-form"));
        }
        this.fadeDiv($('donations-block'));

        var request = new Ajax.Request(
            Url,
            {
                method: 'post',
                parameters: params,
                onComplete: function(transport){

                    var jsonResponse=transport.responseText;
                    json = false;
                    try {
                        json = jsonResponse.evalJSON();
                    } catch (e) {
                    }
                    if(json.error){
                        alert(json.error);
                        donations.unfadeDiv($('donations-block'));
                        return false;
                    }
                    else{
                        $('donations-block').replace(json.outputHtml);
                        if (this.selector !== null){
                            var el = $$('#donations-totals #shopping-cart-totals-table').first().innerHTML;
                            if ($$(this.selector).length > 0){
                                $$(this.selector).first().update(el);
                            } else {
                                if ($$('.cart-totals #shopping-cart-totals-table').first()) {
                                    $$('.cart-totals #shopping-cart-totals-table').first().update(el);
                                } else {
                                    $$('.totals #shopping-cart-totals-table').first().update(el);
                                }
                            }
                        }
                    }
                }.bind(this)
            }
        );
    },

    getCharityInfo: function($obj){
        elements = $('charity-images').childElements();
        for(i = 0; i < elements.length; i++) {
            $(elements[i].readAttribute('id')).hide();
        }
        if ($('charity-'+$obj.value)) {
            $('charity-'+$obj.value).show();
        }
    },

    fadeDiv: function(el) {
        if (!el) return;

        var spinnerDiv = new Element('div');
        spinnerDiv.id = 'donation-spinner';
        spinnerDiv.setStyle({
            top: el.offsetTop + 'px',
            left: el.offsetLeft + 'px',
            height: el.offsetHeight + 'px',
            width: el.offsetWidth + 'px'
        });

        el.insert(spinnerDiv);
        el.style.opacity = 0.2;

    },

    unfadeDiv: function(el) {
        el = $('donations-block');
        $('donation-spinner').remove();
        el.style.opacity = 1;
    }

}
