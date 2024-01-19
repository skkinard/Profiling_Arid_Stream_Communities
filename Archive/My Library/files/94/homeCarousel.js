/****************
 * Chris Soltar
 * Version 0.0.5
 ****************/
var lucidCarousel = lucidCarousel || function(){
    /*****************
     *@ this.visibilityArrows  |  Default: undefined, Options: true
     *****************/
    this.init = function(target){
        this.carousel = '.'+target;
        this.wrapper = jQuery(this.carousel);
        if(this.wrapper.length == 0 )
            return false;
        this.left = jQuery((this.carousel+'-left'));
        this.right = jQuery(this.carousel+'-right');
        this.content = jQuery(this.carousel + '-content');
        this.container = jQuery(this.carousel +'-container ul');
        if(this.container.find('li').length == 0) {
            this.right.hide()
            this.left.hide()
            return false;
        }
        this.getWidth();
        this.setPosition();
        this.run();
        this.updateArrow();
        this.resized();
        return this;
    }
    this.run = function(){
        var currentInst = this;
        this.left.click(function(e){
            var instance = currentInst;
            e.preventDefault();
            instance.scrollLeft();
        });
        this.right.click(function(e){
            var instance = currentInst;
            e.preventDefault();
            instance.scrollRight();
        });
        return this;
    }
    this.scrollRight = function(){
        if((this.currentPosition - this.width < this.maxWidth)){
            return false;
        }
        this.container.css({'left': this.currentPosition - this.width});
        this.setPosition();
        this.updateArrow();
        return this;
    }
    this.scrollLeft = function(){
        if(this.currentPosition >= 0){
            return false;
        }
        if(this.currentPosition + this.width > 0){
            this.container.css({'left': 0 });
        }else {
            this.container.css({'left': this.currentPosition + this.width });
        }
        this.setPosition();
        this.updateArrow();
        return this;
    }
    this.updateArrow = function(){
        if(this.visibilityArrows != true){
            (this.currentPosition >= 0)? this.left.hide() : this.left.show();
            (this.currentPosition - this.width < this.maxWidth)? this.right.hide() : this.right.show();
        }else {
            (this.currentPosition >= 0)? this.left.css({'visibility':'hidden'}) : this.left.css({'visibility':'visible'});
            (this.currentPosition - this.width < this.maxWidth)? this.right.css({'visibility':'hidden'}) : this.right.css({'visibility':'visible'});
        }
        (this.currentPosition - this.width < this.maxWidth && this.currentPosition >= 0)? this.hideButtons = true : this.hideButtons = false;
        this.displayArrows();
        return this;
    }
    this.displayArrows = function(){
        (this.hideButtons)? this.content.addClass('noButtons') : this.content.removeClass('noButtons');
        return this;
    }
    this.getWidth = function(){
        this.width = (this.container.find('li')[0].getBoundingClientRect().width);
        this.maxWidth = -(this.container.find('li').length - Math.round(this.container.css('width').split('px')[0] / this.width)) * this.width;
        return this;
    }
    this.setPosition = function(){
        this.currentPosition = parseFloat(this.container.css('left').split('px')[0]);
        return this;
    }
    this.resized = function(){
        var currentInst = this;
        jQuery(window).resize(function(){
            jQuery(this).trigger('windowResize');
        });
        jQuery(window).bind('windowResize', function(){
            var instance = currentInst;
            if (instance.isResized) clearTimeout(instance.isResized);
            instance.isResized = setTimeout(function(){
                /* Reset carousel*/
                instance.container.css({'left': 0});
                instance.setPosition();
                instance.getWidth();
                instance.updateArrow();
            }, 500);
        });
        return this;
    }
    this.config = function(object){
        if(typeof object === 'object'){
            for(var key in object){
                this[key] = object[key];
            }
        }
        return this;
    }
}