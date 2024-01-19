var AWZBlockSlider = Class.create({
    initialize: function(config) {
        this.items = $$(config.itemsSelector);
        this.interval = config.interval;
        this.sliderWidth = 0;
        this.sliderHeight = 0;
        this.currentItem = 0;
        this.nextItem = null;
        this.fadeTimer = null;
        this.appearTimer = null;
        this.timer = null;

        this.isSlideFaded = false;
        this.isSlideAppeared = false;

        if (!this.interval) {
            this.interval = 5 //5 sec - default value for slider.
        }

        this.items.each(function(elm){
            elm.style.display = 'none';
        });

        this.items[0].style.display = '';

        this.initObserver();
    },

    prepareSlider: function() {
        var parentBlockWidth = this.items[0].up().getWidth();
        var height = 0;

        this.items.each(function(elm){
            elmHeight = elm.getHeight();
            if (elmHeight > height) {
                height = elmHeight
            }
        });

        this.sliderWidth = parentBlockWidth;
        this.sliderHeight = height;

        var me = this;
        this.items.each(function(elm){
            elm.setStyle({
                'width': me.sliderWidth + 'px',
                'height': me.sliderHeight + 'px',
                'maxWidth': me.sliderWidth + 'px',
                'maxHeight': me.sliderHeight + 'px'
            });
        });
    },

    getNextSlide: function(){
        var nextSlide = this.currentItem + 1;
        if (nextSlide >= this.items.length) {
            nextSlide = 0;
        }
        return nextSlide;
    },

    initObserver: function() {
        var me = this;
        Event.observe(window, "load", function(){
            me.prepareSlider();
        });
        Event.observe(window, "resize", function(){
            me.prepareSlider();
        });
        Event.observe(window, "load", function(){
            me.start();
        });

    },

    start: function() {
        var me = this;
        if (this.items.length > 1) {
           this.timer = setTimeout(function(){me.fade()}, me.interval * 1000);
        }
    },

    fade: function() {
        var me = this;

        var opacity = this.items[this.currentItem].getOpacity();
        if (opacity > 0) {
            this.items[this.currentItem].setOpacity(opacity - 0.1);
            this.fadeTimer = setTimeout(function(){me.fade()}, 25);
        } else {
            this.nextItem = this.getNextSlide();
            this.items[this.currentItem].style.display = 'none';
            this.items[this.nextItem].setOpacity(0);
            this.items[this.nextItem].style.display = '';
            this.currentItem = this.nextItem;

            this.appear();
        }
    },

    appear: function() {
        var me = this;
        var opacity = this.items[this.nextItem].getOpacity();

        if (opacity < 1) {
            this.items[this.nextItem].setOpacity(opacity + 0.1);
            this.appearTimer = setTimeout(function(){me.appear()}, 25);
        } else {
            this.timer = setTimeout(function(){me.fade()}, me.interval * 1000);
        }
    }

});