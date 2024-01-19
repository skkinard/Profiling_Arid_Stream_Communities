// (c) 2010 jdbartlett, MIT license
// jquery magnifier library, for examples look at here: http://jdbartlett.github.com/loupe/
(function(a) {
    a.fn.loupe = function(b) {
        var c = a.extend({loupe:"loupe",width:200,height:150}, b || {});
        return this.length ? this.each(function() {
            var j = a(this),g,k,f = j.is("img") ? j : j.find("img:first"),e,h = function() {
                k.hide()
            },i;
            if (j.data("loupe") != null) {
                return j.data("loupe", b)
            }
            e = function(p) {
                var o = f.offset(),q = f.outerWidth(),m = f.outerHeight(),l = c.width / 2,n = c.height / 2;
                if (!j.data("loupe") || p.pageX > q + o.left + 10 || p.pageX < o.left - 10 || p.pageY > m + o.top + 10 || p.pageY < o.top - 10) {
                    return h()
                }
                i = i ? clearTimeout(i) : 0;
                k.show().css({left:p.pageX - l,top:p.pageY - n});
                g.css({left:-(((p.pageX - o.left) / q) * g.width() - l) | 0,top:-(((p.pageY - o.top) / m) * g.height() - n) | 0})
            };
            k = a("<div />").addClass(c.loupe).css({width:c.width,height:c.height,position:"absolute",overflow:"hidden"}).append(g = a("<img />").attr("src", j.attr(j.is("img") ? "src" : "href")).css("position", "absolute")).mousemove(e).hide().appendTo("body");
            j.data("loupe", true).mouseenter(e).mouseout(function() {
                i = setTimeout(h, 10)
            })
        }) : this
    }
}(jQuery));
