var ppt = {};

// handlers
ppt.h = {};
// functions
ppt.f = {};
// data
ppt.d = {};

ppt.init = function(data) {
    var attr;

    for(attr in data) {
        if (ppt.h[attr]) {
            ppt.h[attr](data, data[attr]);
        }
        else {
            console.log("no handler for: " + attr);
        }
    }

    $(document).bind('keypress', 'n', ppt.next);
    $(document).bind('keypress', 'p', ppt.prev);
    $(document).bind('keypress', '1', function () { ppt.show(0); });

    ppt.next();
};

ppt.load = function (url) {
    var callback = function (data) {
        ppt.init(data);
    };

    $.ajax({
      url: url,
      dataType: 'json',
      success: callback,
      error: ppt.errorCb("error loading data")
    });
};

ppt.h.css = function (data, style) {
    var head;

    if (!style.src) {
        ppt.error("no src in style");
        return;
    }

    ppt.f.handleValue(style.src, "addCssLink");
};

ppt.f.addCssLink = function (style) {
    $("head").append('<link rel="stylesheet" href="' + style + '" type="text/css" media="screen" charset="utf-8"/>');
};

ppt.h.order = function (data, order) {
    ppt.d.order = order;
};

ppt.h.external = function (external) {
    var callback;

    if (!external.src) {
        ppt.error("no src in external");
        return;
    }

    callback = function (data) {
        var node = $(data);
        $("body").html(node);

        ppt.f.applyAll(node, external);
    };

    $.ajax({
      url: external.src,
      success: callback,
      error: ppt.errorCb("error loading data")
    });
};

ppt.f.applyAll = function (node, value) {
        ppt.f.applyTemplate(node, value.template);
        ppt.f.applyStyle(node, value.style);
        ppt.f.applyClass(node, value.cls);
};

ppt.prev = function () {
    if (ppt.d.current === undefined) {
        ppt.d.current = 1;
    }

    ppt.show(ppt.d.current - 1);
};

ppt.next = function () {
    if (ppt.d.current === undefined) {
        ppt.d.current = -1;
    }

    ppt.show(ppt.d.current + 1);
};

ppt.show = function (index) {
    var attr, slide = ppt.d.slides[ppt.d.order[index]];

    if (!slide) {
        ppt.error("slide not found");
        return;
    }

    ppt.d.current = index;

    $("body").html("");

    for (attr in slide) {
        if (ppt.h[attr]) {
            ppt.h[attr](slide[attr]);
        }
    }
};

ppt.errorCb = function (msg) {
    return function () {
        ppt.error(msg);
    };
};

ppt.error = function (msg) {
    alert(msg);
};

ppt.h.text = function (value) {
    ppt.f.handleValue(value, "showText");
};

ppt.h.list= function (value) {
    var i, body = $("body"), node = $("<ul></ul>");

    for (i = 0; i < value.data.length; i += 1) {
        node.append("<li>" + ppt.f.markup(value.data[i]) + "</li>");
    }

    ppt.f.applyAll(node, value);
    body.append(node);
};

ppt.h.image = function (value) {
    ppt.f.handleValue(value, "showImage");
};

ppt.f.handleValue = function (value, handler) {
    var i;

    if ($.isPlainObject(value)) {
            ppt.f[handler](value);
    }
    else if ($.isArray(value)) {
        for (i = 0; i < value.length; i += 1) {
            ppt.f[handler](value[i]);
        }
    }
};

ppt.f.showText = function (value) {
    var body = $("body"), node = $("<div></div>");

    node.html(ppt.f.markup(value.data || ""));

    ppt.f.applyAll(node, value);

    body.append(node);
};

ppt.f.markup = function (markup) {
    var index = markup.search(/\*\*.+\*\*/g)

    while (index != -1) {
        markup = markup.replace("**", "<strong>");
        markup = markup.replace("**", "</strong>");
        index = markup.search(/\*\*.+\*\*/g);
    }

    index = markup.search(/\*.+\*/g)

    while (index != -1) {
        markup = markup.replace("*", "<em>");
        markup = markup.replace("*", "</em>");
        index = markup.search(/\*.+\*/g);
    }

    return markup;
};

ppt.f.showImage = function (value) {
    var body = $("body"), node = $("<img>"), center;

    node.attr("src", value.src);

    ppt.f.applyAll(node, value);

    center = $("<center>");
    center.append(node);
    body.append(center);
};

ppt.f.applyTemplate = function (node, template) {
    var tpl;

    if (!template) {
        return;
    }

    tpl = ppt.d.templates[template];

    if (!tpl) {
        return;
    }

    ppt.f.applyStyle(node, tpl);
};

ppt.f.applyStyle = function (node, style) {
    var attr;

    if (!style) {
        return;
    }

    for (attr in style) {
        node.css(attr, style[attr]);
    }
};

ppt.f.applyClass = function (node, cls) {
    var attr;

    if (!cls) {
        return;
    }

    node.attr("class", cls);
};

ppt.h.slides = function (data, slides) {
    ppt.d.slides = slides;
};

ppt.h.templates = function (data, templates) {
    ppt.d.templates = templates;
};

