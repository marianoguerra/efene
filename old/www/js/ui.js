var ui = {};

ui.showPage = function (id) {
    var page = $(id);

    if (page.length > 0) {
        $('.page').hide();
        page.show();
        location.hash = id;
    }
};

$(function () {
    $('.hide').hide();

    if (location.hash !== "") {
        ui.showPage(location.hash);
    }
});

