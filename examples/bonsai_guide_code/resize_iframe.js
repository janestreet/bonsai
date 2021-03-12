
window.addEventListener('DOMContentLoaded', function () {

    var resizeObserver = new ResizeObserver(function (e) {
        console.log(e);
        var height = document.body.scrollHeight;
        if (height == 0) {return;}
        console.log(height);
        var message = {
            height : height,
            hash: window.location.hash
        };
        window.parent.postMessage(JSON.stringify(message), "*");
    });

    resizeObserver.observe(document.body)

});
