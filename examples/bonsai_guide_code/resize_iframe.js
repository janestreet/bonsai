window.addEventListener('DOMContentLoaded', function () {
    var resizeObserver = new ResizeObserver(function () {
        var height = document.body.scrollHeight;
        if (height == 0) { return; }
        var message = {
            height: height,
            hash: window.location.hash
        };
        window.parent.postMessage(JSON.stringify(message), "*");
    });

    resizeObserver.observe(document.body)
});
