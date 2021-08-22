var remap_args = function(k, v) {
    if (k == "pch") {
        this["shape"] = v
    } else if (k == "col") {
        this["stroke"] = v
    } else if (k == "bg") {
        this["fill"] = v
    } else if (k == "cex") {
        this["size"] = v
    } else if (k == "lwd") {
        this["stroke-width"] = v
    } else if (k == "lty") {
        this["stroke-dasharray"] = v
    } else if (k == "lend") {
        this["stroke-linecap"] = v
    } else if (k == "ljoin") {
        this["stroke-linejoin"] = v
    } else if (k == "lmitre") {
        this["stroke-miterlimit"] = v
    } else if (k == "labels") {
        this["text"] = v
    } else return v
    return undefined
}
Shiny.addCustomMessageHandler("animate", function(data) {
    var data = JSON.parse(JSON.stringify(data), remap_args)
    JS_device.record(data)
    return JS_device.dispatch(data)
})
