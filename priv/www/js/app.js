(function($) {
    var WS = null;
    
    function repeat(cb) { setTimeout(cb, 1000); }  

    $.app = {}
    $.app.init = function() { connect(); }

    /*
        Transport
    */

    function connect()
    {
        ui_status("yellow", "Connecting");
        if(WS) {
            if(WS.readyState == WS.CONNECTING) return;

            WS.onopen = null;
            WS.onmessage = null;
            WS.onclose = null;
            WS.onerror = null;

            if(WS.readyState == WS.OPEN) WS.close();
        }
        WS = new WebSocket( 
            (window.location.protocol == "https:" ? "wss" : "ws") + 
            "://" + window.location.host + "/events");

        WS.onopen = function() { ui_status("green", "Connected"); };
        WS.onmessage = function(m) { handle_cast(m.data && m.data.length > 1 ? $.parseJSON(m.data) : {req: "none"}); };
        WS.onclose = function() { ui_status("orange", "Disconnected"); repeat(function() { connect(); }); };
        WS.onerror = function() { ui_status("red", "Error"); repeat(function() { connect(); }); };
    }

    function send(m, idem) {
        if(WS && WS.readyState == WS.OPEN) { WS.send(JSON.stringify(m)); }
        else if(idem) repeat(function() { send(m, idem); }); 
    }
    function req(r) { send(r, true); } 
    
    function handle_cast(obj) {
        switch(obj.op) {
            default: console.log("Got obj.req = " + obj.req + " in income WS message."); break;
        }
    }

    /*
        UI
    */

    function ui_init() {};
    function ui_status(t, s) { $("#statusbox").attr("class", "ui " + t + " label").text(s).show(); }

})(window.jQuery);
$(document).ready(function() { $.app.init(); });
