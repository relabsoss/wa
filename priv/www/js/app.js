(function($) {
    
    function repeat(cb) { setTimeout(cb, 1000); }  

    $.app = { user: {}, ui: {}, ws: { WS: null }}
    $.app.init = function() { 
            $(['user', 'ui', 'ws']).map(function(_k, v) { $.app[v].init(); }); 
        }

    /*
        User
    */

    $.app.user.init = function() {


        // Modal panel
        this.ui = $("#user_modal");
    }

    $.app.user.modal = function(type) {
        $('.e', this.ui).hide();
        $('.' + type, this.ui).show();
        this.ui.modal('show');
    }

    $.app.user.login = function() { return false; }
    $.app.user.register = function() { return false; }

    /*
        Common UI
    */

    $.app.ui.init = function() {};
    $.app.ui.status = function(t, s) { $("#statusbox").attr("class", "ui " + t + " label").text(s).show(); }

    /*
        Transport
    */

    $.app.ws.init = function()
    {
        $.app.ui.status("yellow", "Connecting");
        if(this.WS) {
            if(this.WS.readyState == this.WS.CONNECTING) return;

            this.WS.onopen = null;
            this.WS.onmessage = null;
            this.WS.onclose = null;
            this.WS.onerror = null;

            if(this.WS.readyState == this.WS.OPEN) 
                this.WS.close();
        }
        this.WS = new WebSocket( 
            (window.location.protocol == "https:" ? "wss" : "ws") + 
            "://" + window.location.host + "/events");

        this.WS.onopen = function() { 
                $.app.ui.status("green", "Connected"); 
            };
        this.WS.onmessage = function(m) { 
                this.handle_cast(m.data && m.data.length > 1 ? $.parseJSON(m.data) : {req: "none"}); 
            };
        this.WS.onclose = function() { 
                $.app.ui.status("orange", "Disconnected"); 
                repeat(function() { this.init(); }); 
            };
        this.WS.onerror = function() { 
                $.app.ui.status("red", "Error"); r
                epeat(function() { this.init(); }); 
            };
    }

    $.app.ws.req = function(m, idem) {
        if(WS && WS.readyState == WS.OPEN) { WS.send(JSON.stringify(m)); }
        else if(idem) repeat(function() { send(m, idem); }); 
    }
    
    $.app.ws.handle_cast = function(obj) {
        switch(obj.op) {
            default: 
                console.log("Got obj.req = " + obj.req + " in income WS message."); 
                break;
        }
    }

})(window.jQuery);
$(document).ready(function() { $.app.init(); });
