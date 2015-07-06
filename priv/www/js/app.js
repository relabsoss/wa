(function($) {
   
    var RE_MAIL = /^.+@[^@]+\.[^@]{2,}$/;
    var RE_TOKEN = /^[a-zA-Z0-9=+\/]+$/;
    var RE_ANY = /^.+$/; 
    
    function repeat(cb) { setTimeout(cb, 1000); }  
    function go(l) { window.location.href = l; }
    function reload() { window.location.reload(); }
    function hash_params() {
        var h = window.location.hash.substring(1);                    
        return (h && h.length > 1) ? h.split(':', 2) : ['nope','']; 
    }

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
        $([
            ['input.fname', 'NAME', { parts: ["NAME"] }], 
            ['input.lname', 'NAME', { parts: ["SURNAME"] }], 
            ['input.mail', 'EMAIL', {}]]).map(function(_k, v) {
                $(v[0], this.ui).suggestions({
                    serviceUrl: "https://dadata.ru/api/v2",
                    token: DADATA_TOKEN,
                    type: v[1],
                    params: v[2],
                    count: 5});
            });
        
        // Menu
        $(".item.user.valid").hide();
        $(".item.user.guest").hide();

        // API
        var error = function(m) { $.app.ui.message("#user_message", "negative", "Error", m); };
        var done = function(m) { $.app.ui.message("#user_message", "positive", "Done", m); };
        var fail = function(r, e) { error(r ? r.error : "" || false); };

        $("#menu").api({
                url: '/auth/user/info',
                on: 'now',
                cache: false,
                successTest: function(r) {
                    if(r.result == 'ok') { 
                        $(".item.user.valid").show(); 
                        this.user = r; 
                    } else { 
                        $(".item.user.guest").show(); 
                        $.app.ui.nav(); 
                    }
                }  
            });
        $([
            {btn: '.b_login', url: '/auth/login', fields: [], success: function(r) { reload(); } }
            
            ]).map(function(_k, v) {
                $(v.btn).api({
                    url: v.url,
                    cache: false,
                    serializeForm: true,
                    method: 'post',
                    beforeSend: function(settings) { 
                        return $.app.ui.validate($("#user_form"), v.fields) ? settings : false; 
                    },
                    successTest: function(r) {
                        return (r.result == 'error') ? error($("#msg_" + r.error).text()) : v.success(r);
                    }  
                });
            });
    }

    $.app.user.modal = function(type) {
        $('.e', this.ui).hide();
        $('.' + type, this.ui).show();
        this.ui.modal('show');
    }

    $.app.user.close = function() {
        this.ui.modal('hide');        
    }

    $.app.user.logout = function() {
        
    }

    /*
        Common UI
    */

    $.app.ui.init = function() {};
    $.app.ui.status = function(t, s) { $("#statusbox").attr("class", "ui " + t + " mini label").text(s).show(); }

    $.app.ui.nav = function() {
        var a = hash_params(); 
        switch(a[0]) {
            case 'user.update': {
                if(a[1] && a[1].match(RE_TOKEN)) {
                    $("#_update_token").val(a[1]);
                    $.app.user.modal('upd');
                } else go('/');                   
                break;
            }
        }         
    }

    $.app.ui.message = function(d, cl, head, info) 
    {
        $(d).attr('class', "ui " + cl + " message")
         .html("<div class='header'>" + strip_html(head) + "</div><p>" + strip_html(info) + "</p>");
    }  

    $.app.ui.validate = function(f, list) {
        return true;
    }

    /*
        Utils
    */

    function strip_html(str) {
        str = str ? "" + str : "";
        $.each([["&", "&amp;"], ["<", "&lt;"], [">", "&gt;"], ["\n", "<br/>"]], function(_k, v) { 
                str = str.replace(new RegExp(v[0], 'g'), v[1]); 
            });
        return str;
    }

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
                repeat(function() { $.app.ws.init(); }); 
            };
        this.WS.onerror = function() { 
                $.app.ui.status("red", "Error"); r
                epeat(function() { $.app.ws.init(); }); 
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
