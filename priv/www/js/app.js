(function($) {
   
    var RE_MAIL = /^.+@[^@]+\.[^@]{2,}$/;
    var RE_TOKEN = /^[a-zA-Z0-9=+\/]+$/;
    var RE_ANY = /^.+$/; 
    
    function repeat(cb) { setTimeout(cb, 1000); }  
    function after(t, cb) { setTimeout(cb, t * 1000); }  

    function go(l) { window.location.href = l; }

    function hash_clear() { window.location.hash = ''; }
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
        $.app.user.check();

        // API
        var error = function(m) { $.app.ui.message("#user_message", "negative", "Error", m); };
        var error_m = function(m) { error($("#msg_" + m).text()); };
        var done = function(m) { $.app.ui.message("#user_message", "positive", "Done", m); };
        var done_m = function(m) { done($("#msg_" + m).text()); };
        var fail = function(r, e) { error(r ? r.error : "" || false); };

        $([
            { btn: '.b_login', 
              url: '/auth/login', 
              fields: ['mail', 'pass'], 
              success: function(r) { $.app.user.check(); $.app.user.close(); }},
            { btn: '.b_reset', 
              url: '/auth/reset', 
              fields: ['mail'], 
              success: 'reset_success' },
            { btn: '.b_register', 
              url: '/auth/reg', 
              fields: ['mail', 'fname', 'lname'], 
              success: 'reg_success' },
            { btn: '.b_update', 
              url: '/auth/update', 
              fields: ['pass', 'pass_dup'], 
              success: function(r) { done_m('update_success'); $.app.user.check(); }}            
            ]).map(function(_k, v) {
                $(v.btn).api({
                    url: v.url,
                    cache: false,
                    serializeForm: true,
                    method: 'post',
                    beforeSend: function(settings) { 
                        $.app.ui.message_clean("#user_message");
                        return $.app.ui.validate($("#user_form"), v.fields) ? settings : false; 
                    },
                    successTest: function(r) {
                        grecaptcha.reset();
                        if(r.result == 'error') error_m(r.error);
                        else {
                            $("#user_form")[0].reset();
                            $.isFunction(v.success) ? v.success() : done_m(v.success);
                        }
                    }  
                });
            });
    }

    $.app.user.check = function() {
        $(".item.user.valid").hide();
        $(".item.user.guest").hide();

        $("#menu").api({
                url: '/auth/user/info',
                on: 'now',
                cache: false,
                successTest: function(r) {
                    if(r.result == 'ok') { 
                        $(".item.user.valid").show(); 
                        this.mail = r.mail;                                            
                        $(['mail', 'fname', 'lname']).map(function(_k, v) {
                                $(".u_" + v, $.app.user.ui).text(r[v]);
                            });
                        $("#_update_token").val(r.token);
                        $.app.user.socials(r);
                    } else { 
                        $(".item.user.guest").show(); 
                        $.app.ui.nav();
                    }
                }  
            });
    }
    
    $.app.user.socials = function(r) {
        $("#user_socials *").detach();
        $(r.social).map(function(_k, v) {
                var a = v.id.split('@', 2);
                var net = { 'facebook.com': 'facebook', 
                            'google.com': 'google', 
                            'vk.com': 'vk' };

                var i = $("<div class='item'> \
                        <div class='middle aligned content'> \
                            <i class='" + net[(a[1])] + " icon'></i> \
                             " + strip_html(v.title)  + " \
                        </div> \
                        <div class='middle right aligned content'> \
                            <button class='circular ui icon button'><i class='trash icon'></i></button> \
                        </div> \
                    </div>");
                $("button", i).api({
                        url: '/auth/unbind',
                        method: 'post',
                        cache: false,
                        beforeSend: function(s) {
                                s.data = {
                                    socid: v.id,
                                    token: r.token
                                };
                                return s;
                            },
                        successTest: function(r) { $.app.user.check(); } 
                    });
                $("#user_socials").append(i);
            });
    }

    $.app.user.modal = function(type) {
        $('.e', this.ui).hide();
        $('.' + type, this.ui).show();
        $.app.ui.message_clean("#user_message");
        this.ui.modal({ closable: false }).modal('show');
    }

    $.app.user.close = function() {
        this.ui.modal('hide');  
    }

    $.app.user.logout = function() {
        $("#menu").api({
                url: '/auth/logout',
                on: 'now',
                cache: false,
                successTest: function(r) { $.app.user.check(); }  
            }); 
    }

    /*
        Common UI
    */

    $.app.ui.init = function() {};
    $.app.ui.status = function(t, s) { $("#statusbox").attr("class", "ui " + t + " mini label").text(s).show(); }
    $.app.ui.up = function(p, n) { return n == 0 ? p : $.app.ui.up(p.parent(), n - 1); }

    $.app.ui.nav = function() {
        var a = hash_params(); 
        switch(a[0]) {
            case 'user.update': {
                if(a[1] && a[1].match(RE_TOKEN)) {
                    $("#_update_token").val(a[1]);
                    hash_clear();
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
    $.app.ui.message_clean = function(d) 
    {
        $(d).attr('class', "").html("");
    }   

    $.app.ui.validate = function(f, list) {
        var r = true;

        $(".notificator", f).detach();
        $("input, select", f).map(function(_k, v) {
            if(list.indexOf($(v).attr("name")) != -1) {
                var obj = $(v).attr("valid") ? eval("new Object({" + $(v).attr("valid") + "})") : {};
                var type = obj.type || 'undefined';
                var val = $(v).val();
                var bad = function() {
                    $.app.ui.error_field($(v), obj.message, obj.up); 
                    r = r && false; 
                };

                switch(type) {
                    case('re'): if(!val.match(obj.re || RE_ANY)) bad(); break;
                    case('len'): if(val.length < obj.min) bad(); break;
                    case('equal'): if(val != $("input[name='" + obj.with + "']", f).val()) bad(); break;
                } 
            }
        });
        return r;
    }

    $.app.ui.error_field = function(el, msg, level) {
        var p = $("<div class='ui pointing label notificator'>" + strip_html(msg) + "</div>");
        $.app.ui.up(el, level).after(p);
        $.app.ui.up(el, level + 1).addClass("error");
        (function(elm, pop, l) { elm.focus(function() { 
                $.app.ui.up(el, l + 1).removeClass("error");
                pop.detach();
             }); })(el, p, level);
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
                $.app.ui.status("red", "Error"); 
                repeat(function() { $.app.ws.init(); }); 
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
