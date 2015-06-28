APPNAME=wa
REBAR=./rebar
ERLC=erlc
COOKIE=gRPZkPvMUvHuBMn0Leuw8vzK0yeK5Cu1

MIB_PATH=priv/snmp/mibs
MIB_OPTS=+'{group_check, false}' +'{il, ["otp_mibs/priv/mibs/"]}'

PLT_NAME=.$(APPNAME)_dialyzer.plt
ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       edoc \
                       erts \
                       eunit \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       snmp \
                       snmpa \
                       snmpc \
                       observer \
                       public_key \
                       runtime_tools \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       syntax_tools \
                       tools \
                       webtool \
                       os_mon \
                       xmerl 

DEPS_DIALYZER_APPS = alog gproc epgsql poolboy reloader

.PHONY:deps

all: deps compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"http://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) check-deps || (export GPROC_DIST=true; $(REBAR) get-deps)

pub: $(REBAR)
	@$(REBAR) generate

run:
	erl +A 4 -set_cookie $(COOKIE) -config priv/sys -pa ebin edit deps/*/ebin -boot start_sasl -sname $(APPNAME)@localhost -s $(APPNAME) 

run-local:
	erl +A 4 -set_cookie $(COOKIE) -config priv/local.sys -pa ebin edit deps/*/ebin -boot start_sasl -sname $(APPNAME)@localhost -s $(APPNAME) -s sync

mibs: $(MIBS) 
	mv *.bin $(MIB_PATH)

mibs_clean:
	rm -f $(MIB_PATH)/*.bin

$(MIB_PATH)/%.bin: $(MIB_PATH)/%.mib
	$(ERLC) $(MIB_OPTS) $< -o $@
	
$(PLT_NAME):
	dialyzer --output_plt $(PLT_NAME) --build_plt --apps $(ERLANG_DIALYZER_APPS) -r deps

dialyzer: $(PLT_NAME)
	dialyzer --plt $(PLT_NAME) -Wrace_conditions --src src
