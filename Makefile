APPNAME=wa

REBAR=./rebar
ERL=erl
ERLC=erlc

.PHONY:deps

all: deps compile

./rebar:
	$(ERL) \
		-noshell -s inets start -s ssl start \
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
	$(ERL) \
		-config priv/sys \
		-pa ebin deps/*/ebin \
		-sname $(APPNAME)@localhost \
		-s $(APPNAME) 

run-local: 
	$(ERL) \
		-config priv/local.sys \
		-pa ebin deps/*/ebin \
		-sname $(APPNAME)@localhost \
		-s $(APPNAME) \
		-s sync
