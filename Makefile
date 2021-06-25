DOC_PATH = priv/doc/

.PHONY: deps compile test doc clean

auto: compile
	ERL_FLAGS="+pc unicode" rebar3 auto

deps:
	rebar3 get-deps

compile: deps
	rebar3 compile

ct: compile
	rebar3 ct

eunit: compile
	rebar3 eunit

test:
	rebar3 eunit
	rebar3 ct

shell: compile
	rebar3 shell

release: deps
	rebar3 release

clean:
	rebar3 clean
	rm -rf _build ${DOC_PATH}

doc:
	apidoc -v -i priv/api/ -o ${DOC_PATH}
	@echo "Open in your browser: $(shell pwd)/${DOC_PATH}index.html"

docker-up:
	docker-compose up -d

docker-down:
	docker-compose down

dialyze:
	rebar3 dialyzer
