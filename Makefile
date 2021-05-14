DOC_PATH = priv/doc/

.PHONY: deps compile test doc clean

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

auto: compile
	rebar3 auto


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
