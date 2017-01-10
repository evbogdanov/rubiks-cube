.PHONY: compile run

compile:
	rebar compile

run:
	erl -env ERL_LIBS `pwd`
