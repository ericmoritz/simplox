PROJECT=simplox
VERSION=1
PLT_APPS=ssl crypto public_key
SRC = apps/*/src deps/cowboy/src deps/ranch/src
EBIN = apps/*/ebin deps/*/ebin
DIALYZER_OPTS ?= \
	-Werror_handling\
	-Wunmatched_returns\
	-Iapps/simplox/include

include erlang.mk
