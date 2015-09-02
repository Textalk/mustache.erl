PROJECT = mustache

HAS_MERL := $(shell erl -eval 'erlang:display(list_to_integer(erlang:system_info(otp_release)) >= 18), halt().'  -noshell)

ifneq (true, $(HAS_MERL))
DEPS = merl
else
ERLC_OPTS = -DHAS_MERL
endif

include erlang.mk
