FROM erlang:latest

ADD apps apps
ADD config config
ADD rebar.config rebar.config

RUN rebar3 release

EXPOSE 80
CMD ["_build/default/rel/wbws/bin/wbws", "foreground"]
