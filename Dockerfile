FROM erlang:latest

# Copy over required files
ADD apps apps
ADD config config
ADD rebar.config rebar.config
ADD res res

# Build
RUN rebar3 release

# Remove unnecessary files
RUN rm -rf apps
RUN rm rebar.config

EXPOSE 443
EXPOSE 80
CMD ["_build/default/rel/wbws/bin/wbws", "console"]
