FROM erlang:latest

# Copy over required files
ADD apps apps
ADD config config
ADD rebar.config rebar.config

# Build
RUN rebar3 release

# Remove unnecessary files
RUN rm -rf apps
RUN rm rebar.config

# RUN useradd wbws
# USER wbws
EXPOSE 80

ADD secrets ${HOME}

CMD ["_build/default/rel/wbws/bin/wbws", "foreground"]
