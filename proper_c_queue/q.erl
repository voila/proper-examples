-module(q).
-export([new/1, put/2, get/1, size/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./q_nif", 0).

new(_Size) ->
    exit(nif_library_not_loaded).
put(_Q, _X) ->
    exit(nif_library_not_loaded).
get(_Q) ->
    exit(nif_library_not_loaded).
size(_Q) ->
    exit(nif_library_not_loaded).
