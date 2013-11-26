-ifndef(MULTIREQUEST_PB_H).
-define(MULTIREQUEST_PB_H, true).
-record(multirequest, {
    requests = []
}).
-endif.

-ifndef(REQUEST_PB_H).
-define(REQUEST_PB_H, true).
-record(request, {
    url = erlang:error({required, url}),
    method = "get",
    headers = [],
    content_type,
    body,
    key,
    cache
}).
-endif.

-ifndef(CACHE_PB_H).
-define(CACHE_PB_H, true).
-record(cache, {
    key = erlang:error({required, key}),
    timeout = erlang:error({required, timeout})
}).
-endif.

-ifndef(HEADER_PB_H).
-define(HEADER_PB_H, true).
-record(header, {
    key = erlang:error({required, key}),
    value
}).
-endif.

-ifndef(RESPONSE_PB_H).
-define(RESPONSE_PB_H, true).
-record(response, {
    status = erlang:error({required, status}),
    url = erlang:error({required, url}),
    headers = [],
    body,
    key,
    request_time,
    method
}).
-endif.

