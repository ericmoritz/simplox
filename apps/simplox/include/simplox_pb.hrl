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
    body
}).
-endif.

-ifndef(HEADER_PB_H).
-define(HEADER_PB_H, true).
-record(header, {
    key = erlang:error({required, key}),
    value
}).
-endif.

