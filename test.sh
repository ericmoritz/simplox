./make_message.erl "http://10.189.4.131/UxServices/UxFronts.svc/frontmodule/name/1/news"\
     "http://10.189.4.131/UxServices/UxFronts.svc/frontmodule/name/1/sports"\
     "http://10.189.4.131/UxServices/UxFronts.svc/frontmodule/name/1/life"\
     "http://10.189.4.131/UxServices/UxFronts.svc/frontmodule/name/1/money"\
     "http://10.189.4.131/UxServices/UxFronts.svc/frontmodule/name/1/tech"\
     "http://10.189.4.131/UxServices/UxFronts.svc/frontmodule/name/1/travel" \
    | curl -v -H "Content-Type: application/protobuf+vnd.simplox.multirequest" \
         --data-binary @- "http://localhost:$1/simplox/v1/multi-request"


