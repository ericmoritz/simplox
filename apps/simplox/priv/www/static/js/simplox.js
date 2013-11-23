var ProtoBuf = dcodeIO.ProtoBuf;
var builder = ProtoBuf.protoFromFile(
    "/simplox/static/proto/simplox.proto");

var simplox = {
    "pb": builder.build("simplox"),
    "response_mime_type": "application/protobuf+vnd.simplox.response",

    "multirequest": function(endpoint, mr, callback) {
	// One ws connection per request is probably not the 
	// most efficient.
	var ws = new WebSocket(endpoint);
	ws.binaryType = "arraybuffer";
	
	ws.onopen = function(event) {
	    var msg = new simplox.pb.MultiRequest(mr);
	    var buff = msg.encode().toArrayBuffer()
	    ws.send(buff);
	}

	ws.onmessage = function(message) {
	    var buff = ByteBuffer.wrap(message);
	    var content_type = buff.readUTF8String(multirequest['response_mime_type'].length);
	    if(content_type == multirequest['response_mime_type']) {
		var resp = simplox.pb.Response.decode(buff.slice(multirequest['response_mime_type'].length));
		callback(resp);
	    }

	}
    }
}

    




