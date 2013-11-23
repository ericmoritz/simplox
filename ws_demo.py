import websocket
import thread
import time
from simplox import simplox_pb2
import sys
from simplox import multirequest
from functools import partial
from uuid import uuid1

class Client(object):
      def __init__(self, msg_bin):
            self.start = None
            self.msg_bin = msg_bin

      def on_open(self, ws):
            def run(*args):
                self.start = time.time()
                ws.sock.send_binary(
                      "application/protobuf+vnd.simplox.multirequest="+ self.msg_bin)
                time.sleep(30)
                ws.close()
                print "thread terminating..."
            thread.start_new_thread(run, ())
            
      def on_message(self, ws, message):
            if message.startswith("application/protobuf+vnd.simplox.response="):
                  _, rest = message.split("=", 1)
                  msg = simplox_pb2.Response()
                  msg.ParseFromString(rest)
                  elapsed = (time.time() - self.start) * 1000.0
                  sync_time = msg.request_time / 1000.0
                  diff = sync_time - elapsed
                  print msg.url, msg.batch_key
                  print "{:.2f} - {:.2f} = {:.2f}".format(sync_time, elapsed, diff)

def on_error(ws, error):
      print error

def on_close(ws):
      print "### closed ###"

    
if __name__ == "__main__":
      endpoint = sys.argv[1]
      urls = sys.argv[2:]
      print urls
      msg = multirequest.multirequest(
            batch_key=str(uuid1()),
            *map(multirequest.get, urls))

      client = Client(msg.SerializeToString())

                 
      websocket.enableTrace(True)
      ws = websocket.WebSocketApp(endpoint,
                                  on_message = client.on_message,
                                  on_error = on_error,
                                  on_close = on_close)
      ws.on_open = client.on_open

      ws.run_forever()
