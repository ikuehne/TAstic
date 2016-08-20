import http.client as client
import random
import re
import sys

def get_hash():
    randomHashes = [random.getrandbits(32) for _ in range(256)]
    as_bytes = b""
    for h in randomHashes:
        as_bytes += h.to_bytes(4, byteorder='big')
    return as_bytes

def makeRequest(sub, as_bytes):
    connected = False
    while not connected:
        try:
            conn = client.HTTPConnection("localhost", 3000)
            conn.request(
                method="POST",
                url="pytest4/%s" % sub,
                body=as_bytes,
                headers={"Content-Length": 256 * 4}
            )
            response = conn.getresponse()
            conn.close()
            connected = True
        except ConnectionRefusedError:
            pass

for i in range(10000):
    h = get_hash()
    makeRequest("sub%d" % (2 * i),     h)
    makeRequest("sub%d" % (2 * i + 1), h)
