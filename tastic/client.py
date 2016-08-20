import http.client as http
import os
from os import path
import sys

from tastic import minhash

def submit(filename, hostname, port):
    assignment, submission = path.abspath(filename).split(os.sep)[-3:-1]
    hashes = minhash.MinHash(f=filename).hashes()
    conn = http.HTTPConnection(host=hostname, port=port)
    submit_row(hashes, assignment, submission, conn)

def get_message(response):
    length_bytes = response.getheader("Content-Length").encode(encoding="ASCII")
    length = int(length_bytes.lstrip(b'\x00')[1:])
    return response.read(length)

def submit_row(hashes, assignment, submission, conn):
    conn.request(
            method="POST",
            url="%s/%s" % (assignment, submission),
            body=hashes,
            headers={"Content-Length": 256 * 4}
    )
    response = conn.getresponse()
    if response.status != 201:
        length = response.getheader("Content-Length").encode(encoding="ASCII")
        l = int(length.lstrip(b'\x00')[1:])
        if get_message(response) == b"Resource already present":
            sys.stderr.write("%s/%s already pushed.\n" % (assignment,
                                                          submission))
            sys.exit(1)
        else:
            print("Server connection problem.")
            print(response.msg)
            print(response.read(l))
