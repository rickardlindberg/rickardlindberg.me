import os
import socket

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    print("listening on port 9000")
    s.listen()
    os.dup2(s.fileno(), 0)
    os.close(s.fileno())
    os.execvp("bash", ["bash", "loop.sh", "python", "server-accept.py"])
