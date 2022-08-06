import os
import socket

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    s.listen()
    print("listening on port 9000")
    os.set_inheritable(s.fileno(), True)
    os.execvp("bash", ["bash", "loop.sh", "python", "server-accept-inherit.py", str(s.fileno())])
