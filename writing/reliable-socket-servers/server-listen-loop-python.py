import os
import socket
import subprocess

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    s.listen()
    print("listening on port 9000")
    os.dup2(s.fileno(), 0)
    os.close(s.fileno())
    while True:
        subprocess.Popen(["python", "server-accept.py"]).wait()
        print("restarting")
