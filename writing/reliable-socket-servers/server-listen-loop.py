import os
import socket

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    s.listen()
    print("listening on port 9000")
    import os
    print(os.get_inheritable(s.fileno()))
    print(os.set_inheritable(s.fileno(), True))
    print(os.get_inheritable(s.fileno()))
    os.dup2(s.fileno(), 0)
    os.close(s.fileno())
    os.execvp("bash", ["bash", "loop.sh", "python", "server-accept.py"])
