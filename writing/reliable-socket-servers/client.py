import socket
import time

for i in range(21):
    if i > 0:
        try:
            with socket.socket() as s:
                s.connect(("localhost", 9000))
                if i == 5:
                    s.sendall(b"five\n")
                else:
                    s.sendall(f"{i}\n".encode("ascii"))
                message = s.recv(100).decode("ascii").rstrip()
                diff = int((time.perf_counter() - prev) * 1000)
                print(f"{diff}ms {message}")
        except:
            print(f"{i} failed")
        time.sleep(0.01)
    prev = time.perf_counter()
