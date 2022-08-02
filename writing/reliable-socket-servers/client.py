import socket
import time

def make_request(number):
    with socket.socket() as s:
        s.connect(("localhost", 9000))
        if number == 5:
            s.sendall(b"five\n")
        else:
            s.sendall(f"{number}\n".encode("ascii"))
        return s.recv(100).decode("ascii").rstrip()

for i in range(20):
    try:
        time_start = time.perf_counter()
        message = make_request(i)
        time_end = time.perf_counter()
        diff = int((time_end - time_start) * 1000)
        if message:
            print(f"{message} (request took {diff}ms)")
        else:
            print(f"No response for {i}")
    except:
        print(f"Connection failed for {i}")
    time.sleep(0.01)
