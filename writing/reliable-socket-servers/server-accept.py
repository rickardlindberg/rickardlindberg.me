import socket

with socket.socket(fileno=0) as s:
    while True:
        conn, addr = s.accept()
        print("accepting connection")
        with conn:
            data = conn.recv(100)
            number = int(data)
            conn.sendall(f"{number}*{number}={number*number}\n".encode("ascii"))
