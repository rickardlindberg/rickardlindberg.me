import socket

with socket.socket(fileno=0) as s:
    print("accepting connections")
    while True:
        conn, addr = s.accept()
        with conn:
            data = conn.recv(100)
            number = int(data)
            conn.sendall(f"got number {number}\n".encode("ascii"))
