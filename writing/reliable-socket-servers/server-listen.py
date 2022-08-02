import socket

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    s.listen()
    print("listening on port 9000")
    while True:
        conn, addr = s.accept()
        print("accepting connection")
        with conn:
            data = conn.recv(100)
            number = int(data)
            conn.sendall(f"{number}*{number}={number*number}\n".encode("ascii"))
