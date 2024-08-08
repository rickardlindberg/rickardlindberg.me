import time

with open("new-list.txt") as f:
    for line in f:
        if line.strip():
            date, title = line.strip().split(": ", 1)
            x = time.strptime(date, "%Y-%m-%d")
            y = time.strftime("%e %B %Y", x)
            foo = " This post has not yet been imported to my new blog. In the meantime, you can read it here: …"
            print(f"{title.replace(foo, '')} - {y.strip()}")
