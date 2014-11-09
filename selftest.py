import re
import requests
import sys
import urlparse

def check_single(url):
    response = requests.get(url, timeout=5.0)
    if response.status_code == 200:
        return response.text
    else:
        return None

def check(base):
    bad = []
    processed = []
    to_process = [base]
    while len(to_process) > 0:
        url = to_process.pop()
        if url not in processed:
            print url,
            processed.append(url)
            content = check_single(url)
            if content is None:
                print("ERR")
                bad.append(url)
            else:
                print("OK")
                if url.startswith(base):
                    for match in re.finditer(r"<a.*?href=\"(.*?)\"", content):
                        to_process.append(urlparse.urljoin(url, match.group(1)))
                    for match in re.finditer(r"<img.*?src=\"(.*?)\"", content):
                        to_process.append(urlparse.urljoin(url, match.group(1)))
    print("Summary:")
    for url in bad:
        print(url)
    if len(bad) > 0:
        sys.exit(1)

check("http://rickardlindberg.me")
