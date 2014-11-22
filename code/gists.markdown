---
title: Gists
---

On this page I collect interesting pieces of code.

## Link checker

**Added 22 November 2014.**

This Python script is used to check a web page for broken links. I wrote it to
be used in a workshop about continuous integration with Jenkins.

```python
import re
import requests
import sys
import urlparse

TIMEOUT_IN_SECONDS = 10.0

def check(base_url):
    print("Checking %s" % base_url)
    base_response = requests.get(base_url, timeout=TIMEOUT_IN_SECONDS)
    assert base_response.status_code == 200
    assert base_response.headers["Content-Type"].startswith("text/")
    for link_match in re.finditer(r"<a.*?href=\"(.*?)\"", base_response.text):
        link_url = urlparse.urljoin(base_url, link_match.group(1))
        if link_url.startswith("http"):
            print("  %s" % link_url)
            link_response = requests.get(link_url, timeout=TIMEOUT_IN_SECONDS)
            assert link_response.status_code == 200

if __name__ == "__main__":
    for base_url in sys.argv[1:]:
        check(base_url)
```

Examples:

    python check.py http://gnome.org
    python check.py http://google.com http://duckduckgo.com
