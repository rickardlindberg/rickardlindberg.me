---
title: Refactoring a function to 6 classes
date: 2024-05-02
tags: refactoring,oop
---

I made a video where I show how I refactor a single function, that does many
things, to 6 classes that each does a single thing.

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/rubTUD0EdME" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
</center>

The resulting design is more object oriented.

I don't want to argue which is better, but instead show you what an object
oriented design can look like, because I feel like those examples are rare.

The example I'm refactoring is a function that returns the next version number
given a set of existing versions numbers stored as git tags.

In the first example, we ask for the next release version in the 1.0 series
given that no tags exist. We get the default version 1.0.0.

$:output:python:
>>> nextversion(series="1.0", pre_release=False, tags=[])
'1.0.0'
$:END

In the second example, version 1.0.0 already exists, and we therefore get
version 1.0.1.

$:output:python:
>>> nextversion(series="1.0", pre_release=False, tags=['1.0.0'])
'1.0.1'
$:END

In the third example we ask for the next pre-release version. The next release
version would be 1.0.2, and so the first pre-release version of that release is
1.0.2-1.

$:output:python:
>>> nextversion(series="1.0", pre_release=True, tags=['1.0.0', '1.0.1'])
'1.0.2-1'
$:END

In the fourth example, pre-release 3 already exists, so the next pre-release is
1.0.2-4.

$:output:python:
>>> nextversion(series="1.0", pre_release=True, tags=['1.0.0', '1.0.1', '1.0.2-3'])
'1.0.2-4'
$:END

The initial function looks like this:

$:output:python:
def nextversion(series, pre_release, tags):
    version_pattern = "".join([
        r"^",
        re.escape(series),
        re.escape("."),
        r"(?P<version>\d+)",
        r"(?P<pre_release>-(?P<pre_release_number>(\d+)))?",
        r"$",
    ])
    versions = []
    pre_release_numbers = {}
    for tag in tags:
        match = re.match(version_pattern, tag)
        if match:
            version = int(match["version"])
            if match["pre_release"]:
                if version not in pre_release_numbers:
                    pre_release_numbers[version] = []
                pre_release_numbers[version].append(int(match["pre_release_number"]))
            else:
                versions.append(version)
    next_version = max(
        [0]
        +
        [1+version for version in versions]
        +
        list(pre_release_numbers.keys())
    )
    next_pre_release_number = 1 + max(pre_release_numbers.get(next_version, [0]))
    if pre_release:
        return f"{series}.{next_version}-{next_pre_release_number}"
    else:
        return f"{series}.{next_version}"
$:END

I refactor it to this:

$:output:python:
def nextversion(series, pre_release, tags):
    return Tags(tags).get_next_version(series, pre_release)

class Tags:

    def __init__(self, tags):
        self.tags = tags

    def get_next_version(self, series, pre_release):
        series = Series(series)
        versions = Versions()
        for tag in self.tags:
            series.parse_version(tag).add_to(versions)
        return versions.get_next_version(pre_release).format(series)

class Release:

    def __init__(self, version):
        self.version = version

    def add_to(self, versions):
        versions.add_release(self.version)

    def format(self, series):
        return series.format_release(self.version)

class PreRelease:

    def __init__(self, version, pre_release_number):
        self.version = version
        self.pre_release_number = pre_release_number

    def add_to(self, versions):
        versions.add_pre_release(self.version, self.pre_release_number)

    def format(self, series):
        return series.format_pre_release(self.version, self.pre_release_number)

class NoMatchVersion:

    def add_to(self, versions):
        pass

class Versions:

    def __init__(self):
        self.versions = []
        self.pre_release_numbers = {}

    def add_release(self, version):
        self.versions.append(version)

    def add_pre_release(self, version, pre_release_number):
        if version not in self.pre_release_numbers:
            self.pre_release_numbers[version] = []
        self.pre_release_numbers[version].append(pre_release_number)

    def get_next_version(self, pre_release):
        next_version = max(
            [0]
            +
            [1+version for version in self.versions]
            +
            list(self.pre_release_numbers.keys())
        )
        next_pre_release_number = 1 + max(self.pre_release_numbers.get(next_version, [0]))
        if pre_release:
            return PreRelease(next_version, next_pre_release_number)
        else:
            return Release(next_version)

class Series:

    def __init__(self, series):
        self.series = series
        self.version_pattern = "".join([
            r"^",
            re.escape(series),
            re.escape("."),
            r"(?P<version>\d+)",
            r"(?P<pre_release>-(?P<pre_release_number>(\d+)))?",
            r"$",
        ])

    def parse_version(self, tag):
        match = re.match(self.version_pattern, tag)
        if match:
            version = int(match["version"])
            if match["pre_release"]:
                return PreRelease(version, int(match["pre_release_number"]))
            else:
                return Release(version)
        return NoMatchVersion()

    def format_release(self, version):
        return f"{self.series}.{version}"

    def format_pre_release(self, version, pre_release_number):
        return f"{self.series}.{version}-{pre_release_number}"
$:END
