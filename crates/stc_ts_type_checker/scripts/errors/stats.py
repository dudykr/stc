#!/usr/bin/env python3
import glob
import json
import os


def main():
    files = glob.glob("./tests/conformance/**/*.error-diff.json")

    extras = {}
    required = {}
    for filename in files:
        with open(filename, "r") as filename:
            data = json.load(filename)
            for k, v in data['extra_errors'].items():
                extras[k] = extras.get(k, 0) + v
            for k, v in data['required_errors'].items():
                required[k] = required.get(k, 0) + v

    print(extras)
    print(required)


if __name__ == "__main__":
    main()
