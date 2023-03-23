#!/usr/bin/env python3
import glob
import json


def stats(d: dict):
    items = {k: v for k, v in sorted(
        d.items(), key=lambda item: item[1], reverse=True)}
    return json.dumps(items, indent=4)


def main():
    files = glob.glob(
        "./tests/**/*.error-diff.json", recursive=True)
    # print(len(files))

    extras = {}
    required = {}
    for filename in files:
        with open(filename, "r") as filename:
            data = json.load(filename)
            for k, v in data['extra_errors'].items():
                extras[k] = extras.get(k, 0) + v
            for k, v in data['required_errors'].items():
                required[k] = required.get(k, 0) + v

    print('Extra:', stats(extras))
    print('Required:', stats(required))


if __name__ == "__main__":
    main()
