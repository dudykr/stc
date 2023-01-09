#!/usr/bin/env python3

import sys
lines = sorted(set(sys.stdin.readlines()))

for line in lines:
    print(line,end='')
