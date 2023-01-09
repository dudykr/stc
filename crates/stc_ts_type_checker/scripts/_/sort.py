#!/usr/bin/env python3

import sys
lines = sorted(set([i.strip() for i in sys.stdin.readlines()]))

for line in lines:
    print(line,end='\n')
