import sys
import os
import re

tests = [[], []]
path = 'vendor/https-everywhere/rules/'

for name in os.listdir(path):
    if name.endswith('.xml'):
        with open(path + name, 'r') as f:
            ve = True
            comment = False
            for line in f:
                if '<!--' in line:
                    comment = True
                if comment:
                    continue
                if '-->' in line:
                    comment = False
                if '+ve:' in line:
                    ve = True
                elif '-ve:' in line:
                    ve = False
                elif line.strip().startswith('<test'):
                    tests[ve].append(line.lstrip())

with open('tests.xml', 'w') as f:
    f.write('<ruleset>\n')
    for ve in [0, 1]:
        for line in tests[ve]:
            f.write('<test rewrite="' + str(ve) + '" ' + line[5:])
    f.write('</ruleset>\n')

