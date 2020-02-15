import argparse
import json
import sys

import markdown as m


def run(inp, outp):
    md = m.Markdown(extensions=["meta"])
    md.convert(inp.read())
    return md.Meta


def conv(fm):
    return fm


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Extract front matter from md files."
    )
    parser.add_argument(
        "-i", "--input", dest="input", default=sys.stdin, help="Input file"
    )
    parser.add_argument(
        "-o", "--output", dest="output", default=sys.stdout, help="Output file"
    )
    args = parser.parse_args()

    if type(args.input) == str:
        with open(args.input) as f:
            res = run(f, args.output)
    else:
        res = run(args.input, args.output)

    fm = conv(res)

    if type(args.output) == str:
        with open(args.output, "w") as f:
            json.dump(fm, f)
    else:
        json.dump(fm, args.output)
