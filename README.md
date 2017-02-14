# Weather balloon statistics

## Usage

Install purescript 10.5, npm and bower

```bash
npm install
bower update
pulp test
pulp run -- [command]
```
args can be one of:
```
stats min|max|mean|count|dist K|F|C KM|M filename
generate thousands
normalise K|F|C KM|M filename
```

Warnings/error are written to stderr.

Generate and normalise both output to stdout.

## Decisions made

- Purescript was chosen because i have no yet written any Haskell in anger. Purescript does not produce the most effecient code, so I wouldn't say it's the best tool for the job here.
- I didn't find a decent native purescript command line parser, hence the restrictive command line options.
- Stream libraries are a bit lacking with purescript at the moment, so I just used the
  then wrapper around the Node JS streams.
- The mean is calculated continuously which might suffer from a loss of precision but I thought that was acceptable compared to the possibility of overflowing javascripts number type.
- Initially I thought of buffering measurements and sorting the buffers but that's not going to help if the ordering problems span across buffers
  and then it occurred to me that if you're already assuming straight lines between each position anyway which is just an approximation, missing out on an intermediate measurement is just going to be a
  bit less approximate. I'm assuming there's libraries that are out there which handle this sort of thing better than just straight line approximation with splines or something.
- Parsing errors are just logged, so too with the out of order measurements
- Initially I found a memory leak in the generator due to lack of tail call optimisation and changed it to use a stack safe version of `traverse_` which seemed to alleviate the problem.
