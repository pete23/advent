# Advent of Code Solutions

This repository contains solutions for [Advent of Code](https://adventofcode.com/) across multiple years, all unified into a single Clojure project.

## Structure

All solutions are organized by year under the `advent.yYYYY` namespace:

```
src/advent/
├── core.clj          # Shared utility functions
├── y2017/            # 2017 solutions (days 1, 2, 6, 7, 12, 13, 15, 16, 17, 18, 23, 25)
├── y2018/            # 2018 solutions (days 1-5)
├── y2019/            # 2019 solutions (days 1-10) + intcode interpreter
├── y2020/            # 2020 solutions (days 1-8, 13-25)
├── y2023/            # 2023 solutions (days 1-8, 10, 15, 19, 22, 24)
├── y2024/            # 2024 solutions (in progress)
└── y2025/            # 2025 solutions (in progress)

resources/
├── y2017/            # Input files for 2017
├── y2018/            # Input files for 2018
├── y2019/            # Input files for 2019
├── y2020/            # Input files for 2020
└── y2023/            # Input files for 2023

cpp/                  # 2017 C++ solutions (days 5, 10)
java/                 # 2020 Java utilities
```

## Running Solutions

This is a Leiningen project. To use:

```bash
# Start a REPL
lein repl

# Load a specific day's solution
(require 'advent.y2019.d1)
(in-ns 'advent.y2019.d1)

# Run part 1 or part 2
(part-1)
(part-2)

# Run tests
(run-tests)
```

## Languages Used

- **Clojure**: Primary language for all years
- **C++**: Some 2017 solutions
- **Java**: Helper code for 2020

## License

EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0
