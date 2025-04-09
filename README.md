# qhs
[![CI Status](https://github.com/itchyny/qhs/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/itchyny/qhs/actions?query=branch:main)

### SQL queries on CSV and TSV files
This is a Haskell implementation of [q](https://github.com/harelba/q) command.

## Installation
### Homebrew
```shell
brew install itchyny/tap/qhs
```

### Build with stack
Prepare `stack` command from [here](https://docs.haskellstack.org).
```shell
 $ git clone https://github.com/itchyny/qhs
 $ cd qhs
 $ stack install
 $ export PATH=$PATH:$HOME/.local/bin
 $ qhs "SELECT 100+200"
300
```

## Usage
In the beginning, `qhs [QUERY]` is the basic usage.
```shell
 $ wc * > wc_out.txt
 $ qhs "SELECT * FROM ./wc_out.txt"
66 471 3131 File.hs
118 649 4962 Main.hs
61 258 2346 Option.hs
51 366 2564 Parser.hs
45 273 1769 SQL.hs
341 2017 14772 total
```
You can specify the file name for the table name.
The column names are automatically assigned as `c1`, `c2` and so on.
```shell
 $ qhs "SELECT c4,c1 FROM ./wc_out.txt WHERE c4 <> 'total' ORDER BY c1 DESC"
Main.hs 118
File.hs 66
Option.hs 61
Parser.hs 51
SQL.hs 45
```

The `qhs` command can read the table from the standard input as well.
```shell
 $ wc * | qhs "SELECT c4,c1 FROM - WHERE c4 <> 'total' ORDER BY c1 DESC"
Main.hs 118
File.hs 66
Option.hs 61
Parser.hs 51
SQL.hs 45
```

You can use `-H` flag to make `qhs` regard the head line as the row of column names.
```shell
 $ cat basic.csv
foo,bar,baz
a0,1,a2
b0,3,b2
c0,,c2
 $ qhs -H "SELECT * FROM basic.csv WHERE bar IS NOT NULL"
a0 1 a2
b0 3 b2
```

You can use the basic SQL operations; `GROUP BY`, `ORDER BY`, `LIMIT` and `COUNT(*)`.
```shell
 $ ps -ef | qhs -H -O "SELECT UID,COUNT(*) cnt FROM - GROUP BY UID ORDER BY cnt DESC LIMIT 3"
UID cnt
503 102
0 86
89 3
```
You can also use other SQL operations like `JOIN`, `UNION` and sub-query.
The command helps you deal with multiple CSV files.

Please refer to `qhs --help` for further options.
The command respects the behaviour of the original [q](https://github.com/harelba/q) command.
```shell
 $ qhs --help
qhs - SQL queries on CSV and TSV files

Usage: qhs [-H|--skip-header] [-O|--output-header] [-d|--delimiter DELIMITER]
           [-t|--tab-delimited] [-D|--output-delimiter OUTPUT_DELIMITER]
           [-T|--tab-delimited-output] [-k|--keep-leading-whitespace]
           [-z|--gzipped] [-q|--query-filename QUERY_FILENAME] [QUERY]

Available options:
  -h,--help                Show this help text
  -v,--version             Show the version of the command.
  -H,--skip-header         Skip the header row for row input and use it for
                           column names instead.
  -O,--output-header       Output the header line.
  -d,--delimiter DELIMITER Field delimiter. If not specified, automatically
                           detected.
  -t,--tab-delimited       Same as -d $'\t'.
  -D,--output-delimiter OUTPUT_DELIMITER
                           Field delimiter for output. If not specified, the
                           argument of -d DELIMITER is used.
  -T,--tab-delimited-output
                           Same as -D $'\t'.
  -k,--keep-leading-whitespace
                           Keep leading whitespace in values. The leading
                           whitespaces are stripped off by default.
  -z,--gzipped             Assuming the gzipped input.
  -q,--query-filename QUERY_FILENAME
                           Read query from the provided filename.
```

## Author
itchyny (<https://github.com/itchyny>)

## License
This software is released under the MIT License, see LICENSE.
