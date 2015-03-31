Erldbf
========

It is a very simple Dbf Files parser.

Usage:

``` 
{ok, FileStruct} = erldbf:parse(filename:join([code:priv_dir('erldbf'), "sample_data.dbf"])),
{ok, Rows} = erldbf:get_result_set(FileStruct).
```
For example:

```
{ok, FileStruct} = erldbf:parse(filename:join([code:priv_dir('erldbf'), "sample_data.dbf"])),
{ok, Rows} = erldbf:get_result_set(FileStruct).

[   
    {<<"LEISTNR">>,<<"999999">>},
    {<<"LEISTUNG">>,<<"Leistung">>},
    {<<"PREIS">>,<<"0.00">>},
    {<<"KNAME">>,<<"NoNam">>}
] = hd(Rows).

```
