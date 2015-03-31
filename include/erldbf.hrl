-define(DATAMARK, <<13>>).
-define(RECORDMARK, <<32>>).
-define(ENDMARK, <<26>>).

-record(file_struct, {vsn, date, record_stripe, buffer = <<>>, fields = [], fields_len = 0, count_rows = 0,  rows = []}).

-record(field_descriptor, {field_name, field_type, field_length}).
