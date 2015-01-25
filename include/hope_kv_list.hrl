-record(hope_kv_list_presence_violations,
    { keys_missing     :: [A]
    , keys_duplicated  :: [A]
    , keys_unsupported :: [A]
    }).
