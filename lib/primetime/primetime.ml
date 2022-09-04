type req = { req_method : string; [@key "method"] number : int }
[@@deriving show, eq, yojson]
