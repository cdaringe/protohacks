let map_ok f = function Ok v -> f v | Error _ as e -> e
