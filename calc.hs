evaluate = (pm 0).words
    where pm acc [] = acc
          pm acc full@(left:sign:right:list)
            | sign == "+" = (read left) + (dm acc (right:list))
            | sign == "-" = (read left) - (dm acc (right:list))
            | otherwise = dm acc full
          dm acc [] = acc
          dm acc full@(left:sign:right:list)
            | sign == "*" = (read left) * (dm acc (right:list))
            | sign == "/" = (read left) / (dm acc (right:list))
            | otherwise = pm acc full