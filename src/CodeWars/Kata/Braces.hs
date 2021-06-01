module CodeWars.Kata.Braces where


validBraces :: String -> Bool
validBraces xs =
    let       pair l r = ([l] ++ [r]) `elem` ["{}", "[]", "()"]
              isPair (p, c ->
                let s = fst p
                    b = snd p
                in  if not b then ("", False) else
                      if c `elem` "{[(" then (s ++ [c], True) else
                        if c `elem` "}])" then
                          if (length s > 0 && (pair (last s) c)) then (init s, True) else ("", False)
                        else
                          ("", False)
                )
    in snd $ foldl isPair("", True) xs