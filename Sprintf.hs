module Sprintf (sprintf, (%), (<<)) where

-- An alternative to list append (++) for string formatting in prints
-- (for example, for error messages).

-- We can't *really* do sprintf without heterogeneous lists, but
-- we can at least get string substitution (%s).
-- for example: sprintf "%s + %s = %s" [show 2, show 2, show 4] = "'2' + '2' = '4'"

sprintf :: [Char] -> [[Char]] -> [Char]
sprintf [] [] = []
sprintf [] _ = error "sprintf: too many arguments"
sprintf ('%' : '%' : rest) args = '%' : sprintf rest args
sprintf ('%' : 's' : rest) (arg : args) = arg ++ sprintf rest args
sprintf ('%' : 's' : _) [] = error "sprintf: not enough arguments"
sprintf ('%' : _ : _) _ = error "sprintf: format specifier not implemented"
sprintf (c : rest) args = c : sprintf rest args

-- Infix version of string formatting, like in Python.
-- Example: "%s:%s:%s" % [show 2, show 2, show 4] = "'2' + '2' = '4'"
infixl 7 %

(%) :: [Char] -> [[Char]] -> [Char]
(%) = sprintf

-- Infix operator to convert terms with Show instances into strings.
-- Example: "%s:%s:%s" % 2 << 2 << 4 << [] = "'2' + '2' = '4'"
infixr 8 <<

(<<) :: (Show a) => a -> [[Char]] -> [[Char]]
(<<) a l = show a : l
