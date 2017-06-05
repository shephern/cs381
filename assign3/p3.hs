--
--a)
--1) f and g both return a list, though it is unknown what type.
--    f's header would be [a]->a->[a]
--    g's header would be [a]->b->[b]
--2) For f, null works on a list, so x must be a list, and it returns x if x is
--    not empty. If x is empty, it returns y in a list, so x and y must be the
--    same data type for it to be valid in strong typing.
--   g returns a list both times, but it never returns x, so the type of x
--    isn't important, and can be a different variable.
--3) g, since the type of list is dependent on only y, whereas in f, x and y
--    need to be of the same type.
--4) f returns x as an output, so y must also be of the type x but not in a
--    list, while g only returns an empty list or y in a list, so y does not
--    have to be the same type as x.
--
--b) You could just take head [b] + snd (head [(a,b)]) and return it.
h :: [b] -> [(a, b)] -> [b]
h x y = x ++ snd (unzip y)

--c) You could take the result of (a -> b) and append it to
--    the result of ((a -> b) -> a) as a tuple, then return fst (b,a)
k :: (a -> b) -> ((a -> b) -> a) -> b
k f g = f (g f)

--d) No, it's difficult because the definition is too broad, and you don't have
--    a b to already work with. If you knew what b was to begin with, it's not
--    a -> b, it'd be a -> DataType. It's impossible to create a function that
--    will always output the correct data type b given only a, especially since
--    a might want a different b with the same a type (So 1 might want "b", but
--    2 might want True).
--Partial Examples that don't work:
--s :: a -> b
--s a = null a --Not quite a->b, it's a->Bool.
--s a = show a --a -> String
--s a = read "a" :: Int --a -> Int, assuming a is an int.
