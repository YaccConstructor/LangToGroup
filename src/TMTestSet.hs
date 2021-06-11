module TMTestSet where

testSet :: [(Int, [(String, Bool)])]
testSet = [
    (1, [
        ("", True),
        ("a", True),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True),
        ("aaaaa", True)
    ]),
    (2, [
        ("", False),
        ("a", True),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True),
        ("aaaaa", True)
    ]),
    (3, [
        ("", True),
        ("a", True),
        ("aa", False),
        ("aaa", False)
    ]),
    (4, [
        ("", False),
        ("a", True),
        ("b", True),
        ("ab", False),
        ("bb", False),
        ("baa", False)
    ]),
    (5, [
        ("", False),
        ("ac", False),
        ("bc", False),
        ("aba", False),
        ("abc", True),
        ("abca", False),
        ("abcc", False)
    ]),
    (6, [
        ("", False),
        ("a", False),
        ("b", False),
        ("aba", False),
        ("baba", False),
        ("ababa", True),
        ("ababaa", False)
    ]),
    (7, [
        ("", False),
        ("a", False),
        ("abc", False),
        ("aba", True),
        ("abca", False),
        ("abcb", False),
        ("abcba", True),
        ("abcbcba", True),
        ("abcbaba", False),
        ("abcbcbca", False),
        ("abcbcbcba", True),
        ("abcbcbcbcba", True)
    ])
  ] ++ [
    (i, [
        ("", True),
        ("a", False),
        ("b", False),
        ("ab", True),
        ("bab", False),
        ("abab", True),
        ("abba", False),
        ("aabb", True),
        ("ababa", False),
        ("ababab", True),
        ("abbaab", False),
        ("aabbab", True),
        ("aaabbb", True),
        ("aababbab", True),
        ("aabaababbb", True)
    ])
    | i <- [8..12]
  ] ++ [
    (i, [
        ("", True),
        ("a", False),
        ("aa", True),
        ("ab", False),
        ("aba", False),
        ("abab", False),
        ("abba", True),
        ("bbbb", True),
        ("aaba", False),
        ("aaaaa", False),
        ("babbab", True),
        ("aaaaaa", True)
    ])
    | i <- [13,14]
  ] ++ [
    (15, [
        ("", True),
        ("d", False),
        ("cc", True),
        ("da", False),
        ("cbc", False),
        ("abab", False),
        ("abba", True),
        ("bbbb", True),
        ("dddd", True),
        ("ccdd", False),
        ("adba", False),
        ("aaaaa", False),
        ("cbada", False),
        ("baccab", True),
        ("adccda", True),
        ("cadbbdac", True),
        ("cadbadac", False)
    ]),
    (16, [
        ("", True),
        ("c", False),
        ("ff", True),
        ("fg", False),
        ("hch", False),
        ("hca", False),
        ("dbdb", False),
        ("bddb", True),
        ("gccg", True),
        ("eeee", True),
        ("cehc", False),
        ("deffed", True),
        ("chaahc", True),
        ("gggggg", True)
    ]),
    (17, [
        ("", True),
        ("d", False),
        ("jj", True),
        ("ka", False),
        ("mlm", False),
        ("oabd", False),
        ("lppl", True),
        ("bddb", True),
        ("aaaa", True),
        ("ecee", False),
        ("iiiii", False),
        ("johhoj", True),
        ("iijjii", True),
        ("iphhpi", True),
        ("dnffnd", True),
        ("eiaeia", False)
    ])
  ]
