import Test.DocTest
main = doctest ["-ilib", "-DDOCTESTING", "lib/Board.hs", "lib/TestBoards.hs"]
