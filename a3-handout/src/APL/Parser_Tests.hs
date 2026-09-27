module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      -----------------------------------------
      testGroup
        "Task 1: Function application"
        [ parserTest "f x" $
            Apply (Var "f") (Var "x"),
          ---
          parserTest "f x y" $
            Apply (Apply (Var "f") (Var "x")) (Var "y"),
          ---
          parserTestFail "x if x then y else z",
          ---
          parserTest "x (if x then y else z)" $
            Apply (Var "x") (If (Var "x") (Var "y") (Var "z")),
          ---
          parserTest "x(y z)" $
            Apply (Var "x") (Apply (Var "y") (Var "z")),
          ---
          parserTest "f x + y" $
            Add (Apply (Var "f") (Var "x")) (Var "y")
        ],
      testGroup
        "Task 3:Printing, putting, getting"
        [ parserTest "put x y" $
            KvPut (Var "x") (Var "y"),
          ---
          parserTest "get x + y" $
            Add (KvGet (Var "x")) (Var "y"),
          ---
          parserTest "getx" $
            Var "getx",
          ---
          parserTest "print \"foo\" x" $
            Print "foo" (Var "x"),
          ---
          parserTest "print \"\" x" $
            Print "" (Var "x"),
          ---
          parserTestFail "print \"foo x",
          parserTestFail "print \"foo\"",
          parserTestFail "put x",
          parserTestFail "get"
        ],

      testGroup
        "Task 2: Equality and power operators"
        [
          -- Equality
          parserTest "x == y" $ Eql (Var "x") (Var "y"),
          ---
          parserTest "x + y == y + x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")),
          ---
          parserTest "(x == y) == z" $ Eql (Eql (Var "x") (Var "y")) (Var "z"),
          ---
          parserTest "x == (y == z)" $ Eql (Var "x") (Eql (Var "y") (Var "z")),
          ---
          parserTest "x == y == z" $ Eql (Eql (Var "x") (Var "y")) (Var "z") ,
          --------
          -- Power
          --------
          parserTest "x ** y" $ Pow (Var "x") (Var "y"),
          ---
          parserTest "x * y ** z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          ---
          parserTest "x ** y * z" $ Mul (Pow (Var "x") (Var "y")) (Var "z"),
          ---
          parserTest "(x ** y) ** z" $ Pow (Pow (Var "x") (Var "y")) (Var "z"), 
          ---
          parserTest "x ** (y ** z)" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          ---
          parserTest "x ** y ** z" $ Pow (Var "x") (Pow (Var "y") (Var "z"))
        ],
      testGroup
        "Task 4: Lambdas, let-binding, loops, and try-catch"
        [
          -- Lambda
          parserTest "\\x -> x" $ Lambda "x" (Var "x"),
          ---
          parserTest "\\x -> x + 2" $ Lambda "x" (Add (Var "x") (CstInt 2)),
          ---
          parserTest "(\\x -> x) y" $ Apply (Lambda "x" (Var "x")) (Var "y"),
          ---
          parserTest "(\\x -> x) + y" $ Add (Lambda "x" (Var "x")) (Var "y"),
          ---
          parserTestFail "x \\y -> y",
          --------
          -- Let
          --------
          parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          ---
          parserTest "let x = 2 in x + 3" $ Let "x" (CstInt 2) (Add (Var "x") (CstInt 3)),
          ---
          parserTest "let x = 1 in let y = 2 in x + y" $ Let "x" (CstInt 1) (Let "y" (CstInt 2) (Add (Var "x") (Var "y"))),
          ---
          parserTest "x (let v = 2 in v)" $ Apply (Var "x") (Let "v" (CstInt 2) (Var "v")),
          ---
          parserTestFail "let true = y in z",
          ---
          parserTestFail "x let v = 2 in v",
          --------
          -- Try-catch
          --------
          parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          ---
          parserTest "try x + 2 catch y + 3" $ TryCatch (Add (Var "x") (CstInt 2)) (Add (Var "y") (CstInt 3)),
          ---
          parserTest "try x catch try y catch z" $ TryCatch (Var "x") (TryCatch (Var "y") (Var "z")),
          ---
          parserTestFail "try x",
          ---
          parserTestFail "try x catch",
          --------
          -- ForLoop
          --------
          parserTest "loop x = 0 for i < 10 do x + i" $ ForLoop ("x", CstInt 0) ("i", CstInt 10) (Add (Var "x") (Var "i")),
          ---
          parserTest "loop x = 1 + 2 for i < 3 * 4 do x" $ ForLoop ("x", Add (CstInt 1) (CstInt 2)) ("i", Mul (CstInt 3) (CstInt 4)) (Var "x"),
          ---
          parserTestFail "loop true = 0 for i < 10 do i",
          ---
          parserTestFail "loop x = 0 for true < 10 do x",
          ---
          parserTestFail "loop x = 0 for i < 10"
        ]
    ]
