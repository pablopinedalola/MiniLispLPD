import Test.Hspec
import qualified Surface.Parser as P
import qualified Desugar        as D
import qualified Core.AST       as C
import qualified Core.Eval.Natural as N

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parsea suma simple" $
      P.parseExpr "(+ 1 2)" `shouldSatisfy` isRight

    it "parsea let y suma" $
      P.parseExpr "(let ((x 1) (y 2)) (+ x y))" `shouldSatisfy` isRight

  describe "Desugar" $ do
    it "desazucara (+ 1 2) a CAdd" $
      case P.parseExpr "(+ 1 2)" of
        Right se -> case D.desugar se of
          C.CAdd (C.CNum 1) (C.CNum 2) -> True `shouldBe` True
          _ -> expectationFailure "No es CAdd 1 2"
        Left e -> expectationFailure e

  describe "Eval (big-step)" $ do
    it "suma en let" $
      eval "(let ((x 1) (y 2)) (+ x y))" `shouldReturn` Right (C.VNum 3)

    it "if0" $
      eval "(if0 0 42 99)" `shouldReturn` Right (C.VNum 42)

    it "comparadores" $
      eval "(if (= 2 2) 1 0)" `shouldReturn` Right (C.VNum 1)

    it "división por cero (error)" $
      eval "(/ 1 0)" >>= (`shouldSatisfy` isLeft)

-- helpers
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

eval :: String -> IO (Either String C.Val)
eval src = case P.parseExpr src of
  Left e   -> pure (Left e)
  Right se -> pure (N.runEval (D.desugar se))
