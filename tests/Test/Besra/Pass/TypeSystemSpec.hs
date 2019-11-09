{-# LANGUAGE QuasiQuotes #-}

module Test.Besra.Pass.TypeSystemSpec
  ( module Test.Besra.Pass.TypeSystemSpec
  ) where

import Protolude hiding ( Type, TypeError )
import Test.Hspec
import Test.Besra.Parser.Helpers ( mkParser )
import Control.Monad.Writer
import qualified Data.Map as Map
import Besra
import Besra.Parser.Module ( parser )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Kind
import Besra.Types.IR3
import qualified Besra.TypeSystem.Error as TS
import NeatInterpolation


class Testable a where
  (==>) :: Text -> a -> IO ()

infixr 0 ==>

type TypeEnv = Map Id (Scheme PostTC)

instance Testable TypeEnv where
  input ==> expected =
    case runPass input of
      Left err -> panic $ "Failed to typecheck: " <> show err
      Right typeEnv -> typeEnv `shouldBe` expected

instance Testable TS.Error where
  input ==> expected =
    case runPass input of
      Left err -> err `shouldBe` expected
      Right typeEnv -> panic $ "Bug in typesystem: " <> show typeEnv


runPass :: Text -> Either TS.Error TypeEnv
runPass input = case mkParser parser input of
  Left err -> panic $ "Unexpected parse error: " <> show err
  Right result -> runExceptT (typeCheck "test.besra" result) >>= \case
    Left (TypeErr err) -> Left err
    Left err -> panic $ "Unexpected error: " <> show err
    Right ir -> Right . zeroOutSpans . execWriter $ collect ir

class CollectTypings a where
  collect :: a -> Writer TypeEnv ()

instance CollectTypings a => CollectTypings [a] where
  collect = traverse_ collect

instance (CollectTypings a, CollectTypings b) => CollectTypings (a, b) where
  collect (a, b) = collect a *> collect b

instance CollectTypings (Module PostTC) where
  collect (Module expls) = collect expls

instance CollectTypings (Explicit PostTC) where
  collect (Explicit name sch alts) = do
    tell $ Map.singleton name sch
    collect alts

instance CollectTypings (Implicit PostTC) where
  -- TODO correct? is a var?
  collect (Implicit _ alts) = collect alts

instance CollectTypings (Expr PostTC) where
  collect = \case
    -- TODO vars? store in ann? or via lam?
    ELam _ alts -> collect alts  -- TODO correct?
    EApp _ e1 e2 -> collect e1 *> collect e2
    EIf _ b t f -> collect b *> collect t *> collect f
    ECase _ e clauses -> collect e *> collect clauses
    ELet _ bg e -> collect bg *> collect e
    _ -> pure ()

instance CollectTypings (Pattern PostTC) where
  collect _ = pure ()

-- TODO move to generic spec helpers?
class ZeroOutSpans a where
  zeroOutSpans :: a -> a

sp :: Span
sp = Span 0 0

instance ZeroOutSpans Span where
  zeroOutSpans = const sp

instance ZeroOutSpans (Span, Kind) where
  zeroOutSpans (a, b) = (zeroOutSpans a, b)

instance ZeroOutSpans TypeEnv where
  zeroOutSpans = map zeroOutSpans

instance ZeroOutSpans (Scheme PostTC) where
  zeroOutSpans (ForAll ann ks qt) =
    ForAll (zeroOutSpans ann) ks (zeroOutSpans qt)

instance ZeroOutSpans (Qual PostTC Type) where
  -- TODO zero out spans here also
  zeroOutSpans (ps :=> a) = ps :=> zeroOutSpans a

instance ZeroOutSpans (Type PostTC) where
  zeroOutSpans = \case
    TCon tycon -> TCon $ zeroOutSpans tycon
    TVar tyvar -> TVar $ zeroOutSpans tyvar
    TApp t1 t2 -> TApp (zeroOutSpans t1) (zeroOutSpans t2)
    TGen int -> TGen int

instance ZeroOutSpans (Tycon PostTC) where
  zeroOutSpans (Tycon ann con) =
    Tycon (zeroOutSpans ann) con

instance ZeroOutSpans (Tyvar PostTC) where
  zeroOutSpans (Tyvar ann var) =
    Tyvar (zeroOutSpans ann) var


(~>) :: Text -> Scheme PostTC -> (Id, Scheme PostTC)
name ~> sch = (Id name, sch)

vKI :: Text -> Int -> Int -> Type KindInferred
vKI name begin end=
  TVar (Tyvar (Span begin end, Star) (Id name))

cKI :: Text -> Int -> Int -> Type KindInferred
cKI name begin end=
  TCon (Tycon (Span begin end, Star) (Id name))

v :: Text -> Type PostTC
v name = TVar (Tyvar (sp, Star) (Id name))

c :: Text -> Type PostTC
c name = TCon (Tycon (sp, Star) (Id name))

tyInt', tyBool', tyChar', tyString', tyA' :: Int -> Int -> Type KindInferred
tyInt' = cKI "Int"
tyBool' = cKI "Bool"
tyChar' = cKI "Char"
tyString' = cKI "String"
tyA' = vKI "a"

tyMaybe' :: Int -> Int -> Type KindInferred -> Type KindInferred
tyMaybe' begin end =
  let maybeCon = TCon (Tycon (Span begin end, KArr Star Star) (Id "Maybe"))
   in TApp maybeCon

tyInt, tyBool, tyChar, tyString, tyA, tyB :: Type PostTC
tyInt = c "Int"
tyBool = c "Bool"
tyChar = c "Char"
tyString = c "String"
tyA = v "a"
tyB = v "b"

mkScheme :: Text -> Scheme PostTC
mkScheme name =
   ForAll sp [] ([] :=> TCon (Tycon (sp, Star) (Id name)))

schInt, schBool, schChar, schString :: Scheme PostTC
schInt = mkScheme "Int"
schBool = mkScheme "Bool"
schChar = mkScheme "Char"
schString = mkScheme "String"

schLam :: Type PostTC -> Type PostTC -> Scheme PostTC
schLam t1 t2 =
  let cArrow = Tycon (sp, KArr Star (KArr Star Star)) (Id "->")
      tyArr = TApp (TApp (TCon cArrow) t1) t2
   in ForAll sp [] ([] :=> tyArr)

tyMaybe :: Type PostTC -> Type PostTC
tyMaybe = TApp (TCon (Tycon (sp, KArr Star Star) (Id "Maybe")))

schMaybe :: Type PostTC -> Scheme PostTC
schMaybe tyInner =
  ForAll sp [] ([] :=> tyMaybe tyInner)

schEither :: Type PostTC -> Type PostTC -> Scheme PostTC
schEither tLeft tRight =
  let tEither = TCon (Tycon (sp, KArr Star (KArr Star Star)) (Id "Either"))
   in ForAll sp [] ([] :=> TApp (TApp tEither tLeft) tRight)


-- TODO no implicits anymore after TS pass => IR4?
spec :: Spec
spec = describe "Typesystem" $ parallel $ do
  describe "literals" $ parallel $ do
    it "can typecheck numeric literals" $ do
      [text|
        x : Int
        x = 1
        |] ==> Map.fromList ["x" ~> schInt]

    it "fails to typecheck for badly typed numeric literal" $ do
      [text|
        data Bool = True | False
        x : Bool
        x = 1
        |] ==> TS.UnificationFailure (tyBool' 29 33) (tyInt' 38 39)

    it "can typecheck char literals" $ do
      [text|
        x : Char
        x = 'c'
        |] ==> Map.fromList ["x" ~> schChar]

    it "fails to typecheck for badly typed char literal" $ do
      [text|
        data Bool = True | False
        x : Bool
        x = 'c'
        |] ==> TS.UnificationFailure (tyBool' 29 33) (tyChar' 38 41)

    it "can typecheck string literals" $ do
      [text|
        x : String
        x = "abc"
        |] ==> Map.fromList ["x" ~> schString]

    it "fails to typecheck for badly typed string literal" $ do
      [text|
        data Bool = True | False
        x : Bool
        x = "abc"
        |] ==> TS.UnificationFailure (tyBool' 29 33) (tyString' 38 43)

  describe "variables and constructors" $ parallel $ do
    it "typechecks variables" $
      [text|
        x : Int
        x = 1
        y : Int
        y = x
        |] ==> Map.fromList ["x" ~> schInt, "y" ~> schInt]

    it "fails to typecheck badly typed variable" $
      [text|
        x : Int
        x = 1
        y : Bool
        y = x
        |] ==> TS.UnificationFailure (tyBool' 18 22) (tyInt' 4 7)

    it "typechecks simple constructors" $
      [text|
        data Bool = True | False
        x : Bool
        x = True
        y : Bool
        y = False
        |] ==> Map.fromList ["x" ~> schBool, "y" ~> schBool]

    -- TODO fix bug
{-
    it "typechecks constructors with type vars used in different ways" $ do
      [text|
        data Maybe a = Just a | Nothing
        map : (a -> b) -> Maybe a -> Maybe b
        map f x = case x of
          (Just a) -> Just (f a)
          Nothing -> Nothing
        |] ==> Map.fromList ["x" ~> schBool]
-}
    it "fails to typecheck badly typed simple constructors" $ do
      [text|
        data Bool = True | False
        x : Int
        x = True
        |] ==> TS.UnificationFailure (tyInt' 29 32) (tyBool' 5 9)
      [text|
        data Bool = True | False
        y : Int
        y = False
        |] ==> TS.UnificationFailure (tyInt' 29 32) (tyBool' 5 9)

    -- TODO fix bug
{-
    it "typechecks constructors with type variables" $ do
      [text|
        data Maybe a = Just a | Nothing
        x : Maybe a
        x = Nothing
        y : Maybe Bool
        y = Nothing
        z : Maybe Int
        z = Just 42
        |] ==> Map.fromList [ "x" ~> schMaybe tyA
                            , "y" ~> schMaybe tyBool
                            , "z" ~> schMaybe tyInt]
      [text|
        data Either a b = Left a | Right b
        a : Either a Int
        a = Right 1234
        b : Either String b
        b = Left "abc"
        |] ==> Map.fromList [ "a" ~> schEither tyA tyInt
                            , "b" ~> schEither tyString tyB]
-}

    it "fails to typecheck badly typed constructors with type variables" $ do
      [text|
        data Maybe a = Just a | Nothing
        y : Int
        y = Nothing
        |] ==> TS.UnificationFailure (tyInt' 36 39) (tyMaybe' 5 10 $ tyA' 11 12)
      [text|
        data Maybe a = Just a | Nothing
        y : Int
        y = Just "abc"
        |] ==> TS.UnificationFailure (tyInt' 36 39) (tyMaybe' 5 10 $ tyString' 49 54)
      [text|
        data Maybe a = Just a | Nothing
        y : Int
        y = Just 12345
        |] ==> TS.UnificationFailure (tyInt' 36 39) (tyMaybe' 5 10 $ tyInt' 49 54)

  describe "function applications" $ parallel $ do
    let cArrow spn = Tycon (spn, KArr Star (KArr Star Star)) (Id "->")
        tyArr spn t1 = TApp (TApp (TCon (cArrow spn)) t1)

    it "typechecks function application" $ do
      [text|
        x : (String -> Int) -> String -> Int
        x f a = f a
        y : String -> Int
        y = x (\_ -> 123)
        z : Int
        z = x (\_ -> 123) "abc"
        a : String -> Int
        a = \"abc" -> 0
        |] ==> Map.fromList [ "x" ~> schLam (tyArr (Span 0 0) tyString tyInt) (tyArr (Span 0 0) tyString tyInt)
                            , "y" ~> schLam tyString tyInt
                            , "z" ~> schInt
                            , "a" ~> schLam tyString tyInt
                            ]
      [text|
        data X = X Int
        data Person = Person String Int
        data Maybe a = Just a | Nothing
        data Either a b = Left a | Right b
        x : Int -> X
        x = X
        y : Either a Int
        y = Right 123
        z : Maybe a
        z = Nothing
        a : Int -> Person
        a = Person "Alice"
        |] ==> Map.fromList [ "x" ~> schLam tyInt (c "X")
                            , "y" ~> schEither tyA tyInt
                            , "z" ~> schMaybe tyA
                            , "a" ~> schLam tyInt (c "Person")
                            ]

    it "fails to typecheck type mismatch in function of function application" $ do
      [text|
        x : (String -> String -> Int) -> String -> Int
        x f a = f a
        |] ==> TS.UnificationFailure (tyInt' 43 46) (tyArr (Span 22 24) (tyString' 15 21) (tyInt' 25 28))
      [text|
        x : Int
        x = y 1
        y : String
        y = "abc"
        |] ==> TS.UnificationFailure (tyArr (Span 12 15) (tyInt' 14 15) (vKI "t0" 12 15)) (tyString' 20 26)

    it "fails to typecheck type mismatch in arg of function application" $ do
      [text|
        x : String -> String -> Int
        x a b = 0
        y : String -> Int
        y = x 0
        |] ==> TS.UnificationFailure (tyInt' 62 63) (tyString' 4 10)
      [text|
        x : String -> String -> Int
        x a b = 0
        y : String -> Int
        y = x "abc" 0
        |] ==> TS.UnificationFailure (tyInt' 68 69) (tyString' 14 20)

  describe "functions" $ parallel $ do
    let cArrow spn = Tycon (spn, KArr Star (KArr Star Star)) (Id "->")
        tyArr spn t1 = TApp (TApp (TCon (cArrow spn)) t1)

    it "typechecks lambdas" $ do
      [text|
        x : a -> a
        x = \a -> a
        y : Bool -> Int
        y = \b -> 0
        z : Bool -> Int
        z = \_ -> 0
        a : Int -> Int
        a = \123 -> 0
        |] ==> Map.fromList [ "x" ~> schLam (v "t0") (v "t0")
                            , "y" ~> schLam tyBool tyInt
                            , "z" ~> schLam tyBool tyInt
                            , "a" ~> schLam tyInt tyInt
                            ]
      [text|
        data X = X
        data Y = Y Int
        x : X -> Int
        x = \X -> 0
        y : Y -> Int
        y = \(Y val) -> val
        |] ==> Map.fromList [ "x" ~> schLam (c "X") tyInt
                            , "y" ~> schLam (c "Y") tyInt
                            ]

    it "fails to typecheck badly typed body in lambda" $ do
      [text|
        x : a -> a
        x = \a -> 123
        |] ==> TS.ExplicitTypeMismatch
                (ForAll (Span 6 24) [] ([] :=> tyArr (Span 6 8) (cKI "Int" 21 24) (cKI "Int" 21 24)))
                (ForAll (Span 4 10) [] ([] :=> tyArr (Span 6 8) (vKI "a" 4 5) (vKI "a" 9 10)))
      [text|
        y : Bool -> Int
        y = \b -> b
        |] ==> TS.UnificationFailure (tyInt' 12 15) (tyBool' 4 8)

    it "fails to typecheck badly typed arg in lambda" $ do
      [text|
        x : String -> Int
        x = \0 -> 0
        |] ==> TS.UnificationFailure (tyString' 4 10) (tyInt' 23 24)
      [text|
        x : String -> Int
        x = \'c' -> 0
        |] ==> TS.UnificationFailure (tyString' 4 10) (tyChar' 23 26)
      [text|
        x : Int -> Int
        x = \"abc" -> 0
        |] ==> TS.UnificationFailure (tyInt' 4 7) (tyString' 20 25)
      [text|
        data X = X
        x : Int -> Int
        x = \X -> 0
        |] ==> TS.UnificationFailure (tyInt' 15 18) (cKI "X" 5 6)
      [text|
        data X = X String
        x : Int -> Int
        x = \y@(X str) -> 0
        |] ==> TS.UnificationFailure (tyInt' 22 25) (cKI "X" 5 6)

    it "fails to typecheck wrong number of arguments in lambda" $ do
      [text|
        x : Bool -> Int -> Int
        x = \b -> 0
        |] ==> TS.UnificationFailure (tyArr (Span 16 18) (tyInt' 12 15) (tyInt' 19 22)) (tyInt' 33 34)
      [text|
        y : a -> a -> a
        y = \a -> a
        |] ==> TS.OccursCheck
                (Tyvar (Span 21 22, Star) (Id "t0"))
                (tyArr (Span 11 13) (vKI "t0" 21 22) (vKI "t0" 21 22))

    it "typechecks named single function declaration" $ do
      [text|
        x : a -> a
        x a = a
        y : Bool -> Int
        y b = 0
        z : Bool -> Int
        z _ = 0
        a : Int -> Int
        a 123 = 0
        |] ==> Map.fromList [ "x" ~> schLam (v "t0") (v "t0")
                            , "y" ~> schLam tyBool tyInt
                            , "z" ~> schLam tyBool tyInt
                            , "a" ~> schLam tyInt tyInt
                            ]
      [text|
        data X = X
        data Y = Y Int
        x : X -> Int
        x X = 0
        y : Y -> Int
        y (Y val) = val
        |] ==> Map.fromList [ "x" ~> schLam (c "X") tyInt
                            , "y" ~> schLam (c "Y") tyInt
                            ]

    it "typechecks simple recursive functions" $
      [text|
        x : Int -> a
        x y = x y
        |] ==> Map.fromList ["x" ~> schLam tyInt tyA]

    it "typechecks named function declarations with multiple clauses" $
      [text|
        data Bool = True | False
        data Maybe a = Just a | Nothing
        x : Bool -> Int
        x True = 1
        x False = 0
        isJust : Maybe a -> Bool
        isJust (Just _) = True
        isJust Nothing = False
        |] ==> Map.fromList [ "x" ~> schLam tyBool tyInt
                            , "isJust" ~> schLam (tyMaybe (v "t0")) tyBool
                            ]

    it "fails to typecheck badly typed body in single function declaration" $ do
      [text|
        x : a -> a
        x a = 123
        |] ==> TS.ExplicitTypeMismatch
                (ForAll (Span 6 20) [] ([] :=> tyArr (Span 6 8) (cKI "Int" 17 20) (cKI "Int" 17 20)))
                (ForAll (Span 4 10) [] ([] :=> tyArr (Span 6 8) (vKI "a" 4 5) (vKI "a" 9 10)))
      [text|
        y : Bool -> Int
        y b = b
        |] ==> TS.UnificationFailure (tyInt' 12 15) (tyBool' 4 8)

    it "fails to typecheck badly typed arg in single function declaration" $ do
      [text|
        x : String -> Int
        x 0 = 0
        |] ==> TS.UnificationFailure (tyString' 4 10) (tyInt' 20 21)
      [text|
        x : String -> Int
        x 'c' = 0
        |] ==> TS.UnificationFailure (tyString' 4 10) (tyChar' 20 23)
      [text|
        x : Int -> Int
        x "abc" = 0
        |] ==> TS.UnificationFailure (tyInt' 4 7) (tyString' 17 22)
      [text|
        data X = X
        x : Int -> Int
        x X = 0
        |] ==> TS.UnificationFailure (tyInt' 15 18) (cKI "X" 5 6)
      [text|
        data X = X String
        x : Int -> Int
        x y@(X str) = 0
        |] ==> TS.UnificationFailure (tyInt' 22 25) (cKI "X" 5 6)

    it "fails to typecheck badly typed body in multiple function declaration" $ do
      [text|
        x : Int -> Int
        x 1 = 'c'
        x _ = 'c'
        |] ==> TS.UnificationFailure (tyInt' 11 14) (tyChar' 21 24)
      [text|
        x : Int -> Int
        x 1 = 2
        x _ = 'c'
        |] ==> TS.UnificationFailure (tyInt' 11 14) (tyChar' 29 32)

    it "fails to typecheck badly typed arg in multiple function declaration" $ do
      [text|
        x : String -> Int
        x 1 = 0
        x 'c' = 0
        |] ==> TS.UnificationFailure (tyString' 4 10) (tyInt' 20 21)
      [text|
        x : String -> Int
        x "abc" = 0
        x 'c' = 0
        |] ==> TS.UnificationFailure (tyString' 4 10) (tyChar' 32 35)

  describe "if expressions" $ parallel $ do
    it "typechecks if expressions" $
      [text|
        data Bool = True | False
        x : Int
        x = if True then 1 else 0
        y : String
        y = if False then "nope" else "yep"
        |] ==> Map.fromList ["x" ~> schInt, "y" ~> schString]

    it "fails to typecheck badly typed condition in if" $ do
      [text|
        x : Int
        x = if 0 then 1 else 2
        |] ==> TS.UnificationFailure (tyInt' 15 16) (tyBool' 15 16)

    it "fails to typecheck badly typed true clause in if" $ do
      [text|
        data Bool = True | False
        x : Int
        x = if True then "abc" else 0
        |] ==> TS.UnificationFailure (tyInt' 61 62) (tyString' 50 55)
      [text|
        data Bool = True | False
        x : Int
        x = if True then "abc" else "def"
        |] ==> TS.UnificationFailure (tyInt' 29 32) (tyString' 50 55)
      [text|
        data Bool = True | False
        x : Bool -> Int
        x a = if a then a else "def"
        |] ==> TS.UnificationFailure (tyString' 64 69) (tyBool' 50 51)

    it "fails to typecheck badly typed false clause in if" $ do
      [text|
        data Bool = True | False
        x : Int
        x = if True then 0 else "abc"
        |] ==> TS.UnificationFailure (tyString' 57 62) (tyInt' 50 51)
      [text|
        data Bool = True | False
        x : Int
        x = if True then "abc" else "def"
        |] ==> TS.UnificationFailure (tyInt' 29 32) (tyString' 50 55)
      [text|
        data Bool = True | False
        x : Bool -> Int
        x a = if a then "abc" else a
        |] ==> TS.UnificationFailure (tyBool' 50 51) (tyString' 57 62)

  describe "case expressions" $ parallel $ do
    it "typechecks case pattern with single clause" $
      [text|
        data X = X
        x : Int
        x = case X of
          X -> 0
        |] ==> Map.fromList ["x" ~> schInt]

    it "typechecks case pattern with multiple clauses" $
      [text|
        data X = X | Y
        x : Int
        x = case X of
          X -> 0
          Y -> 1
        |] ==> Map.fromList ["x" ~> schInt]

    it "fails to typecheck badly typed expression in single clause case" $ do
      let tyX' = TCon (Tycon (Span 5 6, Star) (Id "X"))
      [text|
        data X = X
        x : Int
        x = case 0 of
          X -> 0
        |] ==> TS.UnificationFailure (tyInt' 28 29) tyX'

    it "fails to typecheck badly typed expression in multiple clause case" $
      [text|
        data Bool = True | False
        x : Int
        x = case 0 of
          True -> 1
          False -> 0
        |] ==> TS.UnificationFailure (tyInt' 42 43) (tyBool' 5 9)

    it "fails to typecheck badly typed single clause in case" $ do
      let tyX' = TCon (Tycon (Span 5 6, Star) (Id "X"))
          tyY' = TCon (Tycon (Span 16 17, Star) (Id "Y"))
      [text|
        data X = X
        data Y = Y
        x : Int
        x = case X of
          Y -> 1
        |] ==> TS.UnificationFailure tyX' tyY'

    it "fails to typecheck badly typed multiple clause in case" $ do
      let tyX' = TCon (Tycon (Span 5 6, Star) (Id "X"))
      [text|
        data X = X
        x : Int
        x = case X of
          0 -> 1
          1 -> 0
        |] ==> TS.UnificationFailure tyX' (tyInt' 35 36)

  describe "let expressions" $ parallel $ do
    let cArrow spn = Tycon (spn, KArr Star (KArr Star Star)) (Id "->")
        tyArr spn t1 = TApp (TApp (TCon (cArrow spn)) t1)

    it "typechecks let bindings" $ do
      [text|
        data Person = Person String Int
        x : Person
        x =
          let name = "Bob"
              age = 42
          in Person name age
        y : Int
        y =
          let f a b = a
              z = 42
          in f z "abc"
        z : x -> Int
        z =
          let f a b = a
              a = 42
          in f a
        |] ==> Map.fromList [ "x" ~> mkScheme "Person"
                            , "y" ~> mkScheme "Int"
                            , "z" ~> schLam (v "t17") tyInt
                            ]

    it "typechecks mutually recursive bindings in let bindings" $ do
      [text|
        x : a -> b
        x =
          let f x = g x
              g x = f x
          in f
        data Bool = True | False
        y : Bool -> String
        y =
          let f x = if x then "abc" else g x
              g x = f (not x)
              not True = False
              not False = True
          in f
        |] ==> Map.fromList [ "x" ~> schLam (v "t6") (v "t7")
                            , "y" ~> schLam tyBool tyString ]

    it "fails to typecheck badly typed expr in body of let" $ do
      [text|
        data Person = Person String Int
        x : Int
        x =
          let name = "Bob"
              age = 42
          in Person name age
        |] ==> TS.UnificationFailure (cKI "Int" 36 39) (cKI "Person" 5 11)
      [text|
        data Person = Person String Int
        x : Person
        x =
          let name = "Bob"
              age = 42
          in Person age name
        |] ==> TS.UnificationFailure (cKI "Int" 78 80) (cKI "String" 21 27)
      [text|
        data Person = Person String Int
        x : Person
        x =
          let name = "Bob"
              age = 42
          in Person name
        |] ==> TS.UnificationFailure
                (cKI "Person" 36 42)
                (tyArr (Span 14 31) (cKI "Int" 28 31) (cKI "Person" 5 11))
      [text|
        data Person = Person String Int
        x : Person
        x =
          let name = "Bob"
              age = 42
          in age
        |] ==> TS.UnificationFailure (cKI "Person" 36 42) (cKI "Int" 78 80)

    it "fails to typecheck badly explicitly typed binding in let" $ do
      [text|
        x : Int
        x =
          let name : Int
              name = "Bob"
          in name
        |] ==> TS.UnificationFailure (cKI "Int" 25 28) (cKI "String" 42 47)
      [text|
        x : Int
        x =
          let f : Int -> Int
              f _ = "Bob"
          in f 123
        |] ==> TS.UnificationFailure (cKI "Int" 29 32) (cKI "String" 45 50)
      [text|
        x : Int
        x =
          let f : Int -> Int
              f = 456
          in f 123
        |] ==> TS.UnificationFailure
                (tyArr (Span 26 28) (cKI "Int" 22 25) (cKI "Int" 29 32))
                (cKI "Int" 43 46)

    it "fails to typecheck badly implicitly typed binding in let" $ do
      [text|
        x : Int
        x =
          let name = "Bob"
          in name
        |] ==> TS.UnificationFailure (cKI "Int" 4 7) (cKI "String" 25 30)
      [text|
        x : Int
        x =
          let f _ = "Bob"
          in f 123
        |] ==> TS.UnificationFailure (cKI "Int" 4 7) (cKI "String" 24 29)
      [text|
        x : Int
        x =
          let f = 456
          in f 123
        |] ==> TS.UnificationFailure
                (tyArr (Span 31 36) (cKI "Int" 33 36) (vKI "t1" 31 36))
                (cKI "Int" 22 25)

