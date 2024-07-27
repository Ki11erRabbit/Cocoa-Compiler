{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.ClassSpec (main, spec) where

import Text.Megaparsec
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Compiler.Ast.Method
import Compiler.Ast.Class
import Compiler.Parser.Class
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseClass" $ do
    it "parses an empty class" $ do
      parse parseClass "" "class Foo {}" `shouldParse` Class PrivateVis "Foo" RegularType [] Nothing [] []
    it "parses an empty class with type params" $ do
      parse parseClass "" "class Foo<T> {}" `shouldParse` Class PrivateVis "Foo" RegularType [(Generic "T" [])] Nothing [] []
    it "parses an empty class with super" $ do
      parse parseClass "" "class Foo extends Bar {}" `shouldParse` Class PrivateVis "Foo" RegularType [] (Just (SuperClass (Path ["Bar"]) [])) [] []
    it "parses an empty class with super and type params" $ do
      parse parseClass "" "class Foo<T> extends Bar {}" `shouldParse` Class PrivateVis "Foo" RegularType [(Generic "T" [])] (Just (SuperClass (Path ["Bar"]) [])) [] []
    it "parses an empty class with super and interfaces" $ do
      parse parseClass "" "class Foo extends Bar implements Baz {}" `shouldParse` Class PrivateVis "Foo" RegularType [] (Just (SuperClass (Path ["Bar"]) [])) [(SuperClass (Path ["Baz"]) [])] []
    it "parses an empty class with super, interfaces, and type params" $ do
      parse parseClass "" "class Foo<T> extends Bar implements Baz {}" `shouldParse` Class PrivateVis "Foo" RegularType [(Generic "T" [])] (Just (SuperClass (Path ["Bar"]) [])) [SuperClass (Path ["Baz"]) []] []
    it "parses an empty class with visibility" $ do
      parse parseClass "" "pub class Foo {}" `shouldParse` Class PublicVis "Foo" RegularType [] Nothing [] []
    it "parses an empty class with visibility and type params" $ do
      parse parseClass "" "pub class Foo<T> {}" `shouldParse` Class PublicVis "Foo" RegularType [(Generic "T" [])] Nothing [] []
    it "parses an empty class with visibility and super" $ do
      parse parseClass "" "pub class Foo extends Bar {}" `shouldParse` Class PublicVis "Foo" RegularType [] (Just (SuperClass (Path ["Bar"]) [])) [] []
    it "parses an empty class with visibility, super, and type params" $ do
      parse parseClass "" "pub class Foo<T> extends Bar {}" `shouldParse` Class PublicVis "Foo" RegularType [(Generic "T" [])] (Just (SuperClass (Path ["Bar"]) [])) [] []
    it "parses an empty class with visibility, super, and interfaces" $ do
      parse parseClass "" "pub class Foo extends Bar implements Baz {}" `shouldParse` Class PublicVis "Foo" RegularType [] (Just (SuperClass (Path ["Bar"]) [])) [(SuperClass (Path ["Baz"]) [])] []
    it "parses a class with a method" $ do
      parse parseClass "" "class Foo { fn bar() u8 { return 0; } }" `shouldParse` Class PrivateVis "Foo" RegularType [] Nothing [] [MethodMember $ Method PrivateVis False False False "bar" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])]
    it "parses a class with a method and type params" $ do
      parse parseClass "" "class Foo { fn bar<T>() u8 { return 0; } }" `shouldParse` Class PrivateVis "Foo" RegularType [] Nothing [] [MethodMember $ Method PrivateVis False False False "bar" [(Generic "T" [])] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])]
    it "parses a class with a method and super" $ do
      parse parseClass "" "class Foo extends Bar { fn bar() u8 { return 0; } }" `shouldParse` Class PrivateVis "Foo" RegularType [] (Just (SuperClass (Path ["Bar"]) [])) [] [MethodMember $ Method PrivateVis False False False "bar" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])]
    it "parses a class with a method and super and type params" $ do
      parse parseClass "" "class Foo<T> extends Bar { fn bar() u8 { return 0; } }" `shouldParse` Class PrivateVis "Foo" RegularType [(Generic "T" [])] (Just (SuperClass (Path ["Bar"]) [])) [] [MethodMember $ Method PrivateVis False False False "bar" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])]
    it "parses a class with a method and super and interfaces" $ do
      parse parseClass "" "class Foo extends Bar implements Baz { fn bar() u8 { return 0; } }" `shouldParse` Class PrivateVis "Foo" RegularType [] (Just (SuperClass (Path ["Bar"]) [])) [(SuperClass (Path ["Baz"]) [])] [MethodMember $ Method PrivateVis False False False "bar" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])]
    it "parses a class with a field member" $ do
      parse parseClass "" "class Foo { bar: u8; }" `shouldParse` Class PrivateVis "Foo" RegularType [] Nothing [] [FieldMember $ Field PrivateVis "bar" (Primitive U8PrimType)]
    it "parses a class with a field and method member" $ do
      parse parseClass "" "class Foo { bar: u8; fn baz() u8 { return 0; } }" `shouldParse` Class PrivateVis "Foo" RegularType [] Nothing [] [FieldMember $ Field PrivateVis "bar" (Primitive U8PrimType), MethodMember $ Method PrivateVis False False False "baz" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])]
      
     
