{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.C.Clang.Internal.Inline where

import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Language.Haskell.TH 
import Language.Haskell.TH.Quote

import Language.C.Clang.Internal.Types

clangCtx :: C.Context
clangCtx = mempty { C.ctxTypesTable = clangTypes } <> C.vecCtx <> C.bsCtx <> C.cppCtx
  where
    clangTypes =
      M.fromList
        [ (C.TypeName "SharedIndex", [t| SharedIndex |] )
        , (C.TypeName "SharedTranslationUnit", [t| SharedTranslationUnit |] )
        , (C.TypeName "WrappedCursor", [t| WrappedCursor |] )
        , (C.TypeName "WrappedType", [t| WrappedType |] )
        , (C.TypeName "WrappedSourceRange", [t| WrappedSourceRange |] )
        , (C.TypeName "WrappedSourceLocation", [t| WrappedSourceLocation |] )
        , (C.TypeName "WrappedFile", [t| WrappedFile |] )
        , (C.TypeName "WrappedToken", [t| WrappedToken |] )
        , (C.TypeName "CXCursorKind", [t| CXCursorKind |] )
        , (C.TypeName "CXString", [t| CXString |] )
        , (C.TypeName "CXUnsavedFile", [t| CXUnsavedFile |] )
        , (C.TypeName "CXToken", [t| CXToken |] )
        , (C.TypeName "CXSourceLocation", [t| CXSourceLocation |] )
        , (C.TypeName "CXFile", [t| CXFile |] )
        , (C.TypeName "ChildrenVector", [t| ChildrenVector |] )
        , (C.TypeName "Inclusion", [t| Inclusion |] )
        , (C.TypeName "InclusionVector", [t| InclusionVector |] )
        ]

delete :: String -> ExpQ
delete objName = quoteExp C.funPtr ("void delete" ++ objName ++ "(" ++ objName ++ "* obj) { delete obj; }")