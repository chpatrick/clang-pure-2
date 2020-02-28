{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.C.Clang.Internal.FFI where

import Foreign
import Foreign.ForeignPtr.Unsafe
import Foreign.C

import Control.Exception
import Data.Traversable
import qualified Language.C.Inline as C hiding (block, exp)
import qualified Language.C.Inline.Unsafe as C
import qualified Language.C.Inline.Cpp as C hiding (block, exp)
import System.IO.Unsafe
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Control.Monad.IO.Class
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.Maybe

import Language.C.Clang.Internal.Inline
import Language.C.Clang.Internal.Types
import Language.C.Clang.Internal.Hsc

C.context clangCtx
C.include "ClangPure.h"
C.using "namespace std"

class CType a where
  type CTypeRef a
  toForeignPtr :: a -> ForeignPtr (CTypeRef a)
  unsafeConsume :: Ptr (CTypeRef a) -> IO a

referenceEqual :: CType a => a -> a -> Bool
referenceEqual objA objB = unsafeForeignPtrToPtr (toForeignPtr objA) == unsafeForeignPtrToPtr (toForeignPtr objB)

instance CType Index where
  type CTypeRef Index = SharedIndex
  toForeignPtr (Index ptr) = ptr
  unsafeConsume = fmap Index . newForeignPtr $(delete "SharedIndex")

instance CType TranslationUnit where
  type CTypeRef TranslationUnit = SharedTranslationUnit
  toForeignPtr (TranslationUnit ptr) = ptr
  unsafeConsume = fmap TranslationUnit . newForeignPtr $(delete "SharedTranslationUnit")

instance CType Cursor where
  type CTypeRef Cursor = WrappedCursor
  toForeignPtr (Cursor ptr) = ptr
  unsafeConsume = fmap Cursor . newForeignPtr $(delete "WrappedCursor")

instance CType Type where
  type CTypeRef Type = WrappedType
  toForeignPtr (Type ptr) = ptr
  unsafeConsume = fmap Type . newForeignPtr $(delete "WrappedType")

instance CType SourceLocation where
  type CTypeRef SourceLocation = WrappedSourceLocation
  toForeignPtr (SourceLocation ptr) = ptr
  unsafeConsume = fmap SourceLocation . newForeignPtr $(delete "WrappedSourceLocation")

instance CType File where
  type CTypeRef File = WrappedFile
  toForeignPtr (File ptr) = ptr
  unsafeConsume = fmap File . newForeignPtr $(delete "WrappedFile")

instance CType SourceRange where
  type CTypeRef SourceRange = WrappedSourceRange
  toForeignPtr (SourceRange ptr) = ptr
  unsafeConsume = fmap SourceRange . newForeignPtr $(delete "WrappedSourceRange")

instance CType Token where
  type CTypeRef Token = WrappedToken
  toForeignPtr (Token ptr) = ptr
  unsafeConsume = fmap Token . newForeignPtr $(delete "WrappedToken")

defaultCreateIndexOptions :: CreateIndexOptions
defaultCreateIndexOptions = CreateIndexOptions
  { cioExcludeDeclarationsFromPCH = False
  , cioDisplayDiagnostics = False
  }

createIndexWithOptions :: CreateIndexOptions -> IO Index
createIndexWithOptions opts = do
  let
    excludeDecl = fromBool $ cioExcludeDeclarationsFromPCH opts
    displayDiag = fromBool $ cioDisplayDiagnostics opts
  mask_ $
    unsafeConsume =<< [C.block| SharedIndex* {
      return new SharedIndex(clang_createIndex($(int excludeDecl), $(int displayDiag)), clang_disposeIndex);
      } |]

defaultParseTranslationUnitOptions :: ParseTranslationUnitOptions
defaultParseTranslationUnitOptions = ParseTranslationUnitOptions
  { ptuDetailedPreprocessingRecord = False
  , ptuIncomplete = False
  , ptuPrecompiledPreamble = False
  , ptuCacheCompletionResults = False
  , ptuForSerialization = False
  , ptuCXXChainedPCH = False
  , ptuSkipFunctionBodies = False
  , ptuIncludeBriefCommentsInCodeCompletion = False
  , ptuCreatePreambleOnFirstParse = False
  , ptuKeepGoing = False
  , ptuSingleFileParse = False
  }

parseTranslationUnit
  :: Index
  -> FilePath
  -> V.Vector String
  -> V.Vector UnsavedFile
  -> ParseTranslationUnitOptions
  -> IO TranslationUnit
parseTranslationUnit index path args unsavedFiles opts = ffiIO $ do
  cIndex <- withC index
  cPath <- FFI $ withCString path
  cArgs :: VS.Vector CString <- VS.convert <$> traverse (\arg -> FFI (withCString arg)) args
  cUnsavedFiles :: VS.Vector CXUnsavedFile <- VS.convert <$> traverse withUnsavedFile unsavedFiles
  let cOpts = parseTranslationUnitOptionsToEnum opts
  ( cTranslationUnit, result ) <- liftIO $ C.withPtr $ \( outTranslationUnit ) -> [C.block| unsigned int {
    SharedIndex& index = *$(SharedIndex* cIndex);
    CXTranslationUnit translationUnit;

    CXErrorCode result = clang_parseTranslationUnit2(
      index.get(),
      $(const char *cPath),
      $vec-ptr:(const char *const *cArgs),
      $vec-len:cArgs,
      $vec-ptr:(CXUnsavedFile* cUnsavedFiles),
      $vec-len:cUnsavedFiles,
      $(unsigned int cOpts),
      &translationUnit
    );

    if (result == CXError_Success) {
      *$(SharedTranslationUnit** outTranslationUnit) =
        new SharedTranslationUnit(
          new WrappedTranslationUnit(index,
            { translationUnit, clang_disposeTranslationUnit }));
    }

    return result;
    } |]
  case parseClangError result of
    Nothing -> liftIO $ fmap TranslationUnit $ newForeignPtr $(delete "SharedTranslationUnit") cTranslationUnit
    Just err -> fail ("Couldn't parse translation unit, got error: " ++ show err)

translationUnitSpelling :: TranslationUnit -> BS.ByteString
translationUnitSpelling = unaryParse consumeCXString $ \cTransUnit ->
  [C.exp| CXString* { new CXString(clang_getTranslationUnitSpelling((*$(SharedTranslationUnit* cTransUnit))->obj.get())) } |]

ffiIO :: FFI a -> IO a
ffiIO (FFI f) = mask_ $ f return

ffi :: FFI a -> a
ffi = unsafePerformIO . ffiIO

-- This is considered safe because FFI is always masked.
consume :: CType a => Ptr (CTypeRef a) -> FFI a
consume = liftIO . unsafeConsume

-- Like consume but returns null pointers as Nothing.
consumeMaybe :: CType a => Ptr (CTypeRef a) -> FFI (Maybe a)
consumeMaybe ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just <$> consume ptr

-- Lift a function from Clang type to CXString.
unaryParse :: (CType a) => (b -> FFI c) -> (Ptr (CTypeRef a) -> IO b) -> a -> c
unaryParse parse f obj = ffi $ do
  ptr <- withC obj
  res <- liftIO $ f ptr
  parse res

-- Lift a function from Clang type to Clang type.
unary :: (CType a, CType b) => (Ptr (CTypeRef a) -> IO (Ptr (CTypeRef b))) -> a -> b
unary = unaryParse consume

binaryParse :: (CType a, CType b) => (c -> FFI d) -> (Ptr (CTypeRef a) -> Ptr (CTypeRef b) -> IO c) -> a -> b -> d
binaryParse parse f objA objB = ffi $ do
  ptrA <- withC objA
  ptrB <- withC objB
  res <- liftIO $ f ptrA ptrB
  parse res

withC :: CType a => a -> FFI (Ptr (CTypeRef a))
withC obj = FFI $ withForeignPtr $ toForeignPtr obj

withUnsavedFile :: UnsavedFile -> FFI CXUnsavedFile
withUnsavedFile file = do
  cufFilename <- FFI $ withCString (ufFilename file)
  ( cufContents, len ) <- FFI $ BS.unsafeUseAsCStringLen (ufContents file)
  return CXUnsavedFile { cufFilename, cufContents, cufLength = fromIntegral len }

-- NOTE: This takes a CXString allocated with new()
consumeCXString :: Ptr CXString -> FFI BS.ByteString
consumeCXString stringPtr = liftIO $ do
  cStringPtr <- [C.exp| const char* { clang_getCString(*$(CXString* stringPtr)) } |]
  string <- BS.packCString cStringPtr
  [C.block| void {
    CXString* string = $(CXString* stringPtr);
    clang_disposeString(*string); 
    delete string;
    } |]
  return string

translationUnitCursor :: TranslationUnit -> Cursor
translationUnitCursor = unary $ \cTransUnit ->
  [C.block| WrappedCursor* {
    auto& sharedTransUnit = *$(SharedTranslationUnit* cTransUnit);
    return new WrappedCursor(sharedTransUnit, clang_getTranslationUnitCursor(sharedTransUnit->obj.get()));
    } |]

cursorChildren :: Cursor -> V.Vector Cursor
cursorChildren cursor = ffi $ do
  cCursor <- withC cursor
  ( numChildren, childrenPtr, childrenVec ) <- liftIO $ C.withPtrs_ $ \( numChildren, children, childrenVec ) ->
    [C.block| void {
      // Variables used inside the visitor are placed here as they must be passed by a pointer.
      struct Context {
        ChildrenVector* children;
        WrappedCursor* parent;
      } context = {
        .children = new ChildrenVector(),
        .parent = $(WrappedCursor* cCursor)
      };

      const auto visitor = [](CXCursor cursor, CXCursor parent, CXClientData clientData) {
        Context* context = reinterpret_cast<Context*>(clientData);

        context->children->push_back(context->parent->sibling(cursor));

        return CXChildVisit_Continue;
      };

      clang_visitChildren(context.parent->obj, visitor, &context);

      *$(size_t* numChildren) = context.children->size();
      *$(WrappedCursor*** children) = context.children->data();
      *$(ChildrenVector** childrenVec) = context.children;
      } |]
  childrenFPtr <- liftIO $ newForeignPtr_ childrenPtr
  let childrenPtrs = VS.unsafeFromForeignPtr0 childrenFPtr (fromIntegral numChildren)
  children <- traverse consume (V.convert childrenPtrs)
  liftIO [C.block| void { delete $(ChildrenVector* childrenVec); } |]
  return children

cursorKind :: Cursor -> CursorKind
cursorKind = unaryParse (pure . parseCursorKind) $ \cCursor ->
  [C.exp| CXCursorKind { clang_getCursorKind($(WrappedCursor* cCursor)->obj) } |]

cursorSpelling :: Cursor -> BS.ByteString
cursorSpelling = unaryParse consumeCXString $ \cCursor ->
  [C.exp| CXString* { $(WrappedCursor* cCursor)->call(clang_getCursorSpelling) } |]

cursorDisplayName :: Cursor -> BS.ByteString
cursorDisplayName = unaryParse consumeCXString $ \cCursor ->
  [C.exp| CXString* { $(WrappedCursor* cCursor)->call(clang_getCursorDisplayName) } |]

cursorMangling :: Cursor -> BS.ByteString
cursorMangling = unaryParse consumeCXString $ \cCursor ->
  [C.exp| CXString* { $(WrappedCursor* cCursor)->call(clang_Cursor_getMangling) } |]

cursorType :: Cursor -> Maybe Type
cursorType = fmap ifValidType $ unary $ \cCursor ->
  [C.exp| WrappedType* { $(WrappedCursor* cCursor)->callSibling(clang_getCursorType) } |]

includedFile :: Cursor -> Maybe File
includedFile = unaryParse consumeMaybe $ \cCursor ->
  [C.block| WrappedFile* {
    auto& cursor = *$(WrappedCursor* cCursor);
    CXFile file = clang_getIncludedFile(cursor.obj);
    if (file == nullptr) {
      return nullptr;
    } else {
      return cursor.sibling(file);
    }
  } |]

typeSpelling :: Type -> BS.ByteString
typeSpelling = unaryParse consumeCXString $ \cType ->
  [C.exp| CXString* { $(WrappedType* cType)->call(clang_getTypeSpelling) } |]

instance Eq Index where
  (==) = referenceEqual

instance Eq TranslationUnit where
  (==) = referenceEqual

instance Eq Cursor where
  (==) = binaryParse (pure . toBool) $ \cCursorA cCursorB ->
    [C.exp| unsigned int { clang_equalCursors($(WrappedCursor* cCursorA)->obj, $(WrappedCursor* cCursorB)->obj) } |]

instance Eq Type where
  (==) = binaryParse (pure . toBool) $ \cTypeA cTypeB ->
    [C.exp| unsigned int { clang_equalTypes($(WrappedType* cTypeA)->obj, $(WrappedType* cTypeB)->obj) } |]

instance Eq File where
  (==) = binaryParse (pure . toBool) $ \cFileA cFileB ->
    [C.exp| unsigned int { clang_File_isEqual($(WrappedFile* cFileA)->obj, $(WrappedFile* cFileB)->obj) } |]

instance Eq SourceLocation where
  (==) = binaryParse (pure . toBool) $ \cSourceLocationA cSourceLocationB ->
    [C.exp| unsigned int { clang_equalLocations($(WrappedSourceLocation* cSourceLocationA)->obj, $(WrappedSourceLocation* cSourceLocationB)->obj) } |]

instance Eq SourceRange where
  (==) = binaryParse (pure . toBool) $ \cSourceRangeA cSourceRangeB ->
    [C.exp| unsigned int { clang_equalRanges($(WrappedSourceRange* cSourceRangeA)->obj, $(WrappedSourceRange* cSourceRangeB)->obj) } |]

-- | Retrieve the complete file and path name of the given file.
fileName :: File -> BS.ByteString
fileName = unaryParse consumeCXString $ \cFile ->
  [C.exp| CXString* { $(WrappedFile* cFile)->call(clang_getFileName) } |]

-- | Retrieve the last modification time of the given file.
fileTime :: File -> UTCTime
fileTime = unaryParse (pure . posixSecondsToUTCTime . realToFrac) $ \cFile ->
  [C.exp| time_t { clang_getFileTime($(WrappedFile* cFile)->obj) } |]

isFileMultipleIncludeGuarded :: File -> Bool
isFileMultipleIncludeGuarded = unaryParse (pure . toBool) $ \cFile ->
  [C.block| unsigned int { 
    auto& file = *$(WrappedFile* cFile);
    return clang_isFileMultipleIncludeGuarded(file.parent->obj.get(), file.obj); 
    } |]

instance Show File where
  show file = "File {fileName = " ++ show (fileName file) ++ "}"

deriving instance Show Location
deriving instance Eq Location

instance Show SourceLocation where
  show loc = "SourceLocation {spellingLocation = " ++ show (spellingLocation loc) ++ "}"

canonicalType :: Type -> Type
canonicalType = unary $ \cType ->
  [C.exp| WrappedType* { $(WrappedType* cType)->callSibling(clang_getCanonicalType) } |]

isConstQualifiedType :: Type -> Bool
isConstQualifiedType = unaryParse (pure . toBool) $ \cType ->
  [C.exp| unsigned int { clang_isConstQualifiedType($(WrappedType* cType)->obj) } |]

typeKind :: Type -> TypeKind
typeKind = fromMaybe (error "Got an invalid type, this is a bug.") . typeKind'

typeKind' :: Type -> Maybe TypeKind
typeKind' = unaryParse (pure . parseTypeKind) $ \cType ->
  [C.exp| unsigned int { $(WrappedType* cType)->obj.kind } |]

ifValidType :: Type -> Maybe Type
ifValidType type_ = type_ <$ typeKind' type_

resultType :: Type -> Maybe Type
resultType = fmap ifValidType $ unary $ \cType ->
  [C.exp| WrappedType* { $(WrappedType* cType)->callSibling(clang_getResultType) } |]

functionTypeCallingConv :: Type -> Maybe CallingConv
functionTypeCallingConv = unaryParse (pure . parseCallingConv) $ \cType ->
  [C.exp| unsigned int { clang_getFunctionTypeCallingConv($(WrappedType* cType)->obj) } |]

elementType :: Type -> Maybe Type
elementType = fmap ifValidType $ unary $ \cType ->
  [C.exp| WrappedType* { $(WrappedType* cType)->callSibling(clang_getElementType) } |]

typedefName :: Type -> BS.ByteString
typedefName = unaryParse consumeCXString $ \cType ->
  [C.exp| CXString* { $(WrappedType* cType)->call(clang_getTypedefName) } |]

pointeeType :: Type -> Maybe Type
pointeeType = fmap ifValidType $ unary $ \cType ->
  [C.exp| WrappedType* { $(WrappedType* cType)->callSibling(clang_getPointeeType) } |]

isPODType :: Type -> Bool
isPODType = unaryParse (pure . toBool) $ \cType ->
  [C.exp| unsigned int { clang_isPODType($(WrappedType* cType)->obj) } |]

arrayElementType :: Type -> Maybe Type
arrayElementType = fmap ifValidType $ unary $ \cType ->
  [C.exp| WrappedType* { $(WrappedType* cType)->callSibling(clang_getArrayElementType) } |]

arraySize :: Type -> Maybe Int
arraySize = unaryParse (pure . checkRes) $ \cType ->
  [C.exp| long long { clang_getArraySize($(WrappedType* cType)->obj) } |]
  where
    checkRes n
      | n >= 0 = Just (fromIntegral n)
      | otherwise = Nothing

classType :: Type -> Maybe Type
classType = fmap ifValidType $ unary $ \cType ->
  [C.exp| WrappedType* { $(WrappedType* cType)->callSibling(clang_Type_getClassType) } |]

sizeOf :: Type -> Either TypeLayoutError Int
sizeOf = unaryParse (pure . parseTypeLayoutError) $ \cType ->
  [C.exp| long long { clang_Type_getSizeOf($(WrappedType* cType)->obj) } |]

alignOf :: Type -> Either TypeLayoutError Int
alignOf = unaryParse (pure . parseTypeLayoutError) $ \cType ->
  [C.exp| long long { clang_Type_getAlignOf($(WrappedType* cType)->obj) } |]

offsetOf :: Type -> BS.ByteString -> Either TypeLayoutError Int
offsetOf type_ fieldName = flip (unaryParse (pure . parseTypeLayoutError)) type_ $ \cType ->
  BS.useAsCString fieldName $ \cFieldName ->
    [C.exp| long long { clang_Type_getOffsetOf($(WrappedType* cType)->obj, $(const char* cFieldName)) } |]

offsetOfField :: Cursor -> Either TypeLayoutError Int
offsetOfField = unaryParse (pure . parseTypeLayoutError) $ \cCursor ->
  [C.exp| long long { clang_Cursor_getOffsetOfField($(WrappedCursor* cCursor)->obj) } |]

isVariadic :: Cursor -> Bool
isVariadic = unaryParse (pure . toBool) $ \cCursor ->
  [C.exp| unsigned int { clang_Cursor_isVariadic($(WrappedCursor* cCursor)->obj) } |]

argTypes :: Type -> Maybe (V.Vector Type)
argTypes type_ = ffi $ do
  cType <- withC type_
  numArgTypes <- liftIO [C.exp| int { clang_getNumArgTypes($(WrappedType* cType)->obj) } |]
  if numArgTypes < 0
    then return Nothing
    else Just <$> do
      argTypePtrs <- liftIO $ do
        argTypePtrsM <- VSM.new (fromIntegral numArgTypes)
        [C.block| void {
          for (unsigned argTypeIndex = 0; argTypeIndex < static_cast<unsigned>($(int numArgTypes)); argTypeIndex++) {
            $vec-ptr:(WrappedType** argTypePtrsM)[argTypeIndex] = $(WrappedType* cType)->callSibling(clang_getArgType, argTypeIndex);
          }
          } |]
        VS.unsafeFreeze argTypePtrsM
      traverse consume (V.convert argTypePtrs)

templateArguments :: Type -> Maybe (V.Vector Type)
templateArguments type_ = ffi $ do
  cType <- withC type_
  numTemplateArguments <- liftIO [C.exp| int { clang_Type_getNumTemplateArguments($(WrappedType* cType)->obj) } |]
  if numTemplateArguments < 0
    then return Nothing
    else Just <$> do
      templateArgPtrs <- liftIO $ do
        templateArgPtrsM <- VSM.new (fromIntegral numTemplateArguments)
        [C.block| void {
          for (unsigned templateArgIndex = 0; templateArgIndex < static_cast<unsigned>($(int numTemplateArguments)); templateArgIndex++) {
            $vec-ptr:(WrappedType** templateArgPtrsM)[templateArgIndex] = $(WrappedType* cType)->callSibling(clang_Type_getTemplateArgumentAsType, templateArgIndex);
          }
          } |]
        VS.unsafeFreeze templateArgPtrsM
      traverse consume (V.convert templateArgPtrs)

isAnonymous :: Cursor -> Bool
isAnonymous = unaryParse (pure . toBool) $ \cCursor ->
  [C.exp| unsigned int { clang_Cursor_isAnonymous($(WrappedCursor* cCursor)->obj) } |]

isBitField :: Cursor -> Bool
isBitField = unaryParse (pure . toBool) $ \cCursor ->
  [C.exp| unsigned int { clang_Cursor_isBitField($(WrappedCursor* cCursor)->obj) } |]

isVirtualBase :: Cursor -> Bool
isVirtualBase = unaryParse (pure . toBool) $ \cCursor ->
  [C.exp| unsigned int { clang_isVirtualBase($(WrappedCursor* cCursor)->obj) } |]

cursorLocation :: Cursor -> SourceLocation
cursorLocation = unary $ \cCursor ->
  [C.exp| WrappedSourceLocation* { $(WrappedCursor* cCursor)->callSibling(clang_getCursorLocation) } |]

getLocation
  :: FunPtr (Ptr CXSourceLocation -> Ptr CXFile -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ())
  -> SourceLocation
  -> Location
getLocation getter loc = ffi $ do
  cLoc <- withC loc
  ( filePtr, line, column, offset ) <- liftIO $ C.withPtrs_ $ \( file, line, column, offset ) ->
    [C.block| void {
      auto& loc = *$(WrappedSourceLocation* cLoc);
      CXFile file;
      $(void (*getter)(CXSourceLocation*, CXFile*, unsigned int*, unsigned int*, unsigned int*))
        (&loc.obj, &file, $(unsigned int* line), $(unsigned int* column), $(unsigned int* offset));
      *$(WrappedFile** file) = loc.sibling(file);
      } |]
  file <- consume filePtr
  return Location
    { locFile = file
    , locLine = fromIntegral line
    , locColumn = fromIntegral column
    , locOffset = fromIntegral offset
    }

spellingLocation :: SourceLocation -> Location
spellingLocation = getLocation [C.funPtr|
  void spellingLocation(CXSourceLocation* loc, CXFile* file, unsigned int* line, unsigned int* column, unsigned int* offset) {
    clang_getSpellingLocation(*loc, file, line, column, offset);
  } |]

fileLocation :: SourceLocation -> Location
fileLocation = getLocation [C.funPtr|
  void fileLocation(CXSourceLocation* loc, CXFile* file, unsigned int* line, unsigned int* column, unsigned int* offset) {
    clang_getFileLocation(*loc, file, line, column, offset);
  } |]

expansionLocation :: SourceLocation -> Location
expansionLocation = getLocation [C.funPtr|
  void expansionLocation(CXSourceLocation* loc, CXFile* file, unsigned int* line, unsigned int* column, unsigned int* offset) {
    clang_getExpansionLocation(*loc, file, line, column, offset);
  } |]

instantiationLocation :: SourceLocation -> Location
instantiationLocation = getLocation [C.funPtr|
  void instantiationLocation(CXSourceLocation* loc, CXFile* file, unsigned int* line, unsigned int* column, unsigned int* offset) {
    clang_getInstantiationLocation(*loc, file, line, column, offset);
  } |]

locationCursor :: SourceLocation -> Cursor
locationCursor = unary $ \cLocation ->
  [C.block| WrappedCursor* {
    auto& location = *$(WrappedSourceLocation* cLocation);
    return location.sibling(clang_getCursor(location.parent->obj.get(), location.obj));
    } |]

isInSystemHeader :: SourceLocation -> Bool
isInSystemHeader = unaryParse (pure . toBool) $ \cLocation ->
  [C.exp| int { clang_Location_isInSystemHeader($(WrappedSourceLocation* cLocation)->obj) } |]

isFromMainFile :: SourceLocation -> Bool
isFromMainFile = unaryParse (pure . toBool) $ \cLocation ->
  [C.exp| int { clang_Location_isFromMainFile($(WrappedSourceLocation* cLocation)->obj) } |]

cursorExtent :: Cursor -> SourceRange
cursorExtent = unary $ \cCursor ->
  [C.exp| WrappedSourceRange* { $(WrappedCursor* cCursor)->callSibling(clang_getCursorExtent) } |]

rangeStart :: SourceRange -> SourceLocation
rangeStart = unary $ \cRange ->
  [C.exp| WrappedSourceLocation* { $(WrappedSourceRange* cRange)->callSibling(clang_getRangeStart) } |]

rangeEnd :: SourceRange -> SourceLocation
rangeEnd = unary $ \cRange ->
  [C.exp| WrappedSourceLocation* { $(WrappedSourceRange* cRange)->callSibling(clang_getRangeEnd) } |]

instance Show TranslationUnit where
  show transUnit = "TranslationUnit {translationUnitSpelling = " ++ show (translationUnitSpelling transUnit) ++ "}"

instance Show Cursor where
  show cursor = "Cursor {cursorKind = " ++ show (cursorKind cursor) ++ ", cursorSpelling = " ++ show (cursorSpelling cursor) ++ "}"

instance Show Type where
  show type_ = "Type {typeKind = " ++ show (typeKind type_) ++ ", typeSpelling = " ++ show (typeSpelling type_) ++ "}"

instance Show SourceRange where
  show range = "SourceRange {rangeStart = " ++ show (rangeStart range) ++ ", rangeEnd = " ++ show (rangeEnd range) ++ "}"

instance Show Token where
  show token = "Token {tokenSpelling=" ++ show (tokenSpelling token) ++ "}"

tokenize :: TranslationUnit -> SourceRange -> V.Vector Token
tokenize transUnit range = ffi $ do
  cTransUnit <- withC transUnit
  cRange <- withC range
  tokenPtrs <- liftIO $ do
    ( tokens, numTokens ) <- C.withPtrs_ $ \( tokens, numTokens ) ->
      [C.block| void {
        clang_tokenize(
          (*$(SharedTranslationUnit* cTransUnit))->obj.get(),
          $(WrappedSourceRange* cRange)->obj,
          $(CXToken** tokens),
          $(unsigned int* numTokens)
        ); } |]
    tokenPtrsM <- VSM.new (fromIntegral numTokens)
    [C.block| void {
      auto& transUnit = *$(SharedTranslationUnit* cTransUnit);
      const unsigned numTokens = $(unsigned int numTokens);
      CXToken* tokens = $(CXToken* tokens);
      for (unsigned tokenIndex = 0; tokenIndex < numTokens; tokenIndex++) {
        $vec-ptr:(WrappedToken** tokenPtrsM)[tokenIndex] = new WrappedToken(transUnit, tokens[tokenIndex]);
      }
      clang_disposeTokens(transUnit->obj.get(), tokens, numTokens);
      } |]
    VS.unsafeFreeze tokenPtrsM
  traverse consume $ V.convert tokenPtrs

tokenSpelling :: Token -> BS.ByteString
tokenSpelling = unaryParse consumeCXString $ \cToken ->
  [C.block| CXString* {
    auto& token = *$(WrappedToken* cToken);
    return new CXString(clang_getTokenSpelling(token.parent->obj.get(), token.obj));
    } |]

tokenLocation :: Token -> SourceLocation
tokenLocation = unary $ \cToken ->
  [C.block| WrappedSourceLocation* {
    auto& token = *$(WrappedToken* cToken);
    return token.sibling(clang_getTokenLocation(token.parent->obj.get(), token.obj));
    } |]

tokenExtent :: Token -> SourceRange
tokenExtent = unary $ \cToken ->
  [C.block| WrappedSourceRange* {
    auto& token = *$(WrappedToken* cToken);
    return token.sibling(clang_getTokenExtent(token.parent->obj.get(), token.obj));
    } |]

inclusions :: TranslationUnit -> V.Vector ( File, Maybe SourceLocation )
inclusions transUnit = ffi $ do
  cTransUnit <- withC transUnit
  ( numInclusions, inclusionsPtr, inclusionsVec ) <- liftIO $ C.withPtrs_ $ \( numInclusions, inclusionsPtr, inclusionsVec ) ->
    [C.block| void {
      // Variables used inside the visitor are placed here as they must be passed by a pointer.
      struct Context {
        InclusionVector* inclusions;
        SharedTranslationUnit* transUnit;
      } context = {
        .inclusions = new InclusionVector(),
        .transUnit = $(SharedTranslationUnit* cTransUnit)
      };

      const auto visitor = [](CXFile includedFile, CXSourceLocation* inclusionStack, unsigned includeLen, CXClientData clientData) {
        Context* context = reinterpret_cast<Context*>(clientData);

        WrappedFile* file = new WrappedFile(*(context->transUnit), includedFile);

        WrappedSourceLocation* includeLocation;
        if (includeLen == 0) {
          includeLocation = nullptr;
        } else {
          includeLocation = new WrappedSourceLocation(*(context->transUnit), *inclusionStack);
        }

        context->inclusions->push_back(new Inclusion(file, includeLocation));
      };

      clang_getInclusions((*context.transUnit)->obj.get(), visitor, &context);

      *$(size_t* numInclusions) = context.inclusions->size();
      *$(Inclusion*** inclusionsPtr) = context.inclusions->data();
      *$(InclusionVector** inclusionsVec) = context.inclusions;
      } |]
  inclusionFPtr <- liftIO $ newForeignPtr_ inclusionsPtr
  let inclusionPtrs = VS.unsafeFromForeignPtr0 inclusionFPtr (fromIntegral numInclusions)
  result <- for (V.convert inclusionPtrs) $ \inclusionPtr -> do
    ( filePtr, locationPtr ) <- liftIO $ C.withPtrs_ $ \( filePtr, locationPtr ) ->
      [C.block| void {
          auto inclusion = $(Inclusion* inclusionPtr);
          *$(WrappedFile** filePtr) = inclusion->first;
          *$(WrappedSourceLocation** locationPtr) = inclusion->second;
          delete inclusion;
        } |]
    (,) <$> consume filePtr <*> consumeMaybe locationPtr
  liftIO [C.block| void { delete $(InclusionVector* inclusionsVec); } |]
  return result