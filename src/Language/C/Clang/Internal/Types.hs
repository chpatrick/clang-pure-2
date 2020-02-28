{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Language.C.Clang.Internal.Types where

import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Monad
import Foreign
import Foreign.C

-- Note: This is basically Codensity IO with the extra restricton that async exceptions
-- are _always_ masked.
newtype FFI a = FFI (forall r. (a -> IO r) -> IO r)

instance Functor FFI where
  fmap = liftM

instance Applicative FFI where
  pure = return
  (<*>) = ap

instance Monad FFI where
  return x = FFI $ \cont -> cont x
  FFI x >>= f = FFI $ \cont -> x $ \xRes -> case f xRes of
    FFI y -> y cont

instance MonadIO FFI where
  liftIO m = FFI (\cont -> m >>= cont)

newtype Index = Index (ForeignPtr SharedIndex)

data CreateIndexOptions = CreateIndexOptions
  { cioExcludeDeclarationsFromPCH :: Bool
  , cioDisplayDiagnostics :: Bool
  } deriving (Eq, Ord, Show)

data ParseTranslationUnitOptions = ParseTranslationUnitOptions
  { ptuDetailedPreprocessingRecord :: Bool
  , ptuIncomplete :: Bool
  , ptuPrecompiledPreamble :: Bool
  , ptuCacheCompletionResults :: Bool
  , ptuForSerialization :: Bool
  , ptuCXXChainedPCH :: Bool
  , ptuSkipFunctionBodies :: Bool
  , ptuIncludeBriefCommentsInCodeCompletion :: Bool
  , ptuCreatePreambleOnFirstParse :: Bool
  , ptuKeepGoing :: Bool
  , ptuSingleFileParse :: Bool
  } deriving (Eq, Ord, Show)

newtype TranslationUnit = TranslationUnit (ForeignPtr SharedTranslationUnit)

newtype Cursor = Cursor (ForeignPtr WrappedCursor)

newtype Type = Type (ForeignPtr WrappedType)

newtype SourceRange = SourceRange (ForeignPtr WrappedSourceRange)

newtype SourceLocation = SourceLocation (ForeignPtr WrappedSourceLocation)

newtype Token = Token (ForeignPtr WrappedToken)

data Location = Location
  { locFile :: File
  , locLine :: Int
  , locColumn :: Int
  , locOffset :: Int
  }

newtype File = File (ForeignPtr WrappedFile)

data CursorKind
  = CKUnexposedDecl
  | CKStructDecl
  | CKUnionDecl
  | CKClassDecl
  | CKEnumDecl
  | CKFieldDecl
  | CKEnumConstantDecl
  | CKFunctionDecl
  | CKVarDecl
  | CKParmDecl
  | CKObjCInterfaceDecl
  | CKObjCCategoryDecl
  | CKObjCProtocolDecl
  | CKObjCPropertyDecl
  | CKObjCIvarDecl
  | CKObjCInstanceMethodDecl
  | CKObjCClassMethodDecl
  | CKObjCImplementationDecl
  | CKObjCCategoryImplDecl
  | CKTypedefDecl
  | CKCXXMethod
  | CKNamespace
  | CKLinkageSpec
  | CKConstructor
  | CKDestructor
  | CKConversionFunction
  | CKTemplateTypeParameter
  | CKNonTypeTemplateParameter
  | CKTemplateTemplateParameter
  | CKFunctionTemplate
  | CKClassTemplate
  | CKClassTemplatePartialSpecialization
  | CKNamespaceAlias
  | CKUsingDirective
  | CKUsingDeclaration
  | CKTypeAliasDecl
  | CKObjCSynthesizeDecl
  | CKObjCDynamicDecl
  | CKCXXAccessSpecifier
  | CKObjCSuperClassRef
  | CKObjCProtocolRef
  | CKObjCClassRef
  | CKTypeRef
  | CKCXXBaseSpecifier
  | CKTemplateRef
  | CKNamespaceRef
  | CKMemberRef
  | CKLabelRef
  | CKOverloadedDeclRef
  | CKVariableRef
  | CKInvalidFile
  | CKNoDeclFound
  | CKNotImplemented
  | CKInvalidCode
  | CKUnexposedExpr
  | CKDeclRefExpr
  | CKMemberRefExpr
  | CKCallExpr
  | CKObjCMessageExpr
  | CKBlockExpr
  | CKIntegerLiteral
  | CKFloatingLiteral
  | CKImaginaryLiteral
  | CKStringLiteral
  | CKCharacterLiteral
  | CKParenExpr
  | CKUnaryOperator
  | CKArraySubscriptExpr
  | CKBinaryOperator
  | CKCompoundAssignOperator
  | CKConditionalOperator
  | CKCStyleCastExpr
  | CKCompoundLiteralExpr
  | CKInitListExpr
  | CKAddrLabelExpr
  | CKStmtExpr
  | CKGenericSelectionExpr
  | CKGNUNullExpr
  | CKCXXStaticCastExpr
  | CKCXXDynamicCastExpr
  | CKCXXReinterpretCastExpr
  | CKCXXConstCastExpr
  | CKCXXFunctionalCastExpr
  | CKCXXTypeidExpr
  | CKCXXBoolLiteralExpr
  | CKCXXNullPtrLiteralExpr
  | CKCXXThisExpr
  | CKCXXThrowExpr
  | CKCXXNewExpr
  | CKCXXDeleteExpr
  | CKUnaryExpr
  | CKObjCStringLiteral
  | CKObjCEncodeExpr
  | CKObjCSelectorExpr
  | CKObjCProtocolExpr
  | CKObjCBridgedCastExpr
  | CKPackExpansionExpr
  | CKSizeOfPackExpr
  | CKLambdaExpr
  | CKObjCBoolLiteralExpr
  | CKObjCSelfExpr
  | CKOMPArraySectionExpr
  | CKObjCAvailabilityCheckExpr
  | CKUnexposedStmt
  | CKLabelStmt
  | CKCompoundStmt
  | CKCaseStmt
  | CKDefaultStmt
  | CKIfStmt
  | CKSwitchStmt
  | CKWhileStmt
  | CKDoStmt
  | CKForStmt
  | CKGotoStmt
  | CKIndirectGotoStmt
  | CKContinueStmt
  | CKBreakStmt
  | CKReturnStmt
  | CKGCCAsmStmt
  | CKObjCAtTryStmt
  | CKObjCAtCatchStmt
  | CKObjCAtFinallyStmt
  | CKObjCAtThrowStmt
  | CKObjCAtSynchronizedStmt
  | CKObjCAutoreleasePoolStmt
  | CKObjCForCollectionStmt
  | CKCXXCatchStmt
  | CKCXXTryStmt
  | CKCXXForRangeStmt
  | CKSEHTryStmt
  | CKSEHExceptStmt
  | CKSEHFinallyStmt
  | CKMSAsmStmt
  | CKNullStmt
  | CKDeclStmt
  | CKOMPParallelDirective
  | CKOMPSimdDirective
  | CKOMPForDirective
  | CKOMPSectionsDirective
  | CKOMPSectionDirective
  | CKOMPSingleDirective
  | CKOMPParallelForDirective
  | CKOMPParallelSectionsDirective
  | CKOMPTaskDirective
  | CKOMPMasterDirective
  | CKOMPCriticalDirective
  | CKOMPTaskyieldDirective
  | CKOMPBarrierDirective
  | CKOMPTaskwaitDirective
  | CKOMPFlushDirective
  | CKSEHLeaveStmt
  | CKOMPOrderedDirective
  | CKOMPAtomicDirective
  | CKOMPForSimdDirective
  | CKOMPParallelForSimdDirective
  | CKOMPTargetDirective
  | CKOMPTeamsDirective
  | CKOMPTaskgroupDirective
  | CKOMPCancellationPointDirective
  | CKOMPCancelDirective
  | CKOMPTargetDataDirective
  | CKOMPTaskLoopDirective
  | CKOMPTaskLoopSimdDirective
  | CKOMPDistributeDirective
  | CKOMPTargetEnterDataDirective
  | CKOMPTargetExitDataDirective
  | CKOMPTargetParallelDirective
  | CKOMPTargetParallelForDirective
  | CKOMPTargetUpdateDirective
  | CKOMPDistributeParallelForDirective
  | CKOMPDistributeParallelForSimdDirective
  | CKOMPDistributeSimdDirective
  | CKOMPTargetParallelForSimdDirective
  | CKOMPTargetSimdDirective
  | CKOMPTeamsDistributeDirective
  | CKOMPTeamsDistributeSimdDirective
  | CKOMPTeamsDistributeParallelForSimdDirective
  | CKOMPTeamsDistributeParallelForDirective
  | CKOMPTargetTeamsDirective
  | CKOMPTargetTeamsDistributeDirective
  | CKOMPTargetTeamsDistributeParallelForDirective
  | CKOMPTargetTeamsDistributeParallelForSimdDirective
  | CKOMPTargetTeamsDistributeSimdDirective
  | CKTranslationUnit
  | CKUnexposedAttr
  | CKIBActionAttr
  | CKIBOutletAttr
  | CKIBOutletCollectionAttr
  | CKCXXFinalAttr
  | CKCXXOverrideAttr
  | CKAnnotateAttr
  | CKAsmLabelAttr
  | CKPackedAttr
  | CKPureAttr
  | CKConstAttr
  | CKNoDuplicateAttr
  | CKCUDAConstantAttr
  | CKCUDADeviceAttr
  | CKCUDAGlobalAttr
  | CKCUDAHostAttr
  | CKCUDASharedAttr
  | CKVisibilityAttr
  | CKDLLExport
  | CKDLLImport
  | CKPreprocessingDirective
  | CKMacroDefinition
  | CKMacroExpansion
  | CKInclusionDirective
  | CKModuleImportDecl
  | CKTypeAliasTemplateDecl
  | CKStaticAssert
  | CKFriendDecl
  | CKOverloadCandidate

  | CKHaskellUnknown
    deriving (Eq, Ord, Show)

data TypeKind
  = TKUnexposed
  | TKVoid
  | TKBool
  | TKChar_U
  | TKUChar
  | TKChar16
  | TKChar32
  | TKUShort
  | TKUInt
  | TKULong
  | TKULongLong
  | TKUInt128
  | TKChar_S
  | TKSChar
  | TKWChar
  | TKShort
  | TKInt
  | TKLong
  | TKLongLong
  | TKInt128
  | TKFloat
  | TKDouble
  | TKLongDouble
  | TKNullPtr
  | TKOverload
  | TKDependent
  | TKObjCId
  | TKObjCClass
  | TKObjCSel
  | TKFloat128
  | TKHalf
  | TKFloat16
  | TKComplex
  | TKPointer
  | TKBlockPointer
  | TKLValueReference
  | TKRValueReference
  | TKRecord
  | TKEnum
  | TKTypedef
  | TKObjCInterface
  | TKObjCObjectPointer
  | TKFunctionNoProto
  | TKFunctionProto
  | TKConstantArray
  | TKVector
  | TKIncompleteArray
  | TKVariableArray
  | TKDependentSizedArray
  | TKMemberPointer
  | TKAuto
  | TKElaborated
  | TKPipe
  | TKOCLImage1dRO
  | TKOCLImage1dArrayRO
  | TKOCLImage1dBufferRO
  | TKOCLImage2dRO
  | TKOCLImage2dArrayRO
  | TKOCLImage2dDepthRO
  | TKOCLImage2dArrayDepthRO
  | TKOCLImage2dMSAARO
  | TKOCLImage2dArrayMSAARO
  | TKOCLImage2dMSAADepthRO
  | TKOCLImage2dArrayMSAADepthRO
  | TKOCLImage3dRO
  | TKOCLImage1dWO
  | TKOCLImage1dArrayWO
  | TKOCLImage1dBufferWO
  | TKOCLImage2dWO
  | TKOCLImage2dArrayWO
  | TKOCLImage2dDepthWO
  | TKOCLImage2dArrayDepthWO
  | TKOCLImage2dMSAAWO
  | TKOCLImage2dArrayMSAAWO
  | TKOCLImage2dMSAADepthWO
  | TKOCLImage2dArrayMSAADepthWO
  | TKOCLImage3dWO
  | TKOCLImage1dRW
  | TKOCLImage1dArrayRW
  | TKOCLImage1dBufferRW
  | TKOCLImage2dRW
  | TKOCLImage2dArrayRW
  | TKOCLImage2dDepthRW
  | TKOCLImage2dArrayDepthRW
  | TKOCLImage2dMSAARW
  | TKOCLImage2dArrayMSAARW
  | TKOCLImage2dMSAADepthRW
  | TKOCLImage2dArrayMSAADepthRW
  | TKOCLImage3dRW
  | TKOCLSampler
  | TKOCLEvent
  | TKOCLQueue
  | TKOCLReserveID

  | TKHaskellUnknown
    deriving (Eq, Ord, Show)

data TypeLayoutError
  = TLEInvalid
  | TLEIncomplete
  | TLEDependent
  | TLENotConstantSize
  | TLEInvalidFieldName

  | TLEHaskellUnknown
    deriving (Eq, Ord, Show)

-- These CursorKinds are synonyms.
pattern CKAsmStmt, CKMacroInstantiation :: CursorKind
pattern CKAsmStmt = CKGCCAsmStmt
pattern CKMacroInstantiation = CKMacroExpansion

data SharedIndex
data SharedTranslationUnit
data WrappedCursor
data WrappedType
data WrappedSourceLocation
data WrappedSourceRange
data WrappedFile
data WrappedToken

newtype CXCursorKind = CXCursorKind Int

data CXSourceLocation
data CXFile
data CXString
data CXToken

data CXUnsavedFile = CXUnsavedFile
  { cufFilename :: CString
  , cufContents :: CString
  , cufLength :: CULong
  } deriving (Eq, Ord, Show)

data ChildrenVector
data Inclusion
data InclusionVector

data UnsavedFile = UnsavedFile
  { ufFilename :: FilePath
  , ufContents :: BS.ByteString
  } deriving (Eq, Ord, Show)

data ClangError
  = CEFailure
  | CECrashed
  | CEInvalidArguments
  | CEASTReadError
    deriving (Eq, Ord, Show)

data CallingConv
  = CCDefault
  | CCC
  | CCX86StdCall
  | CCX86FastCall
  | CCX86ThisCall
  | CCX86Pascal
  | CCAAPCS
  | CCAAPCS_VFP
  | CCX86RegCall
  | CCIntelOclBicc
  | CCWin64
  | CCX86_64SysV
  | CCX86VectorCall
  | CCSwift
  | CCPreserveMost
  | CCPreserveAll
  | CCUnexposed
  | CCHaskellUnknown

pattern CCX86_64Win64 :: CallingConv
pattern CCX86_64Win64 = CCWin64