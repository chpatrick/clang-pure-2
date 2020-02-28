{-# LANGUAGE NamedFieldPuns #-}

module Language.C.Clang.Internal.Hsc where

import Language.C.Clang.Internal.Types
import Foreign.C
import Data.Bits
import Foreign.Storable

#include  "clang-c/Index.h"

parseCursorKind :: CXCursorKind -> CursorKind
parseCursorKind (CXCursorKind enum) = case enum of
  #{const CXCursor_UnexposedDecl} -> CKUnexposedDecl
  #{const CXCursor_StructDecl} -> CKStructDecl
  #{const CXCursor_UnionDecl} -> CKUnionDecl
  #{const CXCursor_ClassDecl} -> CKClassDecl
  #{const CXCursor_EnumDecl} -> CKEnumDecl
  #{const CXCursor_FieldDecl} -> CKFieldDecl
  #{const CXCursor_EnumConstantDecl} -> CKEnumConstantDecl
  #{const CXCursor_FunctionDecl} -> CKFunctionDecl
  #{const CXCursor_VarDecl} -> CKVarDecl
  #{const CXCursor_ParmDecl} -> CKParmDecl
  #{const CXCursor_ObjCInterfaceDecl} -> CKObjCInterfaceDecl
  #{const CXCursor_ObjCCategoryDecl} -> CKObjCCategoryDecl
  #{const CXCursor_ObjCProtocolDecl} -> CKObjCProtocolDecl
  #{const CXCursor_ObjCPropertyDecl} -> CKObjCPropertyDecl
  #{const CXCursor_ObjCIvarDecl} -> CKObjCIvarDecl
  #{const CXCursor_ObjCInstanceMethodDecl} -> CKObjCInstanceMethodDecl
  #{const CXCursor_ObjCClassMethodDecl} -> CKObjCClassMethodDecl
  #{const CXCursor_ObjCImplementationDecl} -> CKObjCImplementationDecl
  #{const CXCursor_ObjCCategoryImplDecl} -> CKObjCCategoryImplDecl
  #{const CXCursor_TypedefDecl} -> CKTypedefDecl
  #{const CXCursor_CXXMethod} -> CKCXXMethod
  #{const CXCursor_Namespace} -> CKNamespace
  #{const CXCursor_LinkageSpec} -> CKLinkageSpec
  #{const CXCursor_Constructor} -> CKConstructor
  #{const CXCursor_Destructor} -> CKDestructor
  #{const CXCursor_ConversionFunction} -> CKConversionFunction
  #{const CXCursor_TemplateTypeParameter} -> CKTemplateTypeParameter
  #{const CXCursor_NonTypeTemplateParameter} -> CKNonTypeTemplateParameter
  #{const CXCursor_TemplateTemplateParameter} -> CKTemplateTemplateParameter
  #{const CXCursor_FunctionTemplate} -> CKFunctionTemplate
  #{const CXCursor_ClassTemplate} -> CKClassTemplate
  #{const CXCursor_ClassTemplatePartialSpecialization} -> CKClassTemplatePartialSpecialization
  #{const CXCursor_NamespaceAlias} -> CKNamespaceAlias
  #{const CXCursor_UsingDirective} -> CKUsingDirective
  #{const CXCursor_UsingDeclaration} -> CKUsingDeclaration
  #{const CXCursor_TypeAliasDecl} -> CKTypeAliasDecl
  #{const CXCursor_ObjCSynthesizeDecl} -> CKObjCSynthesizeDecl
  #{const CXCursor_ObjCDynamicDecl} -> CKObjCDynamicDecl
  #{const CXCursor_CXXAccessSpecifier} -> CKCXXAccessSpecifier
  #{const CXCursor_ObjCSuperClassRef} -> CKObjCSuperClassRef
  #{const CXCursor_ObjCProtocolRef} -> CKObjCProtocolRef
  #{const CXCursor_ObjCClassRef} -> CKObjCClassRef
  #{const CXCursor_TypeRef} -> CKTypeRef
  #{const CXCursor_CXXBaseSpecifier} -> CKCXXBaseSpecifier
  #{const CXCursor_TemplateRef} -> CKTemplateRef
  #{const CXCursor_NamespaceRef} -> CKNamespaceRef
  #{const CXCursor_MemberRef} -> CKMemberRef
  #{const CXCursor_LabelRef} -> CKLabelRef
  #{const CXCursor_OverloadedDeclRef} -> CKOverloadedDeclRef
  #{const CXCursor_VariableRef} -> CKVariableRef
  #{const CXCursor_InvalidFile} -> CKInvalidFile
  #{const CXCursor_NoDeclFound} -> CKNoDeclFound
  #{const CXCursor_NotImplemented} -> CKNotImplemented
  #{const CXCursor_InvalidCode} -> CKInvalidCode
  #{const CXCursor_UnexposedExpr} -> CKUnexposedExpr
  #{const CXCursor_DeclRefExpr} -> CKDeclRefExpr
  #{const CXCursor_MemberRefExpr} -> CKMemberRefExpr
  #{const CXCursor_CallExpr} -> CKCallExpr
  #{const CXCursor_ObjCMessageExpr} -> CKObjCMessageExpr
  #{const CXCursor_BlockExpr} -> CKBlockExpr
  #{const CXCursor_IntegerLiteral} -> CKIntegerLiteral
  #{const CXCursor_FloatingLiteral} -> CKFloatingLiteral
  #{const CXCursor_ImaginaryLiteral} -> CKImaginaryLiteral
  #{const CXCursor_StringLiteral} -> CKStringLiteral
  #{const CXCursor_CharacterLiteral} -> CKCharacterLiteral
  #{const CXCursor_ParenExpr} -> CKParenExpr
  #{const CXCursor_UnaryOperator} -> CKUnaryOperator
  #{const CXCursor_ArraySubscriptExpr} -> CKArraySubscriptExpr
  #{const CXCursor_BinaryOperator} -> CKBinaryOperator
  #{const CXCursor_CompoundAssignOperator} -> CKCompoundAssignOperator
  #{const CXCursor_ConditionalOperator} -> CKConditionalOperator
  #{const CXCursor_CStyleCastExpr} -> CKCStyleCastExpr
  #{const CXCursor_CompoundLiteralExpr} -> CKCompoundLiteralExpr
  #{const CXCursor_InitListExpr} -> CKInitListExpr
  #{const CXCursor_AddrLabelExpr} -> CKAddrLabelExpr
  #{const CXCursor_StmtExpr} -> CKStmtExpr
  #{const CXCursor_GenericSelectionExpr} -> CKGenericSelectionExpr
  #{const CXCursor_GNUNullExpr} -> CKGNUNullExpr
  #{const CXCursor_CXXStaticCastExpr} -> CKCXXStaticCastExpr
  #{const CXCursor_CXXDynamicCastExpr} -> CKCXXDynamicCastExpr
  #{const CXCursor_CXXReinterpretCastExpr} -> CKCXXReinterpretCastExpr
  #{const CXCursor_CXXConstCastExpr} -> CKCXXConstCastExpr
  #{const CXCursor_CXXFunctionalCastExpr} -> CKCXXFunctionalCastExpr
  #{const CXCursor_CXXTypeidExpr} -> CKCXXTypeidExpr
  #{const CXCursor_CXXBoolLiteralExpr} -> CKCXXBoolLiteralExpr
  #{const CXCursor_CXXNullPtrLiteralExpr} -> CKCXXNullPtrLiteralExpr
  #{const CXCursor_CXXThisExpr} -> CKCXXThisExpr
  #{const CXCursor_CXXThrowExpr} -> CKCXXThrowExpr
  #{const CXCursor_CXXNewExpr} -> CKCXXNewExpr
  #{const CXCursor_CXXDeleteExpr} -> CKCXXDeleteExpr
  #{const CXCursor_UnaryExpr} -> CKUnaryExpr
  #{const CXCursor_ObjCStringLiteral} -> CKObjCStringLiteral
  #{const CXCursor_ObjCEncodeExpr} -> CKObjCEncodeExpr
  #{const CXCursor_ObjCSelectorExpr} -> CKObjCSelectorExpr
  #{const CXCursor_ObjCProtocolExpr} -> CKObjCProtocolExpr
  #{const CXCursor_ObjCBridgedCastExpr} -> CKObjCBridgedCastExpr
  #{const CXCursor_PackExpansionExpr} -> CKPackExpansionExpr
  #{const CXCursor_SizeOfPackExpr} -> CKSizeOfPackExpr
  #{const CXCursor_LambdaExpr} -> CKLambdaExpr
  #{const CXCursor_ObjCBoolLiteralExpr} -> CKObjCBoolLiteralExpr
  #{const CXCursor_ObjCSelfExpr} -> CKObjCSelfExpr
  #{const CXCursor_OMPArraySectionExpr} -> CKOMPArraySectionExpr
  #{const CXCursor_ObjCAvailabilityCheckExpr} -> CKObjCAvailabilityCheckExpr
  #{const CXCursor_UnexposedStmt} -> CKUnexposedStmt
  #{const CXCursor_LabelStmt} -> CKLabelStmt
  #{const CXCursor_CompoundStmt} -> CKCompoundStmt
  #{const CXCursor_CaseStmt} -> CKCaseStmt
  #{const CXCursor_DefaultStmt} -> CKDefaultStmt
  #{const CXCursor_IfStmt} -> CKIfStmt
  #{const CXCursor_SwitchStmt} -> CKSwitchStmt
  #{const CXCursor_WhileStmt} -> CKWhileStmt
  #{const CXCursor_DoStmt} -> CKDoStmt
  #{const CXCursor_ForStmt} -> CKForStmt
  #{const CXCursor_GotoStmt} -> CKGotoStmt
  #{const CXCursor_IndirectGotoStmt} -> CKIndirectGotoStmt
  #{const CXCursor_ContinueStmt} -> CKContinueStmt
  #{const CXCursor_BreakStmt} -> CKBreakStmt
  #{const CXCursor_ReturnStmt} -> CKReturnStmt
  #{const CXCursor_GCCAsmStmt} -> CKGCCAsmStmt
  #{const CXCursor_ObjCAtTryStmt} -> CKObjCAtTryStmt
  #{const CXCursor_ObjCAtCatchStmt} -> CKObjCAtCatchStmt
  #{const CXCursor_ObjCAtFinallyStmt} -> CKObjCAtFinallyStmt
  #{const CXCursor_ObjCAtThrowStmt} -> CKObjCAtThrowStmt
  #{const CXCursor_ObjCAtSynchronizedStmt} -> CKObjCAtSynchronizedStmt
  #{const CXCursor_ObjCAutoreleasePoolStmt} -> CKObjCAutoreleasePoolStmt
  #{const CXCursor_ObjCForCollectionStmt} -> CKObjCForCollectionStmt
  #{const CXCursor_CXXCatchStmt} -> CKCXXCatchStmt
  #{const CXCursor_CXXTryStmt} -> CKCXXTryStmt
  #{const CXCursor_CXXForRangeStmt} -> CKCXXForRangeStmt
  #{const CXCursor_SEHTryStmt} -> CKSEHTryStmt
  #{const CXCursor_SEHExceptStmt} -> CKSEHExceptStmt
  #{const CXCursor_SEHFinallyStmt} -> CKSEHFinallyStmt
  #{const CXCursor_MSAsmStmt} -> CKMSAsmStmt
  #{const CXCursor_NullStmt} -> CKNullStmt
  #{const CXCursor_DeclStmt} -> CKDeclStmt
  #{const CXCursor_OMPParallelDirective} -> CKOMPParallelDirective
  #{const CXCursor_OMPSimdDirective} -> CKOMPSimdDirective
  #{const CXCursor_OMPForDirective} -> CKOMPForDirective
  #{const CXCursor_OMPSectionsDirective} -> CKOMPSectionsDirective
  #{const CXCursor_OMPSectionDirective} -> CKOMPSectionDirective
  #{const CXCursor_OMPSingleDirective} -> CKOMPSingleDirective
  #{const CXCursor_OMPParallelForDirective} -> CKOMPParallelForDirective
  #{const CXCursor_OMPParallelSectionsDirective} -> CKOMPParallelSectionsDirective
  #{const CXCursor_OMPTaskDirective} -> CKOMPTaskDirective
  #{const CXCursor_OMPMasterDirective} -> CKOMPMasterDirective
  #{const CXCursor_OMPCriticalDirective} -> CKOMPCriticalDirective
  #{const CXCursor_OMPTaskyieldDirective} -> CKOMPTaskyieldDirective
  #{const CXCursor_OMPBarrierDirective} -> CKOMPBarrierDirective
  #{const CXCursor_OMPTaskwaitDirective} -> CKOMPTaskwaitDirective
  #{const CXCursor_OMPFlushDirective} -> CKOMPFlushDirective
  #{const CXCursor_SEHLeaveStmt} -> CKSEHLeaveStmt
  #{const CXCursor_OMPOrderedDirective} -> CKOMPOrderedDirective
  #{const CXCursor_OMPAtomicDirective} -> CKOMPAtomicDirective
  #{const CXCursor_OMPForSimdDirective} -> CKOMPForSimdDirective
  #{const CXCursor_OMPParallelForSimdDirective} -> CKOMPParallelForSimdDirective
  #{const CXCursor_OMPTargetDirective} -> CKOMPTargetDirective
  #{const CXCursor_OMPTeamsDirective} -> CKOMPTeamsDirective
  #{const CXCursor_OMPTaskgroupDirective} -> CKOMPTaskgroupDirective
  #{const CXCursor_OMPCancellationPointDirective} -> CKOMPCancellationPointDirective
  #{const CXCursor_OMPCancelDirective} -> CKOMPCancelDirective
  #{const CXCursor_OMPTargetDataDirective} -> CKOMPTargetDataDirective
  #{const CXCursor_OMPTaskLoopDirective} -> CKOMPTaskLoopDirective
  #{const CXCursor_OMPTaskLoopSimdDirective} -> CKOMPTaskLoopSimdDirective
  #{const CXCursor_OMPDistributeDirective} -> CKOMPDistributeDirective
  #{const CXCursor_OMPTargetEnterDataDirective} -> CKOMPTargetEnterDataDirective
  #{const CXCursor_OMPTargetExitDataDirective} -> CKOMPTargetExitDataDirective
  #{const CXCursor_OMPTargetParallelDirective} -> CKOMPTargetParallelDirective
  #{const CXCursor_OMPTargetParallelForDirective} -> CKOMPTargetParallelForDirective
  #{const CXCursor_OMPTargetUpdateDirective} -> CKOMPTargetUpdateDirective
  #{const CXCursor_OMPDistributeParallelForDirective} -> CKOMPDistributeParallelForDirective
  #{const CXCursor_OMPDistributeParallelForSimdDirective} -> CKOMPDistributeParallelForSimdDirective
  #{const CXCursor_OMPDistributeSimdDirective} -> CKOMPDistributeSimdDirective
  #{const CXCursor_OMPTargetParallelForSimdDirective} -> CKOMPTargetParallelForSimdDirective
  #{const CXCursor_OMPTargetSimdDirective} -> CKOMPTargetSimdDirective
  #{const CXCursor_OMPTeamsDistributeDirective} -> CKOMPTeamsDistributeDirective
  #{const CXCursor_OMPTeamsDistributeSimdDirective} -> CKOMPTeamsDistributeSimdDirective
  #{const CXCursor_OMPTeamsDistributeParallelForSimdDirective} -> CKOMPTeamsDistributeParallelForSimdDirective
  #{const CXCursor_OMPTeamsDistributeParallelForDirective} -> CKOMPTeamsDistributeParallelForDirective
  #{const CXCursor_OMPTargetTeamsDirective} -> CKOMPTargetTeamsDirective
  #{const CXCursor_OMPTargetTeamsDistributeDirective} -> CKOMPTargetTeamsDistributeDirective
  #{const CXCursor_OMPTargetTeamsDistributeParallelForDirective} -> CKOMPTargetTeamsDistributeParallelForDirective
  #{const CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective} -> CKOMPTargetTeamsDistributeParallelForSimdDirective
  #{const CXCursor_OMPTargetTeamsDistributeSimdDirective} -> CKOMPTargetTeamsDistributeSimdDirective
  #{const CXCursor_TranslationUnit} -> CKTranslationUnit
  #{const CXCursor_UnexposedAttr} -> CKUnexposedAttr
  #{const CXCursor_IBActionAttr} -> CKIBActionAttr
  #{const CXCursor_IBOutletAttr} -> CKIBOutletAttr
  #{const CXCursor_IBOutletCollectionAttr} -> CKIBOutletCollectionAttr
  #{const CXCursor_CXXFinalAttr} -> CKCXXFinalAttr
  #{const CXCursor_CXXOverrideAttr} -> CKCXXOverrideAttr
  #{const CXCursor_AnnotateAttr} -> CKAnnotateAttr
  #{const CXCursor_AsmLabelAttr} -> CKAsmLabelAttr
  #{const CXCursor_PackedAttr} -> CKPackedAttr
  #{const CXCursor_PureAttr} -> CKPureAttr
  #{const CXCursor_ConstAttr} -> CKConstAttr
  #{const CXCursor_NoDuplicateAttr} -> CKNoDuplicateAttr
  #{const CXCursor_CUDAConstantAttr} -> CKCUDAConstantAttr
  #{const CXCursor_CUDADeviceAttr} -> CKCUDADeviceAttr
  #{const CXCursor_CUDAGlobalAttr} -> CKCUDAGlobalAttr
  #{const CXCursor_CUDAHostAttr} -> CKCUDAHostAttr
  #{const CXCursor_CUDASharedAttr} -> CKCUDASharedAttr
  #{const CXCursor_VisibilityAttr} -> CKVisibilityAttr
  #{const CXCursor_DLLExport} -> CKDLLExport
  #{const CXCursor_DLLImport} -> CKDLLImport
  #{const CXCursor_PreprocessingDirective} -> CKPreprocessingDirective
  #{const CXCursor_MacroDefinition} -> CKMacroDefinition
  #{const CXCursor_MacroExpansion} -> CKMacroExpansion
  #{const CXCursor_InclusionDirective} -> CKInclusionDirective
  #{const CXCursor_ModuleImportDecl} -> CKModuleImportDecl
  #{const CXCursor_TypeAliasTemplateDecl} -> CKTypeAliasTemplateDecl
  #{const CXCursor_StaticAssert} -> CKStaticAssert
  #{const CXCursor_FriendDecl} -> CKFriendDecl
  #{const CXCursor_OverloadCandidate} -> CKOverloadCandidate
  _ -> CKHaskellUnknown

parseTypeKind :: CUInt -> Maybe TypeKind
parseTypeKind enum = case enum of
  #{const CXType_Invalid} -> Nothing
  #{const CXType_Unexposed} -> Just TKUnexposed
  #{const CXType_Void} -> Just TKVoid
  #{const CXType_Bool} -> Just TKBool
  #{const CXType_Char_U} -> Just TKChar_U
  #{const CXType_UChar} -> Just TKUChar
  #{const CXType_Char16} -> Just TKChar16
  #{const CXType_Char32} -> Just TKChar32
  #{const CXType_UShort} -> Just TKUShort
  #{const CXType_UInt} -> Just TKUInt
  #{const CXType_ULong} -> Just TKULong
  #{const CXType_ULongLong} -> Just TKULongLong
  #{const CXType_UInt128} -> Just TKUInt128
  #{const CXType_Char_S} -> Just TKChar_S
  #{const CXType_SChar} -> Just TKSChar
  #{const CXType_WChar} -> Just TKWChar
  #{const CXType_Short} -> Just TKShort
  #{const CXType_Int} -> Just TKInt
  #{const CXType_Long} -> Just TKLong
  #{const CXType_LongLong} -> Just TKLongLong
  #{const CXType_Int128} -> Just TKInt128
  #{const CXType_Float} -> Just TKFloat
  #{const CXType_Double} -> Just TKDouble
  #{const CXType_LongDouble} -> Just TKLongDouble
  #{const CXType_NullPtr} -> Just TKNullPtr
  #{const CXType_Overload} -> Just TKOverload
  #{const CXType_Dependent} -> Just TKDependent
  #{const CXType_ObjCId} -> Just TKObjCId
  #{const CXType_ObjCClass} -> Just TKObjCClass
  #{const CXType_ObjCSel} -> Just TKObjCSel
  #{const CXType_Float128} -> Just TKFloat128
  #{const CXType_Half} -> Just TKHalf
  #{const CXType_Float16} -> Just TKFloat16
  #{const CXType_Complex} -> Just TKComplex
  #{const CXType_Pointer} -> Just TKPointer
  #{const CXType_BlockPointer} -> Just TKBlockPointer
  #{const CXType_LValueReference} -> Just TKLValueReference
  #{const CXType_RValueReference} -> Just TKRValueReference
  #{const CXType_Record} -> Just TKRecord
  #{const CXType_Enum} -> Just TKEnum
  #{const CXType_Typedef} -> Just TKTypedef
  #{const CXType_ObjCInterface} -> Just TKObjCInterface
  #{const CXType_ObjCObjectPointer} -> Just TKObjCObjectPointer
  #{const CXType_FunctionNoProto} -> Just TKFunctionNoProto
  #{const CXType_FunctionProto} -> Just TKFunctionProto
  #{const CXType_ConstantArray} -> Just TKConstantArray
  #{const CXType_Vector} -> Just TKVector
  #{const CXType_IncompleteArray} -> Just TKIncompleteArray
  #{const CXType_VariableArray} -> Just TKVariableArray
  #{const CXType_DependentSizedArray} -> Just TKDependentSizedArray
  #{const CXType_MemberPointer} -> Just TKMemberPointer
  #{const CXType_Auto} -> Just TKAuto
  #{const CXType_Elaborated} -> Just TKElaborated
  #{const CXType_Pipe} -> Just TKPipe
  #{const CXType_OCLImage1dRO} -> Just TKOCLImage1dRO
  #{const CXType_OCLImage1dArrayRO} -> Just TKOCLImage1dArrayRO
  #{const CXType_OCLImage1dBufferRO} -> Just TKOCLImage1dBufferRO
  #{const CXType_OCLImage2dRO} -> Just TKOCLImage2dRO
  #{const CXType_OCLImage2dArrayRO} -> Just TKOCLImage2dArrayRO
  #{const CXType_OCLImage2dDepthRO} -> Just TKOCLImage2dDepthRO
  #{const CXType_OCLImage2dArrayDepthRO} -> Just TKOCLImage2dArrayDepthRO
  #{const CXType_OCLImage2dMSAARO} -> Just TKOCLImage2dMSAARO
  #{const CXType_OCLImage2dArrayMSAARO} -> Just TKOCLImage2dArrayMSAARO
  #{const CXType_OCLImage2dMSAADepthRO} -> Just TKOCLImage2dMSAADepthRO
  #{const CXType_OCLImage2dArrayMSAADepthRO} -> Just TKOCLImage2dArrayMSAADepthRO
  #{const CXType_OCLImage3dRO} -> Just TKOCLImage3dRO
  #{const CXType_OCLImage1dWO} -> Just TKOCLImage1dWO
  #{const CXType_OCLImage1dArrayWO} -> Just TKOCLImage1dArrayWO
  #{const CXType_OCLImage1dBufferWO} -> Just TKOCLImage1dBufferWO
  #{const CXType_OCLImage2dWO} -> Just TKOCLImage2dWO
  #{const CXType_OCLImage2dArrayWO} -> Just TKOCLImage2dArrayWO
  #{const CXType_OCLImage2dDepthWO} -> Just TKOCLImage2dDepthWO
  #{const CXType_OCLImage2dArrayDepthWO} -> Just TKOCLImage2dArrayDepthWO
  #{const CXType_OCLImage2dMSAAWO} -> Just TKOCLImage2dMSAAWO
  #{const CXType_OCLImage2dArrayMSAAWO} -> Just TKOCLImage2dArrayMSAAWO
  #{const CXType_OCLImage2dMSAADepthWO} -> Just TKOCLImage2dMSAADepthWO
  #{const CXType_OCLImage2dArrayMSAADepthWO} -> Just TKOCLImage2dArrayMSAADepthWO
  #{const CXType_OCLImage3dWO} -> Just TKOCLImage3dWO
  #{const CXType_OCLImage1dRW} -> Just TKOCLImage1dRW
  #{const CXType_OCLImage1dArrayRW} -> Just TKOCLImage1dArrayRW
  #{const CXType_OCLImage1dBufferRW} -> Just TKOCLImage1dBufferRW
  #{const CXType_OCLImage2dRW} -> Just TKOCLImage2dRW
  #{const CXType_OCLImage2dArrayRW} -> Just TKOCLImage2dArrayRW
  #{const CXType_OCLImage2dDepthRW} -> Just TKOCLImage2dDepthRW
  #{const CXType_OCLImage2dArrayDepthRW} -> Just TKOCLImage2dArrayDepthRW
  #{const CXType_OCLImage2dMSAARW} -> Just TKOCLImage2dMSAARW
  #{const CXType_OCLImage2dArrayMSAARW} -> Just TKOCLImage2dArrayMSAARW
  #{const CXType_OCLImage2dMSAADepthRW} -> Just TKOCLImage2dMSAADepthRW
  #{const CXType_OCLImage2dArrayMSAADepthRW} -> Just TKOCLImage2dArrayMSAADepthRW
  #{const CXType_OCLImage3dRW} -> Just TKOCLImage3dRW
  #{const CXType_OCLSampler} -> Just TKOCLSampler
  #{const CXType_OCLEvent} -> Just TKOCLEvent
  #{const CXType_OCLQueue} -> Just TKOCLQueue
  #{const CXType_OCLReserveID} -> Just TKOCLReserveID
  _ -> Just TKHaskellUnknown -- for forward compatibility

parseTypeLayoutError :: CLLong -> Either TypeLayoutError Int
parseTypeLayoutError enum = case enum of
  #{const CXTypeLayoutError_Invalid} -> Left TLEInvalid
  #{const CXTypeLayoutError_Incomplete} -> Left TLEIncomplete
  #{const CXTypeLayoutError_Dependent} -> Left TLEDependent
  #{const CXTypeLayoutError_NotConstantSize} -> Left TLENotConstantSize
  #{const CXTypeLayoutError_InvalidFieldName} -> Left TLEInvalidFieldName
  other
    | other < 0 -> Left TLEHaskellUnknown
    | otherwise -> Right (fromIntegral other)

parseTranslationUnitOptionsToEnum :: ParseTranslationUnitOptions -> CUInt
parseTranslationUnitOptionsToEnum options =
  foldr (.|.) 0 $
  map (\( field, flag ) -> if field options then flag else 0)
    [ ( ptuDetailedPreprocessingRecord, #{const CXTranslationUnit_DetailedPreprocessingRecord} )
    , ( ptuIncomplete, #{const CXTranslationUnit_Incomplete} )
    , ( ptuPrecompiledPreamble, #{const CXTranslationUnit_PrecompiledPreamble} )
    , ( ptuCacheCompletionResults, #{const CXTranslationUnit_CacheCompletionResults} )
    , ( ptuForSerialization, #{const CXTranslationUnit_ForSerialization} )
    , ( ptuCXXChainedPCH, #{const CXTranslationUnit_CXXChainedPCH} )
    , ( ptuSkipFunctionBodies, #{const CXTranslationUnit_SkipFunctionBodies} )
    , ( ptuIncludeBriefCommentsInCodeCompletion, #{const CXTranslationUnit_IncludeBriefCommentsInCodeCompletion} )
    , ( ptuCreatePreambleOnFirstParse, #{const CXTranslationUnit_CreatePreambleOnFirstParse} )
    , ( ptuKeepGoing, #{const CXTranslationUnit_KeepGoing} )
    , ( ptuSingleFileParse, #{const CXTranslationUnit_SingleFileParse} )
  ]

instance Storable CXUnsavedFile where
  alignment _ = #{alignment struct CXUnsavedFile}
  sizeOf _ = #{size struct CXUnsavedFile}
  poke ptr file = do
    #{poke struct CXUnsavedFile, Filename} ptr (cufFilename file)
    #{poke struct CXUnsavedFile, Contents} ptr (cufContents file)
    #{poke struct CXUnsavedFile, Length} ptr (cufLength file)
  peek ptr = do
    cufFilename <- #{peek struct CXUnsavedFile, Filename} ptr
    cufContents <- #{peek struct CXUnsavedFile, Contents} ptr
    cufLength <- #{peek struct CXUnsavedFile, Length} ptr
    return CXUnsavedFile { cufFilename, cufContents, cufLength }

parseClangError :: CUInt -> Maybe ClangError
parseClangError code = case code of
  #{const CXError_Failure} -> Just CEFailure
  #{const CXError_Crashed} -> Just CECrashed
  #{const CXError_InvalidArguments} -> Just CEInvalidArguments
  #{const CXError_ASTReadError} -> Just CEASTReadError
  #{const CXError_Success} -> Nothing
  _ -> error ("Unexpected CXError: " ++ show code)

parseCallingConv :: CUInt -> Maybe CallingConv
parseCallingConv enum = case enum of
  #{const CXCallingConv_Default} -> Just CCDefault
  #{const CXCallingConv_C} -> Just CCC
  #{const CXCallingConv_X86StdCall} -> Just CCX86StdCall
  #{const CXCallingConv_X86FastCall} -> Just CCX86FastCall
  #{const CXCallingConv_X86ThisCall} -> Just CCX86ThisCall
  #{const CXCallingConv_X86Pascal} -> Just CCX86Pascal
  #{const CXCallingConv_AAPCS} -> Just CCAAPCS
  #{const CXCallingConv_AAPCS_VFP} -> Just CCAAPCS_VFP
  #{const CXCallingConv_X86RegCall} -> Just CCX86RegCall
  #{const CXCallingConv_IntelOclBicc} -> Just CCIntelOclBicc
  #{const CXCallingConv_Win64} -> Just CCWin64
  #{const CXCallingConv_X86_64SysV} -> Just CCX86_64SysV
  #{const CXCallingConv_X86VectorCall} -> Just CCX86VectorCall
  #{const CXCallingConv_Swift} -> Just CCSwift
  #{const CXCallingConv_PreserveMost} -> Just CCPreserveMost
  #{const CXCallingConv_PreserveAll} -> Just CCPreserveAll
  #{const CXCallingConv_Unexposed} -> Just CCUnexposed
  #{const CXCallingConv_Invalid} -> Nothing
  _ -> Just CCHaskellUnknown