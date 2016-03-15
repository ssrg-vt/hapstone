module Internal.XCore.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.XCore

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary XCoreOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary XCoreOpMemStruct where
    arbitrary = XCoreOpMemStruct <$> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary

instance Arbitrary CsXCoreOp where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        , Mem <$> arbitrary
        , pure Undefined
        ]

instance Arbitrary CsXCore where
    arbitrary = CsXCore <$> (take 8 <$> arbitrary)

instance Arbitrary XCoreReg where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary XCoreInsn where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary XCoreInsnGroup where
    arbitrary = elements [minBound..maxBound]
