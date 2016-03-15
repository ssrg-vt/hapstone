module Internal.Sparc.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Sparc

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary SparcCc where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary SparcHint where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary SparcOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary SparcOpMemStruct where
    arbitrary = SparcOpMemStruct <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsSparcOp where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        , Mem <$> arbitrary
        , pure Undefined
        ]

instance Arbitrary CsSparc where
    arbitrary = CsSparc <$> arbitrary <*> arbitrary <*> (take 4 <$> arbitrary)
instance Arbitrary SparcReg where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary SparcInsn where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary SparcInsnGroup where
    arbitrary = elements [minBound..maxBound]
