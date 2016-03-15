module Internal.Mips.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Mips

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary MipsOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary MipsOpMemStruct where
    arbitrary = MipsOpMemStruct <$> arbitrary <*> arbitrary

instance Arbitrary CsMipsOp where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        , Mem <$> arbitrary
        , pure Undefined
        ]

instance Arbitrary CsMips where
    arbitrary = CsMips <$> (take 8 <$> arbitrary)

instance Arbitrary MipsReg where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary MipsInsn where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary MipsInsnGroup where
    arbitrary = elements [minBound..maxBound]
