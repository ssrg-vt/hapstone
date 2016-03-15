module Internal.Ppc.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Ppc

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary PpcBc where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary PpcBh where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary PpcReg where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary PpcOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary PpcOpMemStruct where
    arbitrary = PpcOpMemStruct <$> arbitrary <*> arbitrary

instance Arbitrary PpcOpCrxStruct where
    arbitrary = PpcOpCrxStruct <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsPpcOp where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        , Mem <$> arbitrary
        , Crx <$> arbitrary
        , pure Undefined
        ]

instance Arbitrary CsPpc where
    arbitrary = CsPpc <$> arbitrary <*> arbitrary <*> arbitrary <*>
        (take 8 <$> arbitrary)

instance Arbitrary PpcInsn where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary PpcInsnGroup where
    arbitrary = elements [minBound..maxBound]
