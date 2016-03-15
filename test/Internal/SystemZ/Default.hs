module Internal.SystemZ.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.SystemZ

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary SysZCc where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary SysZOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary SysZOpMemStruct where
    arbitrary = SysZOpMemStruct <$> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary

instance Arbitrary CsSysZOp where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        , Mem <$> arbitrary
        , pure AcReg
        , pure Undefined
        ]

instance Arbitrary CsSysZ where
    arbitrary = CsSysZ <$> arbitrary <*> (take 6 <$> arbitrary)

instance Arbitrary SysZReg where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary SysZInsn where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary SysZInsnGroup where
    arbitrary = elements [minBound..maxBound]
