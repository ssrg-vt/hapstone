module Internal.M68K.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.M68K

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary M68KReg where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary M68KAddressMode where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary M68KOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary M68KOpMemStruct where
    arbitrary = M68KOpMemStruct <$> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsM68KOp where
    arbitrary = CsM68KOp <$> oneof
        [ Imm <$> arbitrary
        , Dimm <$> arbitrary
        , Simm <$> arbitrary
        , Reg <$> arbitrary
        , Mem <$> arbitrary
        , RegBits <$> arbitrary
        , RegPair <$> arbitrary <*> arbitrary
        , pure Undefined
        ] <*> arbitrary

instance Arbitrary M68KCpuSize where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary M68KFpuSize where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary M68KOpSize where
    arbitrary = oneof [Cpu <$> arbitrary, Fpu <$> arbitrary]

instance Arbitrary CsM68K where
    arbitrary = CsM68K <$> (take 4 <$> arbitrary) <*> arbitrary

instance Arbitrary M68KInsn where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary M68KGroupType where
    arbitrary = elements [minBound..maxBound]
