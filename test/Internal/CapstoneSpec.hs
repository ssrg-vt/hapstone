module Internal.CapstoneSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Capstone

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Capstone" $ do
    csSkipdataStructStorableSpec
    csSetSkipdataSpec
    csDetailStorableSpec
    peekDetailSpec
    csInsnStorableSpec
    peekWithArchSpec
    csInsnOffsetSpec
    csDisasmSpec
    csDisasmIterSpec

csSkipdataStructStorableSpec :: Spec
csSkipdataStructStorableSpec = describe "Storable CsSkipdataStruct" $ do
    it "has a memory layout we can manage" pending
    it "has matching peek- and poke-implementations" pending
    it "parses correctly" pending

csSetSkipdataSpec :: Spec
csSetSkipdataSpec = describe "csSetSkipdata" $ do
    it "resets correctly" pending
    it "sets correctly" pending
    it "throws errors correctly" pending

csDetailStorableSpec :: Spec
csDetailStorableSpec = describe "Storable CsDetail" $ do
    it "has a memory layout we can manage" pending
    it "has matching peek- and poke-implementations" pending
    it "parses correctly" pending

peekDetailSpec :: Spec
peekDetailSpec = describe "peekDetail" $ do
    it "returns results when a pointer is given" pending

csInsnStorableSpec :: Spec
csInsnStorableSpec = describe "Storable CsInsn" $ do
    it "has a memory layout we can manage" pending
    it "has matching peek- and poke-implementations" pending
    it "parses correctly" pending

peekWithArchSpec :: Spec
peekWithArchSpec = describe "Storable CsInsn" $ do
    it "returns results when a pointer is given" pending

csInsnOffsetSpec :: Spec
csInsnOffsetSpec = describe "csInsnOffset" $ do
    it "behaves correctly (sums match)" pending

csDisasmSpec :: Spec
csDisasmSpec = describe "csDisasm" $ do
    it "disassembles a correct number of instructions" pending
    it "integrates with CsArch" pending

csDisasmIterSpec :: Spec
csDisasmIterSpec = describe "csDisasmIter" $ do
    it "fulfills modification guarantees" pending
    it "integrates with CsArch" pending
