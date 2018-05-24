module Test.Pos.Core.TxInWitnessProp
       ( hedgeHogTests
       ) where

import           Universum

import           Hedgehog (checkParallel, discover, forAll, property
                          , Property)
import           Test.Pos.Binary.Tripping (embedGoldenTest, goldenTestBi,trippingBiShow)
import           Test.Pos.Core.CoreGenerators ( genPkWit, genRedWit, genScriptWit, genUnkWit
                                              , pkWitness, scrWitness, redWitness, unkWitness)

-- | Golden tests

------------------------------------------------------------------------

prop_golden_TestPkWitness :: Property
prop_golden_TestPkWitness = goldenTestBi pkWitness $(embedGoldenTest "PkWitness")

prop_golden_TestScrWitness :: Property
prop_golden_TestScrWitness = goldenTestBi scrWitness $(embedGoldenTest "ScriptWitness")

prop_golden_TestRedWitness :: Property
prop_golden_TestRedWitness = goldenTestBi redWitness $(embedGoldenTest "RedeemWitness")

prop_golden_TestUnkWitness :: Property
prop_golden_TestUnkWitness = goldenTestBi unkWitness $(embedGoldenTest "UnknownWitness")


-- | Round trip tests

------------------------------------------------------------------------

prop_bitrip_pkwitness :: Property
prop_bitrip_pkwitness = property $ forAll genPkWit >>= trippingBiShow

prop_bitrip_scriptwitness :: Property
prop_bitrip_scriptwitness = property $ forAll genScriptWit >>= trippingBiShow

prop_bitrip_redeemwitness :: Property
prop_bitrip_redeemwitness = property $ forAll genRedWit >>= trippingBiShow

prop_bitrip_unknownwitness :: Property
prop_bitrip_unknownwitness = property $ forAll genUnkWit >>= trippingBiShow

------------------------------------------------------------------------


hedgeHogTests :: IO Bool
hedgeHogTests =
    checkParallel $$discover
