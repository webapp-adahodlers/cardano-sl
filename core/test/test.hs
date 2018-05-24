import           Universum

import           Test.Hspec (hspec)
import           Spec (spec)

import           Test.Pos.Binary.Tripping (runTests)
import qualified Test.Pos.Core.TxInWitnessProp (hedgeHogTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Core.TxInWitnessProp.hedgeHogTests
        ]
