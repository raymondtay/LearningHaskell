module Props where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog

import IPTypes
import GenIP
import ParseIP
import LookupIP

prop_buildIPs :: Property
prop_buildIPs = property $ do
  ipcs <- forAll genIPComponents
  let ip = buildIP ipcs
  buildIP' ipcs === ip
  buildIP'' ipcs === ip

prop_parseIP :: Property
prop_parseIP = property $ do
  ip <- forAll genIP
  parseIP (show ip) === Just ip

prop_parseIP_show :: Property
prop_parseIP_show = property $ do
  ip <- forAll genIP
  tripping ip show parseIP

prop_parseIPRange_show :: Property
prop_parseIPRange_show = property $ do
  ipr <- forAll genIPRange
  tripping ipr show parseIPRange

prop_parseIPRanges_show :: Property
prop_parseIPRanges_show = property $ do
  iprdb <- forAll genIPRangeDB
  tripping iprdb show parseIPRanges
--    iprdb' <- evalEither (parseIPRanges $ show iprdb)
--    iprdb' === iprdb

prop_no_parseInvalidIPRange :: Property
prop_no_parseInvalidIPRange = property $ do
  inv_ip <- forAll genInvalidIPRange
  parseIPRange (show inv_ip) === Nothing

prop_lookupIP_empty :: Property
prop_lookupIP_empty = property $ do
  ip <- forAll genIP
  assert (not $ lookupIP (IPRangeDB []) ip)

prop_lookupIP_bordersIncluded :: Property
prop_lookupIP_bordersIncluded = property $ do
  iprdb@(IPRangeDB iprdbs) <- forAll genIPRangeDB
  IPRange ip1 ip2 <- forAll $ Gen.element iprdbs
  assert (lookupIP iprdb ip1)
  assert (lookupIP iprdb ip2)

props = [
   testProperty "buildIP agrees with buildIP' and buildIP''" prop_buildIPs
 , testProperty "parseIP works as expected" prop_parseIP
 , testProperty "parseIP agrees with show" prop_parseIP_show
 , testProperty "parseIPRange agrees with show" prop_parseIPRange_show
 , testProperty "parseIPRanges agrees with show" prop_parseIPRanges_show
 , testProperty "no parse of invalid IP ranges" prop_no_parseInvalidIPRange
 , testProperty "no ip in empty list" prop_lookupIP_empty
 , testProperty "lookupIP includes borders" prop_lookupIP_bordersIncluded
 ]
