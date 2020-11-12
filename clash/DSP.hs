module DSP (
        theFilter
    ) where

import Clash.Prelude
import qualified Prelude

import Clash.DSP.Complex
import Clash.DSP.FIRFilter
import Clash.DSP.CORDIC

coeffsHalfBand :: Vec 8 (Signed 18)
coeffsHalfBand = $(listToVecTH (Prelude.map ((round :: Double -> Int) . (* 2**17) . (* 1.3)) [
        4.103190651075981e-4,
        -2.230264832858829e-3,
        7.100791272333269e-3,
        -1.7916864243313808e-2,
        4.010704565915762e-2,
        -9.010608634732692e-2,
        0.3126304219791297,
        0.5 :: Double
    ]))

decimate
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (Signed 24))
    -> Signal dom (Complex (Signed 24))
decimate en dat 
    = fmap (fmap (unpack . (slice d40 d17 :: Signed 48 -> BitVector 24)))
    $ firSystolicHalfBand macPreAddRealComplexPipelined coeffsHalfBand en dat

consts' :: Vec 16 (SFixed 2 24)
consts' = $(listToVecTH (Prelude.take 16 arctans))

cordic 
    :: HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom (Complex (Signed 24))
    -> Signal dom (CordicState (Signed 24) (SFixed 2 24))
cordic en cplxPart
    = fmap (step 14 $ c7)
    $ regEn undefined en
    $ fmap (step 12 $ c6)
    $ regEn undefined en
    $ fmap (step 10 $ c5)
    $ regEn undefined en
    $ fmap (step 8  $ c4)
    $ regEn undefined en
    $ fmap (step 6  $ c3)
    $ regEn undefined en
    $ fmap (step 4  $ c2)
    $ regEn undefined en
    $ fmap (step 2  $ c1)
    $ regEn undefined en
    $ fmap (step 0  $ c0)
    $ CordicState <$> cplxPart <*> pure (0 :: SFixed 2 24)
    where 

    step :: Index 16 -> Vec 2 (SFixed 2 24) -> CordicState (Signed 24) (SFixed 2 24) -> CordicState (Signed 24) (SFixed 2 24)
    step = cordicSteps (\(CordicState (_ :+ y) _) -> y < 0)

    consts :: Vec 8 (Vec 2 (SFixed 2 24))
    consts = unconcatI consts'

    (c0 :> c1 :> c2 :> c3 :> c4 :> c5 :> c6 :> c7 :> Nil) = consts

padRight :: Signed 8 -> Signed 24
padRight x = unpack $ pack x ++# (0 :: BitVector 16)

sliceHigh :: Signed 24 -> BitVector 8
sliceHigh = slice d23 d16

theFilter
    :: forall dom
    .  HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (Signed 8))
    -> (
            Signal dom Bool, 
            Signal dom (Complex (BitVector 8))
        )
theFilter en x = (finalValid, dat)
    where

    finalValid = en .&&. en1 .&&. en2 .&&. en3

    dat
        = fmap (\x -> (slice d25 d18 . unSF . arg) x :+ 0)
        $ cordic finalValid
        $ decimate (en .&&. en1 .&&. en2)
        $ decimate (en .&&. en1)
        $ decimate en 
        $ fmap (fmap padRight) x

    en1, en2, en3 :: Signal dom Bool
    (en3 :> en2 :> en1 :> Nil) = sequenceA $ unpack <$> cntr
        where
        cntr :: Signal dom (BitVector 3)
        cntr =  regEn 0 en (cntr + 1)

