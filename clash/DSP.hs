module DSP (
        theFilter
    ) where

import Clash.Prelude
import qualified Prelude

import Clash.DSP.Complex
import Clash.DSP.FIRFilter

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
theFilter en x = (en .&&. en1, dat)
    where
    dat
        = fmap (fmap sliceHigh) 
        $ decimate (en .&&. en1)
        $ decimate en 
        $ fmap (fmap padRight) x

    en1, en2, en3 :: Signal dom Bool
    (en3 :> en2 :> en1 :> Nil) = sequenceA $ unpack <$> cntr
        where
        cntr :: Signal dom (BitVector 3)
        cntr =  regEn 0 en (cntr + 1)

