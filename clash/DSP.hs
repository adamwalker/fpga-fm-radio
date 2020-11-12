module DSP (
        theFilter
    ) where

import Clash.Prelude
import qualified Prelude

import Clash.DSP.Complex
import Clash.DSP.FIRFilter

coeffsHalfBand :: Vec 8 (Signed 25)
coeffsHalfBand = $(listToVecTH (Prelude.map ((round :: Double -> Int) . (* 2**24) . (* 1.3)) [
        4.103190651075981e-4,
        -2.230264832858829e-3,
        7.100791272333269e-3,
        -1.7916864243313808e-2,
        4.010704565915762e-2,
        -9.010608634732692e-2,
        0.3126304219791297,
        0.5 :: Double
    ]))

theFilter
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (Signed 8))
    -> Signal dom (Complex (BitVector 8))
theFilter en dat 
    = fmap (fmap (slice d31 d24 :: Signed 48 -> BitVector 8)) 
    $ firSystolicHalfBand macPreAddRealComplexPipelined coeffsHalfBand en dat

