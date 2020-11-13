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
    = foldl (flip step) initialState (zip (iterateI (+2) 0) consts)
    where 

    initialState 
        =   CordicState 
        <$> cplxPart 
        <*> pure (0 :: SFixed 2 24)

    step (idx, coeff) = regEn undefined en . fmap (step' idx coeff)
        where
        step' 
            :: Index 16 
            -> Vec 2 (SFixed 2 24) 
            -> CordicState (Signed 24) (SFixed 2 24) 
            -> CordicState (Signed 24) (SFixed 2 24)
        step' = cordicSteps (\(CordicState (_ :+ y) _) -> y < 0)

    consts :: Vec 8 (Vec 2 (SFixed 2 24))
    consts = unconcatI consts'

phaseDiff
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Complex (SFixed 0 24))
    -> Signal dom (Complex (SFixed 0 24))
phaseDiff en x = x * fmap conjugate x'
    where
    x' = regEn undefined en x

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
        $ regEn undefined finalValid 
        $ fmap (fmap unSF)
        $ phaseDiff finalValid 
        $ fmap (fmap (sf (SNat @ 24)))
        $ regEn undefined finalValid 
        $ decimate (en .&&. en1 .&&. en2)
        $ decimate (en .&&. en1)
        $ decimate en 
        $ fmap (fmap padRight) x

    en1, en2, en3 :: Signal dom Bool
    (en3 :> en2 :> en1 :> Nil) = sequenceA $ unpack <$> cntr
        where
        cntr :: Signal dom (BitVector 3)
        cntr =  regEn 0 en (cntr + 1)

