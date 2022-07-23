module DSP (
        fmRadio
    ) where

import Clash.Prelude
import qualified Prelude
import Data.Function
import Data.Coerce

import Clash.Counter
import Clash.DSP.Complex
import Clash.DSP.MAC
import Clash.DSP.FIR.Filter
import Clash.DSP.CORDIC
import Clash.DSP.Fixed
import Clash.DSP.FIR.HalfBand

{-
 - from scipy import signal
 - signal.firwin(29, 0.5)
 -}
coeffsHalfBand :: Vec 8 (SFixed 1 17)
coeffsHalfBand = $(listToVecTH [
        2.23776078e-03, 
        -5.21098854e-03,
        1.20210608e-02, 
        -2.45173921e-02,
        4.70107885e-02, 
        -9.53075386e-02,
        3.14152600e-01,  
        0.5 :: Double
    ])

{-
 - from scipy import signal
 - signal.firwin(29, 0.5)
 -}
coeffsHalfBand2 :: Vec 32 (SFixed 1 17)
coeffsHalfBand2 = $(listToVecTH $ Prelude.map (*2) [
    -4.07163435e-04,  4.43611207e-04,
    -5.06181165e-04,  5.97149189e-04,
    -7.18882587e-04,  8.73864646e-04,
    -1.06472738e-03,  1.29429470e-03,
    -1.56563902e-03,  1.88215527e-03,
    -2.24765779e-03,  2.66650781e-03,
    -3.14378211e-03,  3.68549810e-03,
    -4.29891790e-03,  4.99296405e-03,
    -5.77879742e-03,  6.67063473e-03,
    -7.68692977e-03,  8.85212126e-03,
    -1.01992919e-02,  1.17743452e-02,
    -1.36428182e-02,  1.59015000e-02,
    -1.86993396e-02,  2.22776490e-02,
    -2.70541552e-02,  3.38191101e-02,
    -4.42675757e-02,  6.28091690e-02,
    -1.05616750e-01,  3.18259237e-01 :: Double
    ])


-- | Real * Real multiply and accumulate with pre-add
macPreAddRealReal' 
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => MACPreAdd dom (SFixed 1 a) (SFixed 1 b) (SFixed (c + 3) (a + b))
macPreAddRealReal' = coerce macPreAddRealRealPipelined

-- | Real * Complex multiply and accumulate with pre add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealComplexPipelined'
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => MACPreAdd dom (SFixed 1 a) (Complex (SFixed 1 b)) (Complex (SFixed (c + 3) (a + b)))
macPreAddRealComplexPipelined' = coerce macPreAddRealComplexPipelined

decimateComplex
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (SFixed 1 23))
    -> Signal dom (Complex (SFixed 1 23))
decimateComplex en dat 
    = fmap (fmap (renorm . (truncateInt :: SFixed 3 22 -> SFixed 2 22) . truncateFrac))
    $ firSystolicHalfBand macPreAddRealComplexPipelined' coeffsHalfBand en dat

decimateReal
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (SFixed 1 23)
    -> Signal dom (SFixed 1 23)
decimateReal en dat 
    = fmap (renorm . (truncateInt :: SFixed 3 22 -> SFixed 2 22) . truncateFrac)
    $ firSystolicHalfBand macPreAddRealReal' coeffsHalfBand en dat

cordic 
    :: HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom (Complex (SFixed 1 23))
    -> Signal dom (SFixed 1 24) 
cordic en cplxPart 
    = snd <$> toPolar consts en cplxPart
    where 

    consts :: Vec 16 (Vec 1 (SFixed 1 24))
    consts = unconcatI $ $(listToVecTH (Prelude.take 16 $ Prelude.map (/ pi) arctans))

phaseDiff
    :: HiddenClockResetEnable dom
    => Num a
    => NFDataX a
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex a)
phaseDiff en x = x * xD'
    where
    xD' = delayEn undefined en $ conjugate <$> x

decimateComplex2
    :: (HiddenClockResetEnable dom, KnownNat coeffsPerStage, KnownNat numStages)
    => 1 <= (coeffsPerStage + (numStages + (numStages * coeffsPerStage)))
    => Vec (numStages + 1) (Vec (coeffsPerStage + 1) (SFixed 1 17))
    -> Signal dom Bool
    -> Signal dom (Complex (SFixed 1 23))
    -> (
        Signal dom Bool, 
        Signal dom (Complex (SFixed 1 23)), 
        Signal dom Bool
        )
decimateComplex2 coeffs valid sampleIn = (
        validOut, 
        fmap (truncateFrac . renorm) <$> sampleOut, 
        ready
    )
    where
  
    (validOut, sampleOut, ready)  
        = halfBandDecimate 
            (coerce macPreAddRealComplexPipelined) 
            (SNat @2) 
            (fmap (resizeF :: SFixed 1 23 -> SFixed 5 40))
            coeffs 
            valid 
            sampleIn
fmRadio
    :: forall dom
    .  HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (Signed 8))
    -> (
            Signal dom Bool, 
            Signal dom (BitVector 8)
        )
fmRadio en x = (en5, dat)
    where

    sample0 = fmap (fmap (extendFrac . sf (SNat @7))) x
    (valid1, sample1,  _ready1) = decimateComplex2 (unconcatI coeffsHalfBand2 :: Vec 4 (Vec 8  (SFixed 1 17))) en sample0
    (valid2, sample2,  _ready2) = decimateComplex2 (unconcatI coeffsHalfBand2 :: Vec 2 (Vec 16 (SFixed 1 17))) valid1 sample1
    (valid3, sample3a, _ready3) = decimateComplex2 (unconcatI coeffsHalfBand2 :: Vec 1 (Vec 32 (SFixed 1 17))) valid2 sample2

    dat
        = sample3a
        & delayEn undefined valid3 
        & phaseDiff valid3 
        & delayEn undefined valid3 
        & cordic valid3
        & fmap (truncateFrac . renorm)
        & decimateReal valid3
        & decimateReal en4
        & fmap (pack . truncateFrac)

    en4, en5 :: Signal dom Bool
    (en5 :> en4 :> Nil) = postscanr (.&&.) valid3 $ sequenceA $ unpack <$> cntr
        where
        cntr :: Signal dom (BitVector 2)
        cntr =  count 0 valid3

