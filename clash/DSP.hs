module DSP (
        fmRadio
    ) where

import Clash.Prelude
import Clash.Num.Wrapping
import qualified Prelude
import Data.Function
import Data.Coerce

import Clash.DSP.Complex
import Clash.DSP.MAC
import Clash.DSP.FIR.Filter
import Clash.DSP.CORDIC
import Clash.DSP.Fixed
import Clash.DSP.FIR.HalfBand
import Clash.DSP.FIR.SemiParallel

{-
 - from scipy import signal
 - signal.firwin(29, 0.5)
 -}
coeffsHalfBand :: Vec 32 (Wrapping (SFixed 1 17))
coeffsHalfBand = $(listToVecTH $ Prelude.map (*2) [
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

coeffsAudioFilter :: Vec 64 (Wrapping (SFixed 1 17))
coeffsAudioFilter = $(listToVecTH [
    -3.11904936e-18, -3.83788710e-04,  5.62884971e-04, -4.18144062e-04,
     1.23074420e-18,  4.77121960e-04, -7.30457735e-04,  5.62867628e-04,
     1.71043836e-18, -6.77612469e-04,  1.05583205e-03, -8.23697209e-04,
     1.07099587e-17,  1.00360276e-03, -1.56562171e-03,  1.21999092e-03,
    -8.37195269e-18, -1.47575772e-03,  2.29016883e-03, -1.77410317e-03,
     1.34080781e-18,  2.11862266e-03, -3.26606705e-03,  2.51342706e-03,
    -4.16345434e-17, -2.96330165e-03,  4.54050365e-03, -3.47391843e-03,
     3.64163811e-17,  4.05212259e-03, -6.17887364e-03,  4.70632445e-03,
    -2.10535832e-17, -5.44704415e-03,  8.27865903e-03, -6.28768224e-03,
    -9.36512814e-18,  7.24563309e-03, -1.09962437e-02,  8.34393245e-03,
    -2.79168321e-17, -9.61376375e-03,  1.46030299e-02, -1.10983953e-02,
     9.10055003e-17,  1.28596017e-02, -1.96163171e-02,  1.49886155e-02,
    -3.37352136e-17, -1.76258347e-02,  2.71543135e-02, -2.09987181e-02,
     3.59656030e-17,  2.55010112e-02, -4.01426544e-02,  3.18775989e-02,
    -3.76229319e-17, -4.17262315e-02,  6.93458210e-02, -5.92033760e-02,
     3.86435099e-17,  9.95534292e-02, -2.11771303e-01,  2.99988387e-01 :: Double
    ])

cordic 
    :: HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom (Complex (Wrapping (SFixed 1 23)))
    -> Signal dom (Wrapping (SFixed 1 23))
cordic en cplxPart 
    = snd <$> toPolar consts en cplxPart
    where 

    consts :: Vec 16 (Vec 1 (Wrapping (SFixed 1 23)))
    consts = unconcatI $ $(listToVecTH (Prelude.take 16 $ Prelude.map (/ pi) arctans))

phaseDiff
    :: HiddenClockResetEnable dom
    => Num a
    => NFDataX a
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom a
phaseDiff en x = x - xD
    where
    xD = delayEn undefined en x

decimateReal
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Wrapping (SFixed 1 23))
    -> (
        Signal dom Bool, 
        Signal dom (Wrapping (SFixed 1 23)), 
        Signal dom Bool
        )
decimateReal valid sampleIn = (
        validOut, 
        toWrapping . truncateFrac . renorm . fromWrapping <$> sampleOut, 
        ready
    )
    where
  
    (validOut, sampleOut, ready)  
        = halfBandDecimate 
            (coerce macPreAddRealRealPipelined) 
            (SNat @2) 
            (toWrapping . (extendIntFrac :: SFixed 1 23 -> SFixed 3 40) . fromWrapping)
            (singleton coeffsHalfBand)
            valid 
            sampleIn

decimateComplex
    :: (HiddenClockResetEnable dom, KnownNat coeffsPerStage, KnownNat numStages)
    => 1 <= (coeffsPerStage + (numStages + (numStages * coeffsPerStage)))
    => Vec (numStages + 1) (Vec (coeffsPerStage + 1) (Wrapping (SFixed 1 17)))
    -> Signal dom Bool
    -> Signal dom (Complex (Wrapping (SFixed 1 23)))
    -> (
        Signal dom Bool, 
        Signal dom (Complex (Wrapping (SFixed 1 23))), 
        Signal dom Bool
        )
decimateComplex coeffs valid sampleIn = (
        validOut, 
        fmap (toWrapping . truncateFrac . renorm . fromWrapping) <$> sampleOut, 
        ready
    )
    where
  
    (validOut, sampleOut, ready)  
        = halfBandDecimate 
            (coerce macPreAddRealComplexPipelined) 
            (SNat @2) 
            (fmap (toWrapping . (extendIntFrac :: SFixed 1 23 -> SFixed 3 40) . fromWrapping))
            coeffs
            valid 
            sampleIn

filterReal
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Wrapping (SFixed 1 23))
    -> (
        Signal dom Bool, 
        Signal dom (Wrapping (SFixed 1 23)), 
        Signal dom Bool
        )
filterReal valid sampleIn = (
        validOut, 
        toWrapping . truncateFrac . renorm . fromWrapping <$> sampleOut, 
        ready
    )
    where
  
    (validOut, sampleOut, ready)  
        = semiParallelFIRSystolicSymmetric 
            (coerce macPreAddRealRealPipelined) 
            (oddSymmAccum (SNat @2) (toWrapping . (extendIntFrac :: SFixed 1 23 -> SFixed 3 40) . fromWrapping))
            (SNat @2)
            (singleton coeffsAudioFilter)
            (pure 0)
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
fmRadio en x = (valid6, dat)
    where

    sample0 = fmap (fmap (toWrapping . extendFrac . sf (SNat @7))) x
    (valid1, sample1,  _ready1) = decimateComplex (unconcatI coeffsHalfBand :: Vec 4 (Vec 8  (Wrapping (SFixed 1 17)))) en     sample0
    (valid2, sample2,  _ready2) = decimateComplex (unconcatI coeffsHalfBand :: Vec 2 (Vec 16 (Wrapping (SFixed 1 17)))) valid1 sample1
    (valid3, sample3a, _ready3) = decimateComplex (unconcatI coeffsHalfBand :: Vec 1 (Vec 32 (Wrapping (SFixed 1 17)))) valid2 sample2

    sample3
        = sample3a
        & delayEn undefined valid3 
        & cordic valid3
        & delayEn undefined valid3 
        & phaseDiff valid3

    (valid4, sample4, _ready4) = decimateReal valid3 sample3
    (valid5, sample5, _ready5) = decimateReal valid4 sample4
    (valid6, sample6, _ready6) = filterReal   valid5 sample5

    dat = sample6
        & fmap (pack . truncateFrac . fromWrapping)

