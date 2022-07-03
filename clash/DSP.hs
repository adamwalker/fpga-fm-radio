module DSP (
        fmRadio
    ) where

import Clash.Prelude
import qualified Prelude
import Data.Function
import Data.Coerce

import Clash.DSP.Complex
import Clash.DSP.MAC
import Clash.DSP.FIR.Filter
import Clash.DSP.CORDIC

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
    = fmap (fmap (renorm . (resizeF :: SFixed 3 40 -> SFixed 2 22)))
    $ firSystolicHalfBand macPreAddRealComplexPipelined' coeffsHalfBand en dat

renorm :: forall n m. (KnownNat m, KnownNat n) => SFixed (1 + n) m -> SFixed 1 (n + m)
renorm = sf (SNat @(n + m)) . unSF 

decimateReal
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (SFixed 1 23)
    -> Signal dom (SFixed 1 23)
decimateReal en dat 
    = fmap (renorm . (resizeF :: SFixed 3 40 -> SFixed 2 22))
    $ firSystolicHalfBand macPreAddRealReal' coeffsHalfBand en dat

consts' :: Vec 16 (SFixed 3 24)
consts' = $(listToVecTH (Prelude.take 16 arctans))

cordic 
    :: HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom (Complex (SFixed 1 23))
    -> Signal dom (CordicState (SFixed 1 23) (SFixed 3 24))
cordic en cplxPart 
    = cordicPipeline dirMagPhase (0 :: Index 16) consts en initialState
    where 

    initialState 
        =   CordicState 
        <$> cplxPart 
        <*> pure (0 :: SFixed 3 24)

    consts :: Vec 8 (Vec 2 (SFixed 3 24))
    consts = unconcatI consts'

phaseDiff
    :: HiddenClockResetEnable dom
    => Num a
    => NFDataX a
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex a)
phaseDiff en x = x * fmap conjugate x'
    where
    x' = regEn undefined en x

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

    dat
        = x
        & fmap (fmap (resizeF . sf (SNat @7)))
        & decimateComplex en 
        & decimateComplex en1
        & decimateComplex en2
        & regEn undefined en3 
        & phaseDiff en3 
        & regEn undefined en3 
        & cordic en3
        & fmap (resizeF . renorm . arg)
        & decimateReal en3
        & decimateReal en4
        & fmap (pack . (resizeF :: SFixed 1 23 -> SFixed 1 7))

    en1, en2, en3, en4, en5 :: Signal dom Bool
    (en5 :> en4 :> en3 :> en2 :> en1 :> Nil) = postscanr (.&&.) en $ sequenceA $ unpack <$> cntr
        where
        cntr :: Signal dom (BitVector 5)
        cntr =  regEn 0 en (cntr + 1)

