module DSP (
        fmRadio
    ) where

import Clash.Prelude
import qualified Prelude
import Data.Function

import Clash.DSP.Complex
import Clash.DSP.FIR.FIRFilter
import Clash.DSP.CORDIC

coeffsHalfBand :: Vec 8 (SFixed 1 17)
coeffsHalfBand = $(listToVecTH [
        4.103190651075981e-4,
        -2.230264832858829e-3,
        7.100791272333269e-3,
        -1.7916864243313808e-2,
        4.010704565915762e-2,
        -9.010608634732692e-2,
        0.3126304219791297,
        0.5 :: Double
    ])

-- | Real * Real multiply and accumulate with pre-add
macPreAddRealReal 
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                     -- ^ Enable
    -> Signal dom (SFixed 1 a)             -- ^ Real coefficient
    -> Signal dom (SFixed 1 b)             -- ^ Real input
    -> Signal dom (SFixed 1 b)             -- ^ Real input 2
    -> Signal dom (SFixed (c + 3) (a + b)) -- ^ Real accumulator in
    -> Signal dom (SFixed (c + 3) (a + b)) -- ^ Real accumulator out
macPreAddRealReal en c i1 i2 a 
    = liftA2 (+) a
    $ fmap resizeF 
    $ regEn 0 en
    $ liftA2 mul c
    $ regEn 0 en 
    $ liftA2 add i1 i2 

-- | Real * Complex multiply and accumulate with pre add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealComplexPipelined'
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                               -- ^ Enable
    -> Signal dom (SFixed 1 a)                       -- ^ Real coefficient
    -> Signal dom (Complex (SFixed 1 b))             -- ^ Complex input
    -> Signal dom (Complex (SFixed 1 b))             -- ^ Complex input 2
    -> Signal dom (Complex (SFixed (c + 3) (a + b))) -- ^ Complex accumulator in
    -> Signal dom (Complex (SFixed (c + 3) (a + b))) -- ^ Complex accumulator out
macPreAddRealComplexPipelined' en c i1 i2 accum 
    = sequenceA 
    $ liftA3 (macPreAddRealReal en c) (sequenceA i1) (sequenceA i2) (sequenceA accum)

decimateComplex
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (SFixed 1 23))
    -> Signal dom (Complex (SFixed 1 23))
decimateComplex en dat 
    = fmap (fmap (renorm . (resizeF :: SFixed 3 40 -> SFixed 2 22)))
    $ firSystolicHalfBand macPreAddRealComplexPipelined' coeffsHalfBand en dat

renorm :: forall n m. (KnownNat m, KnownNat n) => SFixed (1 + n) m -> SFixed 1 (n + m)
renorm = sf (SNat @ (n + m)) . unSF 

decimateReal
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (SFixed 1 23)
    -> Signal dom (SFixed 1 23)
decimateReal en dat 
    = fmap (renorm . (resizeF :: SFixed 3 40 -> SFixed 2 22))
    $ firSystolicHalfBand macPreAddRealReal coeffsHalfBand en dat

consts' :: Vec 16 (SFixed 3 24)
consts' = $(listToVecTH (Prelude.take 16 arctans))

cordic 
    :: HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom (Complex (SFixed 1 23))
    -> Signal dom (CordicState (SFixed 1 23) (SFixed 3 24))
cordic en cplxPart 
    = foldl (flip step) initialState (zip (iterateI (+2) 0) consts)
    where 

    initialState 
        =   CordicState 
        <$> cplxPart 
        <*> pure (0 :: SFixed 3 24)

    step (idx, coeff) = regEn undefined en . fmap (step' idx coeff)
        where
        step' 
            :: Index 16 
            -> Vec 2 (SFixed 3 24) 
            -> CordicState (SFixed 1 23) (SFixed 3 24) 
            -> CordicState (SFixed 1 23) (SFixed 3 24)
        step' = cordicSteps (\(CordicState (_ :+ y) _) -> y < 0)

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
        & fmap (fmap (resizeF . sf (SNat @ 7)))
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

