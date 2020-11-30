module DSP (
        theFilter
    ) where

import Clash.Prelude
import qualified Prelude

import Clash.DSP.Complex
import Clash.DSP.FIRFilter
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

-- | Real * Complex multiply and accumulate with pre add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealComplexPipelined'
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                               -- ^ Enable
    -> Signal dom (SFixed 1 a)                       -- ^ Real coefficient
    -> Signal dom (Complex (SFixed 1 b))             -- ^ Complex input
    -> Signal dom (Complex (SFixed 1 b))             -- ^ Complex input 2
    -> Signal dom (Complex (SFixed (c + 2) (a + b))) -- ^ Complex accumulator in
    -> Signal dom (Complex (SFixed (c + 2) (a + b))) -- ^ Complex accumulator out
macPreAddRealComplexPipelined' en c i1 i2 accum 
    = liftA2 
        (:+) 
        (pipeline (realPart <$> i1) (realPart <$> i2) (realPart <$> accum)) 
        (pipeline (imagPart <$> i1) (imagPart <$> i2) (imagPart <$> accum))
    where
    pipeline i1' i2' a
        = liftA2 (+) a
        $ fmap resizeF 
        $ regEn 0 en
        $ liftA2 mul c
        $ regEn 0 en 
        $ liftA2 add i1' i2' 

decimateComplex
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (Signed 24))
    -> Signal dom (Complex (Signed 24))
decimateComplex en dat 
    = fmap (fmap (unSF . (resizeF :: SFixed 3 40 -> SFixed 2 22)))
    $ firSystolicHalfBand macPreAddRealComplexPipelined' coeffsHalfBand en 
    $ fmap (fmap (sf (SNat @ 23))) dat

-- | Real * Real multiply and accumulate with pre-add
macPreAddRealReal 
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a               -- ^ Real coefficient
    -> Signed b               -- ^ Real input
    -> Signed b               -- ^ Real input 2
    -> Signed (a + b + c + 1) -- ^ Real accumulator in
    -> Signed (a + b + c + 1) -- ^ Real accumulator out
macPreAddRealReal c i1 i2 b = extend (c `mul` (i1 `add` i2)) + b

decimateReal
    :: HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Signed 24)
    -> Signal dom (Signed 24)
decimateReal en dat 
    = fmap (unpack . (slice d40 d17 :: Signed 48 -> BitVector 24))
    $ firSystolicHalfBand (const (\x y z w -> macPreAddRealReal <$> x <*> y <*> z <*> w)) (map unSF coeffsHalfBand) en dat

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
    => Num a
    => NFDataX a
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex a)
phaseDiff en x = x * fmap conjugate x'
    where
    x' = regEn undefined en x

padRight :: Signed 8 -> Signed 24
padRight x = unpack $ pack x ++# (0 :: BitVector 16)

theFilter
    :: forall dom
    .  HiddenClockResetEnable dom 
    => Signal dom Bool 
    -> Signal dom (Complex (Signed 8))
    -> (
            Signal dom Bool, 
            Signal dom (BitVector 8)
        )
theFilter en x = (en5, dat)
    where

    dat
        = fmap (slice d23 d16)
        $ decimateReal en4
        $ decimateReal en3
        $ fmap (unpack . slice d25 d2 . unSF . arg)
        $ cordic en3
        $ regEn undefined en3 
        $ fmap (fmap unSF)
        $ phaseDiff en3 
        $ fmap (fmap (sf (SNat @ 24)))
        $ regEn undefined en3 
        $ decimateComplex en2
        $ decimateComplex en1
        $ decimateComplex en 
        $ fmap (fmap padRight) x

    en1, en2, en3, en4, en5 :: Signal dom Bool
    (en5 :> en4 :> en3 :> en2 :> en1 :> Nil) = postscanr (.&&.) en $ sequenceA $ unpack <$> cntr
        where
        cntr :: Signal dom (BitVector 5)
        cntr =  regEn 0 en (cntr + 1)

