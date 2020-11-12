import Clash.Prelude
import Clash.Annotations.TH

import Clash.DSP.Complex
import Clash.Stream.Resize
import Clash.Stream.Packet
import Clash.Misc

import Headers
import DSP

toSample :: Vec 4 (BitVector 4) -> Complex (Signed 8)
toSample = bitCoerce . swapNibbles

fromSample :: Complex (BitVector 8) -> Vec 4 (BitVector 4)
fromSample = swapNibbles . bitCoerce

dspStream' 
    :: HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (BitVector 4)
    -> Signal dom Bool
    -> (
            Signal dom Bool,
            Signal dom Bool,
            Signal dom (BitVector 4)
       )
dspStream' vld eof dat ready = unbundle headeredStream
    where
    --Drop the Ethernet, IP and UDP headers
    noHeaderStream        = dropStream (SNat @ 84) $ bundle (vld, eof, dat)
    --Widen the stream from a single 4 bit value to 16 bits (enough to store a complex number with 8 bit parts)
    (wideStream, _)       = widenStream noHeaderStream (pure True)
    --Pull out the vald and data signals
    (wideVld, _, wideDat) = unbundle wideStream

    --Do the signal processing
    (sampleValid, filterDat) = theFilter wideVld (toSample <$> wideDat)

    --Buffer up enough samples to fill up a packet
    bufferedStream  = packetize @15 (pure 511) (bundle (sampleValid, fromSample <$> filterDat)) narrowedReady
    --Split the samples into 4 bit values for sending over ethernet
    (narrowedStream, narrowedReady) = narrowStream bufferedStream headeredReady
    --Append the Ethernet, IP and UDP headers
    (headeredStream, headeredReady) = prependHeader (pure networkHeaders) narrowedStream ready

dspStream
    :: "clk"    ::: Clock System
    -> "iVld"   ::: Signal System Bool
    -> "iEof"   ::: Signal System Bool
    -> "iDat"   ::: Signal System (BitVector 4)
    -> "iReady" ::: Signal System Bool
    -> (
            "oVld" ::: Signal System Bool,
            "oEof" ::: Signal System Bool,
            "oDat" ::: Signal System (BitVector 4)
       )
dspStream clk = exposeClockResetEnable dspStream' clk resetGen enableGen

makeTopEntity 'dspStream

