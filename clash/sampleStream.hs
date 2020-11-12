{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
import Clash.Prelude
import qualified Prelude

import Data.Maybe

import Clash.DSP.Complex
import Clash.DSP.FIRFilter
import Clash.Container.FIFO
import Clash.Network.Types
import Clash.Stream.Resize
import Clash.Stream.Packet
import Clash.Misc

networkHeaders :: Vec 84 (BitVector 4)
networkHeaders = concatMap func $ bitCoerce (ethernetHeader, ipHeader, udpHeader)
    where
    func :: BitVector 8 -> Vec 2 (BitVector 4)
    func = reverse . unpack

    ethernetHeader = EthernetHeader {
            destMac   = repeat 0xff,
            sourceMac = 0x12 :> 0x34 :> 0x56 :> 0x78 :> 0x90 :> 0xab :> Nil,
            etherType = 0x0800
        }

    ipHeader = IPv4Header {
            version        = 4,
            ihl            = 5,
            dscp           = 0,
            ecn            = 0,
            ipLen          = 0x041C,
            identification = 0,
            flags          = 0,
            fragmentOffset = 0,
            ttl            = 0x80,
            protocol       = 0x11,
            headerChecksum = 0xab7d,
            sourceIP       = 192 :> 168 :> 5 :> 2 :> Nil,
            destIP         = 192 :> 168 :> 5 :> 1 :> Nil
        }

    udpHeader = UDPHeader {
            sourcePort  = 0x1234,
            destPort    = 0x1234,
            udpLen      = 0x0408,
            udpChecksum = 0x0
        }

coeffsHalfBand :: Vec 8 (Signed 25)
coeffsHalfBand = $(listToVecTH (Prelude.map ((round :: Double -> Int) . (* 2**24)) [
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

toSample :: Vec 4 (BitVector 4) -> Complex (Signed 8)
toSample = bitCoerce . swapNibbles

fromSample :: Complex (BitVector 8) -> Vec 4 (BitVector 4)
fromSample = swapNibbles . bitCoerce

topEntity' 
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
topEntity' vld eof dat ready = unbundle headeredStream
    where
    noHeaderStream        = dropStream (SNat @ 84) $ bundle (vld, eof, dat)
    (wideStream, _)       = widenStream noHeaderStream (pure True)
    (wideVld, _, wideDat) = unbundle wideStream

    filterDat = theFilter wideVld (toSample <$> wideDat)

    bufferedStream  = packetize @15 (pure 511) (bundle (wideVld, fromSample <$> filterDat)) narrowedReady
    (narrowedStream, narrowedReady) = narrowStream bufferedStream headeredReady
    (headeredStream, headeredReady) = prependHeader (pure networkHeaders) narrowedStream ready

{-# ANN topEntity
  (Synthesize
    { t_name     = "dspStream"
    , t_inputs   = [
            PortName "clk", 
            PortName "rst", 
            PortName "clkEn",
            PortName "iVld",
            PortName "iEof",
            PortName "iDat",
            PortName "iReady"
        ]
    , t_output   = PortProduct "" [
            PortName "oVld",
            PortName "oEof",
            PortName "oDat"
        ]
    }) #-}
topEntity = exposeClockResetEnable @System topEntity'

