{-# LANGUAGE DuplicateRecordFields #-}
module Headers (
        networkHeaders
    ) where

import Clash.Prelude

import Clash.Network.Types

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

