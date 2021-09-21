# FPGA FM Radio

An FPGA based FM radio. 

Implemented in [Clash](https://clash-lang.org/).

Runs on the [Arty board](https://store.digilentinc.com/arty-a7-artix-7-fpga-development-board-for-makers-and-hobbyists/).

## Build

Make sure Vivado is in your path, then

```
$ ./build.sh
```

## Setup

Program the image onto the FPGA using the Vivado hardware manager, then

```
$ sudo ./setup_net.sh <network interface connected to the fpga>
```

