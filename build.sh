#!/bin/sh
set -x
pushd clash
stack exec clash -- --verilog sampleStream.hs
popd
cp clash/verilog/Main/dspStream/dspStream.v arty/src/dspStream.v
pushd arty
vivado -mode batch -nojournal -source compile.tcl
