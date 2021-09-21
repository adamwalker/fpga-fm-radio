#!/bin/sh
set -xe

pushd clash
stack exec clash -- --verilog sampleStream.hs
popd
mkdir -p arty/src/generated
cp clash/verilog/Main.dspStream/* arty/src/generated
pushd arty
vivado -mode batch -nojournal -source compile.tcl
