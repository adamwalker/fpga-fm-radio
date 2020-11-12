#Compile TCL script for Arty project template
#Configure the VIO input/output ports in the "Generate the VIO IP" section
#Configure the ILA clock in the "Insert the ILA" section

#Outputs go in outputs directory
set ::output_dir "./outputs"
file mkdir $::output_dir

#Create the project
create_project -part xc7a35ticsg324-1l -in_memory 

#Read the sources
read_verilog -quiet [glob -nocomplain -directory src *.v]
read_vhdl    -quiet [glob -nocomplain -directory src *.vhdl]
read_xdc src/arty.xdc

#Enable xilinx XPM modules
auto_detect_xpm

#Synthesize the design
synth_design -top top -flatten_hierarchy rebuilt
write_checkpoint -force "${::output_dir}/post_synth.dcp"

#Insert the ILA
#See the section "Using XDC Commands to Insert Debug Cores" in UG908
set debug_nets [lsort -dictionary [get_nets -hier -filter {mark_debug}]]
set n_nets [llength $debug_nets]

if { $n_nets > 0 } {
    create_debug_core u_ila_0 ila

    set_property C_DATA_DEPTH          8192  [get_debug_cores u_ila_0]
    set_property C_TRIGIN_EN           false [get_debug_cores u_ila_0]
    set_property C_TRIGOUT_EN          false [get_debug_cores u_ila_0]
    set_property C_ADV_TRIGGER         false [get_debug_cores u_ila_0]
    set_property C_INPUT_PIPE_STAGES   0     [get_debug_cores u_ila_0]
    set_property C_EN_STRG_QUAL        false [get_debug_cores u_ila_0]
    set_property ALL_PROBE_SAME_MU     true  [get_debug_cores u_ila_0]
    set_property ALL_PROBE_SAME_MU_CNT 1     [get_debug_cores u_ila_0]

    set_property port_width 1 [get_debug_ports u_ila_0/clk]
    connect_debug_port u_ila_0/clk [get_nets eth_tx_clk_i]

    set_property port_width $n_nets [get_debug_ports u_ila_0/probe0]
    connect_debug_port u_ila_0/probe0 $debug_nets
}

#Continue with implementation
opt_design
write_checkpoint -force "${::output_dir}/post_opt.dcp"

place_design -directive Explore
write_checkpoint -force "${::output_dir}/post_place.dcp"

phys_opt_design -directive AggressiveExplore
write_checkpoint -force "${::output_dir}/post_phys_opt.dcp"

route_design -directive Explore -tns_cleanup
write_checkpoint -force "${::output_dir}/post_route.dcp"

phys_opt_design -directive Explore
write_checkpoint -force "${::output_dir}/post_route_phys_opt.dcp"

#Reports
report_clocks -file "${::output_dir}/clocks.rpt"
report_timing_summary -file "${::output_dir}/timing.rpt"
report_utilization -file "${::output_dir}/utilization.rpt"

#Outputs
write_bitstream "${::output_dir}/arty.bit" -force
write_debug_probes "${::output_dir}/arty.ltx" -force

