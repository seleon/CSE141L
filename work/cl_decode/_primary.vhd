library verilog;
use verilog.vl_types.all;
library work;
entity cl_decode is
    port(
        instruction_i   : in     work.cl_decode_sv_unit.instruction_s;
        ctrl_o          : out    work.cl_decode_sv_unit.ctrl_sig
    );
end cl_decode;
