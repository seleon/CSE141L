library verilog;
use verilog.vl_types.all;
library work;
entity id_exe is
    generic(
        addr_width_p    : integer := 6
    );
    port(
        clk             : in     vl_logic;
        stall           : in     vl_logic;
        id_exe_i        : in     work.\ID_EXE_sv_unit\.id_exe_register;
        id_exe_o        : in     work.\ID_EXE_sv_unit\.id_exe_register
    );
    attribute mti_svvh_generic_type : integer;
    attribute mti_svvh_generic_type of addr_width_p : constant is 1;
end id_exe;
