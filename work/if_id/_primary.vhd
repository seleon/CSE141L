library verilog;
use verilog.vl_types.all;
library work;
entity if_id is
    generic(
        addr_width_p    : integer := 6
    );
    port(
        clk             : in     vl_logic;
        stall           : in     vl_logic;
        if_id_i         : in     work.\IF_ID_sv_unit\.if_id_register;
        if_id_o         : in     work.\IF_ID_sv_unit\.if_id_register
    );
    attribute mti_svvh_generic_type : integer;
    attribute mti_svvh_generic_type of addr_width_p : constant is 1;
end if_id;
