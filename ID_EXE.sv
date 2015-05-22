`include "definitions.sv"

module IF_ID #(parameter addr_width_p = 6)
(
    input clk;
    input id_exe_register id_exe_i;
    input id_exe_register id_exe_o;
);

always_ff @ (posedge clk)
  begin
  end

endmodule

