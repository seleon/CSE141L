`include "definitions.sv"

module IF_ID #(parameter addr_width_p = 6)
(
    input clk;
    input if_id_register if_id_i;
    input if_id_register if_id_o;
);

always_ff @ (posedge clk)
  begin
  end

endmodule

