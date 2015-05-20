// A register file with asynchronous read and synchronous write
module IF_ID #(parameter addr_width_p = 6)
                (
                ,input logic [31:0] rs_val_o
                ,input state_e state_o
                ,input logic [31:0] rd_val_o
                ,input logic is_load_op_o
                ,input logic op_writes_rf_o
                ,input logic is_store_op_o
                ,input logic is_mem_op_o
                ,input logic is_byte_op_o
                ,output  [31:0] rd_i 
                ,output  [31:0] rs_i 
                ,output  instruction_s op_i
                );

always_ff @ (posedge clk)
  begin
  end

endmodule

