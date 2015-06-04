`include "definitions.sv"

//---- Controller ----//
module cl_decode (input instruction_s instruction_i
                 ,output ctrl_sig ctrl_o
                 // ,output logic is_load_op_o
                 // ,output logic op_writes_rf_o
                 // ,output logic is_store_op_o
                 // ,output logic is_mem_op_o
                 // ,output logic is_byte_op_o
                 );

// mem_to_reg signal, to determine whether instr 
// xfers data from dmem to rf 
always_comb
  unique casez (instruction_i)
    `kLW,`kLBU:
      ctrl_o.is_load_op_c = 1'b1;
        
    default:
      ctrl_o.is_load_op_c = 1'b0;
  endcase

// reg_write signal, to determine whether a register file write 
// is needed or not
always_comb
  unique casez (instruction_i)
    `kADDU, `kSUBU, `kSLLV, `kSRAV, `kSRLV,
    `kAND,  `kOR,   `kNOR,  `kSLT,  `kSLTU, `kMYXOR, `kRBR, 
    `kMOV,  `kJALR, `kLW,   `kLBU:
      ctrl_o.op_writes_rf_c = 1'b1; 
    
    default:
      ctrl_o.op_writes_rf_c = 1'b0;
  endcase
  
// is_mem_op_o signal, which indicates if the instruction is a memory operation 
always_comb       
  unique casez (instruction_i)
    `kLW, `kLBU, `kSW, `kSB:
      ctrl_o.is_mem_op_c = 1'b1;
    
    default:
      ctrl_o.is_mem_op_c = 1'b0;
  endcase

// is_store_op_o signal, which indicates if the instruction is a store
always_comb       
  unique casez (instruction_i)
    `kSW, `kSB:
      ctrl_o.is_store_op_c = 1'b1;
    
    default:
      ctrl_o.is_store_op_c = 1'b0;
  endcase

// byte_not_word_c, which indicates the data memory related instruction 
// is byte or word oriented
always_comb
  unique casez (instruction_i)
    `kLBU,`kSB:
      ctrl_o.is_byte_op_c = 1'b1;
       
    default: 
      ctrl_o.is_byte_op_c = 1'b0;
  endcase

endmodule

