`include "definitions.sv"

module core #(parameter imem_addr_width_p=10
                       ,net_ID_p = 10'b0000000001)
             (input  clk
             ,input  reset

             ,input  net_packet_s net_packet_i
             ,output net_packet_s net_packet_o

             ,input  mem_out_s from_mem_i
             ,output mem_in_s  to_mem_o

             ,output logic [mask_length_gp-1:0] barrier_o
             ,output logic                      exception_o
             ,output debug_s                    debug_o
             ,output logic [31:0]               data_mem_addr
             );

if_id_register if_id_i, if_id_o;
id_exe_register id_exe_i, id_exe_o;
exe_mem_register exe_mem_i, exe_mem_o;
mem_wb_register mem_wb_i, mem_wb_o;

//---- Adresses and Data ----//
// Ins. memory address signals
logic [imem_addr_width_p-1:0] PC_r, PC_n,
                              pc_plus1, imem_addr,
                              imm_jump_add;
// Ins. memory output
instruction_s instruction, imem_out, instruction_r;

// Result of ALU, Register file outputs, Data memory output data
logic [31:0] alu_result, rs_val_or_zero, rd_val_or_zero, rs_val, rd_val;

// Data for Reg. File signals
logic [31:0] rf_wd;

//---- Control signals ----//
// ALU output to determin whether to jump or not
logic jump_now;

// controller output signals
logic is_load_op_c,  op_writes_rf_c, valid_to_mem_c,
      is_store_op_c, is_mem_op_c,    PC_wen,
      is_byte_op_c,  PC_wen_r;

// Handshak protocol signals for memory
logic yumi_to_mem_c;

// Final signals after network interfere
logic imem_wen, rf_wen;

// Network operation signals
logic net_ID_match,      net_PC_write_cmd,  net_imem_write_cmd,
      net_reg_write_cmd, net_bar_write_cmd, net_PC_write_cmd_IDLE;

// Memory stages and stall signals
logic [1:0] mem_stage_r, mem_stage_n, mem_hazard;
logic stall, stall_non_mem;

// Exception signal
logic exception_n;

// State machine signals
state_e state_r,state_n;

//---- network and barrier signals ----//
instruction_s net_instruction;
logic [mask_length_gp-1:0] barrier_r,      barrier_n,
                           barrier_mask_r, barrier_mask_n;

ctrl_sig ctrl_o;

logic insert_nop_n, insert_nop_r;

logic [($bits(instruction.rs_imm)) - 1:0] wa_addr, rs_addr, rd_addr, wd_addr;

logic[1:0] nop_ctr;
logic nop;

logic fwd_rs, fwd_rd;

logic[31:0] forward_wd;

mem_out_s mem_wb_from_mem_i;

logic[31:0] mem_wb_alu_result;
logic[imem_addr_width_p-1:0] mem_wb_pc_plus1;

//---- Connection to external modules ----//


// Instructions are shorter than 32 bits of network data
assign net_instruction = net_packet_i.net_data [0+:($bits(instruction))];

// Selection between network and core for instruction address
assign imem_addr = (net_imem_write_cmd) ? net_packet_i.net_addr
                                       : PC_n;

// Insruction memory
instr_mem #(.addr_width_p(imem_addr_width_p)) imem
           (.clk(clk)
           ,.addr_i(imem_addr)
           ,.instruction_i(net_instruction)
           ,.wen_i(imem_wen)
           //,.nop_i(nop)
           ,.instruction_o(imem_out)
           );

assign PC_wen = net_PC_write_cmd_IDLE || (~stall && ~insert_nop_n);

always_comb
begin
  if(PC_wen_r)
    instruction = imem_out;
  else if(insert_nop_r)
    instruction = 16'b1111111111111111;
  else if(stall)
    instruction = instruction_r;
end

// Determine next PC
assign pc_plus1     = PC_r + 1'b1;
assign imm_jump_add = $signed(instruction.rs_imm)  + $signed(PC_r);

// Next pc is based on network or the instruction
always_comb
  begin
    PC_n = pc_plus1;
    if (net_PC_write_cmd_IDLE)
      PC_n = net_packet_i.net_addr;
    else
      unique casez (if_id_i.instruction)
        `kJALR:
        begin
          PC_n = alu_result[0+:imem_addr_width_p];
        end

        `kBNEQZ,`kBEQZ,`kBLTZ,`kBGTZ:
        begin
          if (jump_now)
            PC_n = imm_jump_add;
        end

        default: begin end
      endcase
  end

assign if_id_o.instruction = (~stall) ? instruction : if_id_i.instruction;
assign if_id_o.net_packet_i = net_packet_i;
assign if_id_o.is_load_op_c = 0;
assign if_id_o.op_writes_rf_c = 0;
assign if_id_o.is_store_op_c = 0;
assign if_id_o.is_mem_op_c = 0;
assign if_id_o.is_byte_op_c = 0;
assign if_id_o.net_reg_write_cmd = net_reg_write_cmd;

always_ff @ (posedge clk) begin
  if_id_i <= if_id_o;
  insert_nop_r = insert_nop_n;
end

always_comb
  begin
    insert_nop_n = 1'b0;
    unique casez (instruction)
      `kJALR, `kBNEQZ,`kBEQZ,`kBLTZ,`kBGTZ, `kLW, `kSW, `kLBU, `kSB:
        insert_nop_n = 1'b1;
      default: begin end
    endcase
    unique casez (if_id_i.instruction)
      `kJALR, `kBNEQZ,`kBEQZ,`kBLTZ,`kBGTZ, `kLW, `kSW, `kLBU, `kSB:
        insert_nop_n = 1'b0;
      default : begin end
    endcase
  end

// Decode module
cl_decode decode (.instruction_i(if_id_i.instruction)
                  ,.is_load_op_o(is_load_op_c)
                  ,.op_writes_rf_o(op_writes_rf_c)
                  ,.is_store_op_o(is_store_op_c)
                  ,.is_mem_op_o(is_mem_op_c)
                  ,.is_byte_op_o(is_byte_op_c)
                  );

cl_state_machine state_machine (.instruction_i(instruction)
                               ,.state_i(state_r)
                               ,.exception_i(exception_o)
                               ,.net_PC_write_cmd_IDLE_i(net_PC_write_cmd_IDLE)
                               ,.stall_i(stall)
                               ,.state_o(state_n)
                               );

assign rd_addr = ({{($bits(instruction.rs_imm)-$bits(instruction.rd)){1'b0}}
                    ,{if_id_i.instruction.rd}});


assign wa_addr = (net_reg_write_cmd)
                 ? (net_packet_i.net_addr [0+:($bits(instruction.rs_imm))])
                 : ({{($bits(instruction.rs_imm)-$bits(instruction.rd)){1'b0}}
                    ,{id_exe_i.instruction.rd}});
// Register file
reg_file #(.addr_width_p($bits(instruction.rs_imm))) rf
          (.clk(clk)
          ,.rs_addr_i(if_id_i.instruction.rs_imm)
          ,.rd_addr_i(rd_addr)
          ,.wen_i(rf_wen)
          ,.wa_i(wd_addr)
          ,.write_data_i(rf_wd)
          ,.rs_val_o(rs_val)
          ,.rd_val_o(rd_val)
          );

always_comb
  if(if_id_i.instruction.rs_imm == mem_wb_i.instruction.rd) begin
    rs_val_or_zero = forward_wd;
    fwd_rs = 1;
  end
  else begin
    rs_val_or_zero = if_id_i.instruction.rs_imm ? rs_val : 32'b0;
    fwd_rs = 0;
  end

always_comb 
  if(if_id_i.instruction.rd == mem_wb_i.instruction.rd) begin
    rd_val_or_zero = forward_wd;
    fwd_rd = 1;
  end
  else begin
    rd_val_or_zero = rd_addr ? rd_val : 32'b0;
    fwd_rd = 0;
  end

// ALU
alu alu_1 (.rd_i(rd_val_or_zero)
          ,.rs_i(rs_val_or_zero)
          ,.op_i(if_id_i.instruction)
          ,.result_o(alu_result)
          ,.jump_now_o(jump_now)
          );

assign mem_hazard = (net_reg_write_cmd && op_writes_rf_c) 
  || (net_imem_write_cmd);

// Stall if LD/ST still active; or in non-RUN state
assign stall = mem_hazard || (mem_stage_n != 0) || (state_r != RUN);

// Launch LD/ST
assign valid_to_mem_c = is_mem_op_c & (mem_stage_r < 2'b10);

// Data_mem
assign to_mem_o = '{write_data    : rs_val_or_zero
                   ,valid         : valid_to_mem_c
                   ,wen           : is_store_op_c
                   ,byte_not_word : is_byte_op_c
                   ,yumi          : yumi_to_mem_c
                   };

assign data_mem_addr = alu_result;

always_comb
  begin
    yumi_to_mem_c = 1'b0;
    mem_stage_n   = mem_stage_r;

    if (valid_to_mem_c)
        mem_stage_n   = 2'b01;

    if (from_mem_i.yumi)
        mem_stage_n   = 2'b10;

    // If we can commit the LD/ST this cycle, the acknowledge dmem's response
    if (from_mem_i.valid & ~mem_hazard)
      begin
        mem_stage_n   = 2'b00;
        yumi_to_mem_c = 1'b1;
      end
  end

assign mem_wb_o = (~stall) ? if_id_i : mem_wb_i;

always_ff @ (posedge clk) begin
  mem_wb_i.instruction <= mem_wb_o.instruction;
  if(~stall) begin
    mem_wb_i.is_load_op_c <= is_load_op_c;
    mem_wb_i.op_writes_rf_c <= op_writes_rf_c;
    mem_wb_i.is_store_op_c <= is_store_op_c;
    mem_wb_i.is_mem_op_c <= is_mem_op_c;
    mem_wb_i.is_byte_op_c <= is_byte_op_c;

    mem_wb_from_mem_i <= from_mem_i;
    mem_wb_alu_result <= alu_result;
    mem_wb_pc_plus1 <= pc_plus1;
  end
end

assign forward_wd = rf_wd;

// Register write could be from network or the controller
assign rf_wen    = (net_reg_write_cmd || (mem_wb_i.op_writes_rf_c && ~stall));

assign wd_addr = (net_reg_write_cmd)
      ? (net_packet_i.net_addr [0+:($bits(instruction.rs_imm))])
      : ({{($bits(instruction.rs_imm)-$bits(instruction.rd)){1'b0}}
        ,{mem_wb_i.instruction.rd}});

always_comb
  begin
    if(net_reg_write_cmd)
      rf_wd = net_packet_i.net_data;
    else if(mem_wb_i.instruction==?`kJALR)
      rf_wd = mem_wb_pc_plus1;
    else if(mem_wb_i.is_load_op_c)
      rf_wd = mem_wb_from_mem_i.read_data;
    else
      rf_wd = mem_wb_alu_result;
  end

always_ff @ (posedge clk)
  begin
    if (!reset)
      begin
        PC_r            <= 0;
        barrier_mask_r  <= {(mask_length_gp){1'b0}};
        barrier_r       <= {(mask_length_gp){1'b0}};
        state_r         <= IDLE;
        instruction_r   <= 0;
        PC_wen_r        <= 0;
        exception_o     <= 0;
        mem_stage_r     <= 2'b00;
      end

    else
      begin  
        if(PC_wen)
          PC_r <= PC_n;
        barrier_mask_r <= barrier_mask_n;
        barrier_r <= barrier_n;
        state_r <= state_n;
        instruction_r <= instruction;
        PC_wen_r = PC_wen;
        exception_o <= exception_n;
        mem_stage_r <= mem_stage_n;
      end
    end


// Suppress warnings
assign net_packet_o = net_packet_i;

// DEBUG Struct
assign debug_o = {PC_r, instruction, state_r, barrier_mask_r, barrier_r};

//---- Datapath with network ----//
// Detect a valid packet for this core
assign net_ID_match = (net_packet_i.ID==net_ID_p);

// Network operation
assign net_PC_write_cmd      = (net_ID_match && (net_packet_i.net_op==PC));
assign net_imem_write_cmd    = (net_ID_match && (net_packet_i.net_op==INSTR));
assign net_reg_write_cmd     = (net_ID_match && (net_packet_i.net_op==REG));
assign net_bar_write_cmd     = (net_ID_match && (net_packet_i.net_op==BAR));
assign net_PC_write_cmd_IDLE = (net_PC_write_cmd && (state_r==IDLE));

// Barrier final result, in the barrier mask, 1 means not mask and 0 means mask
assign barrier_o = barrier_mask_r & barrier_r;

// The instruction write is just for network
assign imem_wen  = net_imem_write_cmd;

// barrier_mask_n, which stores the mask for barrier signal
always_comb
  // Change PC packet
  if (net_bar_write_cmd && (state_r != ERR))
    barrier_mask_n = net_packet_i.net_data [0+:mask_length_gp];
  else
    barrier_mask_n = barrier_mask_r;

// barrier_n signal, which contains the barrier value
// it can be set by PC write network command if in IDLE
// or by an an BAR instruction that is committing
assign barrier_n = net_PC_write_cmd_IDLE
                   ? net_packet_i.net_data[0+:mask_length_gp]
                   : ((instruction==?`kBAR) & ~stall)
                     ? alu_result [0+:mask_length_gp]
                     : barrier_r;

// exception_n signal, which indicates an exception
// We cannot determine next state as ERR in WORK state, since the instruction
// must be completed, WORK state means start of any operation and in memory
// instructions which could take some cycles, it could mean wait for the
// response of the memory to aknowledge the command. So we signal that we recieved
// a wrong package, but do not stop the execution. Afterwards the exception_r
// register is used to avoid extra fetch after this instruction.
always_comb
  if ((state_r==ERR) || (net_PC_write_cmd && (state_r!=IDLE)))
    exception_n = 1'b1;
  else
    exception_n = exception_o;

endmodule
