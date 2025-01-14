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

// Reg. File address
logic [($bits(instruction.rs_imm))-1:0] rd_addr, rs_addr, wa_addr;

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
logic [1:0] mem_stage_r, mem_stage_n;
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

logic[31:0] forward_wd;
logic insert_nop_r, insert_nop_n;

logic fwd_rd, fwd_rs;

mem_out_s mem_wb_from_mem_i;

logic[31:0] mem_wb_alu_result;
logic[imem_addr_width_p-1:0] mem_wb_pc_plus1;

//---- Connection to external modules ----//

// Suppress warnings
assign net_packet_o = net_packet_i;


assign data_mem_addr = alu_result;

// DEBUG Struct
assign debug_o = {PC_r, instruction, state_r, barrier_mask_r, barrier_r};

logic[1:0] nop_ctr;
logic nop;
assign nop = (nop_ctr != 0);

// Insruction memory
instr_mem #(.addr_width_p(imem_addr_width_p)) imem
           (.clk(clk)
           ,.addr_i(imem_addr)
           ,.instruction_i(net_instruction)
           ,.wen_i(imem_wen)
           ,.nop_i(nop)
           ,.instruction_o(imem_out)
           );

always_comb
begin
  if(PC_wen_r)
    instruction = imem_out;
  else if(insert_nop_r)
    instruction = 16'b1111111111111111;
  else if(stall)
    instruction = instruction_r;
end

assign if_id_o.instruction = instruction;
assign if_id_o.net_packet_i = net_packet_i;
assign if_id_o.net_reg_write_cmd = net_reg_write_cmd;
assign if_id_o.imm_jump_add = imm_jump_add;
assign if_id_o.pc_plus1 = pc_plus1;

// Register file
reg_file #(.addr_width_p($bits(instruction.rs_imm))) rf
          (.clk(clk)
          ,.rs_addr_i(rs_addr)
          ,.rd_addr_i(rd_addr)
          ,.wen_i(rf_wen)
          ,.wa_i(wa_addr)
          ,.write_data_i(rf_wd)
          ,.rs_val_o(rs_val)
          ,.rd_val_o(rd_val)
          );

assign forward_wd = rf_wd;

//Forwarding id_exe values
always_comb
  if(if_id_i.instruction.rs_imm == id_exe_i.instruction.rd) begin
    rs_val_or_zero = forward_wd;
    fwd_rs = 1;
  end
  else begin
    rs_val_or_zero = if_id_i.instruction.rs_imm ? rs_val : 32'b0;
    fwd_rs = 0;
  end

always_comb
  if(if_id_i.instruction.rd == id_exe_i.instruction.rd) begin
    rd_val_or_zero = forward_wd;
    fwd_rd = 1;
  end
  else begin
    rd_val_or_zero = rd_addr ? rd_val : 32'b0;
    fwd_rd = 0;
  end


assign id_exe_o.instruction = if_id_i.instruction;
assign id_exe_o.ctrl = ctrl_o;
assign id_exe_o.imm_jump_add = if_id_i.imm_jump_add;
assign id_exe_o.pc_plus1 = if_id_i.pc_plus1;
assign id_exe_o.rs_val = rs_val_or_zero;
assign id_exe_o.rd_val = rd_val_or_zero;
assign id_exe_o.state_n = state_n;

// ALU
alu alu_1 (.rd_i(id_exe_i.rd_val)
          ,.rs_i(id_exe_i.rs_val)
          ,.op_i(id_exe_i.instruction)
          ,.result_o(alu_result)
          ,.jump_now_o(jump_now)
          );


cl_state_machine state_machine (.instruction_i(id_exe_i.instruction)
                               ,.state_i(state_r)
                               ,.exception_i(exception_o)
                               ,.net_PC_write_cmd_IDLE_i(net_PC_write_cmd_IDLE)
                               ,.stall_i(stall)
                               ,.state_o(state_n)
                               );

assign exe_mem_i.instruction = id_exe_i.instruction;
assign exe_mem_i.ctrl = id_exe_i.ctrl;
assign exe_mem_i.imm_jump_add = id_exe_i.imm_jump_add;
assign exe_mem_i.pc_plus1 = id_exe_i.pc_plus1;
assign exe_mem_i.rs_val = id_exe_i.rs_val;
assign exe_mem_i.rd_val = id_exe_i.rd_val;
assign exe_mem_i.state_n = id_exe_i.state_n;

// Data_mem
assign to_mem_o = '{write_data    : exe_mem_i.rs_val
                   ,valid         : valid_to_mem_c
                   ,wen           : exe_mem_i.ctrl.is_store_op_c
                   ,byte_not_word : exe_mem_i.ctrl.is_byte_op_c
                   ,yumi          : yumi_to_mem_c
                   };

// Decode module
cl_decode decode (.instruction_i(if_id_i.instruction)
                  ,.ctrl_o(ctrl_o)
                  // ,.is_load_op_o(is_load_op_c)
                  // ,.op_writes_rf_o(op_writes_rf_c)
                  // ,.is_store_op_o(is_store_op_c)
                  // ,.is_mem_op_o(is_mem_op_c)
                  // ,.is_byte_op_o(is_byte_op_c)
                  );
// select the input data for Register file, from network, the PC_plus1 for JALR,
// Data Memory or ALU result
always_comb
  begin
    if (net_reg_write_cmd)
      rf_wd = net_packet_i.net_data;

    else if (id_exe_i.instruction==?`kJALR)
      rf_wd = id_exe_i.instruction.rd ? id_exe_i.pc_plus1 : 0;

    else if (id_exe_i.ctrl.is_load_op_c)
      rf_wd = from_mem_i.read_data;

    else
      rf_wd = alu_result;
  end

// Determine next PC
assign pc_plus1     = PC_r + 1'b1;
assign imm_jump_add = $signed(instruction.rs_imm)  + $signed(PC_r);

// Next pc is based on network or the instruction
always_comb
  begin
    PC_n = nop ? PC_r : id_exe_i.pc_plus1;

    if (net_PC_write_cmd_IDLE)
      PC_n = net_packet_i.net_addr;
    else
      unique casez (id_exe_i.instruction)
        `kJALR:
          PC_n = alu_result[0+:imem_addr_width_p];

        `kBNEQZ,`kBEQZ,`kBLTZ,`kBGTZ:
          if (jump_now)
            PC_n = id_exe_i.imm_jump_add;

        default: begin end
      endcase
  end

assign PC_wen = (net_PC_write_cmd_IDLE || ~stall);
always_ff @ (posedge clk) begin
  if_id_i <= if_id_o;
  insert_nop_r = insert_nop_n;
end

always_comb
begin
  insert_nop_n = 1'b0;
  unique casez(instruction)
    `kJALR, `kBNEQZ, `kBEQZ, `kBLTZ, `kBGTZ, `kLW, `kLBU, `kSW, `kSB:
      insert_nop_n = 1'b1;
    default: begin end
  endcase
  unique casez(if_id_i.instruction)
    `kJALR, `kBNEQZ, `kBEQZ, `kBLTZ, `kBGTZ, `kLW, `kLBU, `kSW, `kSB:
      insert_nop_n = 1'b0;
    default: begin end
  endcase
end

assign mem_wb_o.instruction = exe_mem_i.instruction;
assign mem_wb_o.ctrl = exe_mem_i.ctrl;
assign mem_wb_o.rs_val = exe_mem_i.rs_val;
assign mem_wb_o.rd_val = exe_mem_i.rd_val;
assign mem_wb_o.imm_jump_add = exe_mem_i.imm_jump_add;
assign mem_wb_o.pc_plus1 = exe_mem_i.pc_plus1;

                   
// Sequential part, including PC, barrier, exception and state
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

        if_id_i <= 0;
        id_exe_i <= 0;  
        mem_wb_i <= 0;
        nop_ctr <= 0;
      end

    else
      begin

        nop_ctr <= (nop_ctr + 1) % 3;

        if (PC_wen) begin
          PC_r         <= PC_n;

          if(net_PC_write_cmd) begin
            if_id_i <= 0;
            id_exe_i <= 0;
          end
          else begin
            if_id_i <= if_id_o;
            id_exe_i <= id_exe_o;
            mem_wb_i <= mem_wb_o;
          end
        end

        barrier_mask_r <= barrier_mask_n;
        barrier_r      <= barrier_n;
        state_r        <= state_n;
        instruction_r  <= instruction;
        PC_wen_r       <= PC_wen;
        exception_o    <= exception_n;
        mem_stage_r    <= mem_stage_n;
      end
  end

// stall and memory stages signals
// rf structural hazard and imem structural hazard (can't load next instruction)
assign stall_non_mem = (net_reg_write_cmd && id_exe_i.ctrl.op_writes_rf_c)
                    || (net_imem_write_cmd);
// Stall if LD/ST still active; or in non-RUN state
assign stall = stall_non_mem || (mem_stage_n != 0) || (state_r != RUN);

// Launch LD/ST
assign valid_to_mem_c = id_exe_i.ctrl.is_mem_op_c & (mem_stage_r < 2'b10);

always_comb
  begin
    yumi_to_mem_c = 1'b0;
    mem_stage_n   = mem_stage_r;

    if (valid_to_mem_c)
        mem_stage_n   = 2'b01;

    if (from_mem_i.yumi)
        mem_stage_n   = 2'b10;

    // If we can commit the LD/ST this cycle, the acknowledge dmem's response
    if (from_mem_i.valid & ~stall_non_mem)
      begin
        mem_stage_n   = 2'b00;
        yumi_to_mem_c = 1'b1;
      end
  end


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

// Register write could be from network or the controller
assign rf_wen    = net_reg_write_cmd || (id_exe_i.ctrl.op_writes_rf_c && ~stall);

assign rs_addr = if_id_i.instruction.rs_imm;

// Selection between network and core for instruction address
assign imem_addr = (net_imem_write_cmd) ? net_packet_i.net_addr
                                       : PC_n;

// Selection between network and address included in the instruction which is exeuted
// Address for Reg. File is shorter than address of Ins. memory in network data
// Since network can write into immediate registers, the address is wider
// but for the destination register in an instruction the extra bits must be zero
assign rd_addr = (net_reg_write_cmd)
                 ? (net_packet_i.net_addr [0+:($bits(instruction.rs_imm))])
                 : ({{($bits(instruction.rs_imm)-$bits(instruction.rd)){1'b0}}
                    ,{if_id_i.instruction.rd}});

assign wa_addr = (net_reg_write_cmd)
                 ? (net_packet_i.net_addr [0+:($bits(instruction.rs_imm))])
                 : ({{($bits(instruction.rs_imm)-$bits(instruction.rd)){1'b0}}
                    ,{id_exe_i.instruction.rd}});

// Instructions are shorter than 32 bits of network data
assign net_instruction = net_packet_i.net_data [0+:($bits(instruction))];

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
                   : ((id_exe_i.instruction==?`kBAR) & ~stall)
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
