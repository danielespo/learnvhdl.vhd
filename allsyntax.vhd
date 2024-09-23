---
language: VHDL
filename: all-syntax.vhd
contributors:
    - ["Daniel A Espinosa", "https://github.com/danielespo"]
---
 
-- All of VHDL syntax in one file
-- Daniel A Espinosa 2024
-- ++++++++++++++++++++++++++++++++++++++
-- START SYNTHESIZABLE VHDL BELOW
-- ++++++++++++++++++++++++++++++++++++++

-- Library and use clauses
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Entity declaration
entity entity_name is
    Port (
        clk     : in  STD_LOGIC;
        reset   : in  STD_LOGIC;
        input_signal  : in  STD_LOGIC_VECTOR(width-1 downto 0);
        output_signal : out STD_LOGIC_VECTOR(width-1 downto 0)
    );
end entity entity_name;

-- Architecture declaration
architecture Behavioral of entity_name is

    -- Signal declarations
    signal wire_name : STD_LOGIC_VECTOR(width-1 downto 0);
    signal reg_name  : STD_LOGIC_VECTOR(width-1 downto 0);

    -- Constant declarations
    constant CONST_NAME : integer := value;

    -- Type declarations
    type enum_type is (ENUM_VALUE1, ENUM_VALUE2, ENUM_VALUE3);
    signal enum_signal : enum_type;

    -- Array declarations
    type packed_array_type is array (0 to 3) of STD_LOGIC_VECTOR(7 downto 0);
    signal packed_array : packed_array_type;

    -- Record declarations
    type record_type is record
        field1 : STD_LOGIC_VECTOR(7 downto 0);
        field2 : STD_LOGIC_VECTOR(15 downto 0);
    end record;
    signal record_signal : record_type;

    -- Generics
    generic (
        GENERIC_PARAM1 : integer := default_value1;
        GENERIC_PARAM2 : std_logic := '0'
    );

begin

    -- Concurrent signal assignment
    wire_name <= input_signal;

    -- Process blocks
    process(clk, reset)
    begin
        if reset = '1' then
            output_signal <= (others => '0');
        elsif rising_edge(clk) then
            output_signal <= wire_name;
        end if;
    end process;

    -- Conditional signal assignment (with selected signal assignment)
    with enum_signal select
        wire_name <= "0001" when ENUM_VALUE1,
                     "0010" when ENUM_VALUE2,
                     "0100" when ENUM_VALUE3,
                     "0000" when others;

    -- Case statements inside processes
    process(clk)
    begin
        if rising_edge(clk) then
            case enum_signal is
                when ENUM_VALUE1 =>
                    -- Statements
                when ENUM_VALUE2 =>
                    -- Statements
                when ENUM_VALUE3 =>
                    -- Statements
                when others =>
                    -- Statements
            end case;
        end if;
    end process;

    -- Generate statements
    gen_loop: for i in 0 to N-1 generate
        -- Generated instances or logic
    end generate gen_loop;

    -- Component instantiation
    U1: entity work.another_entity
        port map (
            clk => clk,
            reset => reset,
            input_signal => wire_name,
            output_signal => reg_name
        );

    -- Arithmetic, logical, and relational operators
    wire_name <= std_logic_vector(unsigned(a) + unsigned(b));
    wire_name <= a - b;
    wire_name <= a * b;
    wire_name <= a / b;
    wire_name <= a mod b;
    wire_name <= a = b;
    wire_name <= a /= b;
    wire_name <= a < b;
    wire_name <= a > b;
    wire_name <= a <= b;
    wire_name <= a >= b;
    wire_name <= a and b;
    wire_name <= a or b;
    wire_name <= not a;
    wire_name <= a nand b;
    wire_name <= a nor b;
    wire_name <= a xor b;
    wire_name <= a xnor b;

    -- Concatenation and replication
    wire_name <= signal1 & signal2 & signal3;
    wire_name <= (others => signal);

    -- Conditional operator (selected signal assignment)
    with condition select
        wire_name <= true_expression when true,
                     false_expression when false;

    -- Variables (inside processes)
    process
        variable var_name : integer;
    begin
        var_name := expression;
    end process;

    -- Arrays and slices
    wire_name <= input_signal(7 downto 0);

    -- Attributes and pragmas
    attribute keep : boolean;
    attribute keep of signal_name : signal is true;

    -- Packages
    package my_package is
        -- Declarations
    end package my_package;

    use work.my_package.all;

    -- Types and subtypes
    subtype byte is STD_LOGIC_VECTOR(7 downto 0);
    signal byte_signal : byte;

    -- Records
    type my_record is record
        a : std_logic;
        b : std_logic_vector(3 downto 0);
    end record;
    signal record_inst : my_record;

    -- Functions and procedures
    function my_function(arg1 : integer; arg2 : integer) return integer is
    begin
        return arg1 + arg2;
    end function;

    procedure my_procedure(signal_in : in STD_LOGIC; signal_out : out STD_LOGIC) is
    begin
        signal_out <= signal_in;
    end procedure;

    -- Assertions (using VHDL assert statement)
    assert (condition) report "Error message" severity error;

    -- Finite State Machines (explicit state encoding)
    type state_type is (IDLE, STATE1, STATE2, STATE3);
    signal current_state, next_state : state_type;

    -- Sequential logic for state transitions
    process(clk, reset)
    begin
        if reset = '1' then
            current_state <= IDLE;
        elsif rising_edge(clk) then
            current_state <= next_state;
        end if;
    end process;

    -- Combinational logic for next state
    process(current_state, some_input)
    begin
        case current_state is
            when IDLE =>
                if some_condition then
                    next_state <= STATE1;
                else
                    next_state <= IDLE;
                end if;
            when STATE1 =>
                next_state <= STATE2;
            when STATE2 =>
                next_state <= STATE3;
            when STATE3 =>
                next_state <= IDLE;
            when others =>
                next_state <= IDLE;
        end case;
    end process;

    -- Signed and unsigned types
    signal signed_var   : signed(7 downto 0);
    signal unsigned_var : unsigned(7 downto 0);

    -- Fixed-point types (using IEEE fixed_pkg)
    library IEEE;
    use IEEE.fixed_pkg.all;
    signal fixed_point : sfixed(15 downto -16);

end architecture Behavioral;

-- ============================================================================= #

-- More advanced features, use with caution ...

-- ============================================================================= #

-- Packages (detailed)
package detailed_package is
    -- Package declarations
end package detailed_package;

package body detailed_package is
    -- Package definitions
end package body detailed_package;

-- Configuration
configuration cfg of entity_name is
    for Behavioral
        -- Configuration clauses
    end for;
end configuration cfg;

-- Component declarations
component component_name is
    Port (
        clk     : in  STD_LOGIC;
        reset   : in  STD_LOGIC;
        input_signal  : in  STD_LOGIC_VECTOR(width-1 downto 0);
        output_signal : out STD_LOGIC_VECTOR(width-1 downto 0)
    );
end component component_name;

-- Generics with default values
entity generic_entity is
    generic (
        WIDTH : integer := 8
    );
    Port (
        input_signal  : in  STD_LOGIC_VECTOR(WIDTH-1 downto 0);
        output_signal : out STD_LOGIC_VECTOR(WIDTH-1 downto 0)
    );
end entity generic_entity;

architecture rtl of generic_entity is
begin
    -- Architecture body
end architecture rtl;

-- Process sensitivity list examples
process(clk, reset)
begin
    -- Process body
end process;

process(all)
begin
    -- Process body with automatic sensitivity list
end process;

-- Blocking vs Non-blocking assignments (VHDL uses signal vs variable assignments)
-- Signal assignments are scheduled (similar to non-blocking)
signal_a <= signal_b;
-- Variable assignments are immediate (similar to blocking)
variable_a := variable_b;

-- Assertions with severity levels
assert (x > 0) report "x must be positive" severity warning;

-- Synthesis directives (attributes)
attribute syn_keep : string;
attribute syn_keep of wire_name : signal is "true";

-- External IP integration (using block declarations)
block external_block : entity work.external_entity
    port map (
        clk => clk,
        reset => reset,
        input_signal => wire_name,
        output_signal => reg_name
    );
end block external_block;

-- ============================================================================= #

-- The rest of all of the syntax, unorganized:

-- File I/O (using textio)
library std;
use std.textio.all;

process
    file infile  : text open read_mode is "input.txt";
    file outfile : text open write_mode is "output.txt";
    variable line_in  : line;
    variable line_out : line;
    variable data     : integer;
begin
    while not endfile(infile) loop
        readline(infile, line_in);
        read(line_in, data);
        write(line_out, data);
        writeline(outfile, line_out);
    end loop;
    wait;
end process;

-- Wait statements
process(clk)
begin
    wait until rising_edge(clk);
    -- Statements after the clock edge
end process;

-- Delayed signal assignments
signal_a <= signal_b after 10 ns;

-- Advanced generate constructs
gen_if: if CONDITION generate
    -- Generate items
end generate gen_if;

gen_case: case SELECT_EXPR is
    when OPTION1 =>
        -- Generate items
    when OPTION2 =>
        -- Generate items
    when others =>
        -- Generate items
end case;

-- Vector and array operations
signal_a <= signal_b(15 downto 8) & signal_c(7 downto 0);
signal_d <= signal_a(3 downto 0) & "1010";

-- Access types (pointers)
type ptr_type is access integer;
signal ptr_signal : ptr_type;

-- File types
file my_file : text;

-- Shared variables (limited use in VHDL-2008)
shared variable shared_var : integer := 0;

-- Protected types (VHDL-2008)
protected type my_protected_type is
    procedure increment;
    function get_value return integer;
private
    variable value : integer := 0;
end protected my_protected_type;

-- Implementation of protected types
protected body my_protected_type is
    procedure increment is
    begin
        value := value + 1;
    end procedure increment;

    function get_value return integer is
    begin
        return value;
    end function get_value;
end protected body my_protected_type;

-- Advanced type conversions
signal_a <= std_logic_vector(to_unsigned(integer_var, wire_a'length));
integer_var <= to_integer(unsigned(signal_a));

-- Accessing record fields
signal_inst.field1 <= "10101010";
signal_inst.field2 <= "1111000011110000";

-- Hierarchical references
signal_a <= component_inst.signal_b;

-- Conditional signal assignments with multiple conditions
with sel select
    out_signal <= "00" when "00",
                  "01" when "01",
                  "10" when "10",
                  "11" when others;

-- Complex expressions
signal_a <= (a and b) or (c nand d) xor e;

-- Nested generate statements
gen_outer: for i in 0 to N-1 generate
    gen_inner: for j in 0 to M-1 generate
        -- Nested generate items
    end generate gen_inner;
end generate gen_outer;

-- Assertion on specific signal conditions
assert (reset = '0') report "Reset must be inactive" severity failure;

-- Using aliases for better readability
alias alias_name : STD_LOGIC_VECTOR(7 downto 0) is signal_a;

-- Generic functions
function generic_add<T> (a, b : T) return T is
begin
    return a + b;
end function generic_add;

-- Packages with generic parameters
package generic_pkg is
    type gen_array is array (natural range <>) of integer;
    -- Other declarations
end package generic_pkg;

use work.generic_pkg.all;

-- Derived types
subtype small_int is integer range 0 to 255;

-- Enumerated types with attributes
type traffic_light is (RED, YELLOW, GREEN);
attribute safe : boolean;
attribute safe of traffic_light : type is true;

-- Protected functions within a package
package protected_pkg is
    protected type safe_counter is
        procedure increment;
        function get_count return integer;
    end protected safe_counter;
end package protected_pkg;

package body protected_pkg is
    protected body safe_counter is
        variable count : integer := 0;
        procedure increment is
        begin
            count := count + 1;
        end procedure increment;

        function get_count return integer is
        begin
            return count;
        end function get_count;
    end protected safe_counter;
end package body protected_pkg;


    -- Signal initialization
    signal a : std_logic := '0';
    signal b : STD_LOGIC_VECTOR(7 downto 0) := "10101010";

    -- Variable initialization
    process
        variable x : integer := 100;
    begin
        -- Process body
    end process;

    -- Function declaration outside architecture
    function add(a, b : integer) return integer is
    begin
        return a + b;
    end function add;

    -- Procedure declaration outside architecture
    procedure reset_signal(signal_in : inout std_logic) is
    begin
        signal_in <= '0';
    end procedure reset_signal;

    -- Generic entity instantiation
    U_generic: entity work.generic_entity
        generic map (
            WIDTH => 16
        )
        port map (
            input_signal  => some_input,
            output_signal => some_output
        );

    -- Package instantiation
    library my_lib;
    use my_lib.my_package.all;

    -- Initializing arrays with initial values
    type my_array is array (0 to 7) of integer := (others => 0);
    signal array_signal : my_array := (1, 2, 3, 4, 5, 6, 7, 8);

    -- Initializing records
    signal my_record_inst : my_record := (field1 => "00000000", field2 => "0000000000000000");

        -- Exit statement in a loop
            process
            begin
                for i in 0 to 10 loop
                    if i = 5 then
                        exit; -- Exit the loop when i equals 5
                    end if;
                end loop;
                wait;
            end process;
            
            -- Next statement in a loop
            process
            begin
                for i in 0 to 10 loop
                    if i = 5 then
                        next; -- Skip the rest of the loop when i equals 5
                    end if;
                    -- Other statements
                end loop;
                wait;
            end process;
            
            -- Null statement
            process
            begin
                if condition then
                    null; -- Do nothing
                else
                    -- Other statements
                end if;
                wait;
            end process;
            
            -- Using the 'now' system function
            signal current_time : time;
            
            process
            begin
                current_time <= now;
                wait;
            end process;
            
            -- Wait statements with different forms
            process
            begin
                wait for 10 ns;
                -- Statements after 10 ns delay
                wait until rising_edge(clk);
                -- Statements after rising edge
                wait;
            end process;
        