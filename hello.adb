-- A minimal Ada program demonstrating basic text output
-- Ada programs are typically named with .adb extension for the body
with Ada.Text_IO;  -- Import the text I/O package

procedure Hello is
begin
   -- Put_Line automatically adds a newline character
   Ada.Text_IO.Put_Line("Hello, World!");
end Hello;
