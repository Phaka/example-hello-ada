# Ada Hello World Example

A minimal Ada implementation of "Hello, World!" that demonstrates basic Ada syntax and compilation using the GNAT compiler. Ada is particularly notable for its strong typing system and design features that support reliability and maintainability in large systems.

## Prerequisites

You'll need the GNAT compiler, which is part of GCC. Here's how to install it on different platforms:

### Linux
On Debian/Ubuntu:
```bash
sudo apt update
sudo apt install gnat
```

On Fedora:
```bash
sudo dnf install gcc-gnat
```

### macOS
Using Homebrew:
```bash
brew install gnat
```

### Windows
Download GNAT Community Edition from AdaCore's website:
https://www.adacore.com/download

The installer will add the necessary tools to your PATH.

## Compiling

The compilation process is similar across platforms. GNAT provides several compilation commands, but we'll use the simplest approach with `gnatmake`:

### Linux and macOS
```bash
gnatmake hello.adb
```

### Windows
```cmd
gnatmake hello.adb
```

This will produce several files:
- `hello` (or `hello.exe` on Windows): The executable
- `hello.ali`: GNAT compilation information
- `hello.o`: Object file

## Running

### Linux and macOS
```bash
./hello
```

### Windows
```cmd
hello.exe
```
Or from PowerShell:
```powershell
.\hello.exe
```

## Code Explanation

Let's examine our Ada implementation line by line:

```ada
with Ada.Text_IO;  -- Import the text I/O package

procedure Hello is
begin
   Ada.Text_IO.Put_Line("Hello, World!");
end Hello;
```

The code demonstrates several fundamental Ada concepts:

1. The `with` clause imports the `Ada.Text_IO` package, which provides input/output functionality. This is similar to imports or includes in other languages.

2. The main program is declared as a `procedure` named `Hello`. In Ada, the program's entry point is a procedure whose name matches the filename (hello.adb).

3. The `begin` and `end` keywords define a block of statements, similar to curly braces in C-like languages.

4. `Ada.Text_IO.Put_Line` is a fully qualified call to the `Put_Line` procedure in the `Text_IO` package. Ada uses dot notation for package member access.

## Verifying the Build

You can examine the binary and its dependencies:

### Linux
```bash
# Check binary size
ls -l hello
# Check dynamic dependencies
ldd hello
```

### macOS
```bash
# Check binary size
ls -l hello
# Check dynamic dependencies
otool -L hello
```

### Windows
```cmd
dir hello.exe
dumpbin /dependents hello.exe
```

## Further Reading

- [Ada Programming on Wikibooks](https://en.wikibooks.org/wiki/Ada_Programming)
- [Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)
- [Learn Ada Now](https://learn.adacore.com/)
