{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit callstack_memprofiler_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  callstack_memprofiler, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('callstack_memprofiler_pkg', @Register);
end.
