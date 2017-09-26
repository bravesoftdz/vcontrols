{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  VListBox, VChartControl, vtypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VListBox', @VListBox.Register);
  RegisterUnit('VChartControl', @VChartControl.Register);
end;

initialization
  RegisterPackage('vcontrols', @Register);
end.
