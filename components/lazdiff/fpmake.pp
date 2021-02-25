{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for lazdiff 0.0

   This file was generated on 25/02/2021
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_lazdiff(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazdiff');
    P.Version:='<none>';

    P.Directory:=ADirectory;


    D := P.Dependencies.Add('lcl');
    D := P.Dependencies.Add('lazutils');
    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('lazdiff.pas');


    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('lazdiff.compiled');
    P.InstallFiles.Add('lazdiff.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_lazdiff('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
