{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for lazdiffcomparefiles 1.0

   This file was generated on 25/02/2021
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_lazdiffcomparefiles(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazdiffcomparefiles');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Domingo Galmés';
    P.Description:='File compare tool package for the lazarus IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lazdiff');
    D := P.Dependencies.Add('synedit');
    D := P.Dependencies.Add('ideintf');
    D := P.Dependencies.Add('lcl');
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
    T:=P.Targets.AddUnit('lazdiffcomparefiles.pas');
    t.Dependencies.AddUnit('lazdiffregisteride');
    t.Dependencies.AddUnit('lazdiffform');
    t.Dependencies.AddUnit('findreplacedialog2');
    t.Dependencies.AddUnit('lazdiffuiconsts');
    t.Dependencies.AddUnit('lazdiffutils');

    T:=P.Targets.AddUnit('lazdiffregisteride.pas');
    T:=P.Targets.AddUnit('lazdiffform.pas');
    T:=P.Targets.AddUnit('findreplacedialog2.pas');
    T:=P.Targets.AddUnit('lazdiffuiconsts.pas');
    T:=P.Targets.AddUnit('lazdiffutils.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('lazdiffcomparefiles.compiled');
    P.InstallFiles.Add('lazdiffcomparefiles.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_lazdiffcomparefiles('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
