unit modules.fmx.settingssaver;
interface
uses FMX.Forms;

// VERY SIMPLE unit which implements a class helper to save and restores
// settings

type TFormSaverHelper = class helper for TForm
  procedure SaveName(const TheValue: string);
  function LoadName: string;
  function LoadCheckbox: boolean;
  function LoadShowLogo: boolean;
  procedure SaveCheckbox(const TheValue: boolean);
  procedure SaveShowLogo(const TheValue: boolean);
  procedure SaveFormLoc;
  procedure LoadFormLoc;
end;

implementation
uses IniFiles, SysUtils, System.IOUtils;

const
  Key = 'memory';

function FormSaveName: string;
begin
  var tmpName: string := TPath.GetFileNameWithoutExtension(ParamStr(0));
  Result := TPath.GetHomePath + TPath.DirectorySeparatorChar + tmpName + TPath.DirectorySeparatorChar;
  ForceDirectories(Result);
  Result := TPath.ChangeExtension(Result + tmpName, '.ini');
end;

procedure TFormSaverHelper.SaveName(const TheValue: string);
var
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(FormSaveName);
  try
    Settings.WriteString(Key, 'Name', TheValue);
  finally
    Settings.Free;
  end;
end;

procedure TFormSaverHelper.SaveShowLogo(const TheValue: boolean);
var
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(FormSaveName);
  try
    Settings.WriteBool(Key, 'ShowLogo', TheValue);
  finally
    Settings.Free;
  end;
end;

function TFormSaverHelper.LoadName: string;
var
  Settings: TIniFile;
begin
  Result := '';
  Settings := TIniFile.Create(FormSaveName);
  try
    Result := Settings.ReadString(Key, 'Name', Result);
  finally
    Settings.Free;
  end;
end;


function TFormSaverHelper.LoadShowLogo: boolean;
var
  Settings: TIniFile;
begin
  Result := True;
  Settings := TIniFile.Create(FormSaveName);
  try
    Result := Settings.ReadBool(Key, 'ShowLogo', Result);
  finally
    Settings.Free;
  end;
end;

procedure TFormSaverHelper.SaveCheckbox(const TheValue: boolean);
var
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(FormSaveName);
  try
    Settings.WriteBool(Key, 'Checkbox', TheValue);
  finally
    Settings.Free;
  end;
end;

procedure TFormSaverHelper.SaveFormLoc;
var
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(FormSaveName);
  try
    Settings.WriteInteger(Key, 'Left', Self.Left);
    Settings.WriteInteger(Key, 'Top',  Self.Top);
  finally
    Settings.Free;
  end;
end;

function TFormSaverHelper.LoadCheckbox: boolean;
var
  Settings: TIniFile;
begin
  Result := True;
  Settings := TIniFile.Create(FormSaveName);
  try
    Result := Settings.ReadBool(Key, 'Checkbox', Result);
  finally
    Settings.Free;
  end;
end;

procedure TFormSaverHelper.LoadFormLoc;
var
  Settings: TIniFile;
  iLeft, iTop: integer;
begin
  Settings := TIniFile.Create(FormSaveName);
  try
    iLeft := Settings.ReadInteger(Key, 'Left', Self.Left);
    iTop := Settings.ReadInteger(Key, 'Top', Self.Top);
  finally
    Settings.Free;
  end;
  Self.SetBounds(iLeft, iTop, Self.Width, Self.Height);
end;
end.
