unit Unit26;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, ADODB, ComObj;

type
  TForm26 = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    ADOConnection1: TADOConnection;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Edit4: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Edit7: TEdit;
    Label7: TLabel;
    ADOQuery1: TADOQuery;
    ComboBox1: TComboBox;
    SaveDialog1: TSaveDialog;
    Label2: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Path : String;
    CN : String;
  end;

var
  Form26: TForm26;

implementation

{$R *.dfm}

procedure TForm26.Button1Click(Sender: TObject);
var
  SL : TStringList;
begin
  if (Edit5.Text <> Edit6.Text) or (Edit5.Text = '') then
  begin
    Application.MessageBox('Пароли не совпадают', 'Ошибка');
    Exit;
  end;
  if Edit4.Text  = '' then
  begin
    Application.MessageBox('Выберите имя супер-пользователя', 'Ошибка');
    Exit;
  end;
  if Edit7.Text  = '' then
  begin
    Application.MessageBox('Не указан файл базы данных', 'Ошибка');
    Exit;
  end;
  if Edit3.Text  = '' then
  begin
    Application.MessageBox('Не указан пароль пользователя sa', 'Ошибка');
    Exit;
  end;
  try
  Label11.Visible := True;
  SL := TStringList.Create;
  SL.LoadFromFile(Path + 'setup1.script');
  SL.Text := StringReplace(SL.Text, '%path%', Edit7.Text, [rfReplaceAll]);
  SL.Text := StringReplace(SL.Text, '%user%', Edit4.Text, [rfReplaceAll]);
  SL.Text := StringReplace(SL.Text, '%password%', Edit5.Text, [rfReplaceAll]);
  ADOConnection1.Close;
  ADOConnection1.ConnectionString := Format('Provider=SQLOLEDB.1;Password=%s;Persist Security Info=True;User ID=sa;Initial Catalog=master;Data Source=%s', [Edit3.Text,ComboBox1.Text]);
  ADOQuery1.Connection := ADOConnection1;
  ADOConnection1.ConnectionTimeout := 100000;
  ADOQuery1.SQL.Text := SL.Text;
//  SL.SaveToFile('a.sql');
  SL.Free;
  ADOQuery1.ExecSQL;
  ADOQuery1.Close;
  SL := TStringList.Create;
  SL.LoadFromFile(Path + 'setup2.script');
  SL.Text := StringReplace(SL.Text, '%path%', Edit7.Text, [rfReplaceAll]);
  SL.Text := StringReplace(SL.Text, '%user%', Edit4.Text, [rfReplaceAll]);
  SL.Text := StringReplace(SL.Text, '%password%', Edit5.Text, [rfReplaceAll]);
  ADOConnection1.Close;
  ADOConnection1.ConnectionString := Format('Provider=SQLOLEDB.1;Password=%s;Persist Security Info=True;User ID=sa;Initial Catalog=master;Data Source=%s', [Edit3.Text,ComboBox1.Text]);
  ADOQuery1.Connection := ADOConnection1;
  ADOConnection1.ConnectionTimeout := 100000;
  ADOQuery1.SQL.Text := SL.Text;
//  SL.SaveToFile('a.sql');
  SL.Free;
  ADOQuery1.ExecSQL;
  ADOQuery1.Close;
  SL := TStringList.Create;
  SL.Add('DataSource=' + ComboBox1.Text);
  SL.Add('InitialCatalog=БД Заказов');
  SL.Add('DefaultTemplate=Orders.xls');
  SaveDialog1.FileName := 'database.ini';
  if SaveDialog1.Execute then
      SL.SaveToFile(SaveDialog1.FileName);
  Label11.Caption := 'Установка завершена';
  except
    Label11.Visible := False;
    Application.MessageBox('Проверьте правильность ввода параеметров и повторите', 'Ошибка при устновке');
    ADOQuery1.Close;
  end;
end;

procedure TForm26.Button2Click(Sender: TObject);
begin
  if Self.OpenDialog1.Execute then
      Edit7.Text := OpenDialog1.FileName;
end;


function EnumSqlServers(AStrings : TStrings) : boolean;
var oDmo,oApp,oServers : OleVariant;
    bResult : boolean;
    i : integer;
begin
  AStrings.Clear;

  try
    oDMO := CreateOleObject('SQLDMO.SQLServer');
    oApp := oDMO.Application;
    oServers := oApp.ListAvailableSQLServers;

    try
      AStrings.BeginUpdate;
      for i := 1 to oServers.Count do
        AStrings.Add(oServers.Item(i));
    finally
      AStrings.EndUpdate;
    end;

    bResult := true;
  except
    bResult := false;
  end;

  oServers := Unassigned;
  oApp := Unassigned;
  oDMO := Unassigned;

  Result := bResult;
end;

procedure TForm26.ComboBox1Enter(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  try
    EnumSqlServers(ComboBox1.Items);
  finally
  end;
  ComboBox1.Items.Insert(0, CN);
end;

procedure TForm26.FormCreate(Sender: TObject);
var
  Size : LongWord;
begin
  SetLength(CN, 256);
  Size := Length(CN);
  GetComputerNameW(@CN[1], Size);
  Path := ExtractFilePath(ParamStr(0));
end;

end.
