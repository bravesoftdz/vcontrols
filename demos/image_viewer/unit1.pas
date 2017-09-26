unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, ExtDlgs, VListBox, Types, LazFileUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ButtonCloseAll: TButton;
    IdleTimer1: TIdleTimer;
    Image1: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter2: TSplitter;
    VListBox1: TVListBox;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCloseAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure VListBox1DblClick(Sender: TObject);
    procedure VListBox1DrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure VListBox1Selection(Sender: TObject);
  private
    Folder: string;
    SL: TStringList;
    aTextHeight: integer;
    procedure CalculateMinWidth;
    procedure DoOpenFolder(fn: string);
    procedure OpenFile(fn: string);
    procedure OpenFolder(fn: string);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
const
  Padding = 3;

implementation

uses FileUtil,  math, LCLIntf;

{$R *.lfm}

{ TForm1 }

procedure TForm1.VListBox1DrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  im, x: integer;
  fn, xt: string;
  is_dir: boolean;
begin
  if AIndex=0 then
  begin
    fn:='...';
    is_dir:=true;
    xt:='Parent folder';
  end
  else
  begin
    fn:= SL[AIndex-1];
    is_dir:= DirectoryExistsUTF8(fn);
    xt:='Folder';
  end;
  if is_dir then im:=4
  else
  begin
    xt:=LowerCase(ExtractFileExt(fn));
    case xt of
      '.jpg', '.jpeg': im:= 0;
      '.png': im:= 1;
      '.gif': im:= 2;
      else im:= 3;
    end;
    xt:='Size: '+IntToStr(FileSizeUtf8(fn))+' bytes';
  end;
  x:= ARect.Left + Padding;
  ImageList2.Draw(C, x, ARect.Top + (VListBox1.ItemHeight - ImageList2.Height) div 2, im);
  x:=x+ Padding + ImageList2.Width;
  C.TextOut(x, ARect.Top +Padding, fn);
  C.TextOut(x, ARect.Top+ 2*Padding+ aTextHeight, xt);
end;

procedure TForm1.VListBox1Selection(Sender: TObject);
begin
  IdleTimer1.Enabled:=false;
  IdleTimer1.Enabled:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then OpenFile(OpenDialog1.FileName);
end;

procedure TForm1.ButtonCloseAllClick(Sender: TObject);
begin
  Folder:='';
  SL.Clear;
  CalculateMinWidth;
  VListBox1.ItemCount:=0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SL:= TStringList.Create;
  VListBox1Selection(Sender);
  aTextHeight:=VListBox1.Canvas.TextHeight('Qq');
  VListBox1.ItemHeight:=max(2*aTextHeight, ImageList1.Height) + 3*Padding;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SL.Free;
end;

procedure TForm1.IdleTimer1Timer(Sender: TObject);
var
  n: integer;
  fn: string;
begin
  IdleTimer1.Enabled:=false;
  n:= VListBox1.ItemIndex;
  if n<0 then
  begin
    Label1.Caption:='No selected file';
    Image1.Picture.Clear;
    exit;
  end;
  if n=0 then
  begin
    Label1.Caption:='Current folder: '+Folder;
    Image1.Picture.Clear;
    exit;
  end;
  fn:=SL[n-1];
  if DirectoryExistsUTF8(fn) then
  begin
    Label1.Caption:='Selected folder: '+fn;
    Image1.Picture.Clear;
    exit;
  end;
  Label1.Caption:='Selected file: '+fn;
  Image1.Picture.LoadFromFile(fn);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  k: integer;
  s: string;
begin
  k:= VListBox1.ItemIndex -1;
  if k<0 then s:='Parent folder'
  else
  begin
    s:=SL[k];
    if DirectoryExistsUTF8(s) then s:='Folder: '+s else s:='File: '+s;
  end;
  ShowMessage(s);
end;

procedure TForm1.VListBox1DblClick(Sender: TObject);
var
  n: integer;
  fn: string;
begin
  n:= VListBox1.ItemIndex;
  if n=0 then fn := ExtractFilePath(ExcludeTrailingPathDelimiter(Folder))
  else fn:=SL[n-1];
  if DirectoryExistsUTF8(fn) then OpenFolder(fn)
  else LCLIntf.OpenDocument(fn);
end;

procedure TForm1.OpenFile(fn: string);
begin
  DoOpenFolder(ExtractFilePath(fn));
  VListBox1.ItemIndex:=SL.IndexOf(fn)+1;
end;

procedure TForm1.OpenFolder(fn: string);
begin
  DoOpenFolder(fn);
  VListBox1.ItemIndex:=0;
end;

procedure TForm1.DoOpenFolder(fn: string);
var
  n: integer;
begin
  SL.Clear;
  Folder:= fn;
  if DirectoryExistsUTF8(Folder) then
  begin
    FindAllDirectories(SL, Folder, false);
    FindAllFiles(SL, Folder, GraphicFileMask(TGraphic), false);
  end;
  n:= SL.Count;
  VListBox1.ItemCount:=n+1;
  CalculateMinWidth;
end;

procedure TForm1.CalculateMinWidth;
var
  i, w: integer;
begin
  w:=0;
  for i:=0 to SL.Count-1 do w:=max(w, VListBox1.Canvas.TextWidth(SL[i]));
  if w>0 then w:=w+ ImageList2.Width+ 3*Padding;
  VListBox1.ItemMinWidth:=w;
end;

end.

