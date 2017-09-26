unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, Types, LazFileUtils,
  VChartControl, vtypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCloseAll2: TButton;
    ButtonZoomIn: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    VChartControl1: TVChartControl;
    procedure ButtonZoomInClick(Sender: TObject);
    procedure ButtonCloseAll2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VChartControl1ChartDraw(Sender: TObject; C: TCanvas;
      const AxisRect, ImageRect: TRect);
    procedure VChartControl1CursorDraw(Sender: TObject; ACanvas: TCanvas;
      ImageRect: TRect; isCursorA: boolean; X: integer);
    procedure VChartControl1ZoomRectDraw(Sender: TObject; ACanvas: TCanvas;
      const ZoomRect: TRect);
  private
    { private declarations }
  public
    da: array of double;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses FileUtil, LCLIntf;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonZoomInClick(Sender: TObject);
var
  dr: TDoubleRect;
begin
  dr:= VChartControl1.ZoomExtent;
  dr.Inflate(-dr.Width/10, -dr.Height/10);
  VChartControl1.ZoomExtent:=dr;
end;

procedure TForm1.ButtonCloseAll2Click(Sender: TObject);
begin
  VChartControl1.CancelZoom;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  VChartControl1.Extent:=TDoubleRect.Create(-pi, -1.2, pi, 1.2);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  VChartControl1.RenderChart;
end;

procedure TForm1.VChartControl1ChartDraw(Sender: TObject; C: TCanvas;
  const AxisRect, ImageRect: TRect);

var
  xx, yy: double;
  X, Y: integer;
  n: integer;
  arp: array of TPoint;

const pxl = 2;

procedure DrawSin;
var
  i: integer;
begin
  C.Pen.Color:=clBlue;
  C.Pen.Width:=2;
  n:= 1+ ImageRect.Width div pxl;
  SetLength(arp, n);
  X:=ImageRect.Left;
  for i:=0 to n-1 do
  begin
    xx:=VChartControl1.ImageToGraphX(X);
    yy:= sin(xx);
    arp[i].x:=X;
    arp[i].y:=VChartControl1.GraphToImageY(yy);
    X+=pxl;
  end;
  C.Polyline(arp);
end;

procedure DrawCos;
var
  i: integer;
begin
  C.Pen.Color:=clLime;
  C.Pen.Width:=2;
  n:= 1+ImageRect.Width div pxl;
  SetLength(arp, n);
  X:=ImageRect.Left;
  for i:=0 to n-1 do
  begin
    xx:=VChartControl1.ImageToGraphX(X);
    yy:=cos(xx);
    arp[i].x:=X;
    arp[i].y:=VChartControl1.GraphToImageY(yy);
    X+=pxl;
  end;
  C.Polyline(arp);
  end;

procedure DrawAxis;
begin
  C.Pen.Color:=clSilver;
  C.Pen.Width:=1;
  Y:= VChartControl1.GraphToImageY(0);
  if (Y>=ImageRect.Top) and (Y<ImageRect.Bottom) then C.Line(ImageRect.Left, Y, ImageRect.Right, Y);
  X:= VChartControl1.GraphToImageX(0);
  if (X>=ImageRect.Left) and (X<ImageRect.Right) then C.Line(X, ImageRect.Top, X, ImageRect.Bottom);
end;

begin
  C.ClipRect:=ImageRect;
  C.Clipping:=true;
  DrawAxis;
  DrawCos;
  DrawSin;
end;

procedure TForm1.VChartControl1CursorDraw(Sender: TObject; ACanvas: TCanvas;
  ImageRect: TRect; isCursorA: boolean; X: integer);
var
  Y: integer;
begin
  ACanvas.Pen.Width:=1;
  if isCursorA then
  begin
    ACanvas.Pen.Color:=clMaroon;
    ACanvas.Brush.Style:=bsClear;
    Y:=VChartControl1.GraphToImageY(sin(VChartControl1.CursorA));
    ACanvas.Rectangle(X-5, Y-5, X+6, Y+6);
    Y:=VChartControl1.GraphToImageY(cos(VChartControl1.CursorA));
    ACanvas.Rectangle(X-5, Y-5, X+6, Y+6);
    ACanvas.Pen.Color:=clRed;
  end
  else ACanvas.Pen.Color:=clBlue;
  ACanvas.Pen.Mode:=pmNotXor;
  ACanvas.Line(X, ImageRect.Top, X, ImageRect.Bottom);
  ACanvas.Pen.Mode:=pmCopy;
end;

procedure TForm1.VChartControl1ZoomRectDraw(Sender: TObject; ACanvas: TCanvas;
  const ZoomRect: TRect);
begin
  ACanvas.Pen.Color:=clWindowFrame;
  ACanvas.Pen.Width:=1;
  ACanvas.Frame(ZoomRect);
end;

end.

