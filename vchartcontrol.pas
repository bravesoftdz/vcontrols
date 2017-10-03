{
 Author: Dmitry Vaygant
}
unit VChartControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LMessages, vtypes, Types;

const
  DefMaxScrollBar = 1024;
  DefMaxZoom = 10000;
  DefCursorMin = -1E10;

type

  TVChartDraw = procedure(Sender: TObject; ACanvas: TCanvas; const AxisRect, ImageRect: TRect) of object;
  TVChartCursorsDraw = procedure(Sender: TObject; ACanvas: TCanvas; ImageRect: TRect; aX, bX: integer) of object;
  TVChartZoomRectDraw = procedure(Sender: TObject; ACanvas: TCanvas; ImageRect, ZoomRect: TRect) of object;
  TVChartGetDragPoint = procedure(Sender: TObject; X, Y: integer; var N: integer) of object;
  TVChartMovePoint = procedure(Sender: TObject; Y: integer; N: integer) of object;
 {
  TVCustomDrawer  - общий предок
  TVChartDrawer
  TVCanvasDrawer
  TVBitmapDrawer
  TVPrintDrawer
 }

  { TVChartDrawer }
  TVChartControl = class;

  { TVCanvasChartDrawer }

  TVCanvasChartDrawer = class (TComponent)
  private
    FCursorAColor: TColor;
    FCursorBColor: TColor;
    FLineWidth: integer;
    procedure SetCursorAColor(AValue: TColor);
    procedure SetCursorBColor(AValue: TColor);
  protected
    procedure Changed; virtual;
    procedure DrawChart(aCanvas: TCanvas; aAxisRect, aImageRect: TRect); virtual; abstract;
    procedure DrawCursor(aCanvas: TCanvas; ImageRect: TRect; isCursorA: boolean; X: integer); virtual; abstract;
    procedure DrawCursors(aCanvas: TCanvas; ImageRect: TRect; aX, bX: integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CursorAColor: TColor read FCursorAColor write SetCursorAColor;
    property CursorBColor: TColor read FCursorBColor write SetCursorBColor;
  end;

  { TVScreenChartDrawer }

  TVScreenChartDrawer = class (TVCanvasChartDrawer)
  private
    FChart: TVChartControl;
  protected
    procedure Changed; override;
    procedure DrawCursors(aCanvas: TCanvas; ImageRect: TRect; aX, bX: integer); override;
    procedure AfterDrawChart(aCanvas: TCanvas; aAxisRect, aImageRect: TRect);
    procedure DrawZoomRect(aCanvas: TCanvas; ImageRect, ZoomRect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Chart: TVChartControl read FChart write FChart;
  end;

  { TVChartControl }

  TVChartControl = class(TCustomControl)
  private
    FAllowCursorA: Boolean;
    FAllowCursorB: Boolean;
    FChartDrawer: TVScreenChartDrawer;
    FDragMode: boolean;
    FDraggingPoint: integer;
    FOnAfterChartDraw: TVChartDraw;
    FOnChartDraw: TVChartDraw;
    FOnCursorAMove: TNotifyEvent;
    FOnCursorBMove: TNotifyEvent;
    FOnCursorsDraw: TVChartCursorsDraw;
    FOnGetDragPoint: TVChartGetDragPoint;
    FOnMovePoint: TVChartMovePoint;
    FOnZoomRectDraw: TVChartZoomRectDraw;
    NeedRender: boolean;
    IsZoomRect: boolean;
    FZoomRect: TRect;
    FAllowZoom: Boolean;
    FExtent: TDoubleRect;
    FBottomAxisHeight: integer;
    FLeftAxisWidth: integer;
    FZoomExtent: TDoubleRect;
    FSavedMouse: TPoint;
    FCurrentMouse: TPoint;
    FSavedExtentTopLeft: TDoublePoint;
    FBitmap: TBitmap;
    FImageRect: TRect;
    //FNavRect: TRect;
    FImageCursorA: integer;
    FImageCursorB: integer;
    FCursorA: double;
    FCursorB: double;

    FScale: TDoublePoint;

    procedure Calculate;
    procedure DoDrawBuffer;

    procedure DoSetZoomExtent(aLeft, aTop, aWidth, aHeight: double);
    procedure OnChangeFont(Sender: TObject);
    procedure SetBottomAxisHeight(AValue: integer);
    procedure SetChartDrawer(AValue: TVScreenChartDrawer);
    procedure SetCursorA(AValue: double);
    procedure SetCursorB(AValue: double);
    procedure SetDragMode(AValue: boolean);

    procedure SetExtent(AValue: TDoubleRect);
    procedure SetImageCursorA(AValue: integer);
    procedure SetImageCursorB(AValue: integer);
    procedure SetLeftAxisWidth(AValue: integer);
    procedure SetZoomExtent(AValue: TDoubleRect);
    procedure UpdateFromHorzScrollbarMsg(const Msg: TLMHScroll);
    procedure UpdateFromVertScrollbarMsg(const Msg: TLMVScroll);
    procedure UpdateScrollbar;

  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
    procedure Paint; override;
    procedure DoOnResize; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure LMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DoDrawZoomRect(aCanvas: TCanvas; ir, zr: TRect); virtual;
    procedure DoDrawChart(aCanvas: TCanvas; cr, ir: TRect); virtual;
    procedure DoAfterDrawChart(aCanvas: TCanvas; cr, ir: TRect); virtual;
    procedure DoDrawCursors(aCanvas: TCanvas; ir: TRect; aX, bX: integer); virtual;

    procedure DoMoveCursorA; virtual;
    procedure DoMoveCursorB; virtual;

    procedure DoGetDragPoint(X, Y: integer; var N: integer); virtual;
    procedure DoMovePoint(Y: integer; N: integer); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GraphToImage(ap: TDoublePoint): TPoint;     //convert graph coords to screen coords
    function GraphToImage(X, Y: double): TPoint;         //convert graph coords to screen coords
    function GraphToImageX(X: double): integer; inline;  //convert graph X coord to screen X coord
    function GraphToImageY(Y: double): integer; inline;  //convert graph Y coord to screen Y coord

    function ImageToGraph(pt: TPoint): TDoublePoint;     //convert screen coords to graph coords
    function ImageToGraph(X, Y: integer): TDoublePoint;  //convert screen coords to graph coords
    function ImageToGraphX(X: integer): double; inline;  //convert screen X coord to graph X coord
    function ImageToGraphY(Y: integer): double; inline;  //convert screen Y coord to graph Y coord

    function IsZoomed: boolean;

    procedure CancelZoom;
    procedure RenderChart;
    property Extent: TDoubleRect read FExtent write SetExtent;
    property ZoomExtent: TDoubleRect read FZoomExtent write SetZoomExtent;
    property XScale: double read FScale.X;
    property YScale: double read FScale.Y;
    property ImageCursorA: integer read FImageCursorA write SetImageCursorA;
    property ImageCursorB: integer read FImageCursorB write SetImageCursorB;
    property CursorA: double read FCursorA write SetCursorA;
    property CursorB: double read FCursorB write SetCursorB;
    property DragMode: boolean read FDragMode write SetDragMode;
  published
    property ChartDrawer: TVScreenChartDrawer read FChartDrawer write SetChartDrawer;
    property AllowZoom: Boolean read FAllowZoom write FAllowZoom default true;
    property AllowCursorA: Boolean read FAllowCursorA write FAllowCursorA default true;
    property AllowCursorB: Boolean read FAllowCursorB write FAllowCursorB default true;
//    property Navigator: Boolean read FNavigator write SetNavigator default true;

    property OnChartDraw: TVChartDraw read FOnChartDraw write FOnChartDraw;
    property OnAfterChartDraw: TVChartDraw read FOnAfterChartDraw write FOnAfterChartDraw;
    property OnCursorAMove: TNotifyEvent read FOnCursorAMove write FOnCursorAMove;
    property OnCursorBMove: TNotifyEvent read FOnCursorBMove write FOnCursorBMove;
    property OnCursorsDraw: TVChartCursorsDraw read FOnCursorsDraw write FOnCursorsDraw;
    property OnZoomRectDraw: TVChartZoomRectDraw read FOnZoomRectDraw write FOnZoomRectDraw;
    property OnGetDragPoint: TVChartGetDragPoint read FOnGetDragPoint write FOnGetDragPoint;
    property OnMovePoint: TVChartMovePoint read FOnMovePoint write FOnMovePoint;

    property LeftAxisWidth: integer read FLeftAxisWidth write SetLeftAxisWidth;
    property BottomAxisHeight: integer read FBottomAxisHeight write SetBottomAxisHeight;
    property Align;
    property BorderStyle;
    property BorderSpacing;
    property Color;
    property Font;
    property OnClick;
    property OnDblClick;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
  end;

procedure Register;

function RoundChecked(A: Double): Integer; inline;
function TruncChecked(A: Double): Integer; inline;

implementation

uses
  math, LCLType, LCLIntf;

procedure Register;
begin
  {$I vchartcontrol_icon.lrs}
  RegisterComponents('VControls',[TVChartControl]);
end;

function RoundChecked(A: Double): Integer;
begin
  Result := Round(EnsureRange(A, -MaxInt, MaxInt));
end;

function TruncChecked(A: Double): Integer;
begin
  Result := Trunc(EnsureRange(A, -MaxInt, MaxInt));
end;

{ TVScreenChartDrawer }

procedure TVScreenChartDrawer.Changed;
begin
  FChart.RenderChart;
end;

procedure TVScreenChartDrawer.DrawCursors(aCanvas: TCanvas; ImageRect: TRect;
  aX, bX: integer);
begin
  aCanvas.Pen.Width:=FLineWidth;
  aCanvas.Pen.Mode:=pmNotXor;
  if InRange(aX, ImageRect.Left, ImageRect.Right) then
  begin
    aCanvas.Pen.Color:=FCursorAColor;
    aCanvas.Line(aX, ImageRect.Top, aX, ImageRect.Bottom);
  end;
  if InRange(bX, ImageRect.Left, ImageRect.Right) then
  begin
    aCanvas.Pen.Color:=FCursorBColor;
    aCanvas.Line(bX, ImageRect.Top, bX, ImageRect.Bottom);
  end;
  ACanvas.Pen.Mode:=pmCopy;
end;

procedure TVScreenChartDrawer.AfterDrawChart(aCanvas: TCanvas; aAxisRect,
  aImageRect: TRect);
begin
  //
end;

procedure TVScreenChartDrawer.DrawZoomRect(aCanvas: TCanvas; ImageRect,
  ZoomRect: TRect);
begin
  ACanvas.Pen.Color:=clWindowFrame;
  ACanvas.Pen.Width:=FLineWidth;
  ACanvas.Frame(ZoomRect);
end;

constructor TVScreenChartDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineWidth:=ScaleX(1, 96);
end;

destructor TVScreenChartDrawer.Destroy;
begin
  inherited Destroy;
end;

{ TVCanvasChartDrawer }

procedure TVCanvasChartDrawer.SetCursorAColor(AValue: TColor);
begin
  if FCursorAColor=AValue then Exit;
  FCursorAColor:=AValue;
  Changed;
end;

procedure TVCanvasChartDrawer.SetCursorBColor(AValue: TColor);
begin
  if FCursorBColor=AValue then Exit;
  FCursorBColor:=AValue;
  Changed;
end;

procedure TVCanvasChartDrawer.Changed;
begin
  //
end;

constructor TVCanvasChartDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCursorAColor:=clRed;
  FCursorBColor:=clBlue;
  FLineWidth:=1;
end;

destructor TVCanvasChartDrawer.Destroy;
begin

  inherited Destroy;
end;


{ TVChartControl }

procedure TVChartControl.SetExtent(AValue: TDoubleRect);
begin
  AValue.NormalizeRect;
  if FExtent= AValue then Exit;
  FExtent:=AValue;
  FZoomExtent:= FExtent;
  RenderChart;
end;

procedure TVChartControl.SetImageCursorA(AValue: integer);
begin
  if FImageCursorA=AValue then Exit;
  FImageCursorA:=AValue;
  if AValue<FImageRect.Left then FCursorA:=DefCursorMin else FCursorA:=ImageToGraphX(AValue);
  DoDrawBuffer;
  DoMoveCursorA;
end;

procedure TVChartControl.SetImageCursorB(AValue: integer);
begin
  if FImageCursorB=AValue then Exit;
  FImageCursorB:=AValue;
  if AValue<FImageRect.Left then FCursorB:=DefCursorMin else FCursorB:=ImageToGraphX(AValue);
  DoDrawBuffer;
  DoMoveCursorB;
end;

procedure TVChartControl.SetLeftAxisWidth(AValue: integer);
begin
  AValue:= max(AValue, 0);
  if FLeftAxisWidth=AValue then Exit;
  FLeftAxisWidth:=AValue;
  RenderChart;
end;

procedure TVChartControl.SetZoomExtent(AValue: TDoubleRect);
begin
  AValue.NormalizeRect;
  if not AValue.IntersectsWith(FExtent) then
  begin
    CancelZoom;
    exit;
  end;
  if FZoomExtent=AValue then Exit;
  if ((FExtent.Width/AValue.Width)<DefMaxZoom) and ((FExtent.Height/AValue.Height)<DefMaxZoom) then FZoomExtent:=AValue;
  RenderChart;
end;

procedure TVChartControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

procedure TVChartControl.UpdateScrollbar;
var
  si: TScrollInfo;
  procedure DoUpdateScrollBar(aStyle, aMax, aPage, aPos: integer);
  begin
    si.nMax:=  aMax;
    si.nPage:=  aPage;
    si.nPos:= aPos;
    SetScrollInfo(Handle, aStyle, si, True);
  end;
var
  ky, kx: double;
begin
  if HandleAllocated then
  begin
    FillChar(si{%H-}, SizeOf(si), 0);
    si.cbSize:= SizeOf(si);
    si.fMask:= SIF_ALL;
    si.nMin:= 0;
    kx:=DefMaxScrollBar/FExtent.Width;
    DoUpdateScrollBar(SB_Horz, DefMaxScrollBar, RoundChecked(FZoomExtent.Width*kx), RoundChecked((FZoomExtent.Left- FExtent.Left)*kx));
    ky:= DefMaxScrollBar/FExtent.Height;
    DoUpdateScrollBar(SB_Vert, DefMaxScrollBar, RoundChecked(FZoomExtent.Height*ky), RoundChecked((FExtent.Bottom- FZoomExtent.Bottom )*ky));
  end;
end;

procedure TVChartControl.DoDrawBuffer;
begin
  Canvas.Draw(0,0, FBitmap);
  DoDrawCursors(Canvas, FImageRect, FImageCursorA, FImageCursorB);
  DoAfterDrawChart(Canvas, ClientRect, FImageRect);
  if IsZoomRect then DoDrawZoomRect(Canvas, FImageRect, FZoomRect);
end;

procedure TVChartControl.DoDrawZoomRect(aCanvas: TCanvas; ir, zr: TRect);
begin
  if Assigned(FChartDrawer) then FChartDrawer.DrawZoomRect(aCanvas, ir, zr)
  else if Assigned(FOnZoomRectDraw) then FOnZoomRectDraw(self, aCanvas, ir, zr);
end;

procedure TVChartControl.DoDrawChart(aCanvas: TCanvas; cr, ir: TRect);
begin
  if Assigned(FChartDrawer) then FChartDrawer.DrawChart(ACanvas, cr, ir)
  else if Assigned(FOnChartDraw) then FOnChartDraw(self, ACanvas, cr, ir);
end;

procedure TVChartControl.DoAfterDrawChart(aCanvas: TCanvas; cr, ir: TRect);
begin
  if Assigned(FChartDrawer) then FChartDrawer.AfterDrawChart(ACanvas, cr, ir)
  else if Assigned(FOnAfterChartDraw) then FOnAfterChartDraw(self, ACanvas, cr, ir);
end;

procedure TVChartControl.DoDrawCursors(aCanvas: TCanvas; ir: TRect; aX,
  bX: integer);
begin
  if Assigned(FChartDrawer) then FChartDrawer.DrawCursors(aCanvas, ir, aX, bX)
  else if Assigned(FOnCursorsDraw) then FOnCursorsDraw(self, aCanvas, ir, aX, bX);
end;

procedure TVChartControl.Paint;
var
  R: TRect;
begin
  if (csCreating in FControlState) then Exit;
  UpdateScrollbar;
  if NeedRender then
  begin
    Calculate;
    R:= ClientRect;
    if (FBitmap.Width<>R.Width) or (FBitmap.Height<>R.Height) then FBitmap.SetSize(R.Width, R.Height);
    FBitmap.Canvas.Font.Assign(Font);
    FBitmap.Canvas.Brush.Color:= Color;
    FBitmap.Canvas.FillRect(r);
    if (FImageRect.Width>1) and (FImageRect.Height>1) then DoDrawChart(FBitmap.Canvas, r, FImageRect);
    NeedRender:=false;
  end;
  DoDrawBuffer;
end;

procedure TVChartControl.DoOnResize;
begin
  inherited DoOnResize;
  NeedRender:=true;
  {$IFDEF LCLGtk2}
  Invalidate;
  {$ENDIF}
end;

procedure TVChartControl.Calculate;
begin
  FImageRect.Create(FLeftAxisWidth, 0, ClientWidth, ClientHeight- FBottomAxisHeight);
  FScale.X:= FImageRect.Width/FZoomExtent.Width;
  FScale.Y:= FImageRect.Height/FZoomExtent.Height;
  if (FCursorA<FZoomExtent.Left) or (FCursorA> FZoomExtent.Right) then FImageCursorA:=-1 else FImageCursorA:=GraphToImageX(FCursorA);
  if (FCursorB<FZoomExtent.Left) or (FCursorB> FZoomExtent.Right) then FImageCursorB:=-1 else FImageCursorB:=GraphToImageX(FCursorB);
end;

function TVChartControl.GraphToImage(ap: TDoublePoint): TPoint;
begin
  Result.x:= GraphToImageX(ap.X);
  Result.y:= GraphToImageY(ap.Y);
end;

function TVChartControl.GraphToImage(X, Y: double): TPoint;
begin
  Result.x:= GraphToImageX(X);
  Result.y:= GraphToImageY(Y);
end;

function TVChartControl.GraphToImageX(X: double): integer;
begin
  Result:= RoundChecked(FImageRect.Left + (X -FZoomExtent.Left)*FScale.X);
end;

function TVChartControl.GraphToImageY(Y: double): integer;
begin
  Result:= RoundChecked(FImageRect.Top+ (FZoomExtent.Bottom -Y)*FScale.Y);
end;

function TVChartControl.ImageToGraph(pt: TPoint): TDoublePoint;
begin
  Result.X:= ImageToGraphX(pt.X);
  Result.Y:= ImageToGraphY(pt.Y);
end;

function TVChartControl.ImageToGraph(X, Y: integer): TDoublePoint;
begin
  Result.X:= ImageToGraphX(X);
  Result.Y:= ImageToGraphY(Y);
end;

function TVChartControl.ImageToGraphX(X: integer): double;
begin
  Result:= FZoomExtent.Left + (X- FImageRect.Left)/FScale.X;
end;

function TVChartControl.ImageToGraphY(Y: integer): double;
begin
  Result:= FZoomExtent.Top + (FImageRect.Bottom - Y)/FScale.Y;
end;

function TVChartControl.IsZoomed: boolean;
begin
  Result:=(FZoomExtent<>FExtent);
end;

procedure TVChartControl.CancelZoom;
begin
  if IsZoomed then
  begin
    FZoomExtent:=FExtent;
    RenderChart;
  end;
end;

procedure TVChartControl.RenderChart;
begin
  NeedRender:=true;
  Changed;
  Invalidate;
end;

function TVChartControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  w, h, dw, dh, l, t: double;
begin
  Result:=true;
  w:=FZoomExtent.Width;
  h:=FZoomExtent.Height;
  dw:=12*w/WheelDelta;
  dh:=12*h/WheelDelta;
  if Shift = [ssShift] then //Horz shift
  begin
    l:= -dw;
    t:= 0;
  end
  else if Shift = [] then  //Vert shift
  begin
    l:= 0;
    t:= dh;
  end
  else if (Shift= [ssCtrl]) and FAllowZoom then  //Zoom
  begin
    MousePos.x-= FImageRect.Left;
    w:=min(w- dw, FExtent.Width);
    h:=min(h- dh, FExtent.Height);
    l:= dw* MousePos.x/FImageRect.Width;
    t:= dh* (FImageRect.Height-MousePos.y)/FImageRect.Height;
  end
  else exit;
  DoSetZoomExtent(FZoomExtent.Left + l, FZoomExtent.Top+ t, w, h);
end;

procedure TVChartControl.DoSetZoomExtent(aLeft, aTop, aWidth, aHeight: double);
begin
  if aLeft< FExtent.Left then aLeft:= FExtent.Left
  else if (aLeft+ aWidth)> FExtent.Right then aLeft:= FExtent.Right- aWidth;
  if aTop< FExtent.Top then aTop:= FExtent.Top
  else if (aTop+ aHeight)> FExtent.Bottom then aTop:= FExtent.Bottom- aHeight;
  SetZoomExtent(TDoubleRect.Create(aLeft, aTop, aLeft+ aWidth, aTop+ aHeight));
end;

procedure TVChartControl.SetBottomAxisHeight(AValue: integer);
begin
  AValue:= max(AValue, 0);
  if FBottomAxisHeight=AValue then Exit;
  FBottomAxisHeight:=AValue;
  RenderChart;
end;

procedure TVChartControl.SetChartDrawer(AValue: TVScreenChartDrawer);
begin
  if FChartDrawer=AValue then Exit;
  FChartDrawer:=AValue;
  RenderChart;
end;

procedure TVChartControl.SetCursorA(AValue: double);
begin
  if FCursorA=AValue then Exit;
  FCursorA:=AValue;
  FImageCursorA:=GraphToImageX(AValue);
  DoDrawBuffer;
end;

procedure TVChartControl.SetCursorB(AValue: double);
begin
  if FCursorB=AValue then Exit;
  FCursorB:=AValue;
  FImageCursorB:=GraphToImageX(AValue);
  DoDrawBuffer;
end;

procedure TVChartControl.SetDragMode(AValue: boolean);
begin
  if FDragMode=AValue then Exit;
  FDragMode:=AValue;
  RenderChart;
end;

procedure TVChartControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FSavedMouse:= Point(X, Y);
  if Shift=[ssMiddle] then
  begin
    Cursor:=crHandPoint;
    FSavedExtentTopLeft:=FZoomExtent.TopLeft;
    exit;
  end;
  if Shift = [ssLeft, ssCtrl] then
  begin
    IsZoomRect:=FAllowZoom and FImageRect.Contains(FSavedMouse);
    if IsZoomRect then FZoomRect.Create(FSavedMouse, 0, 0);
    exit;
  end;
  if FDragMode then
  begin
    if Shift = [ssLeft] then DoGetDragPoint(X, Y, FDraggingPoint);
    exit;
  end;
  if FAllowCursorA and (Shift=[ssLeft]) then SetImageCursorA(X) //cursor A
  else if FAllowCursorB and (Shift=[ssRight]) then SetImageCursorB(X); //cursor B
end;

procedure TVChartControl.DoMoveCursorA;
begin
  if Assigned(FOnCursorAMove) then FOnCursorAMove(self);
end;

procedure TVChartControl.DoMoveCursorB;
begin
  if Assigned(FOnCursorBMove) then FOnCursorBMove(self);
end;

procedure TVChartControl.DoGetDragPoint(X, Y: integer; var N: integer);
begin
  N:=-1;
  if Assigned(FOnGetDragPoint) then FOnGetDragPoint(self, X, Y, N);
end;

procedure TVChartControl.DoMovePoint(Y: integer; N: integer);
begin
  if Assigned(FOnMovePoint) then FOnMovePoint(self, Y, N);
end;

procedure TVChartControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  dp: TDoubleRect;
begin
  Cursor:=crDefault;
  FDraggingPoint:=-1;
  inherited MouseUp(Button, Shift, X, Y);
  if IsZoomRect then
  begin
    IsZoomRect:=false;
    if not (ssCtrl in Shift) then DoDrawBuffer
    else if FCurrentMouse.x> FSavedMouse.x then
    begin
      dp.Create(ImageToGraph(FSavedMouse), ImageToGraph(FCurrentMouse));
      SetZoomExtent(dp);
    end
    else
    begin
      if IsZoomed then CancelZoom
      else DoDrawBuffer;
    end;
  end;
end;

procedure TVChartControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ap: TDoublePoint;
begin
  inherited MouseMove(Shift, X, Y);
  FCurrentMouse:=Point(X, Y);
  if Shift=[ssMiddle] then
  begin  //move
    ap:=ImageToGraph(FSavedMouse) -  ImageToGraph(FCurrentMouse);
    DoSetZoomExtent(FSavedExtentTopLeft.X + ap.X, FSavedExtentTopLeft.Y + ap.Y, FZoomExtent.Width, FZoomExtent.Height);
    exit;
  end;
  if IsZoomRect then
  begin  //zoom rect
    IsZoomRect:= (Shift=[ssLeft, ssCtrl]);
    FZoomRect.Create(FSavedMouse, FCurrentMouse, true);
    FZoomRect.Intersect(FImageRect);
    DoDrawBuffer;
    exit;
  end;
  if FDragMode then
  begin  //drag point
    if FDraggingPoint>=0 then DoMovePoint(Y, FDraggingPoint);
    exit;
  end;
  if FAllowCursorA and (Shift=[ssLeft]) then SetImageCursorA(X) //cursor A
  else if FAllowCursorB and (Shift=[ssRight]) then SetImageCursorB(X); //cursor B
end;

procedure TVChartControl.UpdateFromVertScrollbarMsg(const Msg: TLMVScroll);
var
  h: double;
begin
  h:= FZoomExtent.Height;
  case Msg.ScrollCode of
     SB_BOTTOM:  begin
                   FZoomExtent.Top:= FExtent.Top;
                   FZoomExtent.Bottom:= min(FZoomExtent.Top + h, FExtent.Bottom);
                 end;
    SB_TOP:      begin
                   FZoomExtent.Top:= max(FExtent.Bottom - h, FExtent.Top);
                   FZoomExtent.Bottom:= FExtent.Bottom;
                 end;
    SB_LINEDOWN: begin
                   FZoomExtent.Top:= max(FExtent.Top, FZoomExtent.Top-h/10);
                   FZoomExtent.Bottom:= min(FZoomExtent.Top + h, FExtent.Bottom);
                 end;
     SB_LINEUP:begin
                   FZoomExtent.Bottom:= min(FExtent.Bottom, FZoomExtent.Bottom + h/10);
                   FZoomExtent.Top:= max(FZoomExtent.Bottom - h, FExtent.Top);
                 end;
    SB_PAGEDOWN: begin
                   FZoomExtent.Top:= max(FExtent.Top, FZoomExtent.Top-h);
                   FZoomExtent.Bottom:= min(FZoomExtent.Top + h, FExtent.Bottom);
                 end;
    SB_PAGEUP:   begin
                   FZoomExtent.Bottom:= min(FExtent.Bottom, FZoomExtent.Bottom + h);
                   FZoomExtent.Top:= max(FZoomExtent.Bottom - h, FExtent.Top);
                 end;
    SB_THUMBPOSITION, SB_THUMBTRACK:
                 begin
                  FZoomExtent.Bottom:= min(FExtent.Bottom - FExtent.Height*Msg.Pos/DefMaxScrollBar, FExtent.Bottom);
                  FZoomExtent.Top:= max(FZoomExtent.Bottom - h, FExtent.Top);
                 end;
  end;
end;

procedure TVChartControl.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromVertScrollbarMsg(Msg);
  RenderChart;
end;

procedure TVChartControl.UpdateFromHorzScrollbarMsg(const Msg: TLMHScroll);
var
  w: double;
begin
  w:= FZoomExtent.Width;
  case Msg.ScrollCode of
    SB_TOP:      begin
                   FZoomExtent.Left:= FExtent.Left;
                   FZoomExtent.Right:= min(FZoomExtent.Left + w, FExtent.Right);
                 end;
    SB_BOTTOM:   begin
                   FZoomExtent.Left:= max(FExtent.Right -w, FExtent.Left);
                   FZoomExtent.Right:= FExtent.Right;
                 end;
    SB_LINEUP:   begin
                   FZoomExtent.Left:= max(FExtent.Left, FZoomExtent.Left- w/10);
                   FZoomExtent.Right:= min(FZoomExtent.Left + w, FExtent.Right);
                 end;
    SB_LINEDOWN: begin
                   FZoomExtent.Right:= min(FExtent.Right, FZoomExtent.Right + w/10);
                   FZoomExtent.Left:= max(FZoomExtent.Right - w, FExtent.Left);
                 end;
    SB_PAGEUP:   begin
                   FZoomExtent.Left:= max(FExtent.Left, FZoomExtent.Left-w);
                   FZoomExtent.Right:= min(FZoomExtent.Left + w, FExtent.Right);
                 end;
    SB_PAGEDOWN: begin
                   FZoomExtent.Right:= min(FExtent.Right, FZoomExtent.Right + w);
                   FZoomExtent.Left:= max(FZoomExtent.Right - w, FExtent.Left);
                 end;
    SB_THUMBPOSITION, SB_THUMBTRACK:
                 begin
                   FZoomExtent.Left:= max(FExtent.Left+ FExtent.Width*Msg.Pos/DefMaxScrollBar, FExtent.Left);
                   FZoomExtent.Right:= min(FZoomExtent.Left + w, FExtent.Right);
                 end;
  end;
end;

procedure TVChartControl.LMHScroll(var Msg: TLMHScroll);
begin
  UpdateFromHorzScrollbarMsg(Msg);
  RenderChart;
end;

procedure TVChartControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

constructor TVChartControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDragMode:= false;
  FDraggingPoint:=-1;
  FBitmap:=TBitmap.Create;
  IsZoomRect:=false;
  FBottomAxisHeight:=0;
  FLeftAxisWidth:=0;
  FAllowZoom:=true;
  FAllowCursorA:=true;
  FAllowCursorB:=true;
  FExtent.Create(-1, 1, 1, -1);
  FZoomExtent:=FExtent;
  ControlStyle:= ControlStyle+[csOpaque]-[csTripleClicks];
  Color:= clWhite;
  FCursorA:=DefCursorMin;
  FCursorB:=DefCursorMin;
  SetInitialBounds(0, 0, 150, 180);
  Font.OnChange:=@OnChangeFont;
end;

destructor TVChartControl.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TVChartControl.OnChangeFont(Sender: TObject);
begin
  Canvas.Font.Assign(Font);
end;

end.
