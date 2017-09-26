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
 // TCursorType = (ctLeft, ctRight, ctAll);
  TVChartDraw = procedure(Sender: TObject; ACanvas: TCanvas; const AxisRect, ImageRect: TRect) of object;
  TVChartZoomRectDraw = procedure(Sender: TObject; ACanvas: TCanvas; const ZoomRect: TRect) of object;
  TVChartCursorDraw = procedure(Sender: TObject; ACanvas: TCanvas; ImageRect: TRect; isCursorA: boolean; X: integer) of object;
  TVChartCursorMove = procedure(Sender: TObject; isCursorA: boolean) of object;

  { TVChartControl }

  TVChartControl = class(TCustomControl)
  private
    FAllowCursorA: Boolean;
    FAllowCursorB: Boolean;
    FOnAfterChartDraw: TVChartDraw;
    FOnChartDraw: TVChartDraw;
    FOnCursorDraw: TVChartCursorDraw;
    FOnCursorMove: TVChartCursorMove;
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

    procedure DoSetCursor(isCursorA: boolean; X: Integer);
    procedure DoSetZoomExtent(aLeft, aTop, aWidth, aHeight: double);
    procedure OnChangeFont(Sender: TObject);
    procedure SetBottomAxisHeight(AValue: integer);
    procedure SetCursorA(AValue: double);
    procedure SetCursorB(AValue: double);

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

    procedure DoOnDrawZoomRect(aCanvas: TCanvas; r: TRect); virtual;
    procedure DoDrawChart(aCanvas: TCanvas; r: TRect); virtual;
    procedure DoAfterDrawChart(aCanvas: TCanvas; r: TRect); virtual;
    procedure DoDrawCursor(isCursorA: boolean; X: integer); virtual;
    procedure DoMoveCursor(isCursorA: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GraphToImage(ap: TDoublePoint): TPoint;    //convert graph coords to screen coords
    function GraphToImage(X, Y: double): TPoint;        //convert graph coords to screen coords
    function GraphToImageX(X: double): integer; inline; //convert graph X coord to screen X coord
    function GraphToImageY(Y: double): integer; inline; //convert graph Y coord to screen Y coord

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
  published
    property AllowZoom: Boolean read FAllowZoom write FAllowZoom default true;
    property AllowCursorA: Boolean read FAllowCursorA write FAllowCursorA default true;
    property AllowCursorB: Boolean read FAllowCursorB write FAllowCursorB default true;
//    property Navigator: Boolean read FNavigator write SetNavigator default true;
    property OnChartDraw: TVChartDraw read FOnChartDraw write FOnChartDraw;
    property OnAfterChartDraw: TVChartDraw read FOnAfterChartDraw write FOnAfterChartDraw;
    property OnZoomRectDraw: TVChartZoomRectDraw read FOnZoomRectDraw write FOnZoomRectDraw;
    property OnCursorDraw: TVChartCursorDraw read FOnCursorDraw write FOnCursorDraw;
    property OnCursorMove: TVChartCursorMove read FOnCursorMove write FOnCursorMove;

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
  FCursorA:=ImageToGraphX(AValue);
  Changed;
  Invalidate;
end;

procedure TVChartControl.SetImageCursorB(AValue: integer);
begin
  if FImageCursorB=AValue then Exit;
  FImageCursorB:=AValue;
  FCursorB:=ImageToGraphX(AValue);
  Changed;
  Invalidate;
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
  if (FImageCursorA>=FImageRect.Left) and (FImageCursorA<FImageRect.Right) then DoDrawCursor(true, FImageCursorA);
  if (FImageCursorB>=FImageRect.Left) and (FImageCursorB<FImageRect.Right) then DoDrawCursor(false, FImageCursorB);
  DoAfterDrawChart(Canvas, ClientRect);
  if IsZoomRect then DoOnDrawZoomRect(Canvas, FZoomRect);
end;

procedure TVChartControl.DoDrawChart(aCanvas: TCanvas; r: TRect);
begin
  if Assigned(FOnChartDraw) then FOnChartDraw(self, ACanvas, r, FImageRect);
end;

procedure TVChartControl.DoAfterDrawChart(aCanvas: TCanvas; r: TRect);
begin
  if Assigned(FOnAfterChartDraw) then FOnAfterChartDraw(self, ACanvas, r, FImageRect);
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
    if (FImageRect.Width>1) and (FImageRect.Height>1) then DoDrawChart(FBitmap.Canvas, r);
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
 // Result:= RoundChecked(FImageRect.Top+ (Y -FZoomExtent.Top)*FScale.Y);
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

procedure TVChartControl.SetCursorA(AValue: double);
begin
  if FCursorA=AValue then Exit;
  FCursorA:=AValue;
  RenderChart;
end;

procedure TVChartControl.SetCursorB(AValue: double);
begin
  if FCursorB=AValue then Exit;
  FCursorB:=AValue;
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
  if FAllowCursorA and (Shift=[ssLeft]) then DoSetCursor(true, X) //cursor A
  else if FAllowCursorB and (Shift=[ssRight]) then DoSetCursor(false, X); //cursor B
end;

procedure TVChartControl.DoSetCursor(isCursorA: boolean; X: Integer);
var
  d: double;
begin
  if X<FImageRect.Left then d:=DefCursorMin else d:= ImageToGraphX(X);
  if isCursorA then
  begin
    FImageCursorA:=X;
    FCursorA:=d;
  end
  else
  begin
    FImageCursorB:=X;
    FCursorB:=d;
  end;
  DoMoveCursor(isCursorA);
  DoDrawBuffer;
end;

procedure TVChartControl.DoDrawCursor(isCursorA: boolean; X: integer);
begin
  if Assigned(FOnCursorDraw) then FOnCursorDraw(self, Canvas, FImageRect, isCursorA, X);
end;

procedure TVChartControl.DoMoveCursor(isCursorA: boolean);
begin
  if Assigned(FOnCursorMove) then FOnCursorMove(self, isCursorA);
end;

procedure TVChartControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  dp: TDoubleRect;
begin
  Cursor:=crDefault;
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

procedure TVChartControl.DoOnDrawZoomRect(aCanvas: TCanvas; r: TRect);
begin
  if Assigned(FOnZoomRectDraw) then FOnZoomRectDraw(self, aCanvas, r);
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
  if FAllowCursorA and (Shift=[ssLeft]) then DoSetCursor(true, X) //cursor A
  else if FAllowCursorB and (Shift=[ssRight]) then DoSetCursor(false, X); //cursor B
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
  FBitmap:=TBitmap.Create;
  IsZoomRect:=false;
  FBottomAxisHeight:=0;
  FLeftAxisWidth:=0;
  FAllowZoom:=true;
  FAllowCursorA:=true;
  FAllowCursorB:=true;
  FExtent.Create(-1,-1, 1, 1);
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
