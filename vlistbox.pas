{
 Author: Dmitry Vaygant
}
unit VListBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls,
  Menus, Types, LMessages,
  Graphics, Dialogs;

type
  TMouseWheelOption = (mwItems, mwList, mwNone);
  TOnVListBoxDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas; AIndex: integer; const ARect: TRect) of object;

const
  DefItemHeight = 48;

type
  //todo: goSmoothScroll, header, headerpopupmenu

  { TVListBox }

  TVListBox = class(TCustomControl)
  private
    //FHeaderPopupMenu: TPopupMenu;
    FItemPopupMenu: TPopupMenu;
    FMouseWheelOption: TMouseWheelOption;
    FOnSelection: TNotifyEvent;
    FSelectedColor: TColor;
    FShiftX: integer;
    FItemMinWidth: integer;
    FOnDrawItem: TOnVListBoxDrawItemEvent;
    FItemCount,
    FItemIndex,
    FItemHeight,
    FItemTop: integer;
    FBitmap: TBitmap;
    procedure DoPaintTo(C: TCanvas; r: TRect);

    function ItemBottom: integer;
    function MaxItemTop: integer;
    procedure OnChangeFont(Sender: TObject);
    procedure RedrawItem(Index: integer; aColor: TColor);
    procedure SetItemCount(AValue: integer);
    procedure SetItemHeight(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemMinWidth(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure SetSelectedColor(AValue: TColor);
    procedure UpdateFromVertScrollbarMsg(const Msg: TLMVScroll);
    procedure UpdateFromHorzScrollbarMsg(const Msg: TLMHScroll);
    procedure UpdateScrollbar;
    function GetVisibleItems: integer;
    function IsIndexValid(N: integer): boolean;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
    procedure Paint; override;
    procedure DoOnResize; override;
    function DoMouseWheel({%H-}Shift: TShiftState; WheelDelta: Integer;
      {%H-}MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure LMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemTop: integer read FItemTop write SetItemTop;
    property ItemCount: integer read FItemCount write SetItemCount;
    property VisibleItems: integer read GetVisibleItems;
  published
    property MouseWheelOption: TMouseWheelOption read FMouseWheelOption write FMouseWheelOption default mwItems;
    property ItemPopupMenu: TPopupMenu read FItemPopupMenu write FItemPopupMenu;
//    property HeaderPopupMenu: TPopupMenu read FHeaderPopupMenu write FHeaderPopupMenu;
    property Align;
    property BorderStyle;
    property BorderSpacing;
    property Color;
    property DoubleBuffered;
    property Font;
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property ItemMinWidth: integer read FItemMinWidth write SetItemMinWidth default 0;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property OnClick;
    property OnDblClick;
    property OnDrawItem: TOnVListBoxDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnSelection: TNotifyEvent read FOnSelection write FOnSelection;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
  end;

procedure Register;

implementation

uses
  Math, LCLType, LCLIntf;

procedure Register;
begin
  {$I vlistbox_icon.lrs}
  RegisterComponents('VControls',[TVListBox]);
end;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef windows}
    Result:= true;
  {$else}
    Result:= false;
  {$endif}
end;

{ TVListBox }

procedure TVListBox.RedrawItem(Index: integer; aColor: TColor);

procedure DoRedraw(C: TCanvas; aRect: TRect);
begin
  C.Brush.Color:= aColor;
  C.FillRect(aRect);
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, C, Index, aRect);
end;

var
  r: TRect;
  BMP: TBitmap;
begin
  r.Top:= (Index- FItemTop)*FItemHeight;
  if r.Top>=ClientHeight then exit;
  r.Bottom:= r.Top+ FItemHeight;
  if r.Bottom<0 then exit;
  r.Left:= - FShiftX;
  r.Right:= max(r.Left + FItemMinWidth, ClientWidth);
  if DoubleBuffered then
  begin
    BMP:=TBitmap.Create;
    try
      BMP.SetSize(r.Width, r.Height);
      BMP.Canvas.Font.Assign(Font);
      DoRedraw(BMP.Canvas, Rect(0, 0, BMP.Width, BMP.Height));
      Canvas.Draw(r.Left, r.Top, BMP);
    finally
      BMP.Free;
    end;
  end
  else DoRedraw(BMP.Canvas, r);
end;

procedure TVListBox.DoPaintTo(C: TCanvas; r: TRect);
var
  Index: integer;
begin
  C.Brush.Color:= Color;
  C.FillRect(r);
  for Index:= FItemTop to FItemCount-1 do
  begin
    r.Top:= (Index- FItemTop)*FItemHeight;
    if r.Top>=ClientHeight then Break;
    r.Bottom:= r.Top+ FItemHeight;
    r.Left:= - FShiftX;
    r.Right:= max(r.Left + FItemMinWidth, ClientWidth);
    if Index=  FItemIndex then
    begin
      C.Brush.Color:=  FSelectedColor;
      C.FillRect(r);
    end else C.Brush.Color:= Color;
    if Assigned(FOnDrawItem) then FOnDrawItem(Self, C, Index, r);
  end;
end;

function TVListBox.ItemBottom: integer;
var
  n: integer;
begin
  n:= GetVisibleItems;
  Result:= Min(FItemCount-1, FItemTop+ n-1);
end;

function TVListBox.MaxItemTop: integer;
begin
  Result:= max(0, FItemCount - (ClientHeight div FItemHeight));
end;

procedure TVListBox.SetItemCount(AValue: integer);
begin
  if FItemCount=AValue then Exit;
  if AValue<0 then AValue:=0;
  FItemCount:= AValue;
  FItemIndex:= -1;
  PopupMenu:=nil;
  Changed;
  Invalidate;
  if Assigned(FOnSelection) then FOnSelection(self);
end;

procedure TVListBox.SetItemHeight(AValue: integer);
begin
  if FItemHeight=AValue then Exit;
  FItemHeight:=AValue;
  Changed;
  Invalidate;
end;

procedure TVListBox.SetItemIndex(AValue: integer);
var
  ib, gvi: integer;
begin
  if not IsIndexValid(AValue) then AValue:=-1;
  if AValue>=0 then PopupMenu:=FItemPopupMenu else PopupMenu:=nil;
  if FItemIndex=AValue then Exit;
  ib:= ItemBottom;
  gvi:= GetVisibleItems;
  ib:= FItemTop+ gvi -1;
  if (AValue>=0) and ((AValue< FItemTop)  or (AValue>ib)) then
  begin
    if AValue<FItemTop then FItemTop:= max(0, AValue)
    else if AValue>ib then FItemTop:= Min(MaxItemTop, max(AValue- gvi+1, 0));
    FItemIndex:= AValue;
    Changed;
    Invalidate;
  end
  else
  begin
    if FItemIndex>=0 then RedrawItem(FItemIndex, Color);
    if AValue>=0 then RedrawItem(AValue, FSelectedColor);
    FItemIndex:= AValue;
  end;
  if Assigned(FOnSelection) then FOnSelection(self);
end;

procedure TVListBox.SetItemMinWidth(AValue: integer);
var
  NewWidth: integer;
begin
  NewWidth:=max(0, AValue);
  if FItemMinWidth= NewWidth then Exit;
  FItemMinWidth:=NewWidth;
  Changed;
  Invalidate;
end;

procedure TVListBox.SetItemTop(AValue: integer);
var
  NewItemTop: integer;
begin
  NewItemTop:= Min(MaxItemTop, AValue);
  if FItemTop=NewItemTop then Exit;
  FItemTop:= NewItemTop;
  Changed;
  Invalidate;
end;

procedure TVListBox.SetSelectedColor(AValue: TColor);
begin
  if FSelectedColor=AValue then Exit;
  FSelectedColor:=AValue;
  if FItemCount<0 then exit;
  Changed;
  Invalidate;
end;

procedure TVListBox.UpdateFromVertScrollbarMsg(const Msg: TLMVScroll);
var
  NMax: integer;
  NVii: integer;
begin
  NVii:=GetVisibleItems;
  NMax:= MaxItemTop;
  case Msg.ScrollCode of
    SB_TOP:      FItemTop:= 0;
    SB_BOTTOM:   FItemTop:= NMax;
    SB_LINEUP:   FItemTop:= Max(0, FItemTop-1);
    SB_LINEDOWN: FItemTop:= Min(NMax, FItemTop+1);
    SB_PAGEUP:   FItemTop:= Max(0, FItemTop- NVii);
    SB_PAGEDOWN: FItemTop:= Min(NMax, FItemTop+ NVii);
    SB_THUMBPOSITION, SB_THUMBTRACK:
                 FItemTop:= Max(0, Msg.Pos);
  end;
end;

procedure TVListBox.UpdateFromHorzScrollbarMsg(const Msg: TLMHScroll);
var
  WMax: integer;
begin
  WMax:= Max(0, FItemMinWidth - ClientWidth);
  case Msg.ScrollCode of
    SB_TOP:    FShiftX:= 0;
    SB_BOTTOM: FShiftX:= WMax;
    SB_LINEUP, SB_PAGEUP:
               FShiftX:= Max(0, FShiftX - (ClientWidth div 10));
    SB_LINEDOWN, SB_PAGEDOWN:
               FShiftX:= Min(WMax, FShiftX + (ClientWidth div 10));
    SB_THUMBPOSITION, SB_THUMBTRACK:
               FShiftX:= Max(0, Msg.Pos);
  end;
end;

procedure TVListBox.UpdateScrollbar;
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
  w, w2, gvi, gvi2, fiw: integer;
begin
  if HandleAllocated then
  begin
    gvi:= GetVisibleItems;
    FillChar(si{%H-}, SizeOf(si), 0);
    si.cbSize:= SizeOf(si);
    si.fMask:= SIF_ALL;
    si.nMin:= 0;
    w:= ClientWidth;
    if FItemCount=0 then fiw:=w  else fiw:=max(FItemMinWidth, w);
    DoUpdateScrollBar(SB_Horz, fiw, w, FShiftX);
    DoUpdateScrollBar(SB_Vert, FItemCount, gvi, FItemTop);
    w2:=ClientWidth;
    if (w<>w2) and (w2<FItemMinWidth) then DoUpdateScrollBar(SB_Horz, FItemMinWidth, w2, FShiftX);
    gvi2:= GetVisibleItems;
    if (FItemCount>gvi2) and (gvi2<gvi) then DoUpdateScrollBar(SB_Vert, FItemCount, gvi2, FItemTop);
  end;
end;

function TVListBox.GetVisibleItems: integer;
var
  w, h: integer;
begin
  h:= ClientHeight;
  w:= ClientWidth;
  if FItemCount>0 then w:=max(FItemMinWidth, w);
  Result:= h div FItemHeight;
end;

function TVListBox.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FItemCount);
end;

procedure TVListBox.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

procedure TVListBox.Paint;
var
  R: TRect;
begin
  inherited;
  UpdateScrollbar;
  R:= ClientRect;
  if not DoubleBuffered then DoPaintTo(Canvas, R)
  else
  begin
    if not Assigned(FBitmap) then FBitmap:=TBitmap.Create;
    if (FBitmap.Width<>R.Width) or (FBitmap.Height<>R.Height) then FBitmap.SetSize(R.Width, R.Height);
    FBitmap.Canvas.Font.Assign(Self.Font);
    DoPaintTo(FBitmap.Canvas, R);
    Canvas.CopyRect(R, FBitmap.Canvas, R);
  end;
end;

procedure TVListBox.DoOnResize;
begin
  if FItemCount>0 then
  begin
    FItemTop:=min(FItemTop, MaxItemTop);
    FShiftX:=min(FShiftX, max(0, FItemMinWidth - ClientWidth));
  end;
  inherited;
  {$IFDEF LCLGtk2}
  Invalidate;
  {$ENDIF}
end;

function TVListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:= true;
  if FItemCount<1 then exit;
  if FItemIndex<0 then exit;
  case FMouseWheelOption of
  mwItems:
    if WheelDelta>0 then SetItemIndex(max(0, FItemIndex- 1)) else SetItemIndex(min(FItemCount-1, FItemIndex+ 1));
  mwList:
    begin
      if WheelDelta>0 then FItemTop:= Max(0, FItemTop-Mouse.WheelScrollLines)
      else FItemTop:= Max(0, Min(FItemCount-VisibleItems, FItemTop+Mouse.WheelScrollLines));
      Changed;
      Invalidate;
    end;
  end;
end;

procedure TVListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  n: integer;
begin
  if HandleAllocated then LCLIntf.SetFocus(Handle);
  PopupMenu:=nil;
  if FItemCount>0 then
  begin
    n:= Y div FItemHeight + FItemTop;
    if n< FItemCount then SetItemIndex(n);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TVListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  n: integer;
begin
  if FItemCount<0 then exit;
  if not (Shift=[ssLeft]) then exit;
  n:= Y div FItemHeight + FItemTop;
  if n>=FItemCount then exit;
  SetItemIndex(n);
  inherited MouseMove(Shift, X, Y);
end;

procedure TVListBox.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromVertScrollbarMsg(Msg);
  Invalidate;
end;

procedure TVListBox.LMHScroll(var Msg: TLMHScroll);
begin
  UpdateFromHorzScrollbarMsg(Msg);
  Invalidate;
end;

procedure TVListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FItemCount<1 then exit;
  if FItemIndex<0 then exit;
  if (Key=VK_UP) then
  begin
    SetItemIndex(max(0, FItemIndex- 1));
    Key:= 0;
    Exit;
  end;
  if (Key=VK_DOWN) then
  begin
    SetItemIndex(min(FItemCount-1, FItemIndex+ 1));
    Key:= 0;
    Exit;
  end;
  if (Key=VK_PRIOR) then
  begin
    ItemIndex:= Max(0, FItemIndex-(VisibleItems-1));
    Key:= 0;
    Exit;
  end;
  if (Key=VK_NEXT) then
  begin
    ItemIndex:= Min(FItemCount-1, FItemIndex+(VisibleItems-1));
    Key:= 0;
    Exit;
  end;
  if (Key=VK_HOME) then
  begin
    ItemIndex:= 0;
    Key:= 0;
    Exit;
  end;
  if (Key=VK_END) then
  begin
    SetItemIndex(FItemCount-1);
    Key:= 0;
    Exit;
  end;
  if (key=VK_RETURN) then
  begin
    DblClick;
    Key:= 0;
    Exit;
  end;
end;

constructor TVListBox.Create(AOwner: TComponent);
begin
  inherited;
  FShiftX:=0;
  FMouseWheelOption:= mwItems;
  ControlStyle:= ControlStyle+[csOpaque]-[csTripleClicks];
  DoubleBuffered:= IsDoubleBufferedNeeded;
  FSelectedColor:=$00e4e4e4;
  FItemHeight:=ScaleY(DefItemHeight, 96);

  Color:= clWhite;
  FOnDrawItem:= nil;
  FItemCount:= 0;
  FItemIndex:= -1;

  FItemMinWidth:=0;

  FItemTop:= 0;
  FBitmap:= nil;
  SetInitialBounds(0, 0, ScaleX(150, 96), ScaleY(180, 96));
  Font.OnChange:=@OnChangeFont;
end;

destructor TVListBox.Destroy;
begin
  if Assigned(FBitmap) then FreeAndNil(FBitmap);
  inherited;
end;

procedure TVListBox.OnChangeFont(Sender: TObject);
begin
  Canvas.Font.Assign(Font);
  Invalidate;
end;

end.
