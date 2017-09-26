unit vtypes;

{$mode objfpc}{$H+}

interface
{$modeswitch advancedrecords}

uses
  Classes, SysUtils;
type

  { TDoublePoint }

  TDoublePoint  =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
       X, Y : double;
     public
       {$ifdef VER3}
       constructor Create(ax,ay: double); overload;
       constructor Create(apt: TDoublePoint); overload;
       {$endif}
       function Add(const apt: TDoublePoint): TDoublePoint;
       function Distance(const apt: TDoublePoint) : double;
       function Subtract(const apt : TDoublePoint): TDoublePoint;
       procedure SetLocation(const apt: TDoublePoint);
       procedure SetLocation(ax, ay: double);
       procedure Offset(const apt: TDoublePoint);
       procedure Offset(dx, dy: double);
       class function PointInCircle(const apt, acenter: TDoublePoint; const aradius: double): Boolean; static; inline;
       class operator = (const apt1, apt2 : TDoublePoint) : Boolean;
       class operator <> (const apt1, apt2 : TDoublePoint): Boolean;
       class operator + (const apt1, apt2 : TDoublePoint): TDoublePoint;
       class operator - (const apt1, apt2 : TDoublePoint): TDoublePoint;
     end;

  { TDoubleRect }

  TDoubleRect =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
     private
       function  getHeight: double; inline;
       function  getLocation: TDoublePoint;
       function  getSize: TDoublePoint;
       function  getWidth : double; inline;
       procedure setHeight(AValue: double);
       procedure setSize(AValue: TDoublePoint);
       procedure setWidth (AValue: double);
     public
  {$IFDEF VER3_0_0}
       class function Create(Origin: TPoint; AWidth, AHeight: double): TRect; static;
       class function Create(ALeft, ATop, ARight, ABottom: double; Normalize: Boolean = False): TRect; static;
       class function Create(P1, P2: TDoublePoint; Normalize: Boolean = False): TDoubleRect; static;
  {$ELSE}
       constructor Create(Origin: TDoublePoint; AWidth, AHeight: double);
       constructor Create(ALeft, ATop, ARight, ABottom: double; Normalize: Boolean = False);
       constructor Create(P1, P2: TDoublePoint);
  {$ENDIF}
       class operator = (L, R: TDoubleRect): Boolean;
       class operator <> (L, R: TDoubleRect): Boolean;
       class operator + (L, R: TDoubleRect): TDoubleRect; // union
       class operator * (L, R: TDoubleRect): TDoubleRect; // intersection
       procedure NormalizeRect;

       function Contains(Pt: TDoublePoint): Boolean;
       class function Intersect(L, R: TDoubleRect): TDoubleRect; static;
       function IntersectsWith(R: TDoubleRect): Boolean;

       class function Union(L, R: TDoubleRect): TDoubleRect; static;
       procedure Union(R: TDoubleRect);

       procedure Offset(DX, DY: double);
       procedure Offset(DP: TDoublePoint);
       procedure SetLocation(X, Y: double);
       procedure SetLocation(P: TDoublePoint);
       procedure Inflate(DX, DY: double);
       function CenterPoint: TDoublePoint;
     public
       property Height: double read getHeight write setHeight;
       property Width: double read getWidth write setWidth;
       property Size: TDoublePoint  read getSize  write setSize;
       property Location : TDoublePoint read getLocation write setLocation;
       case Longint of
         0: (Left, Top, Right, Bottom : double);
         1: (TopLeft,BottomRight : TDoublePoint);
      //   2: (Vector: TArray4IntegerType);
       end;


implementation

uses
  math;



{ TDoubleRect }

function TDoubleRect.getHeight: double;
begin
  Result:= Bottom - Top;
end;

function TDoubleRect.getLocation: TDoublePoint;
begin
  Result:= TopLeft;
end;

function TDoubleRect.getSize: TDoublePoint;
begin
  Result.X:=Right - Left;
  Result.Y:= Bottom - Top;
end;

function TDoubleRect.getWidth: double;
begin
  Result:= Right- Left;
end;

procedure TDoubleRect.setHeight(AValue: double);
begin
  Bottom:= Top+ AValue;
end;

procedure TDoubleRect.setSize(AValue: TDoublePoint);
begin
  Right:= Left + AValue.X;
  Bottom:= Top + AValue.Y;
end;

procedure TDoubleRect.setWidth(AValue: double);
begin
  Right:= Left+ AValue;
end;

constructor TDoubleRect.Create(Origin: TDoublePoint; AWidth, AHeight: double);
begin
  Left:= Origin.X;
  Top:= Origin.Y;
  Right:= Left+ AWidth;
  Bottom:= Top+ AHeight;
end;

constructor TDoubleRect.Create(ALeft, ATop, ARight, ABottom: double;
  Normalize: Boolean);
begin
  Left:= ALeft;
  Right:= ARight;
  Top:= ATop;
  Bottom:= ABottom;
  if Normalize then NormalizeRect;
end;

constructor TDoubleRect.Create(P1, P2: TDoublePoint);
begin
  TopLeft:=P1;
  BottomRight:=P2;
  NormalizeRect;
end;

class operator TDoubleRect.=(L, R: TDoubleRect): Boolean;
begin
  Result:= (L.TopLeft=R.TopLeft) and (L.BottomRight = R.BottomRight);
end;

class operator TDoubleRect.<>(L, R: TDoubleRect): Boolean;
begin
  Result:= (L.TopLeft<>R.TopLeft) or (L.BottomRight <> R.BottomRight);
end;

class operator TDoubleRect.+(L, R: TDoubleRect): TDoubleRect;
begin
  Result:=Union(L, R);
end;

class operator TDoubleRect.*(L, R: TDoubleRect): TDoubleRect;
begin
  Result:= Intersect(L, R);
end;

procedure TDoubleRect.NormalizeRect;
  procedure Exchange(var x1, x2: double); inline;
  var
    x: double;
  begin
    x:= x1;
    x1:= x2;
    x2:= x;
  end;
begin
  if Left> Right then Exchange(Left, Right);
  if Top> Bottom then Exchange(Top, Bottom);
end;

function TDoubleRect.Contains(Pt: TDoublePoint): Boolean;
begin
  Result:= not((Pt.X<Left) or (Pt.X>Right) or (Pt.Y< Top) or (Pt.Y> Bottom));
end;

class function TDoubleRect.Intersect(L, R: TDoubleRect): TDoubleRect;
begin
  L.NormalizeRect;
  R.NormalizeRect;
  Result.Left:=max(L.Left, R.Left);
  Result.Right:=min(L.Right, R.Right);
  Result.Top:=max(L.Top, R.Top);
  Result.Bottom:=min(L.Bottom, R.Bottom);
end;

function TDoubleRect.IntersectsWith(R: TDoubleRect): Boolean;
begin
  R:=Intersect(self, R);
  Result:= (R.Width>0) and (R.Height>0);
  if Result then self:=R;
end;

class function TDoubleRect.Union(L, R: TDoubleRect): TDoubleRect;
begin
  Result.Left:=min(L.Left, R.Left);
  Result.Right:=max(L.Right, R.Right);
  Result.Top:=min(L.Top, R.Top);
  Result.Bottom:=max(L.Bottom, R.Bottom);
end;

procedure TDoubleRect.Union(R: TDoubleRect);
begin
  Union(self, R);
end;

procedure TDoubleRect.Offset(DX, DY: double);
begin
  Left+= DX;
  Right+= DX;
  Top+= DY;
  Bottom+= DY;
end;

procedure TDoubleRect.Offset(DP: TDoublePoint);
begin
  Left+= DP.X;
  Right+= DP.X;
  Top+= DP.Y;
  Bottom+= DP.Y;
end;

procedure TDoubleRect.SetLocation(X, Y: double);
begin
  Offset(X- Left, Y- Top);
end;

procedure TDoubleRect.SetLocation(P: TDoublePoint);
begin
  Offset(P.X- Left, P.Y- Top);
end;

procedure TDoubleRect.Inflate(DX, DY: double);
begin
  Left-= DX;
  Right+= DX;
  Top-= DY;
  Bottom+= DY;
end;

function TDoubleRect.CenterPoint: TDoublePoint;
begin
  Result.SetLocation((Left+ Right)/2, (Top+Bottom)/2);
end;

{ TDoublePoint }

constructor TDoublePoint.Create(ax, ay: double);
begin
  X:=ax; Y:=ay;
end;

constructor TDoublePoint.Create(apt: TDoublePoint);
begin
  X:= apt.X; Y:=apt.Y;
end;

function TDoublePoint.Add(const apt: TDoublePoint): TDoublePoint;
begin
  Result.X:=X+ apt.X;
  Result.Y:=Y+ apt.Y;
end;

function TDoublePoint.Distance(const apt: TDoublePoint): double;
begin
  Result:=sqrt(sqr(apt.X- X)+ sqr(apt.Y- Y));
end;

function TDoublePoint.Subtract(const apt: TDoublePoint): TDoublePoint;
begin
  Result.X:= X- apt.X;
  Result.Y:= Y- apt.Y;
end;

procedure TDoublePoint.SetLocation(const apt: TDoublePoint);
begin
  X:= apt.X; Y:=apt.Y;
end;

procedure TDoublePoint.SetLocation(ax, ay: double);
begin
  X:=ax; Y:=ay;
end;

procedure TDoublePoint.Offset(const apt: TDoublePoint);
begin
  X:= X+ apt.X;
  Y:= Y+ apt.Y;
end;

procedure TDoublePoint.Offset(dx, dy: double);
begin
  X:= X+ dx;
  Y:= Y+ dy;
end;

class function TDoublePoint.PointInCircle(const apt, acenter: TDoublePoint;
  const aradius: double): Boolean;
begin
  Result := apt.Distance(acenter) <= aradius;
end;

class operator TDoublePoint.=(const apt1, apt2: TDoublePoint): Boolean;
begin
  Result:=SameValue(apt1.x, apt2.x) and SameValue(apt1.y, apt2.y);
end;

class operator TDoublePoint.<>(const apt1, apt2: TDoublePoint): Boolean;
begin
  Result:=not (SameValue(apt1.x, apt2.x) and SameValue(apt1.y, apt2.y));
end;

class operator TDoublePoint.+(const apt1, apt2: TDoublePoint): TDoublePoint;
begin
  Result.X:=apt1.X+ apt2.X;
  Result.Y:=apt1.Y+ apt2.Y;
end;

class operator TDoublePoint.-(const apt1, apt2: TDoublePoint): TDoublePoint;
begin
  Result.X:= apt1.X- apt2.X;
  Result.Y:= apt1.Y- apt2.Y;
end;

end.

