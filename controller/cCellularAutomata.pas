unit cCellularAutomata;

interface

uses
  System.SysUtils, Vcl.Graphics, System.Generics.Collections, Math;

type
  TCellularAutomata = class
    private
      Grid : array of array of Integer;
      OldGrid : array of array of Integer;
      GridMonteCarlo : array of array of Boolean;

      GridDensity : array of array of Extended;
      GridIsCrystalized : array of array of Boolean;

      Colors : TList<TColor>;

      FWidth : Integer;
      FIterations : Integer;

      FCounter : Integer;

      FNeighbourhoodType : Integer;
      FBoundaryCondition : Integer;

      procedure PrepareOldGrid;
      procedure PrepareMonteCarloGrid;
      procedure PrepareGridDensity;
      procedure PrepareGridIsCrystalized;

      procedure ClearOldGrid;
      procedure ClearMonteCarloGrid;
      procedure CleanGridDensity;
      procedure CleanGridIsCrystalized;
    public
      function GetValue(I, J : Integer) : Integer;
      procedure SetValue(I, J, Value : Integer);

      function GetColor(Index : Integer) : TColor;
      procedure SetColor;

      procedure PrepareGridGameOfLife(Iterations, Width : Integer);
      procedure StepGrainGrowth;

      procedure CalculateMoore(I, J : Integer);
      procedure CalculateVonNeumann(I, J : Integer);
      procedure CalculatePentagonal(I, J : Integer);
      procedure CalculateHexagonalLeft(I, J : Integer);
      procedure CalculateHexagonalRight(I, J : Integer);
      procedure CalculateHexagonalRandom(I, J : Integer);
      procedure CalculateRadial(I, J : Integer);

      function CalculateDRX(Time, Density : Extended) : Extended;
      function HasNeighbour(I, J, pValue : Integer) : Boolean;
      procedure NucleateCrystalized;
      function HasNeighbourRecrystalized(I, J : Integer) : Integer;
      function HasGreatestDensityAmongNeighbours(I, J : Integer) : Boolean;

      procedure SetNucleation(NucleationIndex, Variable1, Variable2 : Integer);
      procedure SetValuesForRadialNucleation(I, J, Radius : Integer);

      procedure CalculateMonteCarlo(kT : Double);
      function GetRandomNeighbour(I, J, pValue : Integer) : Integer;
      function CalculateEnergy(I, J, pValue : Integer) : Integer;

      property Width : Integer read FWidth;
      property Iterations : Integer read FIterations;
      property Counter : Integer read FCounter write FCounter;
      property NeighbourhoodType : Integer read FNeighbourhoodType write FNeighbourhoodType;
      property BoundaryCondition : Integer read FBoundaryCondition write FBoundaryCondition;

      constructor Create();
      destructor Destroy; override;
  end;

implementation

uses
  uUtils;

constructor TCellularAutomata.Create;
begin
  Colors := TList<TColor>.Create;
  FCounter := 0;
end;

destructor TCellularAutomata.Destroy;
begin
  inherited;
  Colors.Free;
end;

procedure TCellularAutomata.SetColor;
begin
  Colors.Add(TUtils.GetRandomColor);
end;

function TCellularAutomata.GetColor(Index : Integer) : TColor;
begin
  Result := Colors[Index - 1];
end;

procedure TCellularAutomata.SetNucleation(NucleationIndex, Variable1, Variable2 : Integer);
var RowsIterations, ColumnIterations, I, J, CounterTmp, TempI, TempJ : Integer;
begin
    case NucleationIndex of
      // Homogeneus
      0: begin
        RowsIterations := FIterations div Variable1;
        ColumnIterations := FWidth div Variable2;
        CounterTmp := 1;
        for I := 0 to Variable1 - 1 do begin
          for J := 0 to Variable2 - 1 do begin
            SetValue(I * RowsIterations, J * ColumnIterations, CounterTmp);
            CounterTmp := CounterTmp + 1;
            SetColor;
          end;
        end;
      end;
      // Radius
      1: begin
         for I := 1 to Variable1 do begin
//          TempI := Random(FWidth);
//          TempJ := Random(FIterations);
//          SetValue(TempI, TempJ, I);
//          SetColor;
//          SetValuesForRadialNucleation(TempI, TempJ, Variable2);
        end;
      end;
      // Random
      2: begin
        for I := 1 to Variable1 do begin
          SetValue(Random(Iterations), Random(Width), I);
          SetColor;
        end;
      end;
    end;
end;

procedure TCellularAutomata.StepGrainGrowth;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to High(Grid) do begin
    for J := 0 to High(Grid[I]) do begin
      if (not (Grid[I][J] = 0)) then begin
        case NeighbourhoodType of
          0: CalculateMoore(I, J);
          1: CalculateVonNeumann(I, J);
          2: CalculatePentagonal(I, J);
          3: CalculateHexagonalLeft(I, J);
          4: CalculateHexagonalRight(I, J);
          5: CalculateHexagonalRandom(I, J);
          6: CalculateRadial(I, J);
        end;
      end;
    end;
  end;

  for I := 0 to High(OldGrid) do begin
    for J := 0 to High(OldGrid[I]) do Grid[I][J] := OldGrid[I][J];
  end;

  ClearOldGrid;
end;

procedure TCellularAutomata.SetValuesForRadialNucleation(I, J, Radius : Integer);
begin
  for I := 0 to High(Grid) do begin
    for J := 0 to High(Grid[I]) do begin
      if (not (Grid[I][J] = 0)) then begin
          //sqrt(Power(XPos-X,2)+Power(YPos-Y,2));
      end;
    end;
  end;
end;

procedure TCellularAutomata.CalculateMoore(I: Integer; J: Integer);
var PrevRow, NextRow, Prev, Next : Integer;
begin
  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  OldGrid[I][J] := Grid[I][J];

  if Grid[PrevRow][Prev] = 0 then OldGrid[PrevRow][Prev] := Grid[I][J];
  if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
  if Grid[PrevRow][Next] = 0 then OldGrid[PrevRow][Next] := Grid[I][J];
  if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
  if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
  if Grid[NextRow][Prev] = 0 then OldGrid[NextRow][Prev] := Grid[I][J];
  if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
  if Grid[NextRow][Next] = 0 then OldGrid[NextRow][Next] := Grid[I][J];
end;

procedure TCellularAutomata.CalculateVonNeumann(I: Integer; J: Integer);
var Left, Right, Top, Bottom : Integer;
begin
  Left := J - 1;
  Right := J + 1;
  Top := I + 1;
  Bottom := I - 1;

  case BoundaryCondition of
    0: begin
      if Left < 0 then Left := High(Grid[I]);
      if Right > High(Grid[I]) then Right := Low(Grid[I]);
      if Bottom < 0 then Bottom := High(Grid);
      if Top > High(Grid) then Top := Low(Grid);
    end;
    1: begin
      if Left < 0 then Left := 0;
      if Right > High(Grid[I]) then Right := High(Grid[I]);
      if Bottom < 0 then Bottom := 0;
      if Top > High(Grid) then Top := High(Grid);
    end;
  end;

  OldGrid[I][J] := Grid[I][J];
  if Grid[I][Left] = 0 then OldGrid[I][Left] := Grid[I][J];
  if Grid[I][Right] = 0 then OldGrid[I][Right] := Grid[I][J];
  if Grid[Top][J] = 0 then OldGrid[Top][J] := Grid[I][J];
  if Grid[Bottom][J] = 0 then OldGrid[Bottom][J] := Grid[I][J];
end;

procedure TCellularAutomata.CalculatePentagonal(I: Integer; J: Integer);
var PrevRow, NextRow, Prev, Next, RandomTmp : Integer;
begin
  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  RandomTmp := Random(4);

  OldGrid[I][J] := Grid[I][J];

  case RandomTmp of
    // Ucinamy prawa strone
    0: begin
      if Grid[PrevRow][Prev] = 0 then OldGrid[PrevRow][Prev] := Grid[I][J];
      if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
      if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
      if Grid[NextRow][Prev] = 0 then OldGrid[NextRow][Prev] := Grid[I][J];
      if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
    end;
    // Ucinamy lewa strone
    1: begin
      if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
      if Grid[PrevRow][Next] = 0 then OldGrid[PrevRow][Next] := Grid[I][J];
      if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
      if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
      if Grid[NextRow][Next] = 0 then OldGrid[NextRow][Next] := Grid[I][J];
    end;
    // Ucinamy gore
    2: begin
      if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
      if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
      if Grid[NextRow][Prev] = 0 then OldGrid[NextRow][Prev] := Grid[I][J];
      if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
      if Grid[NextRow][Next] = 0 then OldGrid[NextRow][Next] := Grid[I][J];
    end;
    // Ucinamy dol
    3: begin
      if Grid[PrevRow][Prev] = 0 then OldGrid[PrevRow][Prev] := Grid[I][J];
      if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
      if Grid[PrevRow][Next] = 0 then OldGrid[PrevRow][Next] := Grid[I][J];
      if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
      if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
    end;
  end;

end;

procedure TCellularAutomata.CalculateHexagonalLeft(I: Integer; J: Integer);
var PrevRow, NextRow, Prev, Next: Integer;
begin
  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  OldGrid[I][J] := Grid[I][J];

  if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
  if Grid[PrevRow][Next] = 0 then OldGrid[PrevRow][Next] := Grid[I][J];
  if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
  if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
  if Grid[NextRow][Prev] = 0 then OldGrid[NextRow][Prev] := Grid[I][J];
  if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
end;

procedure TCellularAutomata.CalculateHexagonalRight(I: Integer; J: Integer);
var PrevRow, NextRow, Prev, Next: Integer;
begin
  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  OldGrid[I][J] := Grid[I][J];

  if Grid[PrevRow][Prev] = 0 then OldGrid[PrevRow][Prev] := Grid[I][J];
  if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
  if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
  if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
  if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
  if Grid[NextRow][Next] = 0 then OldGrid[NextRow][Next] := Grid[I][J];
end;

procedure TCellularAutomata.CalculateHexagonalRandom(I: Integer; J: Integer);
var PrevRow, NextRow, Prev, Next, RandomTmp : Integer;
begin
  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  RandomTmp := Random(2);

  OldGrid[I][J] := Grid[I][J];

  case RandomTmp of
    0: begin
      if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
      if Grid[PrevRow][Next] = 0 then OldGrid[PrevRow][Next] := Grid[I][J];
      if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
      if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
      if Grid[NextRow][Prev] = 0 then OldGrid[NextRow][Prev] := Grid[I][J];
      if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
    end;
    1: begin
      if Grid[PrevRow][Prev] = 0 then OldGrid[PrevRow][Prev] := Grid[I][J];
      if Grid[PrevRow][J] = 0 then OldGrid[PrevRow][J] := Grid[I][J];
      if Grid[I][Prev] = 0 then OldGrid[I][Prev] := Grid[I][J];
      if Grid[I][Next] = 0 then OldGrid[I][Next] := Grid[I][J];
      if Grid[NextRow][J] = 0 then OldGrid[NextRow][J] := Grid[I][J];
      if Grid[NextRow][Next] = 0 then OldGrid[NextRow][Next] := Grid[I][J];
    end;
  end;
end;

procedure TCellularAutomata.CalculateRadial(I: Integer; J: Integer);
begin
// TODO
end;

function TCellularAutomata.CalculateDRX(Time, Density : Extended) : Extended;
var A : Extended;
    B : Extended;
    ro, detRo, mediumRo, recrystalizationPackage, randomPackage, roCritical : Extended;
    I, J, randI, randJ, chance, tmp : Integer;
begin
  Result := 0;

  A := 86710969050178.5;
  B := 9.41268203527779;

  NucleateCrystalized;

  ro := (A/B) + (1 - (A/B))* Power(Exp(1.0), (-B*Time));
  detRo := Abs(ro - Density);

  recrystalizationPackage := detRo / ((High(Grid) + 1) * (High(Grid[0]) + 1)) * 0.2;

  for I := Low(Grid) to High(Grid) do begin
    for J := Low(Grid[0]) to High(Grid[0]) do begin
      GridDensity[I][J] := GridDensity[I][J] + recrystalizationPackage;
    end;
  end;

  detRo := detRo - recrystalizationPackage;

  while detRo > 0 do begin
    randI := Random(High(Grid) + 1);
    randJ := Random(High(Grid[0]) + 1);

    randomPackage := detRo * Random;
    chance := Random(10);

    if HasNeighbour(randI, randJ, Grid[randI][randJ]) then begin
      if chance in [2..10] then begin
        GridDensity[randI][randJ] := GridDensity[randI][randJ] + randomPackage;
        detRo := detRo - randomPackage;
      end;
    end else begin
      if chance in [0..1] then begin
        GridDensity[randI][randJ] := GridDensity[randI][randJ] + randomPackage;
        detRo := detRo - randomPackage;
      end;
    end;
  end;

  ClearOldGrid;

  roCritical := ro / ((High(Grid) + 1) * (High(Grid[0]) + 1));
  for I := Low(Grid) to High(Grid) do begin
    for J := Low(Grid[0]) to High(Grid[0]) do begin
      if HasNeighbour(I, J, Grid[I][J]) and
      (GridDensity[I][J] > roCritical) and
      (not GridIsCrystalized[I][J]) then
      begin
        SetValue(I, J, Colors.Count);
        SetColor;
        GridIsCrystalized[I][J] := True;
        GridDensity[I][J] := 0.0;
        OldGrid[I][J] := 1;
      end;
    end;
  end;

  for I := Low(Grid) to High(Grid) do begin
    for J := Low(Grid[0]) to High(Grid[0]) do begin
      tmp := HasNeighbourRecrystalized(I, J);
      if (tmp <> 0) and (OldGrid[I][J] = 0) then begin
        if HasGreatestDensityAmongNeighbours(I, J) then begin
          SetValue(I, J, tmp);
          GridIsCrystalized[I][J] := True;
          GridDensity[I][J] := 0.0;
        end;
      end;
    end;
  end;

  Result := ro;
end;

function TCellularAutomata.HasNeighbour(I, J, pValue : Integer) : Boolean;
var
  topCenterI,
  centerLeftI, centerRightI,
  botCenterI : Integer;

  topCenterJ,
  centerLeftJ, centerRightJ,
  botCenterJ : Integer;
begin
  Result := False;

  topCenterI := I - 1;
  if topCenterI < 0 then topCenterI := High(Grid);
  topCenterJ := J;

  centerLeftI := I;
  centerLeftJ := J - 1;
  if centerLeftJ < 0 then centerLeftJ := High(Grid[0]);

  centerRightI := I;
  centerRightJ := J + 1;
  if centerRightJ > High(Grid[0]) then centerRightJ := 0;

  botCenterI := I + 1;
  if botCenterI > High(Grid) then botCenterI := 0;
  botCenterJ := J;

  if Grid[topCenterI][topCenterJ] <> pValue then Result := True;
  if Grid[centerLeftI][centerLeftJ] <> pValue then Result := True;
  if Grid[botCenterI][botCenterJ] <> pValue then Result := True;
  if Grid[centerRightI][centerRightJ] <> pValue then Result := True;
end;

function TCellularAutomata.HasNeighbourRecrystalized(I, J : Integer) : Integer;
var
  topCenterI,
  centerLeftI, centerRightI,
  botCenterI : Integer;

  topCenterJ,
  centerLeftJ, centerRightJ,
  botCenterJ : Integer;
begin
  Result := 0;

  topCenterI := I - 1;
  if topCenterI < 0 then topCenterI := High(Grid);
  topCenterJ := J;

  centerLeftI := I;
  centerLeftJ := J - 1;
  if centerLeftJ < 0 then centerLeftJ := High(Grid[0]);

  centerRightI := I;
  centerRightJ := J + 1;
  if centerRightJ > High(Grid[0]) then centerRightJ := 0;

  botCenterI := I + 1;
  if botCenterI > High(Grid) then botCenterI := 0;
  botCenterJ := J;

  if GridIsCrystalized[topCenterI][topCenterJ] = True then Result := Grid[topCenterI][topCenterJ];
  if GridIsCrystalized[centerLeftI][centerLeftJ] = True then Result := Grid[centerLeftI][centerLeftJ];
  if GridIsCrystalized[botCenterI][botCenterJ] = True then Result := Grid[botCenterI][botCenterJ];
  if GridIsCrystalized[centerRightI][centerRightJ] = True then Result := Grid[centerRightI][centerRightJ];
end;

function TCellularAutomata.HasGreatestDensityAmongNeighbours(I, J : Integer) : Boolean;
var
  topCenterI,
  centerLeftI, centerRightI,
  botCenterI : Integer;

  topCenterJ,
  centerLeftJ, centerRightJ,
  botCenterJ : Integer;
begin
  Result := True;

  topCenterI := I - 1;
  if topCenterI < 0 then topCenterI := High(Grid);
  topCenterJ := J;

  centerLeftI := I;
  centerLeftJ := J - 1;
  if centerLeftJ < 0 then centerLeftJ := High(Grid[0]);

  centerRightI := I;
  centerRightJ := J + 1;
  if centerRightJ > High(Grid[0]) then centerRightJ := 0;

  botCenterI := I + 1;
  if botCenterI > High(Grid) then botCenterI := 0;
  botCenterJ := J;

  if GridDensity[topCenterI][topCenterJ] < GridDensity[I][J] then Result := False;
  if GridDensity[centerLeftI][centerLeftJ] < GridDensity[I][J] then Result := False;
  if GridDensity[botCenterI][botCenterJ] < GridDensity[I][J] then Result := False;
  if GridDensity[centerRightI][centerRightJ] < GridDensity[I][J] then Result := False;
end;

procedure TCellularAutomata.NucleateCrystalized;
var
  I, J : Integer;

  topCenterI,
  centerLeftI, centerRightI,
  botCenterI : Integer;

  topCenterJ,
  centerLeftJ, centerRightJ,
  botCenterJ : Integer;
begin

  for I := Low(Grid) to High(Grid) do begin
    for J := Low(Grid[0]) to High(Grid[0]) do begin
      if GridIsCrystalized[I][J] then begin
        topCenterI := I - 1;
        if topCenterI < 0 then topCenterI := High(Grid);
        topCenterJ := J;

        centerLeftI := I;
        centerLeftJ := J - 1;
        if centerLeftJ < 0 then centerLeftJ := High(Grid[0]);

        centerRightI := I;
        centerRightJ := J + 1;
        if centerRightJ > High(Grid[0]) then centerRightJ := 0;

        botCenterI := I + 1;
        if botCenterI > High(Grid) then botCenterI := 0;
        botCenterJ := J;

        if not GridIsCrystalized[topCenterI][topCenterJ] then begin
          Grid[topCenterI][topCenterJ] := Grid[I][J];
          GridDensity[topCenterI][topCenterJ] := 0.0;
        end;

        if not GridIsCrystalized[centerLeftI][centerLeftJ] then begin
          Grid[centerLeftI][centerLeftJ] := Grid[I][J];
          GridDensity[centerLeftI][centerLeftJ] := 0.0;
        end;

        if not GridIsCrystalized[botCenterI][botCenterJ] then begin
          Grid[botCenterI][botCenterJ] := Grid[I][J];
          GridDensity[botCenterI][botCenterJ] := 0.0;
        end;

        if not GridIsCrystalized[centerRightI][centerRightJ] then begin
          Grid[centerRightI][centerRightJ] := Grid[I][J];
          GridDensity[centerRightI][centerRightJ] := 0.0;
        end;
      end;
    end;
  end;

end;

procedure TCellularAutomata.CalculateMonteCarlo(kT : Double);
var
  RandomI, RandomJ : Integer;
  I, J, Counter: Integer;
  energyOrigin, energyNeighbour, neighbourIdx : Integer;
  probability, randDouble : Extended;
begin
  ClearMonteCarloGrid;

  Counter := 0;

  while Counter < (High(Grid) + 1) * (High(Grid[0]) + 1) do begin
    RandomI := Random(High(Grid) + 1);
    RandomJ := Random(High(Grid[0]) + 1);

    if not GridMonteCarlo[RandomI][RandomJ] then begin
      GridMonteCarlo[RandomI][RandomJ] := True;

      energyOrigin := CalculateEnergy(RandomI, RandomJ, Grid[RandomI][RandomJ]);

      neighbourIdx := GetRandomNeighbour(RandomI, RandomJ, Grid[RandomI][RandomJ]);

      energyNeighbour := CalculateEnergy(RandomI, RandomJ, neighbourIdx);

      if (energyNeighbour - energyOrigin) <= 0 then begin
        Grid[RandomI][RandomJ] := neighbourIdx;
      end else begin
        probability := Exp(-(energyNeighbour - energyOrigin) / kT);
        randDouble := Random;
        if randDouble <= probability then begin
          Grid[RandomI][RandomJ] := neighbourIdx;
        end;
      end;
      counter := counter + 1;
    end;
  end;
end;

function TCellularAutomata.GetRandomNeighbour(I, J, pValue : Integer) : Integer;
var PrevRow, NextRow, Prev, Next : Integer;
    RandomTmp : Integer;
begin
  Result := 0;

  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  case NeighbourhoodType of
    0: begin
      // Moore
      RandomTmp := Random(8);
      case RandomTmp of
        0: Result := Grid[PrevRow][Prev];
        1: Result := Grid[PrevRow][J];
        2: Result := Grid[PrevRow][Next];
        3: Result := Grid[I][Prev];
        4: Result := Grid[I][Next];
        5: Result := Grid[NextRow][Prev];
        6: Result := Grid[NextRow][J];
        7: Result := Grid[NextRow][Next];
      end;
    end;
    1: begin
      // von Neumann
      case Random(4) of
        0: Result := Grid[I][Prev];
        1: Result := Grid[I][Next];
        2: Result := Grid[PrevRow][J];
        3: Result := Grid[NextRow][J];
      end;
    end;
    2: begin
      // Pentagonal Random
      case Random(4) of
        // Ucinamy prawa strone
        0: begin
          case Random(5) of
            0: Result := Grid[PrevRow][Prev];
            1: Result := Grid[PrevRow][J];
            2: Result := Grid[I][Prev];
            3: Result := Grid[NextRow][Prev];
            4: Result := Grid[NextRow][J];
          end;
        end;
        // Ucinamy lewa strone
        1: begin
          case Random(5) of
            0: Result := Grid[PrevRow][J];
            1: Result := Grid[PrevRow][Next];
            2: Result := Grid[I][Next];
            3: Result := Grid[NextRow][J];
            4: Result := Grid[NextRow][Next];
          end;
        end;
        // Ucinamy gore
        2: begin
          case Random(5) of
            0: Result := Grid[I][Prev];
            1: Result := Grid[I][Next];
            2: Result := Grid[NextRow][Prev];
            3: Result := Grid[NextRow][J];
            4: Result := Grid[NextRow][Next];
          end;
        end;
        // Ucinamy dol
        3: begin
          case Random(5) of
            0: Result := Grid[PrevRow][Prev];
            1: Result := Grid[PrevRow][J];
            2: Result := Grid[PrevRow][Next];
            3: Result := Grid[I][Prev];
            4: Result := Grid[I][Next];
          end;
        end;
      end;
    end;
    3: begin
      // Hexagonal Left
      case Random(6) of
        0: Result := Grid[PrevRow][J];
        1: Result := Grid[PrevRow][Next];
        2: Result := Grid[I][Prev];
        3: Result := Grid[I][Next];
        4: Result := Grid[NextRow][Prev];
        5: Result := Grid[NextRow][J];
      end;
    end;
    4: begin
      // Hexagonal Right
      case Random(6) of
        0: Result := Grid[PrevRow][Prev];
        1: Result := Grid[PrevRow][J];
        2: Result := Grid[I][Prev];
        3: Result := Grid[I][Next];
        4: Result := Grid[NextRow][J];
        5: Result := Grid[NextRow][Next];
      end;
    end;
    5: begin
      // Hexagonal Random
      case Random(2) of
        0: begin
          case Random(6) of
            0: Result := Grid[PrevRow][J];
            1: Result := Grid[PrevRow][Next];
            2: Result := Grid[I][Prev];
            3: Result := Grid[I][Next];
            4: Result := Grid[NextRow][Prev];
            5: Result := Grid[NextRow][J];
          end;
        end;
        1: begin
          case Random(6) of
            0: Result := Grid[PrevRow][Prev];
            1: Result := Grid[PrevRow][J];
            2: Result := Grid[I][Prev];
            3: Result := Grid[I][Next];
            4: Result := Grid[NextRow][J];
            5: Result := Grid[NextRow][Next];
          end;
        end;
      end;
    end;
  end;
end;

function TCellularAutomata.CalculateEnergy(I, J, pValue : Integer): Integer;
var PrevRow, NextRow, Prev, Next : Integer;
begin
  Result := 0;

  PrevRow := I - 1;
  NextRow := I + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if I = 0 then begin
        PrevRow := High(Grid);
      end else
      if I = High(Grid) then begin
        NextRow := Low(Grid);
      end;
    end;
    // Absorbujace
    1:
      if I = 0 then begin
        PrevRow := 0;
      end else
      if I = High(Grid) then begin
        NextRow := High(Grid);
      end;
  end;

  Prev := J - 1;
  Next := J + 1;

  case BoundaryCondition of
    // Periodyczne
    0: begin
      if J = 0 then begin
        Prev := High(Grid[0]);
      end else
      if J = High(Grid[0]) then begin
        Next := Low(Grid[0]);
      end;
    end;
    // Absorbujace
    1:
      if J = 0 then begin
        Prev := 0;
      end else
      if J = High(Grid[0]) then begin
        Next := High(Grid[0]);
      end;
  end;

  case BoundaryCondition of
    0: begin
      // Moore
      if Grid[PrevRow][Prev] <> pValue then Result := Result + 1;
      if Grid[PrevRow][J] <> pValue then Result := Result + 1;
      if Grid[PrevRow][Next] <> pValue then Result := Result + 1;
      if Grid[I][Prev] <> pValue then Result := Result + 1;
      if Grid[I][Next] <> pValue then Result := Result + 1;
      if Grid[NextRow][Prev] <> pValue then Result := Result + 1;
      if Grid[NextRow][J] <> pValue then Result := Result + 1;
      if Grid[NextRow][Next] <> pValue then Result := Result + 1;
    end;
    1: begin
      // von Neumann
      if Grid[I][Prev] <> pValue then Result := Result + 1;
      if Grid[I][Next] <> pValue then Result := Result + 1;
      if Grid[PrevRow][J] <> pValue then Result := Result + 1;
      if Grid[NextRow][J] <> pValue then Result := Result + 1;
    end;
    2: begin
      // Pentagonal Random
      case Random(4) of
        0: begin
          if Grid[PrevRow][Prev] <> pValue then Result := Result + 1;
          if Grid[PrevRow][J] <> pValue then Result := Result + 1;
          if Grid[I][Prev] <> pValue then Result := Result + 1;
          if Grid[NextRow][Prev] <> pValue then Result := Result + 1;
          if Grid[NextRow][J] <> pValue then Result := Result + 1;
        end;
        1: begin
          if Grid[PrevRow][J] <> pValue then Result := Result + 1;
          if Grid[PrevRow][Next] <> pValue then Result := Result + 1;
          if Grid[I][Next] <> pValue then Result := Result + 1;
          if Grid[NextRow][J] <> pValue then Result := Result + 1;
          if Grid[NextRow][Next] <> pValue then Result := Result + 1;
        end;
        2: begin
          if Grid[I][Prev] <> pValue then Result := Result + 1;
          if Grid[I][Next] <> pValue then Result := Result + 1;
          if Grid[NextRow][Prev] <> pValue then Result := Result + 1;
          if Grid[NextRow][J] <> pValue then Result := Result + 1;
          if Grid[NextRow][Next] <> pValue then Result := Result + 1;
        end;
        3: begin
          if Grid[PrevRow][Prev] <> pValue then Result := Result + 1;
          if Grid[PrevRow][J] <> pValue then Result := Result + 1;
          if Grid[PrevRow][Next] <> pValue then Result := Result + 1;
          if Grid[I][Prev] <> pValue then Result := Result + 1;
          if Grid[I][Next] <> pValue then Result := Result + 1;
        end;
      end;
    end;
    3: begin
      // Hexagonal Left
      if Grid[PrevRow][J] <> pValue then Result := Result + 1;
      if Grid[PrevRow][Next] <> pValue then Result := Result + 1;
      if Grid[I][Prev] <> pValue then Result := Result + 1;
      if Grid[I][Next] <> pValue then Result := Result + 1;
      if Grid[NextRow][Prev] <> pValue then Result := Result + 1;
      if Grid[NextRow][J] <> pValue then Result := Result + 1;
    end;
    4: begin
      // Hexagonal Right
      if Grid[PrevRow][Prev] <> pValue then Result := Result + 1;
      if Grid[PrevRow][J] <> pValue then Result := Result + 1;
      if Grid[I][Prev] <> pValue then Result := Result + 1;
      if Grid[I][Next] <> pValue then Result := Result + 1;
      if Grid[NextRow][J] <> pValue then Result := Result + 1;
      if Grid[NextRow][Next] <> pValue then Result := Result + 1;
    end;
    5: begin
      // Hexagonal Random
      case Random(2) of
        0: begin
          if Grid[PrevRow][J] <> pValue then Result := Result + 1;
          if Grid[PrevRow][Next] <> pValue then Result := Result + 1;
          if Grid[I][Prev] <> pValue then Result := Result + 1;
          if Grid[I][Next] <> pValue then Result := Result + 1;
          if Grid[NextRow][Prev] <> pValue then Result := Result + 1;
          if Grid[NextRow][J] <> pValue then Result := Result + 1;
        end;
        1: begin
          if Grid[PrevRow][Prev] <> pValue then Result := Result + 1;
          if Grid[PrevRow][J] <> pValue then Result := Result + 1;
          if Grid[I][Prev] <> pValue then Result := Result + 1;
          if Grid[I][Next] <> pValue then Result := Result + 1;
          if Grid[NextRow][J] <> pValue then Result := Result + 1;
          if Grid[NextRow][Next] <> pValue then Result := Result + 1;
        end;
      end;
    end;
  end;
end;

procedure TCellularAutomata.PrepareOldGrid;
var I, J : Integer;
begin
  SetLength(OldGrid, High(Grid) + 1);
  for I := 0 to High(Grid) do begin
    SetLength(OldGrid[I], High(Grid[I]) + 1);
    for J := 0 to High(Grid[I]) do OldGrid[I][J] := 0;
  end;
end;

procedure TCellularAutomata.ClearOldGrid;
var I, J : Integer;
begin
  for I := 0 to High(OldGrid) do begin
    for J := 0 to High(OldGrid[I]) do OldGrid[I][J] := 0;
  end;
end;

procedure TCellularAutomata.PrepareMonteCarloGrid;
var I, J : Integer;
begin
  SetLength(GridMonteCarlo, High(Grid) + 1);
  for I := 0 to High(Grid) do begin
    SetLength(GridMonteCarlo[I], High(Grid[I]) + 1);
    for J := 0 to High(Grid[I]) do GridMonteCarlo[I][J] := False;
  end;
end;

procedure TCellularAutomata.PrepareGridDensity;
var I, J : Integer;
begin
  SetLength(GridDensity, High(Grid) + 1);
  for I := 0 to High(Grid) do begin
    SetLength(GridDensity[I], High(Grid[I]) + 1);
    for J := 0 to High(Grid[I]) do GridDensity[I][J] := 0.0;
  end;
end;

procedure TCellularAutomata.PrepareGridIsCrystalized;
var I, J : Integer;
begin
  SetLength(GridIsCrystalized, High(Grid) + 1);
  for I := 0 to High(Grid) do begin
    SetLength(GridIsCrystalized[I], High(Grid[I]) + 1);
    for J := 0 to High(Grid[I]) do GridIsCrystalized[I][J] := False;
  end;
end;

procedure TCellularAutomata.ClearMonteCarloGrid;
var I, J : Integer;
begin
  for I := 0 to High(GridMonteCarlo) do begin
    for J := 0 to High(GridMonteCarlo[I]) do GridMonteCarlo[I][J] := False;
  end;
end;

procedure TCellularAutomata.CleanGridDensity;
var I, J : Integer;
begin
  for I := 0 to High(GridDensity) do begin
    for J := 0 to High(GridDensity[I]) do GridDensity[I][J] := 0.0;
  end;
end;

procedure TCellularAutomata.CleanGridIsCrystalized;
var I, J : Integer;
begin
  for I := 0 to High(GridIsCrystalized) do begin
    for J := 0 to High(GridIsCrystalized[I]) do GridIsCrystalized[I][J] := False;
  end;
end;

procedure TCellularAutomata.PrepareGridGameOfLife(Iterations, Width : Integer);
var I : Integer;
begin
  FIterations := Iterations;
  FWidth := Width;

  SetLength(Grid, Iterations);
  for I := 0 to High(Grid) do begin
    SetLength(Grid[I], Width);
  end;

  PrepareOldGrid;
  PrepareMonteCarloGrid;
  PrepareGridDensity;
  PrepareGridIsCrystalized;
end;

function TCellularAutomata.GetValue(I, J: Integer): Integer;
begin
  Result := Grid[I][J];
end;

procedure TCellularAutomata.SetValue(I: Integer; J: Integer; Value: Integer);
begin
  Grid[I][J] := Value;
end;

end.
