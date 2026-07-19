/-! Haskell `show @Double` for Lean `Float`: the shortest decimal that
round-trips, formatted fixed for `0.1 ≤ |x| < 10^7` and scientific
(`d.ddde±e`, no `+`) otherwise — GHC's `showGFloat`. `Float.toString`'s
fixed six decimals are wrong outside a narrow range (`1e-7` would print
as `0.000000`), and interpreter output/Scheme literals must match the
Haskell oracle byte-for-byte.

Method: decompose `f = m53 * 2^e2` exactly (`frExp`), express it exactly
as `D * 10^E` with integer `D` (multiplying by `5^(-e2)` when `e2 < 0`),
then search precisions 1..17 for the shortest half-even rounding whose
`Float.ofScientific` parse-back equals `f`. -/

namespace Malgo

private def natDigits (n : Nat) : Nat :=
  (toString n).length

/-- Round `d` down to `keep = natDigits d - drop` significant digits,
half-even. Returns the rounded digits (possibly `keep + 1` digits after a
`999 → 1000` carry). -/
private def roundHalfEven (d : Nat) (drop : Nat) : Nat :=
  if drop == 0 then d
  else
    let p := 10 ^ drop
    let q := d / p
    let r := d % p
    let half := p / 2
    if r > half then q + 1
    else if r < half then q
    else if q % 2 == 1 then q + 1 else q

private def parseBack (mantissa : Nat) (exp10 : Int) : Float :=
  if exp10 ≥ 0 then
    Float.ofScientific mantissa false exp10.toNat
  else
    Float.ofScientific mantissa true (-exp10).toNat

/-- Shortest `(digits, exp10)` with `digits * 10^exp10` parsing back to
`f` (positive finite `f`). -/
private def shortestDigits (f : Float) : Nat × Int := Id.run do
  -- Exact decomposition f = m53 * 2^e2.
  let (m, e) := f.frExp
  let m53 := (m * Float.ofNat (2 ^ 53)).toUInt64.toNat
  let e2 : Int := e - 53
  -- Exact decimal form f = d0 * 10^exp0.
  let (d0, exp0) : Nat × Int :=
    if e2 ≥ 0 then (m53 * 2 ^ e2.toNat, 0)
    else (m53 * 5 ^ (-e2).toNat, e2)
  let nd := natDigits d0
  for p in [1 : min nd 17 + 1] do
    let drop := nd - p
    let mut cand := roundHalfEven d0 drop
    let mut candExp := exp0 + drop
    -- Renormalize a rounding carry (e.g. 999 → 1000).
    if natDigits cand > p then
      cand := cand / 10
      candExp := candExp + 1
    if parseBack cand candExp == f then
      -- Strip trailing zeros the rounding may have produced (keeps the
      -- digit string canonical; the value is unchanged).
      let mut c := cand
      let mut ce := candExp
      while c % 10 == 0 && c > 9 do
        c := c / 10
        ce := ce + 1
      return (c, ce)
  return (d0, exp0)

/-- Haskell `show` for a `Float` (GHC `showGFloat`). -/
def haskellShowFloat (f : Float) : String :=
  if f.isNaN then "NaN"
  else if f.isInf then (if f < 0 then "-Infinity" else "Infinity")
  else
    let negative := f.toBits >>> 63 == 1
    let sign := if negative then "-" else ""
    let a := f.abs
    if a == 0 then sign ++ "0.0"
    else
      let (digits, exp10) := shortestDigits a
      let ds := toString digits
      -- Decimal-point position: a = 0.ds * 10^(pointPow + 1).
      let pointPow : Int := (ds.length - 1 : Int) + exp10
      if 0.1 ≤ a && a < 1e7 then
        -- Fixed notation, at least one fractional digit.
        if pointPow ≥ 0 then
          let ip := pointPow.toNat + 1
          let intPart :=
            if ds.length ≥ ip then ds.take ip
            else ds ++ String.ofList (List.replicate (ip - ds.length) '0')
          let fracPart := if ds.length > ip then toString (ds.drop ip) else "0"
          sign ++ toString intPart ++ "." ++ fracPart
        else
          sign ++ "0." ++ String.ofList (List.replicate (-pointPow - 1).toNat '0') ++ ds
      else
        -- Scientific: d.ddd e±e, mantissa has at least one fractional digit.
        let headDigit := ds.take 1
        let rest := ds.drop 1
        let mantissa :=
          toString headDigit ++ "." ++ (if rest.isEmpty then "0" else toString rest)
        sign ++ mantissa ++ "e" ++ toString pointPow

#guard haskellShowFloat 0.0 == "0.0"
#guard haskellShowFloat 3.14 == "3.14"
#guard haskellShowFloat 0.25 == "0.25"
#guard haskellShowFloat 0.1 == "0.1"
#guard haskellShowFloat 1.0 == "1.0"
#guard haskellShowFloat (-2.5) == "-2.5"
#guard haskellShowFloat 100.0 == "100.0"
#guard haskellShowFloat 1e-7 == "1.0e-7"
#guard haskellShowFloat 1e20 == "1.0e20"
#guard haskellShowFloat 0.09 == "9.0e-2"
#guard haskellShowFloat 123456.789 == "123456.789"
#guard haskellShowFloat 12345678.9 == "1.23456789e7"
#guard haskellShowFloat (0.1 + 0.2) == "0.30000000000000004"
#guard haskellShowFloat (1.0 / 3.0) == "0.3333333333333333"

/-! Haskell `show @Float` (32-bit) for Lean `Float32`. Same method as
`haskellShowFloat` above (`frExp`, exact decimal decomposition, shortest
half-even-rounded precision that round-trips), but every step stays in
32-bit precision: a `Float32` mantissa only has 24 significant bits (not
53), and the round-trip check parses back as `Float32`, not `Float`.
Widening to `Float.toFloat` before formatting (the bug this fixes)
reproduces the extra noise digits introduced by the widening itself, e.g.
`(3.14 : Float32).toFloat` prints as `3.140000104904175` instead of
`3.14`. -/

private def parseBack32 (mantissa : Nat) (exp10 : Int) : Float32 :=
  if exp10 ≥ 0 then
    Float32.ofScientific mantissa false exp10.toNat
  else
    Float32.ofScientific mantissa true (-exp10).toNat

/-- Shortest `(digits, exp10)` with `digits * 10^exp10` parsing back to
`f` (positive finite `f`, 32-bit). -/
private def shortestDigits32 (f : Float32) : Nat × Int := Id.run do
  -- Exact decomposition f = m24 * 2^e2.
  let (m, e) := f.frExp
  let m24 := (m * Float32.ofNat (2 ^ 24)).toUInt64.toNat
  let e2 : Int := e - 24
  -- Exact decimal form f = d0 * 10^exp0.
  let (d0, exp0) : Nat × Int :=
    if e2 ≥ 0 then (m24 * 2 ^ e2.toNat, 0)
    else (m24 * 5 ^ (-e2).toNat, e2)
  let nd := natDigits d0
  for p in [1 : min nd 9 + 1] do
    let drop := nd - p
    let mut cand := roundHalfEven d0 drop
    let mut candExp := exp0 + drop
    -- Renormalize a rounding carry (e.g. 999 → 1000).
    if natDigits cand > p then
      cand := cand / 10
      candExp := candExp + 1
    if parseBack32 cand candExp == f then
      -- Strip trailing zeros the rounding may have produced (keeps the
      -- digit string canonical; the value is unchanged).
      let mut c := cand
      let mut ce := candExp
      while c % 10 == 0 && c > 9 do
        c := c / 10
        ce := ce + 1
      return (c, ce)
  return (d0, exp0)

/-- Haskell `show` for a `Float` (32-bit, GHC `showGFloat`). -/
def haskellShowFloat32 (f : Float32) : String :=
  if f.isNaN then "NaN"
  else if f.isInf then (if f < 0 then "-Infinity" else "Infinity")
  else
    let negative := f.toBits >>> 31 == 1
    let sign := if negative then "-" else ""
    let a := f.abs
    if a == 0 then sign ++ "0.0"
    else
      let (digits, exp10) := shortestDigits32 a
      let ds := toString digits
      -- Decimal-point position: a = 0.ds * 10^(pointPow + 1).
      let pointPow : Int := (ds.length - 1 : Int) + exp10
      if (0.1 : Float32) ≤ a && a < (1e7 : Float32) then
        -- Fixed notation, at least one fractional digit.
        if pointPow ≥ 0 then
          let ip := pointPow.toNat + 1
          let intPart :=
            if ds.length ≥ ip then ds.take ip
            else ds ++ String.ofList (List.replicate (ip - ds.length) '0')
          let fracPart := if ds.length > ip then toString (ds.drop ip) else "0"
          sign ++ toString intPart ++ "." ++ fracPart
        else
          sign ++ "0." ++ String.ofList (List.replicate (-pointPow - 1).toNat '0') ++ ds
      else
        -- Scientific: d.ddd e±e, mantissa has at least one fractional digit.
        let headDigit := ds.take 1
        let rest := ds.drop 1
        let mantissa :=
          toString headDigit ++ "." ++ (if rest.isEmpty then "0" else toString rest)
        sign ++ mantissa ++ "e" ++ toString pointPow

#guard haskellShowFloat32 0.0 == "0.0"
#guard haskellShowFloat32 3.14 == "3.14"
#guard haskellShowFloat32 0.25 == "0.25"
#guard haskellShowFloat32 1.0 == "1.0"
#guard haskellShowFloat32 (-2.5) == "-2.5"
#guard haskellShowFloat32 100.0 == "100.0"
#guard haskellShowFloat32 1e20 == "1.0e20"

end Malgo
