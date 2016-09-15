module FSharp.Expandable.ActivePatterns

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast

type ParenRange =
  { LeftParenRange: Range.range
    RightParenRange: Range.range option
    Range: Range.range }

type TupleRange =
  { CommaRanges: Range.range list
    Range: Range.range }

type AppArg =
  | SingleUnit of Range.range
  | UnitWithParen of Range.range * ParenRange
  | TupledArgs of SynExpr list * TupleRange * ParenRange
  | SingleArgWithParen of SynExpr * ParenRange
  | SingleArg of SynExpr
  | CurriedArgs of SynExpr list

type AppLikeInfo =
  { ExprAtomicFlag: ExprAtomicFlag
    IsInfix: bool
    FuncExpr: SynExpr
    Arg: AppArg
    Range: Range.range }

let private collectArgsFromCurriedApps expr =
  let rec loop acc = function
  | SynExpr.App (_, _, funcExpr, argExpr, _) ->
      loop (argExpr::acc) funcExpr
  | expr ->
      (expr, acc)

  loop [] expr

let (|AppLike|_|) expr =
  match expr with
  | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->
      let f, arg =
        match argExpr with
        | SynExpr.App _ ->
            // f x y ...
            let f, args = collectArgsFromCurriedApps expr
            (f, CurriedArgs args)
        | SynExpr.Const(SynConst.Unit, range) ->
            // f ()
            (funcExpr, SingleUnit range)
        | SynExpr.Paren(SynExpr.Const(SynConst.Unit, range), left, right, prange) ->
            // f (())
            let arg = UnitWithParen(range, { LeftParenRange = left; RightParenRange = right; Range = prange })
            (funcExpr, arg)
        | SynExpr.Paren(SynExpr.Tuple(exprs, commaRange, range), left, right, prange) ->
            // f (x, y, ...)
            let args = TupledArgs(exprs, { CommaRanges = commaRange; Range = range }, { LeftParenRange = left; RightParenRange = right; Range = prange })
            (funcExpr, args)
        | SynExpr.Paren(expr, left, right, prange) ->
            // f (x)
            let arg = SingleArgWithParen(expr, { LeftParenRange = left; RightParenRange = right; Range = prange })
            (funcExpr, arg)
        | expr ->
            // f x
            (funcExpr, SingleArg expr)

      Some { ExprAtomicFlag = exprAtomicFlag
             IsInfix = isInfix
             FuncExpr = f
             Arg = arg
             Range = range}
  | _ ->
      None