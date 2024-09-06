(*
MIT License

Copyright (c) 2019 - 2023 RINGS CE - Community Edition

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

type bop =
  | Add
  | Mult
  | Leq
  | Min
  | Div

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Float of float
  | Double of double
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | While of expr * expr
  | For of expr * expr
  | Do of expr * expr
  | Call of string list * expr list
  | Lambda of string * expr * expr
