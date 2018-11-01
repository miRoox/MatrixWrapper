(* ::Package:: *)

BeginPackage["MatrixWrapper`"]


Mat::usage="Mat@data treat data as a matrix."
MatQ::uasge="MatQ[expr] check whether expr is a correct Mat type matrix."


Begin["`Private`"]


strictMatrixQ[list_]:=ArrayQ[list,2]


scalerQ[expr_]:=NumericQ[expr]||MatchQ[expr,_Symbol]


MatQ[expr_]:=MatchQ[expr,Mat[_?strictMatrixQ]]


Mat[{{scaler_?scalerQ}}]:=scaler
Mat[scaler_?scalerQ]:=scaler


Mat/:Exp[Mat[data_]]:=Mat@MatrixExp[data]
Mat/:Log[Mat[data_]]:=Mat@MatrixLog[data]
Mat/:Power[Mat[data_],-1]:=Mat@Inverse[data]
Mat/:Power[Mat[data_],n_?scalerQ]:=Mat@MatrixPower[data,n]
Mat/:f_Symbol[Mat[data_]]/;MemberQ[Attributes[f],NumericFunction]:=Mat@MatrixFunction[f,data]
(*Mat/:f_Function[Mat[data_]]*)


Mat/:Transpose[Mat[data_]]:=Mat@Transpose[data]
Mat/:Conjugate[Mat[data_]]:=Mat@Conjugate[data]
Mat/:ConjugateTranspose[Mat[data_]]:=Mat@ConjugateTranspose[data]
Mat/:Abs[Mat[data_]]:=Mat@Abs[data]
Mat/:Tr[Mat[data_]]:=Tr[data]
Mat/:Det[Mat[data_]]:=Det[data]
Mat/:Permanent[Mat[data_]]:=Permanent[data]
Mat/:f_[Mat[data_]]:=Mat@f[data]


End[]


EndPackage[]
