(* ::Package:: *)

BeginPackage["MatrixWrapper`"]


Mat::usage="Mat@data treat data as a matrix.
Mat@\"Column\"@data treat data as a column matrix.
Mat@\"Row\"@data treat data as a row matrix.
Mat[data,patt] suppose every element in data matches patt."
MatQ::usage="MatQ[expr] check whether expr is a correct Mat type matrix."
MatData::usage="MatData[mat] get raw data of Mat type matrix."
MatElementsPattern::usage="MatElementsPattern[mat] get elements pattern of mat."


Mat::invdat="Format of `1` is invalid for Mat."


Begin["`Private`"]


strictMatrixQ[list_,Verbatim[_]]:=ArrayQ[list,2]
strictMatrixQ[list_,patt_]:=ArrayQ[list,2,MatchQ[patt]]
strictMatrixQ[patt_][list_]:=strictMatrixQ[list,patt]


scalerQ[expr_]:=!(ListQ[expr]||ArrayQ[expr]||MatchQ[expr,_Mat])


MatQ[expr_]:=MatchQ[expr,Mat[_?(strictMatrixQ@MatElementsPattern@expr)]]


MatData[Mat[data_,_]]:=data


MatElementsPattern[Mat[_,patt_]]:=patt


Mat/:Exp[mat_Mat]:=Mat[MatrixExp@MatData@mat,MatElementsPattern@mat]
Mat/:Log[mat_Mat]:=Mat[MatrixLog@MatData@mat,MatElementsPattern@mat]
Mat/:Power[mat_Mat,-1]:=Mat[Inverse@MatData@mat,MatElementsPattern@mat]
Mat/:Power[mat_Mat,n_?scalerQ]:=Mat[MatrixPower[MatData@mat,n],MatElementsPattern@mat]
Mat/:f_Symbol[mat_Mat]/;MemberQ[Attributes[f],NumericFunction]:=Mat[MatrixFunction[f,MatData@mat],MatElementsPattern@mat]
(*Mat/:f_Function[mat_Mat]*)


Mat/:Transpose[mat_Mat]:=Mat[Transpose@MatData@mat,MatElementsPattern@mat]
Mat/:Conjugate[mat_Mat]:=Mat[Conjugate@MatData@mat,MatElementsPattern@mat]
Mat/:ConjugateTranspose[mat_Mat]:=Mat[ConjugateTranspose@MatData@mat,MatElementsPattern@mat]
Mat/:Abs[mat_Mat]:=Mat[Abs@MatData@mat,MatElementsPattern@mat]
Mat/:Tr[mat_Mat]:=Tr@MatData@mat
Mat/:Det[mat_Mat]:=Det@MatData@mat
Mat/:Permanent[mat_Mat]:=Permanent@MatData@mat
(*Mat/:f_[mat_Mat]:=Mat@f[data]*)


Mat[data_]:=Mat[data,_]


Mat[{{scaler_?scalerQ}},patt_]:=If[MatchQ[scaler,patt],scaler,Message[Mat::invdat,{{scaler}}];$Failed]


Mat[vec_?VectorQ,patt_]:=Mat["Column"[vec],patt]
Mat["Column"[col_?VectorQ],patt_]:=Mat[{col}//Transpose,patt]
Mat["Row"[row_?VectorQ],patt_]:=Mat[{row},patt]


Mat[data_/;MemberQ[data,_Mat,Infinity],patt_]:=Mat[data/.{mat_Mat:>MatData@mat},patt]


Mat[array_,patt_]/;!strictMatrixQ[array,patt]:=With[{flat=ArrayFlatten[array]},
  If[TrueQ@strictMatrixQ[flat,patt],
    Mat[flat,patt],
    Message[Mat::invdat,array];$Failed
  ]
]


End[]


EndPackage[]
