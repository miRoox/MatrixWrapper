(* ::Package:: *)

BeginPackage["MatrixWrapper`"]


Mat::usage="Mat@data treat data as a matrix.
Mat@\"Column\"@data treat data as a column matrix.
Mat@\"Row\"@data treat data as a row matrix."
MatQ::usage="MatQ[expr] check whether expr is a correct Mat type matrix."
MatData::usage="MatData[mat] get raw data of Mat type matrix."


Mat::invdat="Format of `1` is invalid for Mat."


Begin["`Private`"]


strictMatrixQ[list_]:=ArrayQ[list,2]


scalerQ[expr_]:=!(ListQ[expr]||ArrayQ[expr]||MatchQ[expr,_Mat])


MatQ[expr_]:=MatchQ[expr,Mat[_?strictMatrixQ]]


MatData[Mat[data_]]:=data


Mat/:Exp[mat_Mat]:=Mat@MatrixExp@MatData[mat]
Mat/:Log[mat_Mat]:=Mat@MatrixLog@MatData[mat]
Mat/:Power[mat_Mat,-1]:=Mat@Inverse@MatData[mat]
Mat/:Power[mat_Mat,n_?scalerQ]:=Mat@MatrixPower[MatData[mat],n]
Mat/:f_Symbol[mat_Mat]/;MemberQ[Attributes[f],NumericFunction]:=Mat@MatrixFunction[f,MatData[mat]]
(*Mat/:f_Function[mat_Mat]*)


Mat/:Transpose[mat_Mat]:=Mat@Transpose@MatData[mat]
Mat/:Conjugate[mat_Mat]:=Mat@Conjugate@MatData[mat]
Mat/:ConjugateTranspose[mat_Mat]:=Mat@ConjugateTranspose@MatData[mat]
Mat/:Abs[mat_Mat]:=Mat@Abs@MatData[mat]
Mat/:Tr[mat_Mat]:=Tr@MatData[mat]
Mat/:Det[mat_Mat]:=Det@MatData[mat]
Mat/:Permanent[mat_Mat]:=Permanent@MatData[mat]
(*Mat/:f_[mat_Mat]:=Mat@f[data]*)


Mat[{{scaler_?scalerQ}}]:=scaler


Mat[vec_?VectorQ]:=Mat["Column"[vec]]
Mat["Column"[col_?VectorQ]]:=Mat[{col}//Transpose]
Mat["Row"[row_?VectorQ]]:=Mat[{row}]


Mat[data_/;MemberQ[data,_Mat,Infinity]]:=Mat[data/.{Mat->Identity}]


Mat[array_/;!strictMatrixQ[array]]:=With[{flat=ArrayFlatten[array]},
  If[TrueQ@strictMatrixQ[flat],
    Mat[flat],
    Message[Mat::invdat,array];$Failed
  ]
]


End[]


EndPackage[]
