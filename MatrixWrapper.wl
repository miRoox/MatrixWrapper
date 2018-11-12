(* ::Package:: *)

(* ::Section::Closed:: *)
(*License*)


(* MIT License
 * 
 * Copyright (c) 2018 miRoox
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)


(* ::Title:: *)
(*MatrixWrapper*)


BeginPackage["MatrixWrapper`"]


Unprotect[`Mat]
ClearAll[`Mat]


(* ::Section:: *)
(*Interface*)


Mat::usage="Mat@data treat data as a matrix.
Mat@\"Column\"@data treat data as a column matrix.
Mat@\"Row\"@data treat data as a row matrix.
Mat[data,patt] suppose every element in data matches patt."
MatQ::usage="MatQ[expr] check whether expr is a correct Mat type matrix."
MatData::usage="MatData[mat] get raw data of Mat type matrix."
MatElementsPattern::usage="MatElementsPattern[mat] get elements pattern of mat."
MatrixFunctor::usage="MatrixFunctor[f] give the matrix form of a scaler function f."
RemoveMatrixWrapperPackage::usage="RemoveMatrixWrapperPackage[] remove this package."


Mat::invdat="Format of `1` is invalid for Mat."
Mat::invdat2="Format of `1` is invalid for Mat with pattern constraint `2`."


Options[MatrixFunctor]=Options[MatrixFunction]


(* ::Section:: *)
(*Implement*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*Utilities*)


scalerQ[expr_]:=!(ListQ[expr]||ArrayQ[expr]||MatchQ[expr,_Mat])


strictMatrixQ[list_,Verbatim[_]]:=ArrayQ[list,2,scalerQ]
strictMatrixQ[list_,patt_]:=ArrayQ[list,2,MatchQ[patt]]
strictMatrixQ[patt_][list_]:=strictMatrixQ[list,patt]


invalidDataMsg[data_,Verbatim[_]]:=Message[Mat::invdat,data]
invalidDataMsg[data_,patt_]:=Message[Mat::invdat2,data,patt]


MatQ[expr_]:=MatchQ[expr,Mat[_?(strictMatrixQ@MatElementsPattern@expr)]]


MatData[Mat[data_,_]]:=data


MatElementsPattern[Mat[_,patt_]]:=patt


MatrixFunctor[f_,opts:OptionsPattern[]][m_Mat]:=Mat[MatrixFunction[f,MatData@m,opts],MatElementsPattern@m]
MatrixFunctor[f_,opts:OptionsPattern[]][m_]:=MatrixFunction[f,m,opts]


(* ::Subsection::Closed:: *)
(*Unary*)


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


(* ::Subsection::Closed:: *)
(*Index*)


Mat/:mat_Mat[[i_]]:=mat[[i,All]]
Mat/:mat_Mat[[i_Integer,j_]]:=mat[[i;;i,j]]
Mat/:mat_Mat[[i_,j_Integer]]:=mat[[i,j;;j]]
Mat/:mat_Mat[[i_Integer,j_Integer]]:=mat[[i;;i,j;;j]]
Mat/:mat_Mat[[i_,j_]]:=Mat[MatData[mat][[i,j]],MatElementsPattern[mat]]
(*Mat/:mat_Mat[[i_,j_,rest__]]:=mat[[i,j]][[rest]]*)


(* ::Subsection::Closed:: *)
(*Format*)


Mat/:MakeBoxes[Mat[data_,Verbatim[_]],StandardForm]:=
  With[{databox=Map[ToBoxes,data,{2}]},
    TemplateBox[{GridBox[databox]},"Mat",
      DisplayFunction:>(RowBox[{"(",#1,")"}]&),
      InterpretationFunction:>(RowBox[{"Mat","[",#1,",","_","]"}]&),
      Tooltip->Automatic
    ]
  ]


Mat/:MakeBoxes[Mat[data_,patt_],StandardForm]:=
  With[{databox=Map[ToBoxes,data,{2}],pattbox=MakeBoxes[patt]},
    TemplateBox[{GridBox[databox],pattbox},"Mat",
      DisplayFunction:>(SubscriptBox[RowBox[{"(",#1,")"}],
          FrameBox[#2,RoundingRadius->5,Background->RGBColor[0.87, 0.94, 1],FrameStyle->GrayLevel[0.5],StripOnInput->False]]&),
      InterpretationFunction:>(RowBox[{"Mat","[",#1,",",#2,"]"}]&),
      Tooltip->Automatic
    ]
  ]


(* ::Subsection::Closed:: *)
(*Constructor*)


Mat[data_]:=Mat[data,_]


Mat[{{scaler_}},Verbatim[_]]:=If[scalerQ[scaler],scaler,invalidDataMsg[{{scaler}},_];$Failed]
Mat[{{scaler_}},patt_]:=If[MatchQ[scaler,patt],scaler,invalidDataMsg[{{scaler}},patt];$Failed]


Mat[vec_?VectorQ,patt_]:=Mat["Column"[vec],patt]
Mat["Column"[col_?VectorQ],patt_]:=Mat[{col}//Transpose,patt]
Mat["Row"[row_?VectorQ],patt_]:=Mat[{row},patt]


Mat[array_,patt_]/;!strictMatrixQ[array,patt]:=
  With[{flat=Quiet[ArrayFlatten[array/.{mat_Mat:>MatData@mat}],{ArrayFlatten::depth}]},
    If[TrueQ@strictMatrixQ[flat,patt],
      Mat[flat,patt],
      invalidDataMsg[array,patt];$Failed
    ]
  ]


(* ::Subsection::Closed:: *)
(*Remove Package*)


RemoveMatrixWrapperPackage[]:=(
  Unprotect[MatrixWrapper`Mat];
  Remove["MatrixWrapper`*"]
  )


(* ::Section::Closed:: *)
(*End*)


End[]


Protect[`Mat]


EndPackage[]
