#!/usr/bin/env wolframscript
(* ::Package:: *)

Print["Running tests..."];


Get["MatrixWrapper`",Path->Directory[]]


With[{tr=TestReport[FileNameJoin[{Directory[],"Tests.wlt"}]]},
  Exit@If[TrueQ@tr["AllTestsSucceeded"],
    Print["All tests passed!"];0,
    Print["Tests failed!\nID: ",tr["TestsFailedIndices"]];1
  ]
]
