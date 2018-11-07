BeginTestSection["Tests"]

VerificationTest[(* 1 *)
	Mat[List[List[1]]]
	,
	1	
	,
	TestID->"Scaler_1"
]

VerificationTest[(* 2 *)
	Mat[List[1, 2, 3]]
	,
	Mat[List[List[1], List[2], List[3]], Blank[]]	
	,
	TestID->"Vector_1"
]

VerificationTest[(* 3 *)
	Mat["Row"[List[1, 2, 3]]]
	,
	Mat[List[List[1, 2, 3]], Blank[]]	
	,
	TestID->"Vector_2"
]

VerificationTest[(* 4 *)
	Mat["Column"[List[1, 2, 3]]]
	,
	Mat[List[List[1], List[2], List[3]], Blank[]]	
	,
	TestID->"Vector_3"
]

VerificationTest[(* 5 *)
	With[List[Set[m, List[List[1, 2], List[3, 4]]]], Mat[List[List[m, 0], List[m, m]]]]
	,
	Mat[List[List[1, 2, 0, 0], List[3, 4, 0, 0], List[1, 2, 1, 2], List[3, 4, 3, 4]], Blank[]]	
	,
	TestID->"AutoFlatten_1"
]

VerificationTest[(* 6 *)
	With[List[Set[m, Mat[List[List[1, 2], List[3, 4]]]]], Mat[List[List[m, 0], List[m, m]]]]
	,
	Mat[List[List[1, 2, 0, 0], List[3, 4, 0, 0], List[1, 2, 1, 2], List[3, 4, 3, 4]], Blank[]]	
	,
	TestID->"AutoFlatten_2"
]

VerificationTest[(* 7 *)
	Mat[List[List[1, 2], List[3, 4, 5]]]
	,
	$Failed
	,
	{Mat::invdat}
	,
	TestID->"IrregularData_1"
]

VerificationTest[(* 8 *)
	Mat[List[List[1, 2], List[List[3, 4], 5]]]
	,
	$Failed
	,
	{Mat::invdat}
	,
	TestID->"IrregularData_2"
]

EndTestSection[]
