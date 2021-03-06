BeginTestSection["Tests"]

BeginTestSection["Construct"]

VerificationTest[(* 1 *)
	Mat[List[List[1]]]
	,
	1	
	,
	TestID->"Scaler_1"
]

VerificationTest[(* 2 *)
	Mat[List[List[List[1, 2]]], Blank[List]]
	,
	List[1, 2]	
	,
	TestID->"Scaler_2"
]

VerificationTest[(* 3 *)
	Mat[List[1, 2, 3]]
	,
	Mat[List[List[1], List[2], List[3]], Blank[]]	
	,
	TestID->"Vector_1"
]

VerificationTest[(* 4 *)
	Mat["Row"[List[1, 2, 3]]]
	,
	Mat[List[List[1, 2, 3]], Blank[]]	
	,
	TestID->"Vector_2"
]

VerificationTest[(* 5 *)
	Mat["Column"[List[1, 2, 3]]]
	,
	Mat[List[List[1], List[2], List[3]], Blank[]]	
	,
	TestID->"Vector_3"
]

VerificationTest[(* 6 *)
	With[List[Set[m, List[List[1, 2], List[3, 4]]]], Mat[List[List[m, 0], List[m, m]]]]
	,
	Mat[List[List[1, 2, 0, 0], List[3, 4, 0, 0], List[1, 2, 1, 2], List[3, 4, 3, 4]], Blank[]]	
	,
	TestID->"AutoFlatten_1"
]

VerificationTest[(* 7 *)
	With[List[Set[m, Mat[List[List[1, 2], List[3, 4]]]]], Mat[List[List[m, 0], List[m, m]]]]
	,
	Mat[List[List[1, 2, 0, 0], List[3, 4, 0, 0], List[1, 2, 1, 2], List[3, 4, 3, 4]], Blank[]]	
	,
	TestID->"AutoFlatten_2"
]

VerificationTest[(* 8 *)
	Mat[List[List[1, 2], List[3, 4, 5]]]
	,
	$Failed
	,
	{Mat::invdat}
	,
	TestID->"IrregularData_1"
]

VerificationTest[(* 9 *)
	Mat[List[List[1, 2], List[List[3, 4], 5]]]
	,
	$Failed
	,
	{Mat::invdat}
	,
	TestID->"IrregularData_2"
]

VerificationTest[(* 10 *)
	Mat[List[List[List[1, 2]]]]
	,
	$Failed
	,
	{Mat::invdat}
	,
	TestID->"IrregularData_3"
]

VerificationTest[(* 11 *)
	Mat[List[List[1, 2], List[3, 4]], Blank[Real]]
	,
	$Failed
	,
	{Mat::invdat2}
	,
	TestID->"PatternConstraint_1"
]

VerificationTest[(* 12 *)
	Mat[List[List[1, 2], List[3, 4]], Blank[Integer]]
	,
	Mat[List[List[1, 2], List[3, 4]], Blank[Integer]]	
	,
	TestID->"PatternConstraint_2"
]

VerificationTest[(* 13 *)
	Mat[List[List[List[1, 2, 3], List[2, List[4, 6], 8]], List[List[3, 9], List[4]]], Blank[List]]
	,
	Mat[List[List[List[1, 2, 3], List[2, List[4, 6], 8]], List[List[3, 9], List[4]]], Blank[List]]	
	,
	TestID->"PatternConstraint_3"
]

VerificationTest[(* 14 *)
	With[List[Set[m, Mat[List[List[1, 2], List[3, 4]]]]], Mat[List[List[m, m], List[m, m]], Blank[Mat]]]
	,
	Mat[List[List[Mat[List[List[1, 2], List[3, 4]], Blank[]], Mat[List[List[1, 2], List[3, 4]], Blank[]]], List[Mat[List[List[1, 2], List[3, 4]], Blank[]], Mat[List[List[1, 2], List[3, 4]], Blank[]]]], Blank[Mat]]	
	,
	TestID->"PatternConstraint_4"
]

VerificationTest[(* 15 *)
	With[List[Set[m, Mat[List[List[1, 2], List[3, 4]]]]], Mat[List[List[m, m], List[m, m]], Blank[List]]]
	,
	$Failed
	,
	{Mat::invdat2}
	,
	TestID->"PatternConstraint_5"
]

EndTestSection[]

BeginTestSection["Format"]

VerificationTest[(* 16 *)
	ToExpression[TemplateBox[List[GridBox[List[List["1", "2"], List["3", "4"]]]], "Mat", RuleDelayed[DisplayFunction, Function[RowBox[List["(", Slot[1], ")"]]]], RuleDelayed[InterpretationFunction, Function[RowBox[List["Mat", "[", Slot[1], ",", "_", "]"]]]], Rule[Tooltip, Automatic]]]
	,
	Mat[List[List[1, 2], List[3, 4]], Blank[]]	
	,
	TestID->"Format_1"
]

VerificationTest[(* 17 *)
	ToExpression[TemplateBox[List[GridBox[List[List["1", "2"], List["3", "4"]]], "_"], "Mat", RuleDelayed[DisplayFunction, Function[SubscriptBox[RowBox[List["(", Slot[1], ")"]], FrameBox[Slot[2], Rule[RoundingRadius, 5], Rule[Background, RGBColor[0.87`, 0.94`, 1]], Rule[FrameStyle, GrayLevel[0.5`]], Rule[StripOnInput, False]]]]], RuleDelayed[InterpretationFunction, Function[RowBox[List["Mat", "[", Slot[1], ",", Slot[2], "]"]]]], Rule[Tooltip, Automatic]]]
	,
	Mat[List[List[1, 2], List[3, 4]], Blank[]]	
	,
	TestID->"Format_2"
]

VerificationTest[(* 18 *)
	ToExpression[TemplateBox[List[GridBox[List[List["1", "2"], List["3", "4"]]], "_Integer"], "Mat", RuleDelayed[DisplayFunction, Function[SubscriptBox[RowBox[List["(", Slot[1], ")"]], FrameBox[Slot[2], Rule[RoundingRadius, 5], Rule[Background, RGBColor[0.87`, 0.94`, 1]], Rule[FrameStyle, GrayLevel[0.5`]], Rule[StripOnInput, False]]]]], RuleDelayed[InterpretationFunction, Function[RowBox[List["Mat", "[", Slot[1], ",", Slot[2], "]"]]]], Rule[Tooltip, Automatic]]]
	,
	Mat[List[List[1, 2], List[3, 4]], Blank[Integer]]	
	,
	TestID->"Format_3"
]

EndTestSection[]

EndTestSection[]
