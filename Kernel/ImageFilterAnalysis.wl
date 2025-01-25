BeginPackage["FaizonZaman`ImageFilterAnalysis`"]

TestFilters::usage = "TestFilters[image] returns a gui for exploring different filters"
$DefaultTestFilters::usage = "$DefaultTestFilters returns a list of the available filters"
ImageFilterAnalysisData::usage = "ImageFilterAnalysisData contains analysis results"

Begin["`Private`"]

FilteredEntropy[image_Image] :=
	Function[GradientFilter[#, 1]] /* ImageAdjust /* Function[ImageMeasurements[#, "Entropy"]] @ image

$DefaultTestFilters = {
	(Blur[#1, 1]&) -> "Blur",
	(Sharpen[#1, 1]&) -> "Sharpen",
	(ImageAdjust[GaussianFilter[#1, 1, 1]]&) -> "Gaussian",
	(LaplacianFilter[#1, 1]&) -> "Laplacian",
	(MinFilter[#1, 1]&) -> "Min",
	(MaxFilter[#1, 1]&) -> "Max",
	(MeanFilter[#1, 1]&) -> "Mean",
	(MedianFilter[#1, 1]&) -> "Median",
	(CommonestFilter[#1, 1]&) -> "Commonest", (* FIXME: Image updates to a point then changes only appear when starting/stopping the process*)
	(ImageEffect[#1, "Charcoal"]&) -> "Charcoal",
	(ImageEffect[#1, "Embossing"]&) -> "Embossing"
};

(* TODO: #1 TestFilters should return a results object *)

Options[TestFilters] = {
	"Filters" -> Automatic
};
TestFilters[opts:OptionsPattern[{TestFilters}]] :=
	With[
		{defaultImage = ExampleData[{"TestImage", "TestPattern"}]},
		TestFilters[defaultImage,opts]
	]
TestFilters[exampleImageName_String, opts:OptionsPattern[{TestFilters}]] :=
	With[
		{defaultImage = ExampleData[{"TestImage", exampleImageName}]},
		TestFilters[defaultImage,opts]
	]

TestFilters[testImage_Image, opts:OptionsPattern[{TestFilters}]] :=
	DynamicModule[
		{
			toggle = False, tlabel, source = testImage, startEntropy, frame = 0, image, filter, data = {},
			msg = "",
			filters = Replace[OptionValue["Filters"], Automatic -> $DefaultTestFilters]
		},
		
		tlabel[True] = "Stop";
		tlabel[False] = "Start";
		image = source;
		startEntropy = FilteredEntropy[source];
		DynamicWrapper[
			Column[
				{
					Row[
						{
							Panel[
								Column[
									{
										Row[
											{
												Style["Select Filter", Bold],": ",
												PopupMenu[
													Dynamic[filter],
													filters
												]
											}
										],
										ButtonBar[
											{
												Dynamic[tlabel[toggle]] :> (msg="";toggle //= Not),
												"Reset" :> (frame = 0;image = source),
												"Bookmark" :> (
													AppendTo[data,
														<|
															"Source" -> source,
															"Processed" -> image,
															"Filter" -> Hold[Evaluate @ filter],
															"Frames" -> frame,
															ExtendedKey["Entropy","Start"] -> startEntropy, 
															ExtendedKey["Entropy", "End"] -> FilteredEntropy[image]
														|>
													];
												),

												"Save" :> (With[
													{sym= Unique["ifaResult"]},
													msg = Row[{Style["\[Checkmark]", Darker[Green]], " Saved to ", ToString[sym]}];
													sym = ImageFilterAnalysisData[<|"Source" -> source, "Filters"-> filters, "Data" -> ToTabular[data]|>];
												];)
											}
										]
									}
								]
							],
							" ",
							Column[
								{
									"",
									Dynamic[msg],
									""
								}
							]
						}
					]
					,
					Column[
						{
							Row[{"Frame: ", Dynamic[frame]}],
							Dynamic[Magnify[image, 1.96]]
						}
					]
				}
			]
			,
			If[toggle,
				frame++;
				With[{res = filter[image]},
					If[image != res,
						image = res
						,
						toggle = False
					]
				]
			]
		]
	];

ImageFilterAnalysisDataAscQ[asc_?AssociationQ] :=
	AllTrue[
		{"Source", "Filters", "Data"},
		KeyExistsQ[asc, #]&
	]

ImageFilterAnalysisDataAscQ[_] = False;

$thumbnail[i_Image] := $thumbnail[i] = Graphics[{Inset[i]}, ImageSize -> Dynamic[
	{(*this seems to be the standard icon size*)Automatic, 3.5 CurrentValue[
	"FontCapHeight"] / AbsoluteCurrentValue[Magnification]}]];


ImageFilterAnalysisData /: MakeBoxes[obj : ImageFilterAnalysisData[asc_?ImageFilterAnalysisDataAscQ], form : (StandardForm | TraditionalForm)] :=
	Module[{above, below},
		above =
			{
				{BoxForm`SummaryItem[{"MeanFrames: ", AggregateRows[asc["Data"], "MeanFrames" -> Function[Mean[#Frames]]] // Normal/*Values/*Flatten/*First}], SpanFromLeft},
				{BoxForm`SummaryItem[{"Filters: ", Length[asc["Filters"]]}]}
			};
		below ={};
		BoxForm`ArrangeSummaryBox[
			ImageFilterAnalysisData,
			obj,
			$thumbnail[asc["Source"]],
			above,
			below,
			form,
			"Interpretable" -> Automatic
		]
	];

ImageFilterAnalysisData[asc_?ImageFilterAnalysisDataAscQ][key_] := Lookup[asc,key]

End[] (* End `Private` *)

EndPackage[]
