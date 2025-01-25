BeginPackage["FaizonZaman`ImageFilterAnalysis`"]

(* Declare your package's public symbols here. *)

TestFilters::usage = "TestFilters[image] returns a gui for exploring different filters"
$DefaultTestFilters::usage = "$DefaultTestFilters returns a list of the available filters"

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
								Row[
									{
										ButtonBar[
											{
												Dynamic[tlabel[toggle]] :> (toggle //= Not),
												"Reset" :> (frame = 0;image = source),
												"Bookmark" :> (
													AppendTo[data,
														<|
															"Source" -> source,
															"Processed" -> image,
															"Filter" -> Hold[Evaluate @ filter],
															"Frames" -> frame,
															"Entropy" -> <|"Start" -> startEntropy, "End" -> FilteredEntropy[image]|>
														|>
													];
												),
												"Data" :> (CellPrint[{data}];),
												"Dataset" :> (CellPrint[Dataset[data]];)
											}
										],
										PopupMenu[
											Dynamic[filter],
											filters
										]
									}
								]
							],
							" ",
							Row[{"Frame: ", Dynamic[frame]}]
						}
					]
					,
					Dynamic[Magnify[image, 1.96]]
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

End[] (* End `Private` *)

EndPackage[]
