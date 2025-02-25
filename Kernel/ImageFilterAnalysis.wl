BeginPackage["FaizonZaman`ImageFilterAnalysis`"]

TestFilters::usage = "TestFilters[image] returns a gui for exploring different filters"
RunImageFilter::usage="RunImageFilter[filter, image] repeated applies filter to image until a difference threshold is reached.\nRunImageFilter[filter] represents an operator form that can be applied to an Image."
$DefaultTestFilters::usage = "$DefaultTestFilters returns a list of the available filters"
ImageFilterAnalysisData::usage = "ImageFilterAnalysisData contains analysis results"

Begin["`Private`"]

FilteredEntropy[image_Image] :=
	Function[GradientFilter[#, 1]] /* ImageAdjust /* Function[ImageMeasurements[#, "Entropy"]] @ image

$DefaultTestFilters = {
	(Blur[#1, 2]&) -> "Blur",
	(Sharpen[#1, 2]&) -> "Sharpen",
	(ImageAdjust[GaussianFilter[#1,2]]&) -> "Gaussian",
	(LaplacianFilter[#1, 2]&) -> "Laplacian",
	(LaplacianGaussianFilter[#1, 2]&) -> "LaplacianGaussian",
	(MinFilter[#1, 2]&) -> "Min",
	(MaxFilter[#1, 2]&) -> "Max",
	(MeanFilter[#1, 2]&) -> "Mean",
	(MedianFilter[#1, 2]&) -> "Median",
	(CommonestFilter[#1, 2]&) -> "Commonest", (* FIXME: Image updates to a point then changes only appear when starting/stopping the process*)
	(ImageAdjust[GradientFilter[#1, 2]]&) -> "GradientAdjusted", (* FIXME: Image updates to a point then changes only appear when starting/stopping the process*)
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
			filters = Replace[OptionValue["Filters"], {Automatic -> $DefaultTestFilters, {All,userFilters_} :> Flatten[{$DefaultTestFilters, userFilters}]}]
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
				With[
					{res = filter[image]},
					(* If FixedPoint can reliably terminate on cycles, then the following code could be refactored to use FixedPoint. *)
					If[
						image != res,
						(* [^] IF the previous image and the current image are different *)
						image = res,
						(* [^] Replace the previous image with the current image *)
						toggle = False
						(* [^] If the images are the same, stop running *)
					]
				]
			]
		]
	];


ImageEntropyDifference[image1_, image2_] := Block[
  {
   eA = ImageMeasurements[image1, "Entropy"],
   eB = ImageMeasurements[image2, "Entropy"]
   },
  Abs[eB - eA]
  ]

Options[RunImageFilter] = {
	"DifferenceThreshold" -> Automatic
};

(* RunImageFilter[filter_Function][img_Image]:=
	RunImageFilter[filter, img] *)

RunImageFilter[filter_, image_Image, opts:OptionsPattern[RunImageFilter]]:= Block[
		{
			frameCount=0,
			source = image,
			threshold=Replace[OptionValue["DifferenceThreshold"],Automatic-> 0.01],
			res, first, last,
			data
		},
		res=NestWhileList[(frameCount++; filter[#]) &, image, Function[ImageEntropyDifference[#1, #2]] /* (Not@*LessThan[threshold]), 2];
		{first,last} = Comap[{First,Last}][res];
		data = <|
					"Source" -> first,
					"Processed" -> last,
					"Filter" -> Hold[Evaluate @ filter],
					"Frames" -> frameCount,
					"FrameList" -> Iconize[res,"FrameList"],
					ExtendedKey["Entropy","Start"] -> FilteredEntropy[first], 
					ExtendedKey["Entropy", "End"] -> FilteredEntropy[last]
				|>;
				
		ImageFilterAnalysisData[<|"Source" -> image,"Filters" -> filter,"Data" -> ToTabular[{data}]|>]
	]



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

ImageFilterAnalysisData[asc_?ImageFilterAnalysisDataAscQ]["Properties"] = {"Source","Filters","Data"};
ImageFilterAnalysisData[asc_?ImageFilterAnalysisDataAscQ][key_] := Lookup[asc,key]

End[] (* End `Private` *)

EndPackage[]
