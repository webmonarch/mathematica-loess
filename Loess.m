(* ::Package:: *)

(* ::Title:: *)
(*Loess` Package*)


BeginPackage["Loess`"];


(* ::Subtitle:: *)
(*Loess*)


Loess::usage="y = Loess[x, {{x1,y1},..}, \[Lambda], \[Alpha]].";


(* ::Subtitle:: *)
(*Tricube Weight*)


TricubeWeight::usage="Tricube weight function for Loess.
See http://en.wikipedia.org/wiki/Local_regression#Weight_function";


EndPackage[];


(* ::Title:: *)
(*Loess`Private` Package*)


Begin["Loess`Private`"];


(* ::Subtitle:: *)
(*Loess*)


Loess[x_?NumericQ,points:{{_?NumericQ,_?NumericQ}..},\[Lambda]_Integer /; 0<=\[Lambda]<=3,\[Alpha]_?NumericQ/;0<\[Alpha]<1]:=(
	Block[{n,delta,idxs,model,range,weights,fit},
		(* Test \[Alpha] range *)
		If[\[Alpha]<=(\[Lambda]+1)/Length[points],
			Message[Loess::alpharange,\[Alpha]],
			True
		];
  
		(* Determine points in the regression subset *)
		n = Ceiling[\[Alpha] Length[points]];
  
		delta = points;
		delta[[All,1]] = delta[[All,1]]-x;
		idxs = Ordering[delta, n, Abs[#1[[1]]]<Abs[#2[[1]]]&];
		delta = delta[[idxs]];

		(* Find fit *)
		model = Table[z^n,{n,0,\[Lambda]}];
		range = Max[Abs /@ delta[[All,1]]];
		weights = TricubeWeight/@(delta[[All,1]]/range);
		fit = LinearModelFit[points[[idxs]],model,z,Weights->weights];

		fit[x]
	]
);


Loess::alpharange="\[Alpha] `1` out of range (\!\(\*FractionBox[\(\[Lambda] + 1\), \(Length[points]\)]\),1)";


(* ::Subtitle:: *)
(*Tricube Weight*)


TricubeWeight[x_]:= Piecewise[{{(1-Abs[x]^3)^3,Abs[x]< 1}}];


End[];
