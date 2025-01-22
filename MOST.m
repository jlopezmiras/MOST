(* ::Package:: *)

(* ::Title:: *)
(*MOST*)


(* ::Text:: *)
(*Authors: J. L. Miras, F. Vilches*)
(*Date: 2024*)


BeginPackage["MOST`"];


$ContextPath = Join[{"FeynCalc`","FeynArts`","KinematicSubstitution`"},$ContextPath];


(* ::Section:: *)
(*Declaration of variables*)


inv\[CapitalLambda]::usage = "Parameter representing the inverse of the cut-off of the EFT"

RedBasis
RedBasisDebug

MassReduction
AllMassReduction

AmplitudeComputation::usage = "AmplitudeComputation[process,EFTOrder,model] computes the amplitude of the given process (list of incoming particles)
up to a given EFT order in a specific model"
SeparateKinStructs

PropagatorAttributes::usage = "PropagatorAttributes[field,EFTOrder,model]"
AllPropagatorAttributes

EFTSeries
EFTSeriesRational
PerturbativeExpand
SolvePerturbative
ExplicitEFTOrder

mphys2
Z
Prop
mbare
mInternal
MPhysSymbol
Bare2PhysMass

GetFields
GetExternalMasses
GetInternalMasses
ProcessList
DefineCoefficient
CoeffList
Coeff
TrueEFTOrder

EFTOrderSust

ampFull
ampPhys
matchEqs

NumArgs
IdentifyKinematics
EvaluateTerms
IdentifyKinematics::nosymb="The argument `1` needs to be either None or a symbol"
RecoverKinematics

FormatCoeff

RedBasisOld
AmplitudeComputationOld


(* ::Section:: *)
(*Loading Packages*)


Block[{fontfamily="Alegreya SC", bigSize=32, smallSize=30, colorize},
	
	colorize[str_String,cf_]:=Row@MapThread[Style,{#,cf/@Subdivide[Length@#-1]}&@Characters@str]~ToString~StandardForm;

	Print[  Style[colorize["MOST:",ColorData[{"ValentineTones",{0,1.2}}]], "Text", Bold, bigSize, FontFamily->fontfamily],
			Style["   a ", "Text", smallSize, Italic, FontFamily->fontfamily], 
			Style["M", "Text",RGBColor[0.52, 0.11, 0.20], Bold, bigSize, Italic, FontFamily->fontfamily],
			Style["atching ", "Text", smallSize, Italic, FontFamily->fontfamily], 
			Style["O", "Text",RGBColor[0.62, 0.17, 0.27], Bold, bigSize, Italic, FontFamily->fontfamily],
			Style["n-", "Text", smallSize, Italic, FontFamily->fontfamily], 
			Style["S", "Text", RGBColor[0.71, 0.30, 0.40], Bold, bigSize, Italic, FontFamily->fontfamily],
			Style["hell ", "Text", smallSize, Italic, FontFamily->fontfamily], 
			Style["T", "Text", RGBColor[0.83, 0.49, 0.58], Bold, bigSize, Italic, FontFamily->fontfamily],
			Style["ool", "Text", smallSize, Italic, FontFamily->fontfamily]
	];
	
	Print[ Style["by Javier L\[OAcute]pez Miras and Fuensanta Vilches"]]
]


Begin["`Private`"]


N[153/256]


RGBColor[1,0.45,0.19]


(* ::Section:: *)
(*Basis Reduction*)


AllMassReduction[modelfull_, modelphys_, EFTOrder_]:=Module[{fields},

	fields = GetFields[modelfull];
	Return[DeleteDuplicates@Flatten[MassReduction[#,modelfull,modelphys,EFTOrder]&/@fields]];

];

MassReduction[field_, modelfull_, modelphys_, EFTOrder_]:=Module[{massphys,solveparameters,massphyssol},
	
	If[!allPropComputed[#], AllPropagatorAttributes[#,EFTOrder]]&/@{modelfull,modelphys};
	
	(*Change mass in the physical basis for its perturbative expansion*)
	massphys = ExplicitEFTOrder@PerturbativeExpand[mphys2[field, modelphys],EFTOrder,modelphys];
	
	(*Parameters to solve in modelphys*)
	solveparameters = Flatten[PerturbativeExpand[CoeffList[modelphys],EFTOrder,modelphys]/.{Plus->List}];
	
	(*First order solved apart, since it is quadratic. We keep Subscript[mbare, phys]^(4)->Subscript[mbare, full]^(4)*)
	GetExternalMasses/@{modelfull,modelphys};
	massphyssol = {mbare[field,modelphys]->mbare[field,modelfull]};
	
	massphyssol=Join[massphyssol,
						SolvePerturbative[(massphys/.massphyssol)==mphys2[field,modelfull],solveparameters]];
						
	Return[massphyssol];
]


Options[RedBasis] = {CoefDimensions->{}, Process->{}};
RedBasis[modelfull_, modelphys_, EFTOrder_, OptionsPattern[]]:=Module[{reduction={}, ampsNumeric},
	
	(*Computation of physical poles, residues and propagators*)
	AllPropagatorAttributes[modelfull,EFTOrder];
	AllPropagatorAttributes[modelphys,EFTOrder];
	
	reduction = Join[reduction, AllMassReduction[modelfull,modelphys,EFTOrder]];
	
	(*Get processes and order them with respect to number of particles and a greater number of identical particles*)
	processes = SortBy[DeleteCases[ProcessList[modelphys],{_,_}], {Length,Last/@Tally[#]&}];
	(*processes = DeleteCases[ProcessList[modelphys],{_,_}];*)
	(*Sort each process so that we have first fermions, then vectors and finally scalars*)
	processesSorted = SortBy[#,Position[{F,V,S},Head[-#/.{-f_:>f}]]&]&/@ processes;
	
	(*List of vertices with coefficients appearing in their Feynman rules*)
	CoeffInteraction=Association@@Cases[M$CouplingMatrices,C[int__] == coeffs_:>{int}->Variables[coeffs]];
	
	Do[
	
	Echo[processesSorted[[k]],"Computing process: "];
	Echo[CoeffInteraction[processes[[k]]],"Appearing couplings: "];
	
	(*If all appearing couplings are already computed, skip this process*)
	If[
		FreeQ[MemberQ[Keys[reduction], #]&/@ 
				Flatten@(PerturbativeExpand[CoeffInteraction[processes[[k]]]/.RenameCoeff[modelphys],EFTOrder,modelphys]/.Plus->List), False],
		Echo["Next process"];
		Continue[];
	];
	
	matchEqs={};
	(*Computation of amplitudes*)
		{ampFull, crossingsFull} = EchoTiming[AmplitudeComputation[processesSorted[[k]],EFTOrder,modelfull,SeparateCrossings->True],"Amplitude Full"];
		{ampPhys, crossingsPhys} = EchoTiming[AmplitudeComputation[processesSorted[[k]],EFTOrder,modelphys,SeparateCrossings->True],"Amplitude Phys"];
		
	(*Change bare masses by physical ones*)
		ampFull = ampFull/.Flatten@{#[[1]]^n_:>EFTSeries[#[[2]]^n,EFTOrder]&/@Flatten[{Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelfull]}],
							Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelfull]};
		ampPhys = ampPhys/.Flatten@{#[[1]]^n_:>EFTSeries[#[[2]]^n,EFTOrder]&/@Flatten[{Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelphys]}],
							Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelphys]};
		
		ampPhys = ExplicitEFTOrder @ PerturbativeExpand[ampPhys,EFTOrder,modelphys];
	(*EFT Series*)
		ampFull = EchoTiming[EFTSeries[ExplicitEFTOrder @ ampFull,EFTOrder],"EFTSeries in ampFull"];
		ampPhys = EchoTiming[EFTSeries[ampPhys/.reduction,EFTOrder],"EFTSeries in ampPhys and replace previous results"];
		
	(*Computation of rational kinematics*)
		(*Symbolic physical masses to be replaced with already computed mphys2 at the end*)
		(*If any of mphys2 is 0, we take advantage of this and put MPhysSymbol to 0 at this stage.
		Also, if physical masses of two different particles are the same, we put them equal*)
		masseslist = If[mphys2[#,modelfull]=!=0, MPhysSymbol[#], 0]&/@ processesSorted[[k]];
		masseslist = masseslist /. Select[
						Flatten@Replace[(DeleteDuplicates/@GatherBy[masseslist,#/.{MPhysSymbol[f_]:>mphys2[f,modelphys]}&]),
										{x_,y__}:>(#->x&/@List[y]),2], 
						Head[#]===Rule &];
		
		varsPhys = DeleteDuplicates@Cases[ampPhys,Coeff[___,modelphys],Infinity];
		nEqs = Max[Values[CountsBy[varsPhys,#[[2]]&]]]; (*Max number of coefficients with same EFTOrder*)
		
		(*First, flue back the full amplitude with all crossings*)
		ampFull = Quiet@Simplify[Plus@@UnDoCrossings[MomentumExpand@ampFull, crossingsFull], TimeConstraint->{0.01,1}];
		ampPhys = Quiet@Simplify[Plus@@UnDoCrossings[MomentumExpand@ampPhys, crossingsPhys], TimeConstraint->{0.01,1}];
		
		Do[
			ampsNumeric = EchoTiming[SustMomentaOptimized[{ampFull,ampPhys},##,masseslist]&@@(Count[processesSorted[[k]],#,Infinity,Heads->True]&/@{F,V,S}),"Substitution Momenta"];
			(*ampsNumeric = ampsNumeric /. MapThread[#1->#2&, {masseslist, EFTSeries[Sqrt@mphys2[#,modelfull],EFTOrder]&/@processes[[k]]}];
			ampsNumeric = EchoTiming[EFTSeries[EchoTiming[ExplicitEFTOrder @ ampsNumeric,"ExplicitEFTOrder"], EFTOrder],"Last EFT Series"];*)
			matchEqs = Join[matchEqs, MapThread[Equal,Transpose@ampsNumeric]],
		nEqs];
		
		red = EchoTiming[SolvePerturbative[ExplicitEFTOrder@matchEqs, varsPhys],"Solving system"];
		
		(*Symplifying the solution according to the EFT power of the reduced coefficients*)
		(*This could be achieved with a standard simplify, but due to the huge rational expressions
		this can last forever. However, by knowing the mass dimensions of the reduced coefficient
		and the coefficients appearing in the reduction we can perform a much faster Taylor expansion
		up to the highest power of mass that we can find in such reduction*)
		maxPowerOfMass = Max/@(Cases[#,Coeff[_,order_,___]:>order,{-2}]/.{}->{0}&/@ Values[red]);
		coeffPowerOfMass = TrueEFTOrder/@ Keys[red];
		

		red = MapThread[
			#1 -> Normal@Series[
					Expand[#2]/.{MPhysSymbol[a_]:> xxx MPhysSymbol[a], coeff:Coeff[c_,o_,modelphys] :> xxx^(o-TrueEFTOrder[coeff]) coeff},
					{xxx,0,#3}]&
				,{Keys[red], Values[red], maxPowerOfMass-coeffPowerOfMass}];
		
		reduction = Echo@Join[reduction, red/.{xxx->1}//Simplify];
		,
		
	{k,Length[processes]}];
	
	(*Replacing physical masses by bare masses assuming the latter are real-valued*)
	reduction = Assuming[
		And @@ (#>0&/@ Cases[DownValues[mbare],mbare[_,modelfull],Infinity]),

		reduction/.{MPhysSymbol[a_]:>EFTSeries[Sqrt[mphys2[a,modelfull]],EFTOrder]}
	];
	
	(*Join all EFT orders of the same coefficient*)
	reduction = ( # -> EFTSeries[
		ExplicitEFTOrder@PerturbativeExpand[#,EFTOrder,modelphys]/.reduction,EFTOrder]/.{inv\[CapitalLambda]->1}
			)&/@ CoeffList[modelphys];
			
	(*Printing the results*)
	Echo["Showing the reduction"];
	FormatCoeff[VisibleModel->False,VisibleOrder->False];
	Print /@ reduction;

	
Return[reduction]]


Options[RedBasis] = {CoefDimensions->{}, Process->{}};
RedBasis[modelfull_, modelphys_, EFTOrder_, OptionsPattern[]]:=Module[{reduction={}, ampsNumeric},

tIni = SessionTime[];
	
	(*Computation of physical poles, residues and propagators*)
	AllPropagatorAttributes[modelfull,EFTOrder];
	AllPropagatorAttributes[modelphys,EFTOrder];
	
	reduction = Join[reduction, AllMassReduction[modelfull,modelphys,EFTOrder]];
	
	(*Get processes and order them with respect to number of particles and a greater number of identical particles*)
	processes = SortBy[DeleteCases[ProcessList[modelphys],{_,_}], {Length,Last/@Tally[#]&}];
	(*processes = DeleteCases[ProcessList[modelphys],{_,_}];*)
	(*Sort each process so that we have first fermions, then vectors and finally scalars*)
	processesSorted = SortBy[#,Position[{F,V,S},Head[-#/.{-f_:>f}]]&]&/@ processes;
	
	(*List of vertices with coefficients appearing in their Feynman rules*)
	CoeffInteraction=Association@@Cases[M$CouplingMatrices,C[int__] == coeffs_:>{int}->Variables[coeffs]];
	
	Do[
	
	(*If all appearing couplings are already computed, skip this process*)
	If[
		FreeQ[MemberQ[Keys[reduction], #]&/@ 
				Flatten@(PerturbativeExpand[CoeffInteraction[processes[[k]]]/.RenameCoeff[modelphys],EFTOrder,modelphys]/.Plus->List), False],
		Continue[];
	];
	
	matchEqs={};
	(*Computation of amplitudes*)
		{ampFull, crossingsFull} = AmplitudeComputation[processesSorted[[k]],EFTOrder,modelfull,SeparateCrossings->True];
		{ampPhys, crossingsPhys} = AmplitudeComputation[processesSorted[[k]],EFTOrder,modelphys,SeparateCrossings->True];
		
	(*Change bare masses by physical ones*)
		ampFull = ampFull/.Flatten@{#[[1]]^n_:>EFTSeries[#[[2]]^n,EFTOrder]&/@Flatten[{Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelfull]}],
							Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelfull]};
		ampPhys = ampPhys/.Flatten@{#[[1]]^n_:>EFTSeries[#[[2]]^n,EFTOrder]&/@Flatten[{Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelphys]}],
							Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelphys]};
		
		ampPhys = ExplicitEFTOrder @ PerturbativeExpand[ampPhys,EFTOrder,modelphys];
	(*EFT Series*)
		ampFull = EFTSeries[ExplicitEFTOrder @ ampFull,EFTOrder];
		ampPhys = EFTSeries[ampPhys/.reduction,EFTOrder];
		
	(*Computation of rational kinematics*)
		(*Symbolic physical masses to be replaced with already computed mphys2 at the end*)
		(*If any of mphys2 is 0, we take advantage of this and put MPhysSymbol to 0 at this stage.
		Also, if physical masses of two different particles are the same, we put them equal*)
		masseslist = If[mphys2[#,modelfull]=!=0, MPhysSymbol[#], 0]&/@ processesSorted[[k]];
		masseslist = masseslist /. Select[
						Flatten@Replace[(DeleteDuplicates/@GatherBy[masseslist,#/.{MPhysSymbol[f_]:>mphys2[f,modelphys]}&]),
										{x_,y__}:>(#->x&/@List[y]),2], 
						Head[#]===Rule &];
		
		varsPhys = DeleteDuplicates@Cases[ampPhys,Coeff[___,modelphys],Infinity];
		nEqs = Max[Values[CountsBy[varsPhys,#[[2]]&]]]; (*Max number of coefficients with same EFTOrder*)
		
		(*First, flue back the full amplitude with all crossings*)
		ampFull = Quiet@Simplify[Plus@@UnDoCrossings[MomentumExpand@ampFull, crossingsFull], TimeConstraint->{0.01,1}];
		ampPhys = Quiet@Simplify[Plus@@UnDoCrossings[MomentumExpand@ampPhys, crossingsPhys], TimeConstraint->{0.01,1}];
		
		
		Do[
			ampsNumeric = SustMomentaOptimized[{ampFull,ampPhys},##,masseslist]&@@(Count[processesSorted[[k]],#,Infinity,Heads->True]&/@{F,V,S});
			(*ampsNumeric = ampsNumeric /. MapThread[#1->#2&, {masseslist, EFTSeries[Sqrt@mphys2[#,modelfull],EFTOrder]&/@processes[[k]]}];
			ampsNumeric = EchoTiming[EFTSeries[EchoTiming[ExplicitEFTOrder @ ampsNumeric,"ExplicitEFTOrder"], EFTOrder],"Last EFT Series"];*)
			matchEqs = Join[matchEqs, MapThread[Equal,Transpose@ampsNumeric]],
		nEqs];
		
		red = SolvePerturbative[ExplicitEFTOrder@matchEqs, varsPhys];
		
		(*Symplifying the solution according to the EFT power of the reduced coefficients*)
		(*This could be achieved with a standard simplify, but due to the huge rational expressions
		this can last forever. However, by knowing the mass dimensions of the reduced coefficient
		and the coefficients appearing in the reduction we can perform a much faster Taylor expansion
		up to the highest power of mass that we can find in such reduction*)
		maxPowerOfMass = Max/@(Cases[#,Coeff[_,order_,___]:>order,{-2}]/.{}->{0}&/@ Values[red]);
		coeffPowerOfMass = TrueEFTOrder/@ Keys[red];
		

		red = MapThread[
			#1 -> Normal@Series[
					Expand[#2]/.{MPhysSymbol[a_]:> xxx MPhysSymbol[a], coeff:Coeff[c_,o_,modelphys] :> xxx^(o-TrueEFTOrder[coeff]) coeff},
					{xxx,0,#3}]&
				,{Keys[red], Values[red], maxPowerOfMass-coeffPowerOfMass}];
		
		reduction = Join[reduction, red/.{xxx->1}//Simplify];
		,
		
	{k,Length[processes]}];
	
	(*Replacing physical masses by bare masses assuming the latter are real-valued*)
	reduction = Assuming[
		And @@ (#>0&/@ Cases[DownValues[mbare],mbare[_,modelfull],Infinity]),

		reduction/.{MPhysSymbol[a_]:>EFTSeries[Sqrt[mphys2[a,modelfull]],EFTOrder]}
	];
	
	(*Join all EFT orders of the same coefficient*)
	reduction = ( # -> EFTSeries[
		ExplicitEFTOrder@PerturbativeExpand[#,EFTOrder,modelphys]/.reduction,EFTOrder]/.{inv\[CapitalLambda]->1}
			)&/@ CoeffList[modelphys];
			
	(*Printing the results*)
	Echo["Mostrando la reducci\[OAcute]n: "];
	FormatCoeff[modelfull,"",VisibleModel->False,VisibleOrder->False];
	FormatCoeff[modelphys,"",VisibleModel->False,VisibleOrder->False];
	Print["\[Bullet] "<>ToString@StandardForm[#]] &/@ Expand[reduction];

tFin = SessionTime[];

Print["\nTiempo de ejecuci\[OAcute]n: "<>ToString[N[tFin-tIni,4]]<>" s"];
	
Return[reduction]]


(* ::Section:: *)
(*Amplitude Computation*)


(*General behaviour of some functions*)
mphys2[-field_,model_]:=mphys2[field,model]
mbare[-field_,model_]:=mbare[field,model]
mInternal[-field_,model_]:=mInternal[field,model]
Z[-field_,model_]:=Z[field,model]
Prop[-field_,model_]:=Prop[field,model]

MPhysSymbol[-field_]:=MPhysSymbol[field]


Options[AmplitudeComputation]={SeparateCrossings->False};
AmplitudeComputation[process_,EFTOrder_,model_,OptionsPattern[]]:=Module[{fields,nParticles,momentaList,adjacencies,topos,diags,amp,ampStructs,ampKins,PropSust},

	fields = DeleteDuplicatesBy[process,Abs];

	nParticles = Length[process];
	momentaList = Symbol["P"<>ToString[#]]&/@Range[nParticles];
	
	(*Trying to compute topologies with the needed adjacencies*)
	adjacencies = DeleteCases[Length/@ProcessList[model],x_/;x<=2];
	topos = CreateTopologies[0, nParticles->0, Adjacencies->adjacencies];
	
	(*Identifying identical particles for the crossings*)
	identicalParticles =Flatten@Position[process,#,{1}]&/@DeleteDuplicates[process];
	{unCrossedTopos, crossingPerms, numCrossedTopos} = FindCrossings[topos, identicalParticles];
	
	diags = Quiet@InsertFields[unCrossedTopos,process->{},InsertionLevel->{Particles},Model->model,GenericModel->model];
	
	(*See what topologies have succeeded upon the insertion of fields*)
	insertedTopos=List@@(TakeGraph/@diags/.a_[b___,Field[__]]:>a[b]);
	(*Find the position of these*)
	posInsertedTopos=Flatten@Map[Position[List@@unCrossedTopos,#]&,insertedTopos];
	
	amp = (FCFAConvert[CreateFeynAmp[Head[diags]@#],IncomingMomenta->momentaList,List->True] &/@(List@@diags))/.RenameCoeff[model];
	(*JAVI Echo["Amplitude computed"];*)
	amp = Contract[amp(*,EpsContract->True,EpsExpand->False,Expanding->False,ExpandScalarProduct->False,Factoring->False,
									FCE->False,FCI->False,FCVerbose->False,MomentumCombine->False*)];
	amp = ExplicitEFTOrder@amp /. FeynAmpDenominator[PD[P_,m_]] :> DenProp[Pair[P,P],m^2];
	
	amp = Sum[inv\[CapitalLambda]^k Coefficient[#,inv\[CapitalLambda],k],{k,0,EFTOrder-4}]&/@ amp;
	
	(*Getting propagator attributes*)
	GetInternalMasses[model];
	GetExternalMasses[model];
	If[MatchQ[mphys2[#,model],mphys2[_,_]],
		mphys2[#,model]=mbare[#,model]^2;
		Z[#,model]=1;
		Prop[#,model][p_]:=1/(Pair[Momentum[p],Momentum[p]]-mphys2[#,model])
	]&/@fields;
	
	(*!!!!!!!!!!!!!!!!!!!!
	Pedir usuario que ponga distintas masas bare para cada field*) 
	PropSust = (DenProp[p2_,mInternal[#,model]^2]:>EFTSeries[Prop[#,model][p2],EFTOrder])&/@ GetFields[model];
	
	amp = EFTSeries[Times@@(Z[#,model]^(1/2)&/@process), EFTOrder] amp /. PropSust;
	
	amp = Sum[inv\[CapitalLambda]^k Coefficient[#,inv\[CapitalLambda],k],{k,0,EFTOrder-4}]&/@ amp;
	
	If[OptionValue[SeparateCrossings]===True, 
		
		Return[{amp, crossingPerms[[posInsertedTopos]]}]
	 ];
	
	Return[Plus@@UnDoCrossings[amp, crossingPerms[[posInsertedTopos]]]];
]


(*!!!!!!!!!!!!!!!!!!
Change this so that if no 2pt rule can be extracted from the model then it assumes standard propagator*)
PropagatorAttributes[field_,EFTOrder_,model_]:=Module[{topo2pt,diag2pt,amp2pt,
			pi,mphys2Condition,mphys2,mphysCondition,massSol,Z,Prop,PropExp,P},
	
	(*Here a function that extracts 2-pt rule*)
	(*The 2-pt function must be with CTOrder->1 in the FeynArts model*)
	topo2pt = CreateTopologies[0,1->1,CTOrder->1];
	diag2pt = Quiet@InsertFields[topo2pt,{field}->{field},InsertionLevel->{Particles},Model->model,GenericModel->model];
	amp2pt = FCFAConvert[CreateFeynAmp[diag2pt, Truncated->True],IncomingMomenta->{P},OutgoingMomenta->{P},List->False]/.RenameCoeff[model];
	amp2pt = ExplicitEFTOrder[amp2pt]//Simplify;
	
	(*For vectors we are only interested in the g\[Mu]\[Nu] part, in the \[Xi]->0 gauge*)
	If[Head[field]===V, amp2pt=-Coefficient[amp2pt/.Pair[LorentzIndex[_],LorentzIndex[_]]->x,x]];
	
	Prop[pp_] := amp2pt/.Pair[Momentum[P],Momentum[P]]->pp;

	(*Perturbatively computing Subscript[m, phys]^2-Subscript[m, 0]^2-\[CapitalPi](Subscript[m, phys]^2)*)
	mphys2Condition = PerturbativeExpand[Prop[Coeff[mphys2,4]],EFTOrder];
	massSol = SolvePerturbative[EFTSeries[ExplicitEFTOrder@mphys2Condition,EFTOrder]==0, 
																	List@@PerturbativeExpand[Coeff[mphys2,4],EFTOrder]];
	mphys2 = ExplicitEFTOrder[PerturbativeExpand[Coeff[mphys2,4],EFTOrder]/.massSol];
	
	Z = EFTSeries[(Prop'[mphys2])^(-1),EFTOrder];
	
	(*Propagator*)
	PropExp[p2_]:=EFTSeries[1/Prop[p2],EFTOrder];
	
	Return[{mphys2, Z, PropExp}]
]


AllPropagatorAttributes[model_,EFTOrder_]:=Module[{fields, masses, allPropAtt},
	
	fields = GetFields[model];
	allPropAtt = Set[{mphys2[#1,model], Z[#1,model], Prop[#1,model]},PropagatorAttributes[#1,EFTOrder,model]]&/@fields;
	
	allPropComputed[model]=True;
]


Bare2PhysMass[fields_List,EFTOrder_,model_] := Bare2PhysMass[#,EFTOrder,model]&/@fields

Bare2PhysMass[field_,EFTOrder_,model_] := Bare2PhysMass[field,EFTOrder,model] = Block[{res,sol,mbaresol},

	res = EFTSeries[ExplicitEFTOrder@(mphys2[field,model]/.mbare[field,model]->PerturbativeExpand[mbare[field,model],EFTOrder,model]),EFTOrder];
	sol = SolvePerturbative[MPhysSymbol[field]==res/.{mbare[field,model]->MPhysSymbol[field]},List@@PerturbativeExpand[mbare[field,model],EFTOrder,model]];
	mbaresol = PerturbativeExpand[mbare[field,model],EFTOrder,model]/.sol/.{mbare[field,model]->MPhysSymbol[field]};
	
	(*If physical mass equals 0, set it to 0*)
	If[mphys2[field,model]===0, mbaresol=(mbaresol/.{MPhysSymbol[field]->0})];
	
	Return[mbare[field,model]->mbaresol];

]


(* ::Section:: *)
(*Crossing - related functions*)


FindCrossings[graphs_]:=Block[{graphsIDordered,graphWPos,crossedDiags,vertsOrder,cycles,nParticles,crossingPerms,numCrossedDiags},
	
	(*Order in a canonical way id of internal vertices*)
		graphsIDordered=ReorderVerticesID/@graphs;
	
	(*Adding the position of the diagram {{diag1,1},{diag2,2}...}*)
		graphWPos=MapIndexed[{#1,First[#2]}&,List@@graphsIDordered];
	
	(*Finding crossedDiags and the number of their positions in graphs*)
		{crossedDiags,numCrossedDiags}=Transpose[Transpose/@GatherBy[graphWPos,InternalLines[#[[1]]]&]];
	
	(*Finding the permutation crossing*)
		vertsOrder=internalVertsOrder/@crossedDiags;
		cycles=Table[FindPermutation[vertsOrder[[k]][[1]],#]&/@vertsOrder[[k]],{k,Length[vertsOrder]}];
		nParticles=Length@ExternalLines[graphsIDordered[[1]]];
		crossingPerms=Table[Permute[Range[nParticles],#]&/@cycles[[k]],{k,Length[cycles]}];

	Return[{TopologyList@@(First/@crossedDiags),crossingPerms,numCrossedDiags}];
]

FindCrossings[graphs_,possibleCrossings_]:=Block[{graphsIDordered,graphWPos,crossedDiags,vertsOrder,cycles,nParticles,crossingPerms,numCrossedDiags},
	
	(*Order in a canonical way id of internal vertices*)
		graphsIDordered=ReorderVerticesID/@graphs;
	
	(*Adding the position of the diagram {{diag1,1},{diag2,2}...}*)
		graphWPos=MapIndexed[{#1,First[#2]}&,List@@graphsIDordered];
	
	(*Finding crossedDiags and the number of their positions in graphs*)
		{crossedDiags,numCrossedDiags}=Transpose[Transpose/@GatherBy[graphWPos,InternalLines[#[[1]]]&]];
	
	(*Finding the permutation crossing*)
		vertsOrder=internalVertsOrder/@crossedDiags;
		cycles=Table[FindPermutation[vertsOrder[[k]][[1]],#]&/@vertsOrder[[k]],{k,Length[vertsOrder]}];
		nParticles=Length@ExternalLines[graphsIDordered[[1]]];
	
	(*Selecting allowed crossings according to identical particles*)
		trueCrossings={};
	Do[
	
		(*Permutations with diagram position in graphs*)
		perms=Transpose[{cycles[[k]],numCrossedDiags[[k]]}];
		(*List containing true identified crossings and those which have not been identified yet*)
		truefalseCrossings={trueCrossings,perms};
		(*While there are left non-identified crossings...*)
		While[Length[truefalseCrossings]===2,
			
			(*Select the first diagram as a new non-crossed diagram*)
			newCrossingPerm=First[truefalseCrossings[[2]]];
			(*Find permutations of rest diagrams with respect to this one*)
			permsCycle=FindPermutation[Permute[Range[nParticles],newCrossingPerm[[1]]],Permute[Range[nParticles],First@#]]&/@truefalseCrossings[[2]];
			(*Add the number identifying the position of the diagram in the list graphs*)
			truefalseCrossings=MapThread[{#1,#2[[2]]}&,{permsCycle,truefalseCrossings[[2]]}];
			(*Combine diagrams whose permutation is allowed according to identical particles*)
			truefalseCrossings=GatherBy[truefalseCrossings,IsGoodPermutationQ[#[[1]],possibleCrossings]&];
			(*Append identified crossings to the list*)
			AppendTo[trueCrossings,truefalseCrossings[[1]]];
		]
	,{k,Length[cycles]}];
	
		{cycles,numCrossedDiags}=Transpose[Transpose/@trueCrossings];
		crossingPerms=Table[Permute[Range[nParticles],#]&/@cycles[[k]],{k,Length[cycles]}];

	Return[{TopologyList@@(graphs[[#[[1]]]]&/@numCrossedDiags),crossingPerms,numCrossedDiags}];
]


ComputeCrossings[amp_,crossings_]:=Block[{substRules},
	substRules=(Thread/@(#->First[crossings]&/@crossings))/.{i_Integer:>Symbol["P"<>ToString@i]};
	Return[Table[(amp[[k]]/.#)&/@substRules,{k,Length[amp]}]];
]

UnDoCrossings[amps_List,crossingsList_List]:=Flatten[MapThread[ComputeCrossings[#1,#2]&,{amps,crossingsList}]];


(*Vertices are characterized by a unique ID. To see if two diagrams are crossing-related one just have
to verify if the internal structure (i.e., internal propagators) is the same. However, they can be connected
to the same vertices but these be named with different IDs. To have these two diagrams identified as crossings,
we need a function to order the ID of vertices in a canonical way*)
ReorderVerticesID[graph_]:=Block[{intLines,idVertices,changeID,graphmod},

(*Select internal lines*)
intLines=Cases[graph,Propagator[Internal][__]];

(*Reorder id of vertices in a canonical way*)
idVertices=DeleteDuplicates@Cases[intLines,Vertex[_][id_]:>id,Infinity];
changeID=Thread[idVertices->Sort[idVertices]];
graphmod=graph/.Vertex[adj_][id_]:>Vertex[adj][id/.changeID];

Return[graphmod];

]


InternalLines[graph_]:=Cases[graph,Propagator[Internal][__]]
ExternalLines[graph_]:=Cases[graph,Propagator[Incoming][__]]


internalVertsOrder[graph_:Topology[_][__]]:=ExternalLines[graph]/.Propagator[_][_,Vertex[_][n_]]:>n
internalVertsOrder[graphs_List]:=internalVertsOrder/@graphs


(*A permutation is good if it permutes only numbers within the corresponding subset. For instance,
the permutation 2 <-> 3 is not allowed if 2 and 3 are different particles.*)
IsGoodPermutationQ[perm_Cycles,subSets_]:=IsGoodPermutationQ[Level[perm,{2}],subSets]
IsGoodPermutationQ[perm_List,subSets_]:=And@@Table[AnyTrue[subSets,SubsetQ[#,perm[[k]]]&],{k,Length[perm]}]


(* ::Section::Closed:: *)
(*Algebraic Tools*)


(*EFTSeries[A_*B_,EFTOrder_]:=Module[{ASeries,BSeries},
	ASeries=EFTSeries[A,EFTOrder];
	ASeries=Table[inv\[CapitalLambda]^k Coefficient[ASeries,inv\[CapitalLambda],k],{k,0,EFTOrder-4}];
	BSeries=EFTSeries[B,EFTOrder];
	BSeries=Reverse@Table[inv\[CapitalLambda]^k Coefficient[BSeries,inv\[CapitalLambda],k],{k,0,EFTOrder-4}];
	Return[Sum[ASeries[[k]]Total[BSeries[[k;;-1]]],{k,Length[ASeries]}]];
]

EFTSeries[A_ +B_,EFTOrder_]:=EFTSeries[A,EFTOrder]+EFTSeries[B,EFTOrder]*)

EFTSeries[exp_,n_]:=Normal@Series[ExplicitEFTOrder@exp,{inv\[CapitalLambda],0,n-4}]


EFTSeriesRational[exp_List,EFTOrder_] := EFTSeriesRational[#,EFTOrder]&/@exp

EFTSeriesRational[exp_Plus,EFTOrder_]:=Block[{expList=List@@exp,num,den,DenominatorSeries,expnew},
	num=Numerator/@expList;
	num=Total/@CoefficientList[num,inv\[CapitalLambda],EFTOrder-3];
	
	den=Denominator/@expList;
	den=CoefficientList[den,inv\[CapitalLambda],EFTOrder-3];
	DenominatorSeries=Function@@{EFTSeries[1/Sum[a[i]inv\[CapitalLambda]^i,{i,0,EFTOrder-4}],EFTOrder]}/.MapIndexed[Function[{a,b},a->Slot[b[[1]]]],Table[a[i],{i,0,4}]];
	den=(DenominatorSeries@@#)&/@den;
	expnew=Total[CoefficientList[num*den,inv\[CapitalLambda],EFTOrder-3],Infinity];
	Return[expnew];
]


ExplicitEFTOrder[amp_]:=(amp/.inv\[CapitalLambda]->1)/.{Coeff[c_,order_/;order>4,model___]:>inv\[CapitalLambda]^(order-4 ) Coeff[c,order,model]}


(*PerturbativeExpand[coefOrderTogether_,EFTOrder_]:=PerturbativeExpand[##,EFTOrder]&@@Transpose[coefOrderTogether]

PerturbativeExpand[coefs_List,coefsOrder_List,EFTOrder_]:=MapThread[PerturbativeExpand[#1,#2,EFTOrder] &,{coefs,coefsOrder}]

PerturbativeExpand[coef_,coefOrder_,EFTOrder_]:=Module[{expandCoef},

	If[Head[coef]===Symbol
		,
		expandCoef=Sum[inv\[CapitalLambda]^(k-4) Symbol[ToString[coef]<>"dim"<>ToString[k]],{k,coefOrder,EFTOrder}]
		,
	If[Head[coef]===String
		,
		expandCoef=Sum[inv\[CapitalLambda]^(k-4) Symbol[coef<>"dim"<>ToString[k]],{k,coefOrder,EFTOrder}]
		,
		expandCoef=Sum[inv\[CapitalLambda]^(k-4)Symbol[ToString[Head@coef]<>"dim"<>ToString[k]]@@Level[coef,{-1}],{k,coefOrder,EFTOrder}]
	]];

	Return[expandCoef]
]*)

PerturbativeExpand[exp_,EFTOrder_,m___]:=(exp/.inv\[CapitalLambda]->1)/.{Coeff[c_,o_,m]:>PerturbativeExpand[Coeff[c,o,m],EFTOrder,m]}

PerturbativeExpand[Coeff[c_,o_,___],EFTOrder_,___]:=Nothing/;EFTOrder<o
PerturbativeExpand[Coeff[c_,o_,m___],EFTOrder_,m___]:=Sum[Coeff[c,onew,m],{onew,o,EFTOrder}]


SolvePerturbative[True,_] := {}

SolvePerturbative[eq_Equal,vars_]:=SolvePerturbative[{eq},vars]

SolvePerturbative[eqs_,vars_]:=Block[{maxdegree, eqs2=DeleteCases[eqs,True], solution={}},

	maxdegree=Max[Length/@Level[CoefficientList[eqs2/.{Equal->List},inv\[CapitalLambda]],{2}]];
	eqs2=Transpose[MapThread[#1==#2&,#]&/@CoefficientList[eqs2/.{Equal->List},inv\[CapitalLambda],maxdegree]];
	eqs2=Select[eqs2, MemberQ[Table[FreeQ[#,vars[[k]]],{k,Length[vars]}], False] &];
	
	(solution=Flatten[AppendTo[solution,
	Quiet[Solve[#/.solution,vars],Solve::svars]]])&/@ eqs2;

	Return[solution];
]


(* ::Section::Closed:: *)
(*Reading model and coefficient tools*)


GetFields[model_] := Module[{},
	Get[model<>".mod"];
	Return[Cases[M$ClassesDescription,Equal[field_,__]:>field]];
]


GetExternalMasses[model_] := Module[{},
	Get[model<>".mod"];
	masses = Cases[M$ClassesDescription,Equal[_,opt_]:>(Mass[External]/.opt)];
	fields = GetFields[model];
	MapThread[(mbare[#1,model]=#2)&,{fields,masses/.RenameCoeff[model]}];
	
	Return[MapThread[ToString["mbare["]<>ToString[#1]<>"]"->#2 &,{fields,masses}]];
]

GetInternalMasses[model_] := Module[{},
	Get[model<>".mod"];
	masses = Cases[M$ClassesDescription,Equal[_,opt_]:>(Mass[Internal]/.opt)];
	fields = GetFields[model];
	MapThread[(mInternal[#1,model]=#2)&,{fields,masses/.RenameCoeff[model]}];
	
	Return[MapThread[ToString["mInternal["]<>ToString[#1]<>"]"->#2 &,{fields,masses}]];
]


Options[DefineCoefficient]={
	"EFTOrder"->4,
	"Complex"->False
};

DefineCoefficient[coefs_List,model_,opts:OptionsPattern[]]:=DefineCoefficient[#,model,opts]&/@coefs

DefineCoefficient[coef_,model_,OptionsPattern[]]:=Module[{},
	
	If[Head[CoeffList[model]]=!=List
		, 
		CoeffList[model]={};
		,
		CoeffList[model] = DeleteCases[CoeffList[model], Coeff[coef,__]];
	];
	CoeffList[model] = Append[CoeffList[model], Coeff[coef,OptionValue["EFTOrder"],model]];
]


CouplingOrderSust[coupList_List]:=MapThread[#1->inv\[CapitalLambda]^(#2-4) #1 &,Transpose@coupList]


RenameCoeff[model_] := Flatten@Cases[CoeffList[model], Coeff[c_,s__]:>{c->Coeff[c,s]}]


ProcessList[model_] := Module[{},

	Get[model<>".mod"];
	Return[Sort[Cases[FeynArts`M$CouplingMatrices,(x_==y_):>x]/.{C->List}]];	
]


TrueEFTOrder[Coeff[c_,o_,m_]] := FirstCase[CoeffList[m], Coeff[c,trueorder_,m]:>trueorder]


(* ::Section::Closed:: *)
(*Other Tools*)


NumArgs[f_] := If[#==={},0,Max[#]]&@DeleteDuplicates[Cases[f/.{Function[a__]:>a},Slot[n_]:>n,Infinity]]


Options[IdentifyKinematics]={EvaluateTerms->None};
IdentifyKinematics[amp_Plus,OptionsPattern[]]:=Block[{ampList,kinValues,kinStructs,groupList},

	ampList=List@@amp;
	kinValues=DeleteDuplicates[Cases[#,Pair[__],-2]]&/@ampList;
	kinStructs=MapThread[Function@@{#1}/.Thread[#2->Slot/@Range[Length[#2]]]&,{ampList,kinValues}];
	groupList=GatherBy[Range@Length[kinStructs],kinStructs[[#]]@@(x/@Range[NumArgs[kinStructs[[#]]]])&];
	kinStructs=kinStructs[[First[#]]]&/@groupList;
	kinValues=(kinValues[[#]]&/@groupList);

	If[OptionValue[EvaluateTerms]===None,Return[{kinStructs,kinValues}]];

	If[Head@OptionValue[EvaluateTerms]===Symbol,
		Return[{#@@(OptionValue[EvaluateTerms]/@Range[NumArgs[#]])&/@kinStructs,kinValues}]];

	Message[IdentifyKinematics::nosymb,OptionValue[EvaluateTerms]]
	
]

IdentifyKinematics[amp_,OptionsPattern[]]:=Block[{ampList,kinValues,kinStructs,groupList},

	ampList={amp};
	kinValues=DeleteDuplicates[Cases[#,Pair[__],-2]]&/@ampList;
	kinStructs=MapThread[Function@@{#1}/.Thread[#2->Slot/@Range[Length[#2]]]&,{ampList,kinValues}];
	groupList=GatherBy[Range@Length[kinStructs],kinStructs[[#]]@@(x/@Range[NumArgs[kinStructs[[#]]]])&];
	kinStructs=kinStructs[[First[#]]]&/@groupList;
	kinValues=(kinValues[[#]]&/@groupList);

	If[OptionValue[EvaluateTerms]===None,Return[{kinStructs,kinValues}]];

	If[Head@OptionValue[EvaluateTerms]===Symbol,
		Return[{#@@(OptionValue[EvaluateTerms]/@Range[NumArgs[#]])&/@kinStructs,kinValues}]];

	Message[IdentifyKinematics::nosymb,OptionValue[EvaluateTerms]]
	
]

RecoverKinematics[structs_,values_]:=Flatten@MapThread[#1@@@#2&,{structs,values}]


(* ::Section:: *)
(*Format*)


Options[FormatCoeff] = {VisibleModel->True,VisibleOrder->True};
FormatCoeff[model_,subscript_,OptionsPattern[]]:=Block[{},

	If[OptionValue[VisibleModel]&&OptionValue[VisibleOrder]
	,
	Format[Coeff[c_,o_,model],StandardForm]:=\!\(\*SuperscriptBox[
SubscriptBox[\(c\), \(subscript\)], \("\<(\>" <> ToString[o] <> "\<)\>"\)]\);
	,
	If[OptionValue[VisibleModel]
	,
	Format[Coeff[c_,o_,model],StandardForm]:=Subscript[c, subscript];
	,
	If[OptionValue[VisibleOrder]
	,
	Format[Coeff[c_,o_,model],StandardForm]:=\!\(\*SuperscriptBox[\(c\), \("\<(\>" <> ToString[o] <> "\<)\>"\)]\);
	,
	Format[Coeff[c_,o_,model],StandardForm]:=c;
	]]]
]



(* ::Section:: *)
(*Old functions*)


Options[RedBasisOld] = {CoefDimensions->{}, Process->{}};
RedBasisOld[modelfull_, modelphys_, EFTOrder_, OptionsPattern[]]:=Module[{reduction={}, ampsNumeric},
	
	(*Computation of physical poles, residues and propagators*)
	AllPropagatorAttributes[modelfull,EFTOrder];
	AllPropagatorAttributes[modelphys,EFTOrder];
	
	reduction = Join[reduction, AllMassReduction[modelfull,modelphys,EFTOrder]];
	
	(*Get processes and order them with respect to number of particles and a greater number of identical particles*)
	processes = SortBy[DeleteCases[ProcessList[modelphys],{_,_}], {Length,Last/@Tally[#]&}];
	processes = DeleteCases[ProcessList[modelphys],{_,_}];
	(*Sort each process so that we have first fermions, then vectors and finally scalars*)
	processesSorted = SortBy[#,Position[{F,V,S},Head[-#/.{-f_:>f}]]&]&/@ processes;
	
	(*List of vertices with coefficients appearing in their Feynman rules*)
	CoeffInteraction=Association@@Cases[M$CouplingMatrices,C[int__] == coeffs_:>{int}->Variables[coeffs]];
	
	Do[
	
	Echo[processesSorted[[k]],"Computing process: "];
	Echo[CoeffInteraction[processes[[k]]],"Appearing couplings: "];
	
	(*If all appearing couplings are already computed, skip this process*)
	If[
		FreeQ[MemberQ[Keys[reduction], #]&/@ 
				Flatten@(PerturbativeExpand[CoeffInteraction[processes[[k]]]/.RenameCoeff[modelphys],EFTOrder,modelphys]/.Plus->List), False],
		Echo["Next process"];
		Continue[];
	];
	
	matchEqs={};
	(*Computation of amplitudes*)
		{ampFullStructs,ampFullKins} = EchoTiming[AmplitudeComputation[processesSorted[[k]],EFTOrder,modelfull,SeparateKinStructs->True],"Amplitude Full"];
		{ampPhysStructs,ampPhysKins} = EchoTiming[AmplitudeComputation[processesSorted[[k]],EFTOrder,modelphys,SeparateKinStructs->True],"Amplitude Phys"];
		
	(*Evaluation of kinematical structures with arbitrary kinematical arguments*)
		ampFullStructs = #@@(kinArg/@Range[NumArgs[#]])&/@ ampFullStructs;
		ampPhysStructs = #@@(kinArg/@Range[NumArgs[#]])&/@ ampPhysStructs;
		
	(*Change bare masses by physical ones*)
		ampFullStructs = ampFullStructs/.Flatten@{#[[1]]^n_:>EFTSeries[#[[2]]^n,EFTOrder]&/@Flatten[{Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelfull]}],
							Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelfull]};
		ampPhysStructs = ampPhysStructs/.Flatten@{#[[1]]^n_:>EFTSeries[#[[2]]^n,EFTOrder]&/@Flatten[{Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelphys]}],
							Bare2PhysMass[DeleteDuplicates@processes[[k]],EFTOrder,modelphys]};
		
		ampPhysStructs = ExplicitEFTOrder @ PerturbativeExpand[ampPhysStructs,EFTOrder,modelphys];
	(*EFT Series*)
		ampFullStructs = EchoTiming[EFTSeries[ExplicitEFTOrder @ ampFullStructs,EFTOrder],"EFTSeries in ampFull"];
		ampPhysStructs = EchoTiming[EFTSeries[ampPhysStructs/.reduction,EFTOrder],"EFTSeries in ampPhys and replace previous results"];
		
	(*Glue back the full amplitude*)
		ampFull = Plus@@RecoverKinematics[Function@@{#/.{kinArg->Slot}}&/@ampFullStructs, ampFullKins] //MomentumExpand;
		ampPhys = Plus@@RecoverKinematics[Function@@{#/.{kinArg->Slot}}&/@ampPhysStructs, ampPhysKins] //MomentumExpand;
		
		ampFull = Quiet@Simplify[ampFull,TimeConstraint->{0.01,1}];
		ampPhys = Quiet@Simplify[ampPhys,TimeConstraint->{0.01,1}];
		
	(*Computation of rational kinematics*)
		(*Symbolic physical masses to be replaced with already computed mphys2 at the end*)
		(*If any of mphys2 is 0, we take advantage of this and put MPhysSymbol to 0 at this stage.
		Also, if physical masses of two different particles are the same, we put them equal*)
		masseslist = If[mphys2[#,modelfull]=!=0, MPhysSymbol[#], 0]&/@ processesSorted[[k]];
		masseslist = masseslist /. Select[
						Flatten@Replace[(DeleteDuplicates/@GatherBy[masseslist,#/.{MPhysSymbol[f_]:>mphys2[f,modelphys]}&]),
										{x_,y__}:>(#->x&/@List[y]),2], 
						Head[#]===Rule &];
		
		varsPhys = DeleteDuplicates@Cases[ampPhys,Coeff[___,modelphys],Infinity];
		nEqs = Max[Values[CountsBy[varsPhys,#[[2]]&]]]; (*Max number of coefficients with same EFTOrder*)
		Do[
			ampsNumeric = EchoTiming[SustMomentaOptimized[{ampFull,ampPhys},##,masseslist]&@@(Count[processesSorted[[k]],#,Infinity,Heads->True]&/@{F,V,S}),"Substitution Momenta"];
			(*ampsNumeric = ampsNumeric /. MapThread[#1->#2&, {masseslist, EFTSeries[Sqrt@mphys2[#,modelfull],EFTOrder]&/@processes[[k]]}];
			ampsNumeric = EchoTiming[EFTSeries[EchoTiming[ExplicitEFTOrder @ ampsNumeric,"ExplicitEFTOrder"], EFTOrder],"Last EFT Series"];*)
			matchEqs = Join[matchEqs, MapThread[Equal,Transpose@ampsNumeric]],
		nEqs];
		
		red = EchoTiming[SolvePerturbative[ExplicitEFTOrder@matchEqs, varsPhys],"Solving system"];
		
		(*Symplifying the solution according to the EFT power of the reduced coefficients*)
		(*This could be achieved with a standard simplify, but due to the huge rational expressions
		this can last forever. However, by knowing the mass dimensions of the reduced coefficient
		and the coefficients appearing in the reduction we can perform a much faster Taylor expansion
		up to the highest power of mass that we can find in such reduction*)
		maxPowerOfMass = Max/@(Cases[#,Coeff[_,order_,___]:>order,{-2}]/.{}->{0}&/@ Values[red]);
		coeffPowerOfMass = TrueEFTOrder/@ Keys[red];
		
		red = MapThread[
			#1 -> Normal@Series[Expand[#2]/.{MPhysSymbol[a_]:> xxx MPhysSymbol[a]},{xxx,0,#3}]&
				,{Keys[red], Values[red], maxPowerOfMass-coeffPowerOfMass}];
		
		reduction = Echo@Join[reduction, red/.{xxx->1}//Simplify];
		,
		
	{k,Length[processes]}];
	
	(*Replacing physical masses by bare masses assuming the latter are real-valued*)
	reduction = Assuming[
		And @@ (#>0&/@ Cases[DownValues[mbare],mbare[_,modelfull],Infinity]),

		reduction/.{MPhysSymbol[a_]:>EFTSeries[Sqrt[mphys2[a,modelfull]],EFTOrder]}
	];
	
	(*Join all EFT orders of the same coefficient*)
	reduction = ( # -> EFTSeries[
		ExplicitEFTOrder@PerturbativeExpand[#,EFTOrder,modelphys]/.reduction,EFTOrder]/.{inv\[CapitalLambda]->1}
			)&/@ CoeffList[modelphys];
			
	(*Printing the results*)
	Echo["Showing the reduction"];
	FormatCoeff[VisibleModel->False,VisibleOrder->False];
	Print /@ reduction;

	
Return[reduction]]


Options[AmplitudeComputationOld]={SeparateKinStructs->False};
AmplitudeComputationOld[process_,EFTOrder_,model_,OptionsPattern[]]:=Module[{fields,nParticles,momentaList,adjacencies,topos,diags,amp,ampStructs,ampKins,PropSust},

	fields = DeleteDuplicatesBy[process,Abs];

	nParticles = Length[process];
	momentaList = Symbol["P"<>ToString[#]]&/@Range[nParticles];
	
	(*Trying to compute topologies with the needed adjacencies*)
	adjacencies = DeleteCases[Length/@ProcessList[model],x_/;x<=2];
	topos = CreateTopologies[0,nParticles->0,Adjacencies->adjacencies];
	
	diags = Quiet@InsertFields[topos,process->{},InsertionLevel->{Particles},Model->model,GenericModel->model];
	amp = FCFAConvert[CreateFeynAmp[diags],IncomingMomenta->momentaList,List->False]/.RenameCoeff[model];
	amp = Contract[amp];
	amp = ExplicitEFTOrder[amp];
	
	(*Detect different kinematic structures to work only with some few terms*)
	{ampStructs,ampKins} = IdentifyKinematics[amp/.{FeynAmpDenominator[PD[P_,m_]] :> DenProp[Pair[P,P],m^2]}, EvaluateTerms->kinArg];
	
	ampStructs = Sum[inv\[CapitalLambda]^k Coefficient[#,inv\[CapitalLambda],k],{k,0,EFTOrder-4}]&/@ampStructs;
	
	(*Getting propagator attributes*)
	GetInternalMasses[model];
	GetExternalMasses[model];
	If[MatchQ[mphys2[#,model],mphys2[_,_]],
		mphys2[#,model]=mbare[#,model]^2;
		Z[#,model]=1;
		Prop[#,model][p_]:=1/(Pair[Momentum[p],Momentum[p]]-mphys2[#,model])
	]&/@fields;
	
	(*!!!!!!!!!!!!!!!!!!!!
	Pedir usuario que ponga distintas masas bare para cada field*) 
	PropSust = (DenProp[p2_,mInternal[#,model]^2]:>EFTSeries[Prop[#,model][p2],EFTOrder])&/@ GetFields[model];
	
	ampStructs = EFTSeries[Times@@(Z[#,model]^(1/2)&/@process), EFTOrder] ampStructs /. PropSust;
	
	ampStructs = Sum[inv\[CapitalLambda]^k Coefficient[#,inv\[CapitalLambda],k],{k,0,EFTOrder-4}]&/@ ampStructs;
	
	(*Set ampStructs as a pure function again*)
	ampStructs = Function@@{#/.{kinArg->Slot}}&/@ampStructs;
	
	If[OptionValue[SeparateKinStructs]===False, Return[Collect[Plus@@RecoverKinematics[ampStructs,ampKins],inv\[CapitalLambda]]]];
	
	Return[{ampStructs,ampKins}];

Return[amp];
]


End[];


EndPackage[];
