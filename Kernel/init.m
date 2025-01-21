(* ::Package:: *)

(*file init.m*) 

(*Load FeynCalc+FeynArts*)
$LoadAddOns={"FeynArts"};
$FeynCalcStartupMessages=False;
Get["MOST`ExternalPackages`FeynCalc`"];
$FAVerbose=0;

Get["MOST`KinematicSubstitution`"];
Get["MOST`MOST`"];
