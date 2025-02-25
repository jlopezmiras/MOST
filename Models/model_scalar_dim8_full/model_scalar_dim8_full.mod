(* Patched for use with FeynCalc *)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(*                                                                             *)
(*         This file has been automatically generated by FeynRules.            *)
(*                                                                             *)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)


FR$ModelInformation={
  ModelName->"model"};

FR$ClassesTranslation={};

FR$InteractionOrderPerturbativeExpansion={};

FR$GoldstoneList={};

(*     Declared indices    *)

(*     Declared particles    *)

M$ClassesDescription = {
S[1] == {
    SelfConjugate -> True,
    PropagatorLabel -> S,
    PropagatorType -> ScalarDash,
    PropagatorArrow -> None,
    Mass -> m0,
    Indices -> {} }
}


(*        Definitions       *)


m0[ ___ ] := m0;




(*      Couplings (calculated by FeynRules)      *)

M$CouplingMatrices = {

C[ S[1] , S[1] ] == {{(-I)*m0^2, (-I)*m0^2}, {(-I)*r81, (-I)*r81}, {-I, -I}, {(2*I)*r61, (2*I)*r61}, {(-I)*r81, (-I)*r81}},

C[ S[1] , S[1] , S[1] , S[1] ] == {{(24*I)*lmbd, 0}, {(-6*I)*r62, 0}, {(6*I)*r82, 0}, {(4*I)*a82, 0}, {(4*I)*a82, 0}, {(4*I)*a82, 0}, {(-6*I)*r62, 0}, {(4*I)*r83, 0}, {(6*I)*r82, 0}, {(4*I)*a82, 0}, {(4*I)*a82, 0}, {(-6*I)*r62, 0}, {(4*I)*r83, 0}, {(4*I)*r83, 0}, {(6*I)*r82, 0}, {(4*I)*a82, 0}, {(-6*I)*r62, 0}, {(4*I)*r83, 0}, {(4*I)*r83, 0}, {(4*I)*r83, 0}, {(6*I)*r82, 0}},

C[ S[1] , S[1] , S[1] , S[1] , S[1] , S[1] ] == {{(720*I)*a61, 0}, {(-120*I)*r84, 0}, {(-120*I)*r84, 0}, {(-120*I)*r84, 0}, {(-120*I)*r84, 0}, {(-120*I)*r84, 0}, {(-120*I)*r84, 0}},

C[ S[1] , S[1] , S[1] , S[1] , S[1] , S[1] , S[1] , S[1] ] == {{(40320*I)*a81, 0}}

}

