(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Input::Initialization:: *)
BeginPackage["StaticEquilibrium`"]


(* ::Input::Initialization:: *)
Unprotect@@Names["StaticEquilibrium`*"];
ClearAll@@Names["StaticEquilibrium`*"];


(* ::Input::Initialization:: *)
(*interface*)
moment::usage="Torque as cross product";
zmoment::usage="Classical 2D-statics torque about z axis";
radiusVectors::usage="Radius-vectors from a certain origin to specified points";
zmomentEquation::usage="Classical 2D-statics torque equation(s) about certain center(s)";
projectionEquation::usage="Classical projection-on-axis equation(s)";
momentAboutAxis::usage="Classical 3D-statics torque about certain axis with specified origin";
momentAboutAxisEquation::usage="Classical 3D-statics torque equation(s) about certain axis(es) with specified origin";


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Input::Initialization:: *)
vector3dQ[arg_List]:=Length@arg==3
vector3dQ[___]:=False


(* ::Input::Initialization:: *)
moment[{r_?vector3dQ,force_?vector3dQ}]:=(r\[Cross]force)

zmoment[{r_?vector3dQ,force_?vector3dQ}]:=moment[{r,force}][[3]]

zmoment[input:{{_?vector3dQ,_?vector3dQ}..}]:=Total[(zmoment/@input)]

zmoment[input:{{_?vector3dQ,_?vector3dQ}..},Optional[zmoments:{___},{}]]:=Total[(zmoment/@input)~Join~zmoments]


(* ::Input::Initialization:: *)
radiusVectors[center_?vector3dQ,points:{_?vector3dQ..}]:=#-center&/@points


(* ::Input::Initialization:: *)
zmomentEquation[
centers:{__?vector3dQ},
forces:{__?vector3dQ},
applicationpoints:{__?vector3dQ},
Optional[moments:{___},{}]]:=
zmoment[Transpose[{#,forces}],moments]==0&/@(radiusVectors[#,applicationpoints]&/@centers)

zmomentEquation[
centers:{__?vector3dQ},forcespoints:{{(*force*)_?vector3dQ,(*application point*)_?vector3dQ}..},
Optional[moments:{___},{}]]:=zmomentEquation[centers,##,moments]&@@Transpose[forcespoints]


(* ::Input::Initialization:: *)
axisOrts["x"]:={1,0,0}
axisOrts["y"]:={0,1,0}
axisOrts["z"]:={0,0,1}


(* ::Input::Initialization:: *)
axisNameQ[name:"x"|"y"|"z"]:=True
axisNameQ[___]:=False


(* ::Input::Initialization:: *)
projectionEquation[orts:{__?vector3dQ},forces:{__?vector3dQ}]:=Total[#]==0&/@Outer[Dot,orts,forces,1]

projectionEquation[axisname:{__?axisNameQ},forces:{__?vector3dQ}]:=projectionEquation[axisOrts/@axisname,forces]


(* ::Input::Initialization:: *)
momentAboutAxis[axis_?vector3dQ,moment_?vector3dQ]:=moment.axis/Norm[axis]
momentAboutAxis[
axis_?axisNameQ,
axesOrigin_?vector3dQ,
forces:{__?vector3dQ},
applicationpoints:{__?vector3dQ},
Optional[moments_,{}]]:=momentAboutAxis[
axisOrts[axis],axesOrigin,forces,applicationpoints,moments]

momentAboutAxis[
axes:{_?axisNameQ..},
axesOrigin_?vector3dQ,
forces:{__?vector3dQ},
applicationpoints:{__?vector3dQ},
Optional[moments_,{}]]:=momentAboutAxis[
axisOrts[#],axesOrigin,forces,applicationpoints,moments]&/@axes

momentAboutAxis[
axis_?vector3dQ,
axesOrigin_?vector3dQ,
forces:{__?vector3dQ},
applicationpoints:{__?vector3dQ},
Optional[moments_,{}]]:=
momentAboutAxis[axis,
Total[
(moment/@Transpose[{radiusVectors[axesOrigin,applicationpoints],forces}])~Join~moments
]]


(* ::Input::Initialization:: *)
momentAboutAxisEquation[
axes:{_?vector3dQ..},
axesOrigin_?vector3dQ,
forces:{__?vector3dQ},
applicationpoints:{__?vector3dQ},
Optional[moments_,{}]]:=momentAboutAxis[
#,axesOrigin,forces,applicationpoints,moments]==0&/@axes

momentAboutAxisEquation[
axes:{_?axisNameQ..},
axesOrigin_?vector3dQ,
forces:{__?vector3dQ},
applicationpoints:{__?vector3dQ},
Optional[moments_,{}]]:=momentAboutAxisEquation[
axisOrts/@axes,axesOrigin,forces,applicationpoints,moments]


(* ::Input::Initialization:: *)
End[];


Protect@@Names["StaticEquilibrium`*"];


(* ::Input::Initialization:: *)
EndPackage[]


