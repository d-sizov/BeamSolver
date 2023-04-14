(* ::Package:: *)

BeginPackage["MyPlots`"]


myPlot;
myLogPlot;
myListPlot3D;
myListPlot;
plots;
domain;


Begin["`Private`"]


ClearAll@myPlot
SetAttributes[myPlot,HoldFirst];
myPlot[fun_,limits_,opts:OptionsPattern[]]:=Plot[fun,limits,opts,GridLines->Automatic,
Frame->True,PlotStyle->Thick,PlotRange->Full,RotateLabel->False,
LabelStyle->{Directive[Black,Bold],12}, ImageSize->400,ImagePadding->{{100,30},{Automatic,Automatic}}]


ClearAll@myLogPlot
SetAttributes[myLogPlot,HoldFirst];
myLogPlot[fun_,limits_,opts:OptionsPattern[]]:=LogPlot[fun,limits,opts,GridLines->Automatic,
Frame->True,PlotStyle->Thick,PlotRange->Full,RotateLabel->False,
LabelStyle->{Directive[Black,Bold],12}, ImagePadding->{{100,30},{Automatic,Automatic}}]


ClearAll@myListPlot3D
myListPlot3D[pts_,opts:OptionsPattern[]]:=ListPlot3D[pts,opts,
PlotRange->Full,LabelStyle->{Directive[Black,Bold],12}, ImagePadding->{{100,30},{Automatic,Automatic}}(*  <-- ImagePadding \:043f\:043e\:0437\:0432\:043e\:043b\:044f\:0435\:0442 \:0432\:044b\:0440\:043e\:0432\:043d\:044f\:0442\:044c \:0433\:0440\:0430\:0444\:0438\:043a\:0438 \:043f\:043e \:043b\:0435\:0432\:043e\:0439 \:0441\:0442\:043e\:0440\:043e\:043d\:0435 \:043a\:043e\:0440\:0440\:0435\:043a\:0442\:043d\:043e! http://reference.wolfram.com/language/howto/AlignPlotsWithEachOther.html *)]


ClearAll@myListPlot
myListPlot[pts_,opts:OptionsPattern[]]:=ListPlot[pts,opts,
Frame->True,RotateLabel->False,PlotMarkers->{Automatic,Medium},GridLines->Automatic,PlotRange->Full,LabelStyle->{Directive[Black,Bold],12}, ImagePadding->{{70,30},{Automatic,Automatic}}]


ClearAll@grid
grid[plotarray_,opts:OptionsPattern[]]:=(Grid[ArrayReshape[plotarray,{Length@plotarray,1}](*,Spacings->{Automatic,1}*),opts])


(* \:043f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:0433\:0440\:0430\:0444\:0438\:043a\:043e\:0432 \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:043e\:043d\:043d\:044b\:0445 \:0444\:0443\:043d\:043a\:0446\:0438\:0439, \:043f\:043e\:043b\:0443\:0447\:0430\:044e\:0449\:0438\:0445\:0441\:044f \:0432 \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:0435 \:0440\:0435\:0448\:0435\:043d\:0438\:044f \:0414\:0423 *)


(* ::Input::Initialization:: *)
ClearAll@domain
domain[ndSolutionRule:Rule[x_[t_],InterpolatingFunction_[t_]]]:=domain@Head@ndSolutionRule[[2]](*First@(Head@ndSolutionRule[[2]])["Domain"]*)

domain[ndSolutionRule:Rule[x_,InterpolatingFunction_]]:=(*First@(ndSolutionRule[[2]])["Domain"]*)domain@ndSolutionRule[[2]]

domain[arg:InterpolatingFunction_[t_]]:=First@(Head@arg)["Domain"]

domain[arg:InterpolatingFunction_]:=First@arg["Domain"]


(* ::Input::Initialization:: *)
ClearAll@symbol
symbol[ndSolutionRule:Rule[x_[t_],InterpolatingFunction_[t_]]]:=Head@(ndSolutionRule[[1]])
symbol[ndSolutionRule:Rule[x_,InterpolatingFunction_]]:=ndSolutionRule[[1]]


ClearAll@removeContextName
removeContextName[arg_]:=Last@StringSplit[ToString[arg],"`"]


(* ::Input::Initialization:: *)
ClearAll@plot
plot[ndSolutionRule:Rule[x_[t_],InterpolatingFunction_[t_]],opts:OptionsPattern[]]:=myPlot[(ndSolutionRule[[1]])/.ndSolutionRule,{t}~Join~domain[ndSolutionRule],FrameLabel->{removeContextName@t,removeContextName@symbol@ndSolutionRule},opts]

plot[ndSolutionRule:Rule[x_[t_],InterpolatingFunction_[t_]],dom_,opts:OptionsPattern[]]:=myPlot[(ndSolutionRule[[1]])/.ndSolutionRule,{t}~Join~dom,FrameLabel->{removeContextName@t,removeContextName@symbol@ndSolutionRule},opts]

plot[ndSolutionRule:Rule[x_,InterpolatingFunction_],opts:OptionsPattern[]]:=myPlot[((ndSolutionRule[[1]])/.ndSolutionRule)[t],{t}~Join~domain[ndSolutionRule],FrameLabel->{removeContextName@t,removeContextName@symbol@ndSolutionRule},opts]


(* ::Input::Initialization:: *)
ClearAll@plots
plots[ndsolution:{_Rule..},opts:OptionsPattern[]]:=Column[plot[#,opts]&/@ndsolution]

plots[ndsolution:{_Rule..},dom_,opts:OptionsPattern[]]:=Column[plot[#,dom,opts]&/@ndsolution]


End[]


EndPackage[]
