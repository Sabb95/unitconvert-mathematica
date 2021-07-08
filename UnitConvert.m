(* ::Package:: *)

BeginPackage["UnitConvert`"]

toNaturalUnits::usage = "Convert the input to Natural units";
fromNaturalUnits::usage ="Convert the input to the Desired units";
getUnitsInterface::usage = "Display the interface";


Begin["`Private`"]


getDimension[qu_,unit_]:=Module[{result,dims},
dims = UnitDimensions[qu];
If[Position[dims,unit]=={},result=0,result = dims[[Position[dims,unit][[1,1]],2]]];
result
]


SItoNaturalFactor[qu_]:= Module[{\[Alpha],\[Beta],\[Gamma],\[Delta],\[Epsilon]},
\[Alpha]=getDimension[qu,"MassUnit"];\[Beta]=getDimension[qu,"LengthUnit"];\[Gamma]=getDimension[qu,"TimeUnit"];
\[Delta]=getDimension[qu,"ElectricCurrentUnit"];
\[Epsilon]=getDimension[qu,"TemperatureUnit"];
 Quantity[1,"ReducedPlanckConstant"]^(1/2 (2 \[Beta]+2 \[Gamma]-\[Delta]))*Quantity[1,"SpeedOfLight"]^(1/2 (-4 \[Alpha]+2 \[Beta]+\[Delta]))*Quantity[1,"BoltzmannConstant"]^-\[Epsilon]*Quantity[1,"ElectricConstant"]^(\[Delta]/2)
]


strToip[arg1_]:=Module[{ipf},ipf= Quiet@Check[ToString@InputForm@Quantity@arg1,"Check Units"];
If[SameQ[ipf,"Check Units"],Return["Check Units"]];
ipf=StringReplace[ipf,{"["-> "","]"->"","Quantity"->""}];StringSplit[ipf,","][[2]]]


convertToUnits[stri_]:=Module[{i,temp,t2,t1,quIter="1",size=Length[stri]},For[i=1,i<=size,i=i+1,   temp= StringSplit[stri[[i]],"^"] ;
t1=temp[[1]];
t1=strToip[t1];
If[SameQ[t1,"Check Units"],Return["Check Units"]];
t2=Quiet@Check[temp[[2]],"1"];
quIter=quIter<>"*"<>t1<>"^"<>t2;
quIter=StringReplace[quIter,"Difference"-> ""]];
quIter]


stringToUnits[str_]:=Module[{ numsplit,densplit,den,num,correctstr,splitstr},Which[UnsameQ[Head@str,String],correctstr=ToString[InputForm@str],SameQ[Head@str,String],correctstr=ToString[InputForm[ToExpression@str]]];
If[SameQ[correctstr,"1"],Return[1]];
splitstr=StringSplit[correctstr,"/"];
(*Print@splitstr;*)
num=Quiet@Check[StringDelete[splitstr[[1]],{"(",")"}],1];
den=Quiet@Check[StringDelete[splitstr[[2]],{"(",")"}],1];
numsplit=If[UnsameQ[num,"1"],StringSplit[num,"*"],1];
numsplit=convertToUnits@numsplit;
If[SameQ[numsplit,"Check Units"],Return["Check Units"]];
(*Print@numsplit;*)
(*Print[convertToUnits@numsplit];*)
densplit=Quiet@Check[StringSplit[den,"*"],1];
densplit=convertToUnits@densplit;
If[SameQ[densplit,"Check Units"],Return["Check Units"]];
densplit=StringReplace[densplit,"^"->"^-"];
(*Print@densplit;*)
Quiet@Check[Quantity[ToExpression[Quiet@Check[numsplit<>"*"<>densplit,convertToUnits[numsplit]]]],"Check Units"]
]


toNaturalUnits[quS_,natu_:"Electronvolts"]:= Module[{factor,natqu,qu},If[SameQ[Head@quS,Quantity],qu=quS,qu=stringToUnits[quS]];
If[SameQ[qu,"Check Units"],Return["Check Units"]];
factor = SItoNaturalFactor[qu];natqu =qu/factor;
 UnitConvert[natqu,natu^getDimension[natqu,"MassUnit"]]
]


fromNaturalUnits[natqu_,siqu_]:=Module[{factor,qu},factor=SItoNaturalFactor[siqu];
UnitConvert[natqu*factor,siqu]
]


getUnitsInterface[]:=Row[{Manipulate[Module[{temp},temp=mag*stringToUnits@string;Column[{Row[{"Input : ",temp}],Row[{"Output : ",ScientificForm@toNaturalUnits[temp,finalUnits]}]}]],{{mag,0,"Magnitude"},ControlType-> InputField},{{string,"Kilometer","Units"},ControlType-> InputField[String]},
{{finalUnits,"Electronvolts",""},{"Electronvolts"->"eV","Gigaelectronvolts"-> "GeV"},ControlType->RadioButton},
SaveDefinitions->True,FrameMargins->Tiny],
Manipulate[Module[{desunits,natunits},natunits=mag*stringToUnits@natstring;desunits=stringToUnits@desiredString;Column[{Row[{"Input : ",natunits}],Row[{"Output : ",Quiet@Check[ScientificForm@fromNaturalUnits[natunits,desunits],"Check Units"]}]}]],{{mag,0,"Magnitude"},ControlType-> InputField[FieldSize-> 2]},
{{natstring,"eV","Natural Units"},ControlType-> InputField[String]},{{desiredString,"eV","Desired Units"},ControlType-> InputField[String]},
SaveDefinitions->True,FrameMargins->Tiny]}]


End[]
EndPackage[]
