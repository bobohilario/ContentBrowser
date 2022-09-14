

BeginPackage["Bob`ContentBrowser`"]

Bob`ContentBrowser`StandardizeContent

Begin["`Private`"]


Bob`ContentBrowser`StandardizeContent[gr_Graph,___]:=gr

Bob`ContentBrowser`StandardizeContent[as_Association]:=
    With[{data=Last@Reap[
        KeyValueMap[assocToGraphData,as];
        Sow[#->{"Root"->True},"annotations"]&/@Keys[as],
        {"vertices","edges","annotations"}
        ]},
        Graph[Flatten@data[[1]],Flatten@data[[2]],AnnotationRules->Flatten@data[[3]]]
    ]

assocToGraphData[key_,info:KeyValuePattern[{"Label"->label_}]]:=
   ( Sow[key,"vertices"];Sow[key->{"Information"->info},"annotations"])

assocToGraphData[key_,assoc_Association]:=(
    Sow[key,"vertices"];
    Sow[
    Thread[key->Keys[assoc]],"edges"];
    KeyValueMap[assocToGraphData,assoc];
)


End[]
EndPackage[]