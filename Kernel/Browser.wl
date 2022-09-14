

BeginPackage["Bob`ContentBrowser`"]
Begin["`Private`"]


Options[Browser]=Options[browser]={};

Bob`ContentBrowser`Browser[args___]:=Catch[
    browser[args],
    "ContentBrowser"
]

browser[content_,opts:OptionsPattern[]]:=With[{$browserID=Unique[]},
    iBrowser[$browserID,Bob`ContentBrowser`StandardizeContent[content,opts],opts]
]


iBrowser[browserID_,gr_Graph,opts___]:=
    buildBrowser[browserID,gr,rootContent[gr]]




rootContent[gr_]:=
Cases[AnnotationRules /. Options[gr, AnnotationRules], 
 HoldPattern[v_ -> KeyValuePattern[{"Root" -> True}]] :> v]


buildBrowser[browserID_,gr_,buttons0_]:=DynamicModule[{buttonids=buttons0},
Panel[
    Grid[{
        {
            Button[Style["Home",10],buttonids=buttons0,ImageSize->50]
        },
        {
        Dynamic[
            If[FreeQ[#,_Bob`ContentBrowser`GenerateItem],#,"Expired"]&@
                Row[DeleteMissing[Bob`ContentBrowser`GenerateItem[browserID,Hold[buttonids],#,gr]&/@buttonids]]
        ]
        }
    },Alignment->Left
    ]
]
]

End[]
EndPackage[]