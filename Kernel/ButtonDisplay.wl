

BeginPackage["Bob`ContentBrowser`"]

Bob`ContentBrowser`GenerateItem
Bob`ContentBrowser`ButtonDisplay
Begin["`Private`"]


GenerateItem[browserID_,heldsym_, contentid_, gr_] := generateitem[browserID,heldsym, contentid, gr]

generateitem[browserID_,heldsym_, contentid_, gr_]:=Catch[
        With[{info = AnnotationValue[{gr, contentid}, "Information"]},
            With[{browse = browseButton[heldsym, contentid, info,gr], 
                ext= externalButton[contentid, info]},
                generateitem[browserID,_, contentid, _]=Panel[Grid[{{browse}, {ext}}]]
            ]
        ]
        ,
        "item"
    ]

browseButton[heldsym_,contentid_,gr_Graph]:=
browseButton[heldsym,contentid,AnnotationValue[{gr,contentid},"Information"],gr]

browseButton[Hold[heldsym_],contentid_,info_,gr_]:=With[{outs=VertexOutComponent[gr, {contentid},1]},
    Button[browseDisplay[contentid,info],heldsym=outs,Method->"Queued"]
]

browseButton[args___]:=Throw[Missing[],"item"]


browseDisplay[_,KeyValuePattern@{"Label"->label_}]:=label
browseDisplay[id_,_]:=browseDisplay[id,<|"Label"->id|>]
browseDisplay[args___]:=Throw[Missing[],"item"]

externalButton[_,KeyValuePattern@{"URL"->link_Hyperlink}]:=link
externalButton[_,KeyValuePattern@{"URL"->url_}]:=Hyperlink[url]
externalButton[args__]:=SpanFromAbove
End[]
EndPackage[]