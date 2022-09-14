

BeginPackage["Bob`ContentBrowser`"]
Begin["`Private`"]

Bob`ContentBrowser`PacletObjectBrowser[args___]:=Catch[
    PacletObject;
    PacletSiteUpdate/@PacletSites[];
    pacletObjectBrowser[args]]

pacletObjectBrowser[]:=pacletObjectBrowser["Local"]

pacletObjectBrowser[prop:("Local"|"UpdateAvailable"), rest___]:=With[{allinfo = With[{ps = DeleteDuplicatesBy[PacletFind[All],#["Name"]&]},
   Module[{n = Length[ps]},
    Progress`EvaluateWithProgress[
     Select[Table[Quiet[
        With[{p=ps[[i]]},
            {p["Name"],p["Version"],First[PacletFindRemote[p["Name"]],<|All->{}|>][All]}
        ]
     ], {i, n}], 
      MatchQ[{_String,_,_}]], <|"Text" -> 
       "Comparing local versions to latest available versions", 
      "Progress" -> (i - 1)/n,
      "ItemCurrent" -> i, "ItemTotal" -> n|>]
    ]
   ]},
    pacletObjectBrowser[Global`$data=allinfo,prop, rest]
]

pacletObjectBrowser[info:{{_,_,_}..},prop_]:=
                  Panel@ makeResourceBrowserGrid[DeleteMissing[
                       makePacletBrowserPanel/@filterPacletInfo[info,prop]
                      ]]

makePacletBrowserPanel[{name_,v_,KeyValuePattern[{"Version"->v_,"Location"->loc_}]}]:=
    {pacletnameversiongrid[name,v],"Latest"}

makePacletBrowserPanel[{name_,old_,KeyValuePattern[{"Version"->new_,"Location"->loc_}]}]:=
    {pacletnameversiongrid[name,old],pacletUpdateButton[name, new, loc]}

makePacletBrowserPanel[{name_,old_,_}]:={pacletnameversiongrid[name,old],
    SpanFromLeft
    }

makePacletBrowserPanel[___]:=Missing[]

pacletnameversiongrid[name_,v_]:=Grid[{{c2cPButton[name],versionlabel[v]}}]

versionlabel[v_]:=Style["("<>v<>")",10]

c2cPButton[name_String]:=ClickToCopy[Style[name,14,FontFamily->"Source Code Pro"],Defer[PacletObject[name]]]

pacletUpdateButton[name_,v_, loc_]:=DynamicModule[{label="Install "<>v<>" from "<>loc,en=True},
    Button[Dynamic@label,label=ProgressIndicator[Appearance->"Necklace"];label=updatePaclet[name,v];en=False,Enabled->Dynamic[en]]
    ]


pacletObjectBrowser[___]:=$Failed

$MaxPanels=20;


updatePaclet[name_,_]:=With[{update=PacletInstall[name]},
If[FailureQ[update],
    "Update Failed",
    "Updated to version "<>update["Version"]
]]

filterPacletInfo[info_,"Local"]:=info
filterPacletInfo[info_,"UpdateAvailable"]:=Select[info,MatchQ[{_,local_,KeyValuePattern[{"Version"->remote_}]}/;(ResourceFunction["VersionOrder"][local,remote]===1)]]

End[]
EndPackage[]